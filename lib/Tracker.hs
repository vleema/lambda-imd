{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tracker where

import Control.Monad (filterM, forM_)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Text as T (Text, pack, split, unlines, unpack)
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (Connection, FromRow, ToRow, execute, execute_, field, fromRow, query, query_, toRow)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Network.HaskellNet.SMTP.SSL (AuthType (LOGIN), authenticate, doSMTPPort, sendMail)
import Network.Mail.Mime (Address (Address), simpleMail)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H (Html, a, body, docTypeHtml, h2, hr, p, stringValue, toHtml, (!))
import Text.Blaze.Html5.Attributes as A (href)
import Text.HTML.Scalpel
import Text.Read (readMaybe)

baseURL :: String
baseURL = "https://portal.imd.ufrn.br"

createSchema :: Connection -> IO ()
createSchema conn = do
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS notices (\
        \id INTEGER PRIMARY KEY,\
        \title TEXT NOT NULL,\
        \desc TEXT,\
        \url TEXT NOT NULL)"
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS subscribers (\
        \uuid UUID PRIMARY KEY,\
        \mail TEXT NOT NULL,\
        \verified BOOLEAN NOT NULL,\
        \created_at TEXT NOT NULL)"

data SMTPConfig = SMTPConfig
    { smtpHost :: String
    , smtpPort :: Int
    , smtpUser :: String
    , smtpPass :: String
    , fromEmail :: Address
    }
    deriving (Show)

data Notice = Notice
    { id :: Int
    , title :: Text
    , desc :: Text
    , url :: String
    }
    deriving (Show)

instance FromRow Notice where
    fromRow = Notice <$> field <*> field <*> field <*> field

instance ToRow Notice where
    toRow (Notice _id title desc url) = toRow (title, desc, url)

saveNotice :: Notice -> Connection -> IO ()
saveNotice n conn = do
    execute conn "INSERT INTO notices (title, desc, url) VALUES (?, ?, ?)" n

fetchNotices :: IO [Notice]
fetchNotices = do
    refs <- scrapeURL (baseURL ++ "/portal/editais") fetchNoticeRefs
    sequence $ maybe [] (toNotice <$>) refs
  where
    toNotice :: (Text, Text) -> IO Notice
    toNotice (ref, title) = do
        -- FIXME: This should fail gracefully and log, not panic the program.
        let id = fromJust $ readMaybe $ unpack $ last $ split (== '/') ref
        let url = baseURL ++ unpack ref
        desc <- scrapeURL url fetchDesc
        return
            Notice
                { id = id
                , title = title
                , desc = fromMaybe "" desc
                , url = url
                }

    fetchNoticeRefs :: Scraper Text [(Text, Text)]
    fetchNoticeRefs =
        chroot
            ( "div"
                @: [hasClass "box-editais-andamentos"]
                // "div"
                @: [hasClass "card"]
                // "div"
                @: [hasClass "card-body"]
            )
            $ chroots "a"
            $ do
                ref <- attr "href" "a"
                title <- text $ "span" @: [hasClass "badge"]
                return (ref, title)

    fetchDesc :: Scraper Text Text
    fetchDesc = text $ "div" @: [hasClass "conteudo-noticia"]

filterNewNotices :: Connection -> [Notice] -> IO [Notice]
filterNewNotices conn = filterM isNew
  where
    isNew :: Notice -> IO Bool
    isNew (Notice id _ _ _) = do
        ret <- query conn "SELECT 1 FROM notices WHERE id = ? LIMIT 1" [id] :: IO [Notice]
        return $ null ret

buildNoticeHtmlMsg :: Notice -> Html
buildNoticeHtmlMsg (Notice _ title desc url) = do
    h2 $ toHtml title
    p $ toHtml desc
    a ! href (stringValue url) $ "Link do edital"
    hr

buildNoticeHtmlMsgList :: [Notice] -> Html
buildNoticeHtmlMsgList ns = H.docTypeHtml $ do
    body $ do
        mapM_ buildNoticeHtmlMsg ns

buildNoticeMsg :: Notice -> Text
buildNoticeMsg (Notice _ title desc url) =
    T.unlines
        [ title
        , ""
        , desc
        , ""
        , "LINK DO EDITAL"
        , pack url
        , pack $ replicate 50 '-'
        ]

buildNoticeMsgList :: [Notice] -> Text
buildNoticeMsgList = T.unlines . (buildNoticeMsg <$>)

-- TODO: Add unsubscribe option (unsubscribe endpoint with person token)
sendNotices :: [Notice] -> SMTPConfig -> Connection -> IO ()
sendNotices ns conf conn = do
    doSMTPPort (smtpHost conf) (fromIntegral $ smtpPort conf) $ \smtpConn -> do
        auth <- authenticate LOGIN (smtpUser conf) (smtpPass conf) smtpConn
        if not auth
            then putStrLn "auth failed"
            else do
                subs <- query_ conn "SELECT uuid, mail FROM subscribers" :: IO [Subscriber]
                let plain = buildNoticeMsgList ns
                let html = buildNoticeHtmlMsgList ns
                forM_ (filter verified subs) $ \sub -> do
                    msg <-
                        simpleMail
                            (Address Nothing $ pack $ mail sub)
                            (fromEmail conf)
                            (pack $ buildTitle $ length ns)
                            (fromStrict plain)
                            (renderHtml html)
                            []
                    sendMail msg smtpConn
  where
    buildTitle :: Int -> String
    buildTitle n = if n > 1 then show n ++ " novos editais foram abertos" else "Um novo edital foi aberto"

newtype UUID = UUID String
    deriving (FromField, ToField)

instance Show UUID where
    show (UUID s) = s

data Subscriber = Subscriber
    { uuid :: UUID
    , mail :: String
    , verified :: Bool
    , created_at :: UTCTime
    }
    deriving (Show)

instance FromRow Subscriber where
    fromRow = Subscriber <$> field <*> field <*> field <*> field

instance ToRow Subscriber where
    toRow (Subscriber uuid mail verified created_at) = toRow (uuid, mail, verified, created_at)

getSubscriber :: UUID -> Connection -> IO (Maybe Subscriber)
getSubscriber uuid conn = listToMaybe <$> query conn "SELECT * FROM subscribers WHERE uuid = ?" [uuid]

getSubscriberByMail :: String -> Connection -> IO (Maybe Subscriber)
getSubscriberByMail mail conn = listToMaybe <$> query conn "SELECT * FROM subscribers WHERE mail = ?" [mail]

saveSubscriber :: String -> Connection -> IO Subscriber
saveSubscriber mail conn = do
    currentTime <- getCurrentTime
    uuid <- UUID . toString <$> nextRandom
    execute conn "INSERT INTO subscribers (uuid, mail, verified, created_at) VALUES (?, ?, ?, ?)" (uuid, mail, False, currentTime)
    return $ Subscriber uuid mail False currentTime

verifySubscriber :: UUID -> Connection -> IO ()
verifySubscriber uuid conn = execute conn "UPDATE subscribers SET verified = true WHERE uuid = ?" [uuid]

deleteSubscriber :: UUID -> Connection -> IO ()
deleteSubscriber uuid conn = execute conn "DELETE from subscribers WHERE uuid = ?" [uuid]
