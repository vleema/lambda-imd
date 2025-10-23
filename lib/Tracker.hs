{-# LANGUAGE OverloadedStrings #-}

module Tracker where

import Control.Monad (forM_)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack, split, unpack)
import Data.Text.IO qualified as TIO (putStrLn)
import Data.Text.Lazy (fromStrict)

import Database.SQLite.Simple (Connection, FromRow, ToRow, execute, execute_, field, fromRow, query_, toRow)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.Types (Only (..))

import Network.HaskellNet.SMTP.SSL (AuthType (PLAIN), authenticate, doSMTPSSL, sendMail)
import Network.Mail.Mime (Address (Address), simpleMail')

import Text.HTML.Scalpel
import Text.Read (readMaybe)

baseURL :: String
baseURL = "https://portal.imd.ufrn.br"

createSchema :: Connection -> IO ()
createSchema conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS notices  (id INTEGER PRIMARY KEY, title TEXT NOT NULL, desc TEXT, url TEXT NOT NULL)"
    execute_ conn "CREATE TABLE IF NOT EXISTS subscribers (uuid UUID PRIMARY KEY, mail TEXT NOT NULL)"

data SMTPConfig = SMTPConfig
    { smtpHost :: String
    , smtpPort :: Int
    , smtpUser :: String
    , smtpPass :: String
    , fromEmail :: Address
    }

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

{-
 TODO: Change mail body to html
 TODO: Pass attachements in the email
-}
sendNotice :: Notice -> SMTPConfig -> Connection -> IO ()
sendNotice n conf conn = do
    subscribers <- query_ conn "SELECT uuid, mail FROM subscribers" :: IO [Subscriber]
    forM_ subscribers $ \sub -> do
        let mailMsg =
                simpleMail'
                    (Address Nothing $ pack $ mail sub)
                    (fromEmail conf)
                    (title n)
                    (fromStrict $ desc n)
        doSMTPSSL (smtpHost conf) $ \smtpConn -> do
            authSucceed <- authenticate PLAIN (smtpUser conf) (smtpPass conf) smtpConn
            if authSucceed
                then sendMail mailMsg smtpConn
                else TIO.putStrLn "SMTP authentication failed"

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
        $ do
            hrefs <- attrs "href" "a"
            titles <- texts $ "span" @: [hasClass "badge"]
            return $ zip hrefs titles

fetchDesc :: Scraper Text Text
fetchDesc = text $ "div" @: [hasClass "conteudo-noticia"]

fetchNotices :: IO [Notice]
fetchNotices = do
    refs <- scrapeURL (baseURL ++ "/portal/editais") fetchNoticeRefs
    sequence $ maybe [] mapNotices refs
  where
    mapNotices :: [(Text, Text)] -> [IO Notice]
    mapNotices headers = toNotice <$> headers

    toNotice :: (Text, Text) -> IO Notice
    toNotice (ref, title) = do
        -- FIXME: This should fail gracefully and log, not panic the program.
        let id' = fromJust $ readMaybe $ unpack $ last $ split (== '/') ref
        let url = baseURL ++ unpack ref
        desc <- scrapeURL url fetchDesc
        return
            Notice
                { id = id'
                , title = title
                , desc = fromMaybe "" desc
                , url = url
                }

newtype UUID = UUID Text
    deriving (FromField, ToField)

data Subscriber = Subscriber
    { uuid :: UUID
    , mail :: String
    }

instance FromRow Subscriber where
    fromRow = Subscriber <$> field <*> field

instance ToRow Subscriber where
    toRow (Subscriber subscriberUuid mail) = toRow (subscriberUuid, mail)

saveSubscriber :: Subscriber -> Connection -> IO ()
saveSubscriber s conn = execute conn "INSERT INTO subscribers (uuid, mail) VALUES (?, ?)" s

deleteSubscriber :: Text -> Connection -> IO ()
deleteSubscriber uuid conn = execute conn "DELETE from subscribers WHERE uuid = ?" (Only uuid)
