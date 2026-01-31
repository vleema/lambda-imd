{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (forkIO)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, open)
import Network.HTTP.Types.Status (status404, status409)
import Network.HaskellNet.SMTP.SSL (AuthType (LOGIN), authenticate, doSMTPPort, sendMail)
import Network.Mail.Mime (Address (Address), simpleMail)
import System.Cron
import System.Environment (getEnv)
import System.Log.FastLogger (LogType' (LogStdout), TimedFastLogger, defaultBufSize, newTimeCache, newTimedFastLogger, simpleTimeFormat)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H (ToValue (toValue), a, button, docTypeHtml, form, h1, hr, input, label, p, (!))
import Text.Blaze.Html5.Attributes as A (action, class_, href, method, name, type_, value)
import Text.Read (readMaybe)
import Tracker (
    SMTPConfig (SMTPConfig, fromEmail, smtpHost, smtpPass, smtpPort, smtpUser),
    Subscriber (mail, uuid, verified),
    UUID (..),
    createSchema,
    deleteSubscriber,
    deleteUnverifiedOlderThan,
    fetchNotices,
    filterNewNotices,
    getSubscriber,
    getSubscriberByMail,
    logError,
    logInfo,
    saveNotices,
    saveSubscriber,
    sendNotices,
    verifySubscriber,
 )
import Web.Scotty

initDB :: IO Connection
initDB = do
    conn <- open "db.sqlite3"
    createSchema conn
    return conn

smtpConfig :: IO SMTPConfig
smtpConfig = do
    host <- getEnv "SMTP_HOST"
    port <- fromMaybe (error "SMTP_PORT should be a integer") . readMaybe <$> getEnv "SMTP_PORT"
    user <- getEnv "SMTP_USER"
    pass <- getEnv "SMTP_PASS"
    from <- getEnv "SMTP_FROM"
    return
        SMTPConfig
            { smtpHost = host
            , smtpPort = port
            , smtpUser = user
            , smtpPass = pass
            , fromEmail = Address Nothing $ pack from
            }

contributing :: Html
contributing = do
    p $ do
        "Acesse "
        a ! href "https://codeberg.org/vleema/lambda-imd" $ "https://codeberg.org/vleema/lambda-imd"
        " para sugestões de melhorias, bugs e outros"
        hr

sendNewNotices :: Connection -> SMTPConfig -> TimedFastLogger -> IO ()
sendNewNotices conn conf logger = do
    logInfo logger "Started fetching and sending new notices"
    ns <- filterNewNotices conn =<< fetchNotices
    logInfo logger ("Found " <> pack (show $ length ns) <> " new notices")
    unless (null ns) $ do
        saveNotices ns conn
        sendNotices ns conf conn logger

sendVerificationMail :: Subscriber -> TimedFastLogger -> SMTPConfig -> IO ()
sendVerificationMail sub logger conf = do
    logInfo logger $ pack $ "Sending verification mail to " ++ mail sub
    doSMTPPort (smtpHost conf) (fromIntegral $ smtpPort conf) $ \smtpConn -> do
        auth <- authenticate LOGIN (smtpUser conf) (smtpPass conf) smtpConn
        if not auth
            then logError logger "Failed to authenticate with SMTP server"
            else do
                url <- getEnv "URL"
                let plain = "Accesse " ++ url ++ "/verify?uuid=" ++ show (uuid sub) ++ " para verificar seu email. O link expirará em 15 minutos."
                let html = renderHtml $ docTypeHtml $ do
                        p $ do
                            "Acesse "
                            a ! href (toValue $ url ++ "/verify?uuid=" ++ show (uuid sub)) $ "aqui"
                            " para verificar seu email. O link expirará em 15 minutos."
                msg <-
                    simpleMail
                        (Address Nothing $ pack $ mail sub)
                        (fromEmail conf)
                        (pack "Email de verificação")
                        (fromStrict $ pack plain)
                        html
                        []
                sendMail msg smtpConn

cleanupExpiredSubscribers :: Connection -> IO ()
cleanupExpiredSubscribers conn = do
    now <- getCurrentTime
    let cutoff = addUTCTime (-(15 * 60)) now
    deleteUnverifiedOlderThan cutoff conn

main :: IO ()
main = do
    loadFile defaultConfig
    conn <- initDB
    conf <- smtpConfig
    timeCache <- newTimeCache simpleTimeFormat
    (logger, _) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
    sendNewNotices conn conf logger

    _ <- execSchedule $ do
        addJob (sendNewNotices conn conf logger) "0 9 * * *"
        addJob (cleanupExpiredSubscribers conn) "*/5 * * * *"

    scotty 8080 $ do
        get "/" $ do
            html $ renderHtml $ docTypeHtml $ do
                contributing
                h1 "Observador de editais do IMD"
                p $ do
                    "Notificamos novos editais publicados em "
                    a ! href "https://www.imd.ufrn.br/portal/editais" $ "https://www.imd.ufrn.br/portal/editais"
                    " no seu email."
                p "Basta se cadastrar abaixo:"
                form ! method "POST" ! action "/subscribe" $ do
                    label "Seu email: "
                    input ! type_ "mail" ! name "mail"
                    input ! type_ "submit" ! value "Enviar"

        post "/subscribe" $ do
            mail <- formParam "mail"
            sub <- liftIO $ getSubscriberByMail mail conn
            case sub of
                Nothing -> do
                    sub <- liftIO $ saveSubscriber mail conn
                    _ <- liftIO $ forkIO $ sendVerificationMail sub logger =<< smtpConfig
                    html $ renderHtml $ docTypeHtml $ do
                        contributing
                        h1 "Cadastro realizado"
                        p "Verifique seu email para confirmar a inscrição. Seu cadastro irá expirar em 15 minutos se não verificar seu email."
                        p $ a ! href "/" $ "Voltar"
                Just _ -> do
                    status status409
                    html $ renderHtml $ docTypeHtml $ do
                        contributing
                        h1 "Email já cadastrado"
                        p "Este email já está cadastrado no sistema."
                        p $ a ! href "/" $ "Voltar"

        get "/verify" $ do
            uuid <- UUID <$> queryParam "uuid"
            html $ renderHtml $ docTypeHtml $ do
                contributing
                h1 "Verificar email"
                p "Clique no botão abaixo para confirmar seu endereço de email"
                form ! method "POST" ! action "/verify" $ do
                    input ! type_ "hidden" ! name "uuid" ! value (toValue $ show uuid)
                    button ! type_ "submit" ! class_ "large-button" $ "Verificar email"

        post "/verify" $ do
            uuid <- UUID <$> formParam "uuid"
            sub <- liftIO $ getSubscriber uuid conn
            case sub of
                Nothing -> do
                    status status404
                    html $ renderHtml $ docTypeHtml $ do
                        contributing
                        h1 "Identificador inválido"
                        p "O link de verificação é inválido ou expirou."
                        p $ a ! href "/" $ "Realize o cadastro novamente"
                Just sub -> do
                    if verified sub
                        then html $ renderHtml $ docTypeHtml $ do
                            contributing
                            h1 "Seu email já foi verificado"
                            p "Seu email já foi verificado, não precisa verifica-lo novamente."
                        else do
                            liftIO $ verifySubscriber uuid conn
                            liftIO $ logInfo logger $ pack $ "Subscriber " ++ mail sub ++ " just verified it's email"
                            html $ renderHtml $ docTypeHtml $ do
                                contributing
                                h1 "Verificação realizada"
                                p "Seu email foi verificado com sucesso!"

        get "/unsubscribe" $ do
            uuid <- queryParam "uuid" :: ActionM String
            html $ renderHtml $ docTypeHtml $ do
                contributing
                h1 "Desinscrever"
                p "Tem certeza que quer se desinscrever?"
                form ! method "POST" ! action "/unsubscribe" $ do
                    input ! type_ "hidden" ! name "uuid" ! value (toValue uuid)
                    button ! type_ "submit" $ "Desinscrever"

        post "/unsubscribe" $ do
            uuid <- UUID <$> formParam "uuid"
            sub <- liftIO $ getSubscriber uuid conn
            case sub of
                Nothing -> do
                    status status404
                    html $ renderHtml $ docTypeHtml $ do
                        contributing
                        h1 ""
                Just sub -> do
                    liftIO $ deleteSubscriber uuid conn
                    liftIO $ logInfo logger $ pack $ "Unsubscribed " ++ mail sub
                    html $ renderHtml $ docTypeHtml $ do
                        contributing
                        h1 "Desinscrição realizada"
                        p "Desinscrição realizada com sucesso!"
