{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, EmptyDataDecls,
             FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Text.Lazy as T (pack)
import Database.Persist
import Database.Persist.TH
import qualified Database.Persist.Sqlite as Db
import Web.Scotty as S
import Network.HTTP.Types
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import System.Environment (getEnv)
import Control.Exception (try, SomeException)

import Templates
import Utils

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ShortUrl
    origUrl String
    created UTCTime
    followed Int
    deriving Show
|]

main :: IO ()
main = do
    port_ <- try (getEnv "PORT" >>= return . read) :: IO (Either SomeException Int)
    (port :: Int) <- case port_ of
        Right p -> return p
        Left _ -> return 3000
    runResourceT $ runStderrLoggingT $ Db.runSqlite "app.db" $ Db.runMigration migrateAll
    runStderrLoggingT $ Db.withSqlitePool "app.db" 10 $ \pool -> liftIO $ scotty port $ do
        S.get "/" $
            html $ renderHtml indexTpl

        post "/" $ do
            (urlToShort :: String) <- param "url"
            time <- liftIO getCurrentTime
            urlId <- liftIO $ flip Db.runSqlPersistMPool pool 
                   $ Db.insert $ ShortUrl (prefixHttp urlToShort) time 0
            html $ renderHtml $ doneTpl $ T.pack . idToHash . fromIntegral $ Db.fromSqlKey urlId

        S.get "/s/:urlHash" $ do
            (urlHash :: String) <- param "urlHash"
            let urlKey = Db.toSqlKey $ fromIntegral $ hashToId urlHash
            unshortUrl <- liftIO $ flip Db.runSqlPersistMPool pool $ Db.get urlKey
            case unshortUrl of
                Just (ShortUrl u _ _) -> do
                    liftIO $ flip Db.runSqlPersistMPool pool $ Db.updateWhere [ShortUrlId ==. urlKey] [ShortUrlFollowed +=. 1]
                    status status302
                    setHeader "location" (T.pack u)
                Nothing -> do
                    status notFound404
                    html $ renderHtml notFoundTpl

        S.get "/i/:urlHash" $ do
            (urlHash :: String) <- param "urlHash"
            let urlKey = Db.toSqlKey $ fromIntegral $ hashToId urlHash
            unshortUrl <- liftIO $ flip Db.runSqlPersistMPool pool $ Db.get urlKey
            case unshortUrl of
                Just (ShortUrl url time clicks) -> html $
                    renderHtml $ infoTpl time clicks (T.pack url) (T.pack urlHash)
                Nothing -> do
                    status notFound404
                    html $ renderHtml notFoundTpl

        notFound $ do
            status notFound404
            html $ renderHtml notFoundTpl
