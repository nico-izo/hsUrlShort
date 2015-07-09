{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, EmptyDataDecls,
             FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where
import Data.Char (ord, chr)
import Data.Time.Clock
import Data.Text.Lazy as T (pack)
import Database.Persist
import Database.Persist.TH
import qualified Database.Persist.Sqlite as Db
import Text.Hamlet
import Web.Scotty as S
import Network.HTTP.Types
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Applicative (pure)
import System.Environment (getEnv)
import Control.Exception (try, SomeException)

import Templates

idToHash :: Int -> [Char]
idToHash i | i <= 0 = error "invalid id"
idToHash i = let numberOfChars = length ['a'..'z']
                 firstCharNum = ord 'a'
           in map (chr.(+ firstCharNum).(`mod` numberOfChars))
           $ reverse 
           $ takeWhile (/= 0) 
           $ iterate (`div` numberOfChars) i

hashToId :: [Char] -> Int
hashToId str | str == "" = error "empty string"
hashToId str = let numberOfChars = length ['a'..'z']
                   firstCharNum = ord 'a'
                   arrOfExps = reverse $ take (length str) $ iterate (* numberOfChars) 1
             in sum $ zipWith (*) arrOfExps $ map ((+ negate firstCharNum).(ord)) str

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ShortUrl
    origUrl String
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
        S.get "/" $ do
            html $ renderHtml indexTpl

        post "/" $ do
            (urlToShort :: String) <- param "url"
            urlId <- liftIO $ flip Db.runSqlPersistMPool pool $ Db.insert $ (ShortUrl urlToShort)
            html $ renderHtml $ doneTpl $ T.pack . idToHash . fromIntegral $ Db.fromSqlKey urlId

        S.get "/s/:urlHash" $ do
            (urlHash :: String) <- param "urlHash"
            unshortUrl <- liftIO $ flip Db.runSqlPersistMPool pool $ Db.get
                        $ (Db.toSqlKey $ fromIntegral $ hashToId urlHash)
            case unshortUrl of
                Just (ShortUrl u) -> do
                    status status302
                    setHeader "location" (T.pack u)
                Nothing -> do
                    status notFound404
                    html $ renderHtml notFoundTpl

        notFound $ do
            status notFound404
            html $ renderHtml notFoundTpl
