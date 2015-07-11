{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Templates (indexTpl, notFoundTpl, doneTpl, infoTpl) where

import Text.Hamlet
import Data.Text.Lazy as T
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Data.Time.Format (formatTime)
import Data.Time.Clock (UTCTime)
import Data.Monoid
import Styles (mainCss)

base_ :: Html -> Text -> Text -> Html
base_ body title style = [shamlet|
$doctype 5
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <meta http-equiv="X-UA-Compatible" content="IE=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" />
        <style>
            #{style}
        <title>hsUrlShort — #{title}
    <body>
        <div class="container">
            #{body}
|]

base :: Html -> Text -> Html
base body title = base_ body title mainCss

indexTpl :: Html
indexTpl = base body "Main page"
           where body = [shamlet|
<form class="form-urlshort" action="/" method="post">
    <h2 class="form-shorturl-heading">Type your URL
    <label for="inputUrl" class="sr-only">Url to short
    <input name="url" type="url" id="inputUrl" class="form-control" placeholder="Url to short" required autofocus />
    <button class="btn btn-lg btn-primary btn-block" type="submit">Shorten!
|]
        
notFoundTpl :: Html
notFoundTpl = base body "404 Not Found"
              where body = [shamlet|
<h1>404 Not Found
<p>Sorry, but we can't find requested URL.
|]

doneTpl :: Text -> Html
doneTpl url = base body "Get your URL"
              where body = [shamlet|
<h1>Get your shortened link
<p>http://localhost:3000/s/#{url}
|]

infoTpl :: UTCTime -> Int -> Text -> Text -> Html
infoTpl time clicks url key = base body ("Information about #" <> key)
                          where body = [shamlet|
<h2>Some information about shortened #{url}
<ul>
    <li>
        <b>Created:
        #{formatTime defaultTimeLocale rfc822DateFormat time}
    <li>
        <b>Clicks:
        #{show clicks}
|]
