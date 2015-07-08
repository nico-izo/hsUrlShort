{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Templates (indexTpl, notFoundTpl, doneTpl) where

import Text.Hamlet
import Data.Text.Lazy as T (Text (..), pack)

base :: Html -> Text -> Html
base body title = [shamlet|
$doctype 5
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>hsUrlShort — #{title}
    <body>
        #{body}
|]

indexTpl :: Html
indexTpl = base body "Main page"
           where body = [shamlet|
<h1>hsUrlShort 0.1.0.0
<form action="/" method="post">
    <input name="url" type="text" placeholder="Url to short" value="http://" />
    <input type="submit" />
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
<p>http://localhost:3000/#{url}
|]
