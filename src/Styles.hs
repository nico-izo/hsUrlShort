{-# LANGUAGE QuasiQuotes #-}

module Styles (mainCss) where

import Text.Lucius

render = undefined

mainCss' = [lucius|
.form-urlshort {
    max-width: 550px;
    padding: 15px;
    margin: 0 auto;
    margin-top: 25vh;
    
    * {
        margin: 15px 0 15px 0;
    }
}
|]

mainCss = renderCss $ mainCss' render
