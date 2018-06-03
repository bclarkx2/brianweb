{-# LANGUAGE OverloadedStrings #-}

module Main where

{- Imports -}
import Happstack.Server
import Control.Monad
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

{- Conf -}
conf :: Conf
conf = Conf { port = 8888
            , validator = Nothing
            , logAccess = Just logMAccess
            , timeout = 30
            , threadGroup = Nothing
          }

{- Main -}
main :: IO ()
main = simpleHTTP conf $ handlers

handlers = msum
  [ dir "hello" $ do method [GET, HEAD]
                     greet
  , ok $ toResponse $ homePage
  ]

{- Responses -}
greet = path (\s -> ok $ toResponse $ "Hello, " ++ s ++ "\n")


{- Templates -}
appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        body

{- Pages -}
homePage :: H.Html
homePage = appTemplate "h o me pag e" 
                       [H.meta ! A.name "keywords" ! A.content "happstack, blaze, html"] 
                       (H.p "HOME!")
