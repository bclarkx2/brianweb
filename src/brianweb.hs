{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module Main where

{- Imports -}
import Happstack.Server
import Control.Monad
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

import Data.Functor
import Language.Haskell.TH
import System.Directory
import System.FilePath

{- Conf -}
conf :: Conf
conf = Conf { port = 80
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
  , dir "css" $ serveDirectory DisableBrowsing [] $ staticDir "css"
  , dir "img" $ serveDirectory DisableBrowsing [] $ staticDir "img"
  , dir "resume" $ resumePage
  , resumePage
  ]

{- Responses -}
greet = path $ \s -> ok $ toResponse $ (("Hello, " ++ s ++ "\n") :: String)
resumePage = serveFile (asContentType "text/html") "../src/static/html/resume.html"


{- Navigation -}
static :: String
static = $(do
    dir <- runIO getCurrentDirectory
    filename <- loc_filename <$> location
    litE $ stringL $ dir)

staticDir :: String -> String
staticDir dir = "../src/static/" ++ dir



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
