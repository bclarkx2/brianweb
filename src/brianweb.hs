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
import System.Environment


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
  , dir "css"   $ serveDirectory DisableBrowsing [] $ staticSubDir "css"
  , dir "img"   $ serveDirectory DisableBrowsing [] $ staticSubDir "img"
  , dir "files" $ serveFiles
  , dir "resume" $ resumePage
  , resumePage
  ]

{- Responses -}
greet = path $ \s -> ok $ toResponse $ (("Hello, " ++ s ++ "\n") :: String)
resumePage = serveFile (asContentType "text/html") $ staticPage "resume.html"

serveFiles =
  require getFilesPath $ \filesPath -> 
  serveDirectory EnableBrowsing [] filesPath


{- Navigation -}
staticDir :: String
staticDir = $(do
    dir <- runIO getCurrentDirectory
    filename <- loc_filename <$> location
    litE $ stringL $ dir ++ "/static")

staticSubDir :: String -> String
staticSubDir dir = staticDir ++ "/" ++ dir

staticPage :: String -> String
staticPage page = (staticSubDir "html") ++ "/" ++ page

getFilesPath :: IO (Maybe FilePath)
getFilesPath = lookupEnv "BRIANWEB_FILES"

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
