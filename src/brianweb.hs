{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module Main where

{- Imports -}
import Happstack.Server
import Control.Monad

import Text.Blaze ((!))
import Text.Blaze (toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
  , dir "index" $ ok $ toResponse $ indexTemplate "index.css"
  , ok $ toResponse $ indexTemplate "index.css"
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
indexTemplate :: String -> H.Html
indexTemplate css =
  H.docTypeHtml $ do
    H.html $ do
      H.title $ htmlStr "Clarknet"
      H.meta ! A.httpEquiv "Content-Type"
             ! A.content "text/html;charset=utf-8"
      H.link ! A.rel "stylesheet"
             ! A.type_ "text/css"
             ! A.href (toValue $ "css/" <> css)
    H.body $ do
      H.div ! A.class_ "Banner" $ do
        H.p "Clarknet" ! A.class_ "BannerText"
      H.ul $ do
        H.li $ do
          H.a "Resume" ! A.href "resume"
        H.li $ do
          H.a "Files" ! A.href "files"

htmlStr :: String -> H.Html
htmlStr str = H.toHtml str

