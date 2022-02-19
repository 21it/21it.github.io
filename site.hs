{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Hakyll
import System.FilePath

main :: IO ()
main = hakyllWith cfg $ do
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "fontawesome-free-5.12.1-web/css/**" $ do
    route idRoute
    compile compressCssCompiler
  match "fontawesome-free-5.12.1-web/webfonts/**" $ do
    route idRoute
    compile copyFileCompiler
  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler
  match "favicon/*" $ do
    route $ gsubRoute "favicon/" (const "")
    compile copyFileCompiler
  match "index.html" $ do
    route idRoute
    compile $ do
      blocks <- chronological =<< loadAll "index/*"
      let indexCtx =
            listField "blocks" defaultContext (return blocks)
              <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
  create ["about.html", "pricing.html", "blog.html", "contact.html"] $ do
    route idRoute
    compile $ do
      anchor <- takeBaseName . toFilePath <$> getUnderlying
      let ctx = constField "anchor" anchor <> defaultContext
      makeItem ""
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/index-redirect.html" ctx
        >>= relativizeUrls
  match "license.markdown" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
  match "blog/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
  create ["blog-archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "blog/*"
      let blogCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Blog Archive"
              <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/blog-archive.html" blogCtx
        >>= loadAndApplyTemplate "templates/default.html" blogCtx
        >>= relativizeUrls
  match "templates/*" $ compile templateBodyCompiler
  match "license.markdown" $ compile pandocCompiler
  match "index/*.markdown" $ compile pandocCompiler
  match "index/*.html" $
    compile $ do
      posts <- recentFirst =<< loadAll "blog/*"
      meta <- getMetadata =<< getUnderlying
      pairs <-
        mapM makeItem $
          makePairs $
            maybe [] id $
              lookupStringList "pairs" meta
      let pairCtx =
            field "left" (return . fst . itemBody)
              <> field "right" (return . snd . itemBody)
      let ctx =
            listField "posts" postCtx (return posts)
              <> listField "pairs" pairCtx (return pairs)
              <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= relativizeUrls

cfg :: Configuration
cfg =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

makePairs :: [a] -> [(a, a)]
makePairs =
  reverse . this []
  where
    this acc [] = acc
    this acc (x0 : x1 : xs) = this ((x0, x1) : acc) xs

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext
