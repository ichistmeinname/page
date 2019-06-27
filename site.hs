--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration {
    destinationDirectory = "public"
}

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "sections/*" $ do -- compile templateBodyCompiler
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "default"
                >>= return . fmap demoteHeaders
                -- >>= loadAndApplyTemplate "templates/post.html" postCtx
                -- >>= loadAndApplyTemplate "templates/content.html" defaultContext
                -- >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "index.html" $ do
        route $ setExtension "html"
        compile $ do
          let indexContext =
                listField "sections" defaultContext (loadAll "sections/*") <>
                defaultContext
          pandocCompiler
            >>= applyAsTemplate indexContext
            -- >>= loadAndApplyTemplate "templates/sections.html" indexContext
            >>= loadAndApplyTemplate "templates/default.html" indexContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

makeHtml :: Rules ()
makeHtml = do
  route $ setExtension "html"
  compile $ pandocCompiler
      >>= applyAsTemplate defaultContext
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
