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

    match "templates/*" $ compile templateBodyCompiler

    match "sections/pubs/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "default"
                >>= return . fmap demoteHeaders
                >>= relativizeUrls

    match "sections/*" $ do
        route   $ setExtension ".html"
        compile $ do
          pandocCompiler
                >>= saveSnapshot "default"
                >>= return . fmap demoteHeaders
                >>= relativizeUrls

    match "index.html" $ do
        route $ setExtension "html"
        compile $ do
          let ctxt = listField "pubs" defaultContext (loadAll "sections/pubs/*" >>= return . reverse) <>
                     defaultContext
          let indexContext =
                listField "sections" ctxt (loadAll "sections/*") <>
                defaultContext

          pandocCompiler
            >>= applyAsTemplate indexContext
            >>= loadAndApplyTemplate "templates/default.html" indexContext
            >>= relativizeUrls

    -- match "sections/3_publications.html" $ do
    --     route $ setExtension "html"
    --     compile $ do
    --       let indexContext =
    --             listField "pubs" defaultContext (loadAll "pubs/*") <>
    --             defaultContext
    --       pandocCompiler
    --         >>= applyAsTemplate indexContext
    --         >>= loadAndApplyTemplate "../templates/pubs.html" indexContext
    --         >>= relativizeUrls

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
