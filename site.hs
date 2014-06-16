--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat, (<>))
import           Hakyll
import           Text.Pandoc.Options
import           Data.List(intersperse)
import qualified Data.Set                    as Set
import qualified Data.Map                    as M
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    {- Build Tags (unused for now, but handy) -}
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    
    {- Simple alias -}
    let postCtx' = postCtx tags

    {- A bit redundant given archive.html -}
    tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots pattern "content"
        let title = "Tag: " ++ tag
            tagsCtx =
              listField  "posts" postCtx' (return posts)  <>
              constField "title" title                    <>
              defaultContext
        makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" tagsCtx
              >>= loadAndApplyTemplate "templates/default.html" tagsCtx
              >>= relativizeUrls

    {- Images -}
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    {- Stylesheets -}
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    {- Javascript Scripts -}    
    match "js/*" $ do
        route idRoute
        compile copyFileCompiler
   
   {- Static Pages -}
    let static = do
        route $ setExtension "html"
        compile $ pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

    match "contact.markdown" static
    match "cv.markdown" static

    match (fromList ["4Space.html", "What-the-Haskell.pdf", "wth.html"]) $ do
      route idRoute
      compile copyFileCompiler

    {- Single Posts -}
    match "posts/*" $ do
        let blogPostCtx = postCtx' <> (constField "post_full" "true")
        route $ setExtension "html"
        compile $ mathJaxPandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post_body.html"      blogPostCtx
            >>= loadAndApplyTemplate "templates/disqus_partial.html" blogPostCtx
            >>= loadAndApplyTemplate "templates/default.html"        blogPostCtx
            >>= relativizeUrls

    {- List all post headers -}
    create [ "archive.html" ] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                  listField  "posts" postCtx' (return posts)  <>
                  constField "title" "All Posts"              <>
                  defaultContext
          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls

    {- List most recent posts w/ body included-}
    create [ "index.html" ] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
        let blogCtx =
              listField  "posts"   postCtx' (return (take 4 posts) ) <>
              constField "title"   "Ramblings"                       <>
              defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/post_snip.html" blogCtx
            >>= loadAndApplyTemplate "templates/default.html"   blogCtx
            >>= relativizeUrls

    {- Make Atom/RSS Feeds -}
    let mkFeed render = do
        route idRoute
        compile $ do
            let feedCtx = postCtx' <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            render feedConfig feedCtx posts

    create ["rss.xml" ] (mkFeed renderRss ) -- Parse error atm :(
    create ["atom.xml"] (mkFeed renderAtom)

    {- Load Templates -}
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%B %e, %Y" <>
    tagsField "tags" tags        <>
    defaultContext

postCtxNoTags :: Context String
postCtxNoTags = dateField "date" "%B %e, %Y" <> defaultContext

tagsField' :: String -> Tags -> Context a
tagsField' = tagsFieldWith getTags renderTag mconcat
  where -- (modified from Hakyll.Web.Tags source)
        renderTag :: String -> (Maybe FilePath) -> Maybe H.Html
        renderTag _   Nothing         = Nothing
        renderTag tag (Just filePath) =
          Just $ H.li ! A.class_ "tag" 
               $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

mathJaxPandocCompiler :: Compiler (Item String)
mathJaxPandocCompiler = pandocCompilerWith 
                          defaultHakyllReaderOptions
                          defaultHakyllWriterOptions
                          { writerHTMLMathMethod = MathJax "" }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Abstract Nonsense"
  , feedDescription = "Ramblings on Programming"
  , feedAuthorName  = "Benjamin Kovach"
  , feedAuthorEmail = "bkovach5@uga.edu"
  , feedRoot        = "http://5outh.github.io"
  }