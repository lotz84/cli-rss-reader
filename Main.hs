{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text.Lens
import Data.Yaml.YamlLight
import Data.Yaml.YamlLight.Lens
import Graphics.Vty
import Network.HTTP.Conduit
import System.Process
import Text.XML
import Text.XML.Lens
import qualified Text.XML.Lens as XML

data RSS = RSS { _title :: String, _url :: String } deriving Show

getRSSList :: IO [RSS]
getRSSList = do
    yaml <- parseYamlFile "rss.yml"
    let y2r y = do
            title <- y ^? key "title" . _Yaml
            url   <- y ^? key "url"   . _Yaml
            return $ RSS title url
    return $ yaml ^.. each . folding y2r

data AppException = AppEscape

type App = ExceptT AppException (ReaderT Vty IO)

runApp :: Vty -> App a -> IO (Either AppException a)
runApp vty = (flip runReaderT vty) . runExceptT

selectionView :: [RSS] -> Int -> App RSS
selectionView rssList selecting = do
    vty <- ask
    let header       = string (defAttr `withStyle` underline) "閲覧するRSSを選択してください"
        tableStyle n = if n == selecting then defAttr `withStyle` reverseVideo else defAttr
        table        = vertCat $ map (\(rss, n) -> string (tableStyle n) (_title rss)) $ zip rssList [0..]
        pic          = picForImage $ header <-> table
    liftIO $ update vty pic
    e <- liftIO $ nextEvent vty
    case e of
        EvKey KEsc        _ -> throwError AppEscape
        EvKey KEnter      _ -> return $ rssList !! selecting
        EvKey (KChar 'j') _ -> selectionView rssList (min (length rssList - 1) (selecting + 1))
        EvKey (KChar 'k') _ -> selectionView rssList (max 0 (selecting - 1))
        _                   -> selectionView rssList selecting

loadingView :: RSS -> App Document
loadingView rss = do
    vty    <- ask
    result <- liftIO $ newEmptyMVar
    liftIO . forkIO $ do
        body <- simpleHttp (_url rss)
        let doc = parseLBS_ def body
        putMVar result doc
    liftIO . ($ 0) . fix $ \loop n -> do
        let gauge = string defAttr $ "Downloading" ++ take n (repeat '.')
            pic   = picForImage gauge
        update vty pic
        threadDelay 200000
        doc <- tryTakeMVar result
        case doc of
            Nothing  -> loop (n+1)
            Just doc -> return doc

data RSSFeedViewAction = RSSFeedViewBack | RSSFeedViewPreview String

rssfeedView :: Document -> Int -> App RSSFeedViewAction
rssfeedView doc selecting = do
    vty <- ask
    let title = maybe "no title" id $ doc ^? root ./ el "channel" ./ el "title" . XML.text . unpacked
        items = doc ^.. root ./ el "channel" ./ el "item" ./ el "title" . XML.text .unpacked
        header    = string (defAttr `withStyle` underline) $ title
        tableStyle n  = if n == selecting then defAttr `withStyle` reverseVideo else defAttr
        table = vertCat $ map (\(item, n) -> string (tableStyle n) item) $ zip items [0..]
        pic       = picForImage $ header <-> table
    liftIO $ update vty pic
    e <- liftIO $ nextEvent vty
    case e of
        EvKey KEsc        _ -> return RSSFeedViewBack
        EvKey KEnter      _ -> do
            let url = (!! selecting) $ doc ^.. root ./ el "channel" ./ el "item" ./ el "link" . XML.text . unpacked
            return $ RSSFeedViewPreview url
        EvKey (KChar 'j') _ -> rssfeedView doc (min (length items - 1) (selecting + 1))
        EvKey (KChar 'k') _ -> rssfeedView doc (max 0 (selecting - 1))
        _                   -> rssfeedView doc selecting

previewView :: String -> App ()
previewView url = do
    liftIO $ createProcess $ shell $ "open " ++ url
    return ()

main :: IO ()
main = do
    vty <- standardIOConfig >>= mkVty
    rssList <- getRSSList
    runApp vty . forever $ do
        rss <- selectionView rssList 0
        doc <- loadingView rss
        fix $ \loop -> do
            act <- rssfeedView doc 0
            case act of
                RSSFeedViewBack        -> return ()
                RSSFeedViewPreview url -> previewView url >> loop
    shutdown vty
