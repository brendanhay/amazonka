module Gen.IO where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import Gen.Output.Template (Template)
import qualified Gen.Output.Template as Template
import Gen.Prelude
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Text.EDE as EDE
import qualified UnliftIO
import qualified UnliftIO.Directory as UnliftIO

title :: (MonadIO m) => String -> m ()
title = liftIO . putStrLn

say :: (MonadIO m) => String -> m ()
say = title . mappend " -> "

done :: (MonadIO m) => m ()
done = liftIO (putStrLn "")

readBSFile :: (MonadIO m) => FilePath -> m ByteString
readBSFile path =
  say ("Reading " ++ path)
    >> liftIO (ByteString.readFile path)

writeLTFile :: (UnliftIO.MonadUnliftIO m) => FilePath -> Text.Lazy.Text -> m ()
writeLTFile path text = do
  say ("Writing " ++ path)
  UnliftIO.withFile path IO.WriteMode $ \handle ->
    liftIO $ do
      IO.hSetEncoding handle IO.utf8
      Text.Lazy.IO.hPutStr handle text

touchFile :: (UnliftIO.MonadUnliftIO m) => FilePath -> Text.Lazy.Text -> m ()
touchFile path text = do
  exists <- UnliftIO.doesFileExist path
  unless exists $
    writeLTFile path text

createDir :: (MonadIO m) => FilePath -> m ()
createDir dir = do
  exists <- UnliftIO.doesDirectoryExist dir
  unless exists $ do
    say ("Creating " ++ dir)
    UnliftIO.createDirectoryIfMissing True dir

copyDir :: (MonadIO m) => FilePath -> FilePath -> m ()
copyDir src dst =
  UnliftIO.listDirectory src >>= mapM_ copy
  where
    copy fsrc = do
      let fdst = dst </> FilePath.takeFileName fsrc

      say $
        "Copying "
          ++ fsrc
          ++ " to "
          ++ FilePath.takeDirectory fdst

      UnliftIO.copyFile fsrc fdst

readTemplate ::
  (MonadIO m) =>
  FilePath ->
  FilePath ->
  m EDE.Template
readTemplate dir name =
  liftIO $
    readBSFile (dir </> name)
      >>= EDE.parseWith EDE.defaultSyntax (EDE.includeFile dir) (fromString name)
      >>= EDE.result (UnliftIO.throwString . show) pure

readTypedTemplate ::
  (MonadIO m) =>
  (input -> HashMap Text Aeson.Value) ->
  FilePath ->
  FilePath ->
  m (Template input)
readTypedTemplate arguments dir name =
  liftIO $
    readBSFile (dir </> name)
      >>= Template.parseWith arguments (EDE.includeFile dir) (Text.pack name)
      >>= either UnliftIO.throwString pure
