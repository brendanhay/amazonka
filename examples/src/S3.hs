{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module S3 where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as Fold
import Data.Generics.Product
import qualified Data.Text.IO as Text
import Data.Time
import Amazonka
import Amazonka.S3
import System.IO

getPresignedURL ::
  -- | Region to operate in.
  Region ->
  BucketName ->
  -- | The source object key.
  ObjectKey ->
  IO ByteString
getPresignedURL r b k = do
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set (field @"_envLogger") lgr . set (field @"_envRegion") r
  ts <- getCurrentTime
  runResourceT $ presignURL env ts 60 (newGetObject b k)

listAll ::
  -- | Region to operate in.
  Region ->
  IO ()
listAll r = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set (field @"_envLogger") lgr . set (field @"_envRegion") r

  let val :: ToText a => Maybe a -> Text
      val = maybe "Nothing" toText

      lat v = maybe mempty (mappend " - " . toText) (v ^. field @"isLatest")
      key v = val (v ^. field @"key") <> ": " <> val (v ^. field @"versionId") <> lat v

  runResourceT $ do
    say "Listing Buckets .."
    Just bs <- view (field @"buckets") <$> send env newListBuckets
    say $ "Found " <> toText (length bs) <> " Buckets."

    forM_ bs $ \(view (field @"name") -> b) -> do
      say $ "Listing Object Versions in: " <> toText b
      runConduit $
        paginate env (newListObjectVersions b)
          .| CL.concatMap (toListOf $ field @"versions" . _Just . folded)
          .| CL.mapM_ (say . mappend " -> " . key)

getFile ::
  -- | Region to operate in.
  Region ->
  BucketName ->
  -- | The source object key.
  ObjectKey ->
  -- | The destination file to save as.
  FilePath ->
  IO ()
getFile r b k f = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set (field @"_envLogger") lgr . set (field @"_envRegion") r

  runResourceT $ do
    rs <- send env (newGetObject b k)
    view (field @"body") rs `sinkBody` CB.sinkFile f
    say $
      "Successfully Download: "
        <> toText b
        <> " - "
        <> toText k
        <> " to "
        <> toText f

putChunkedFile ::
  -- | Region to operate in.
  Region ->
  -- | The bucket to store the file in.
  BucketName ->
  -- | The destination object key.
  ObjectKey ->
  -- | The chunk size to send env.
  ChunkSize ->
  -- | The source file to upload.
  FilePath ->
  IO ()
putChunkedFile r b k c f = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set (field @"_envLogger") lgr . set (field @"_envRegion") r

  runResourceT $ do
    bdy <- chunkedFile c f
    void . send env $ newPutObject b k bdy
    say $
      "Successfully Uploaded: "
        <> toText f
        <> " to "
        <> toText b
        <> " - "
        <> toText k

tagBucket ::
  -- | Region to operate in.
  Region ->
  -- | Name of the bucket to tag.
  BucketName ->
  -- | List of K/V pairs to apply as tags.
  [(ObjectKey, Text)] ->
  IO ()
tagBucket r bkt xs = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set (field @"_envLogger") lgr . set (field @"_envRegion") r

  let tags = map (uncurry newTag) xs
      kv t = toText (t ^. field @"key") <> "=" <> (t ^. field @"value")

  runResourceT $ do
    void . send env $ newPutBucketTagging bkt (newTagging & field @"tagSet" .~ tags)
    say $ "Successfully Put Tags: " <> Fold.foldMap kv tags

    ts <- view (field @"tagSet") <$> send env (newGetBucketTagging bkt)
    forM_ ts $ \t ->
      say $ "Found Tag: " <> kv t

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
