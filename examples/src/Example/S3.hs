{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Example.S3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Example.S3 where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString         (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary     as CB
import qualified Data.Conduit.List       as CL
import qualified Data.Foldable           as Fold
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text.IO            as Text
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.S3
import           System.IO

getPresignedURL :: Region     -- ^ Region to operate in.
                -> BucketName
                -> ObjectKey  -- ^ The source object key.
                -> IO ByteString
getPresignedURL r b k = do
    lgr <- newLogger Trace stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r
    ts  <- getCurrentTime
    runResourceT . runAWST env $
        presignURL ts 60 (getObject b k)

listAll :: Region -- ^ Region to operate in.
        -> IO ()
listAll r = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    let val :: ToText a => Maybe a -> Text
        val   = maybe "Nothing" toText

        lat v = maybe mempty (mappend " - " . toText) (v ^. ovIsLatest)
        key v = val (v ^. ovKey) <> ": " <> val (v ^. ovVersionId) <> lat v

    runResourceT . runAWST env $ do
        say "Listing Buckets .."
        bs <- view lbrsBuckets <$> send listBuckets
        say $ "Found " <> toText (length bs) <> " Buckets."

        forM_ bs $ \(view bName -> b) -> do
            say $ "Listing Object Versions in: " <> toText b
            paginate (listObjectVersions b)
                =$= CL.concatMap (view lrsVersions)
                 $$ CL.mapM_     (say . mappend " -> " . key)

getFile :: Region     -- ^ Region to operate in.
        -> BucketName
        -> ObjectKey  -- ^ The source object key.
        -> FilePath   -- ^ The destination file to save as.
        -> IO ()
getFile r b k f = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    runResourceT . runAWST env $ do
        rs <- send (getObject b k)
        view gorsBody rs `sinkBody` CB.sinkFile f
        say $ "Successfully Download: "
            <> toText b <> " - " <> toText k <> " to " <> toText f

putChunkedFile :: Region     -- ^ Region to operate in.
               -> BucketName -- ^ The bucket to store the file in.
               -> ObjectKey  -- ^ The destination object key.
               -> ChunkSize  -- ^ The chunk size to send.
               -> FilePath   -- ^ The source file to upload.
               -> IO ()
putChunkedFile r b k c f = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    runResourceT . runAWST env $ do
        bdy <- chunkedFile c f
        void . send $ putObject b k bdy
        say $ "Successfully Uploaded: "
           <> toText f <> " to " <> toText b <> " - " <> toText k

tagBucket :: Region              -- ^ Region to operate in.
          -> BucketName          -- ^ Name of the bucket to tag.
          -> [(ObjectKey, Text)] -- ^ List of K/V pairs to apply as tags.
          -> IO ()
tagBucket r  bkt xs = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    let tags = map (uncurry tag) xs
        kv t = toText (t ^. tagKey) <> "=" <> (t ^. tagValue)

    runResourceT . runAWST env $ do
        void . send $ putBucketTagging bkt (tagging & tTagSet .~ tags)
        say $ "Successfully Put Tags: " <> Fold.foldMap kv tags

        ts <- view gbtrsTagSet <$> send (getBucketTagging bkt)
        forM_ ts $ \t ->
            say $ "Found Tag: " <> kv t

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
