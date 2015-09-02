{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Example.S3
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Example.S3 where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import qualified Data.Foldable           as Fold
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text.IO            as Text
import           Network.AWS.Data
import           Network.AWS.S3
import           System.IO

listAll :: Region -- ^ Region to operate in.
        -> IO ()
listAll r = do
    lgr <- newLogger Debug stdout
    env <- newEnv r Discover <&> envLogger .~ lgr

    let say = liftIO . Text.putStrLn

        val :: ToText a => Maybe a -> Text
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
                =$= CL.concatMap (view lovrsVersions)
                 $$ CL.mapM_     (say . mappend " -> " . key)

tagBucket :: Region              -- ^ Region to operate in.
          -> BucketName          -- ^ Name of the bucket to tag.
          -> [(ObjectKey, Text)] -- ^ List of K/V pairs to apply as tags.
          -> IO ()
tagBucket r b xs = do
    lgr <- newLogger Debug stdout
    env <- newEnv r Discover <&> envLogger .~ lgr

    let say  = liftIO . Text.putStrLn
        tags = map (uncurry tag) xs
        kv t = toText (t ^. tagKey) <> "=" <> (t ^. tagValue)

    runResourceT . runAWST env $ do
        void . send $ putBucketTagging b (tagging & tTagSet .~ tags)
        say $ "Successfully Put Tags: " <> Fold.foldMap kv tags

        ts <- view gbtrsTagSet <$> send (getBucketTagging b)
        forM_ ts $ \t ->
            say $ "Found Tag: " <> kv t
