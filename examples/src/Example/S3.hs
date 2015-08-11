{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Foldable           as Fold
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text.IO            as Text
import           Network.AWS.Data
import           Network.AWS.S3
import           System.IO

tagBucket :: BucketName          -- ^ Name of the bucket to tag.
          -> [(ObjectKey, Text)] -- ^ List of K/V pairs to apply as tags.
          -> IO ()
tagBucket bkt xs = do
    lgr <- newLogger Debug stdout
    env <- newEnv Ireland Discover <&> envLogger .~ lgr

    let say  = liftIO . Text.putStrLn
        tags = map (uncurry tag) xs
        kv t = toText (t ^. tagKey) <> "=" <> (t ^. tagValue)

    runResourceT . runAWST env $ do
        void . send $ putBucketTagging bkt (tagging & tTagSet .~ tags)
        say $ "Successfully Put Tags: " <> Fold.foldMap kv tags

        ts <- view gbtrsTagSet <$> send (getBucketTagging bkt)
        forM_ ts $ \t ->
            say $ "Found Tag: " <> kv t
