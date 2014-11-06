{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the tag set associated with the bucket.
module Network.AWS.S3.GetBucketTagging
    (
    -- * Request
      GetBucketTagging
    -- ** Request constructor
    , getBucketTagging
    -- ** Request lenses
    , gbtrBucket

    -- * Response
    , GetBucketTaggingOutput
    -- ** Response constructor
    , getBucketTaggingOutput
    -- ** Response lenses
    , gbtoTagSet
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

newtype GetBucketTagging = GetBucketTagging
    { _gbtrBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketTagging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbtrBucket' @::@ 'BucketName'
--
getBucketTagging :: BucketName -- ^ 'gbtrBucket'
                 -> GetBucketTagging
getBucketTagging p1 = GetBucketTagging
    { _gbtrBucket = p1
    }

gbtrBucket :: Lens' GetBucketTagging BucketName
gbtrBucket = lens _gbtrBucket (\s a -> s { _gbtrBucket = a })

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = mconcat
        [ "/"
        , toText _gbtrBucket
        ]

instance ToQuery GetBucketTagging where
    toQuery = const "tagging"

instance ToHeaders GetBucketTagging

instance ToBody GetBucketTagging

newtype GetBucketTaggingOutput = GetBucketTaggingOutput
    { _gbtoTagSet :: [Tag]
    } deriving (Eq, Ord, Show, Generic, Monoid)

instance AWSRequest GetBucketTagging where
    type Sv GetBucketTagging = S3
    type Rs GetBucketTagging = GetBucketTaggingOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "TagSet"
