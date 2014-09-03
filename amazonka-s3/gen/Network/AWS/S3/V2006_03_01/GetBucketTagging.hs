{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the tag set associated with the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketTagging
    (
    -- * Request
      GetBucketTagging
    -- ** Request constructor
    , getBucketTagging
    -- ** Request lenses
    , gbtrBucket

    -- * Response
    , GetBucketTaggingResponse
    -- ** Response lenses
    , gbtoTagSet
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetBucketTagging' request.
getBucketTagging :: BucketName -- ^ 'gbtrBucket'
                 -> GetBucketTagging
getBucketTagging p1 = GetBucketTagging
    { _gbtrBucket = p1
    }

data GetBucketTagging = GetBucketTagging
    { _gbtrBucket :: BucketName
    } deriving (Show, Generic)

gbtrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> GetBucketTagging
    -> f GetBucketTagging
gbtrBucket f x =
    (\y -> x { _gbtrBucket = y })
       <$> f (_gbtrBucket x)
{-# INLINE gbtrBucket #-}

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = mconcat
        [ "/"
        , toBS _gbtrBucket
        ]

instance ToQuery GetBucketTagging where
    toQuery GetBucketTagging{..} = mconcat
        [ "tagging"
        ]

instance ToHeaders GetBucketTagging

instance ToBody GetBucketTagging

data GetBucketTaggingResponse = GetBucketTaggingResponse
    { _gbtoTagSet :: [Tag]
    } deriving (Show, Generic)

gbtoTagSet
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> GetBucketTaggingResponse
    -> f GetBucketTaggingResponse
gbtoTagSet f x =
    (\y -> x { _gbtoTagSet = y })
       <$> f (_gbtoTagSet x)
{-# INLINE gbtoTagSet #-}

instance FromXML GetBucketTaggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketTagging where
    type Sv GetBucketTagging = S3
    type Rs GetBucketTagging = GetBucketTaggingResponse

    request = get
    response _ = xmlResponse
