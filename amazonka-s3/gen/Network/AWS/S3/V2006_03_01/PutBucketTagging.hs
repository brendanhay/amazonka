{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the tags for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketTagging
    (
    -- * Request
      PutBucketTagging
    -- ** Request constructor
    , putBucketTagging
    -- ** Request lenses
    , pbtrTagging
    , pbtrBucket
    , pbtrContentMD5

    -- * Response
    , PutBucketTaggingResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketTagging' request.
putBucketTagging :: Tagging -- ^ 'pbtrTagging'
                 -> BucketName -- ^ 'pbtrBucket'
                 -> PutBucketTagging
putBucketTagging p1 p2 = PutBucketTagging
    { _pbtrTagging = p1
    , _pbtrBucket = p2
    , _pbtrContentMD5 = Nothing
    }
{-# INLINE putBucketTagging #-}

data PutBucketTagging = PutBucketTagging
    { _pbtrTagging :: Tagging
    , _pbtrBucket :: BucketName
    , _pbtrContentMD5 :: Maybe Text
    } deriving (Show, Generic)

pbtrTagging :: Lens' PutBucketTagging (Tagging)
pbtrTagging f x =
    f (_pbtrTagging x)
        <&> \y -> x { _pbtrTagging = y }
{-# INLINE pbtrTagging #-}

pbtrBucket :: Lens' PutBucketTagging (BucketName)
pbtrBucket f x =
    f (_pbtrBucket x)
        <&> \y -> x { _pbtrBucket = y }
{-# INLINE pbtrBucket #-}

pbtrContentMD5 :: Lens' PutBucketTagging (Maybe Text)
pbtrContentMD5 f x =
    f (_pbtrContentMD5 x)
        <&> \y -> x { _pbtrContentMD5 = y }
{-# INLINE pbtrContentMD5 #-}

instance ToPath PutBucketTagging where
    toPath PutBucketTagging{..} = mconcat
        [ "/"
        , toBS _pbtrBucket
        ]

instance ToQuery PutBucketTagging where
    toQuery PutBucketTagging{..} = mconcat
        [ "tagging"
        ]

instance ToHeaders PutBucketTagging where
    toHeaders PutBucketTagging{..} = concat
        [ "Content-MD5" =: _pbtrContentMD5
        ]

instance ToBody PutBucketTagging where
    toBody = toBody . encodeXML . _pbtrTagging

data PutBucketTaggingResponse = PutBucketTaggingResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketTagging where
    type Sv PutBucketTagging = S3
    type Rs PutBucketTagging = PutBucketTaggingResponse

    request = put
    response _ = nullaryResponse PutBucketTaggingResponse
