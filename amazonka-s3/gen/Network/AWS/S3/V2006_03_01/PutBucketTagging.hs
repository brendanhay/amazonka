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
    , mkPutBucketTaggingRequest
    -- ** Request lenses
    , pbtrBucket
    , pbtrContentMD5
    , pbtrTagging

    -- * Response
    , PutBucketTaggingResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketTagging' request.
mkPutBucketTaggingRequest :: BucketName -- ^ 'pbtrBucket'
                          -> Tagging -- ^ 'pbtrTagging'
                          -> PutBucketTagging
mkPutBucketTaggingRequest p1 p2 = PutBucketTagging
    { _pbtrBucket = p1
    , _pbtrContentMD5 = Nothing
    , _pbtrTagging = p3
    }
{-# INLINE mkPutBucketTaggingRequest #-}

data PutBucketTagging = PutBucketTagging
    { _pbtrBucket :: BucketName
    , _pbtrContentMD5 :: Maybe Text
    , _pbtrTagging :: Tagging
    } deriving (Show, Generic)

pbtrBucket :: Lens' PutBucketTagging (BucketName)
pbtrBucket = lens _pbtrBucket (\s a -> s { _pbtrBucket = a })
{-# INLINE pbtrBucket #-}

pbtrContentMD5 :: Lens' PutBucketTagging (Maybe Text)
pbtrContentMD5 = lens _pbtrContentMD5 (\s a -> s { _pbtrContentMD5 = a })
{-# INLINE pbtrContentMD5 #-}

pbtrTagging :: Lens' PutBucketTagging (Tagging)
pbtrTagging = lens _pbtrTagging (\s a -> s { _pbtrTagging = a })
{-# INLINE pbtrTagging #-}

instance ToPath PutBucketTagging where
    toPath PutBucketTagging{..} = mconcat
        [ "/"
        , toBS _pbtrBucket
        ]

instance ToQuery PutBucketTagging where
    toQuery PutBucketTagging{..} = mconcat
        [ "tagging"
        ]

instance ToHeaders PutBucketTagging

instance ToBody PutBucketTagging

data PutBucketTaggingResponse = PutBucketTaggingResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketTagging where
    type Sv PutBucketTagging = S3
    type Rs PutBucketTagging = PutBucketTaggingResponse

    request = put
    response _ = nullaryResponse PutBucketTaggingResponse
