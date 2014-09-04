{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the cors configuration for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketCors
    (
    -- * Request
      PutBucketCors
    -- ** Request constructor
    , mkPutBucketCorsRequest
    -- ** Request lenses
    , pbcrBucket
    , pbcrCORSConfiguration
    , pbcrContentMD5

    -- * Response
    , PutBucketCorsResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketCors' request.
mkPutBucketCorsRequest :: BucketName -- ^ 'pbcrBucket'
                       -> PutBucketCors
mkPutBucketCorsRequest p1 = PutBucketCors
    { _pbcrBucket = p1
    , _pbcrCORSConfiguration = Nothing
    , _pbcrContentMD5 = Nothing
    }
{-# INLINE mkPutBucketCorsRequest #-}

data PutBucketCors = PutBucketCors
    { _pbcrBucket :: BucketName
    , _pbcrCORSConfiguration :: Maybe CORSConfiguration
    , _pbcrContentMD5 :: Maybe Text
    } deriving (Show, Generic)

pbcrBucket :: Lens' PutBucketCors (BucketName)
pbcrBucket = lens _pbcrBucket (\s a -> s { _pbcrBucket = a })
{-# INLINE pbcrBucket #-}

pbcrCORSConfiguration :: Lens' PutBucketCors (Maybe CORSConfiguration)
pbcrCORSConfiguration = lens _pbcrCORSConfiguration (\s a -> s { _pbcrCORSConfiguration = a })
{-# INLINE pbcrCORSConfiguration #-}

pbcrContentMD5 :: Lens' PutBucketCors (Maybe Text)
pbcrContentMD5 = lens _pbcrContentMD5 (\s a -> s { _pbcrContentMD5 = a })
{-# INLINE pbcrContentMD5 #-}

instance ToPath PutBucketCors where
    toPath PutBucketCors{..} = mconcat
        [ "/"
        , toBS _pbcrBucket
        ]

instance ToQuery PutBucketCors where
    toQuery PutBucketCors{..} = mconcat
        [ "cors"
        ]

instance ToHeaders PutBucketCors

instance ToBody PutBucketCors

data PutBucketCorsResponse = PutBucketCorsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketCors where
    type Sv PutBucketCors = S3
    type Rs PutBucketCors = PutBucketCorsResponse

    request = put
    response _ = nullaryResponse PutBucketCorsResponse
