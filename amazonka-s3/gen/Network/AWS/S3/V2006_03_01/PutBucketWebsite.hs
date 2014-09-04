{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the website configuration for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketWebsite
    (
    -- * Request
      PutBucketWebsite
    -- ** Request constructor
    , mkPutBucketWebsiteRequest
    -- ** Request lenses
    , pbwrBucket
    , pbwrContentMD5
    , pbwrWebsiteConfiguration

    -- * Response
    , PutBucketWebsiteResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketWebsite' request.
mkPutBucketWebsiteRequest :: BucketName -- ^ 'pbwrBucket'
                          -> WebsiteConfiguration -- ^ 'pbwrWebsiteConfiguration'
                          -> PutBucketWebsite
mkPutBucketWebsiteRequest p1 p2 = PutBucketWebsite
    { _pbwrBucket = p1
    , _pbwrContentMD5 = Nothing
    , _pbwrWebsiteConfiguration = p3
    }
{-# INLINE mkPutBucketWebsiteRequest #-}

data PutBucketWebsite = PutBucketWebsite
    { _pbwrBucket :: BucketName
    , _pbwrContentMD5 :: Maybe Text
    , _pbwrWebsiteConfiguration :: WebsiteConfiguration
    } deriving (Show, Generic)

pbwrBucket :: Lens' PutBucketWebsite (BucketName)
pbwrBucket = lens _pbwrBucket (\s a -> s { _pbwrBucket = a })
{-# INLINE pbwrBucket #-}

pbwrContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwrContentMD5 = lens _pbwrContentMD5 (\s a -> s { _pbwrContentMD5 = a })
{-# INLINE pbwrContentMD5 #-}

pbwrWebsiteConfiguration :: Lens' PutBucketWebsite (WebsiteConfiguration)
pbwrWebsiteConfiguration = lens _pbwrWebsiteConfiguration (\s a -> s { _pbwrWebsiteConfiguration = a })
{-# INLINE pbwrWebsiteConfiguration #-}

instance ToPath PutBucketWebsite where
    toPath PutBucketWebsite{..} = mconcat
        [ "/"
        , toBS _pbwrBucket
        ]

instance ToQuery PutBucketWebsite where
    toQuery PutBucketWebsite{..} = mconcat
        [ "website"
        ]

instance ToHeaders PutBucketWebsite

instance ToBody PutBucketWebsite

data PutBucketWebsiteResponse = PutBucketWebsiteResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketWebsite where
    type Sv PutBucketWebsite = S3
    type Rs PutBucketWebsite = PutBucketWebsiteResponse

    request = put
    response _ = nullaryResponse PutBucketWebsiteResponse
