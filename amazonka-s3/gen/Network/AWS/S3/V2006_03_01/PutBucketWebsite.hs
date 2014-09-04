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
    , putBucketWebsite
    -- ** Request lenses
    , pbwrWebsiteConfiguration
    , pbwrBucket
    , pbwrContentMD5

    -- * Response
    , PutBucketWebsiteResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketWebsite' request.
putBucketWebsite :: WebsiteConfiguration -- ^ 'pbwrWebsiteConfiguration'
                 -> BucketName -- ^ 'pbwrBucket'
                 -> PutBucketWebsite
putBucketWebsite p1 p2 = PutBucketWebsite
    { _pbwrWebsiteConfiguration = p1
    , _pbwrBucket = p2
    , _pbwrContentMD5 = Nothing
    }
{-# INLINE putBucketWebsite #-}

data PutBucketWebsite = PutBucketWebsite
    { _pbwrWebsiteConfiguration :: WebsiteConfiguration
    , _pbwrBucket :: BucketName
    , _pbwrContentMD5 :: Maybe Text
    } deriving (Show, Generic)

pbwrWebsiteConfiguration :: Lens' PutBucketWebsite (WebsiteConfiguration)
pbwrWebsiteConfiguration f x =
    f (_pbwrWebsiteConfiguration x)
        <&> \y -> x { _pbwrWebsiteConfiguration = y }
{-# INLINE pbwrWebsiteConfiguration #-}

pbwrBucket :: Lens' PutBucketWebsite (BucketName)
pbwrBucket f x =
    f (_pbwrBucket x)
        <&> \y -> x { _pbwrBucket = y }
{-# INLINE pbwrBucket #-}

pbwrContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwrContentMD5 f x =
    f (_pbwrContentMD5 x)
        <&> \y -> x { _pbwrContentMD5 = y }
{-# INLINE pbwrContentMD5 #-}

instance ToPath PutBucketWebsite where
    toPath PutBucketWebsite{..} = mconcat
        [ "/"
        , toBS _pbwrBucket
        ]

instance ToQuery PutBucketWebsite where
    toQuery PutBucketWebsite{..} = mconcat
        [ "website"
        ]

instance ToHeaders PutBucketWebsite where
    toHeaders PutBucketWebsite{..} = concat
        [ "Content-MD5" =: _pbwrContentMD5
        ]

instance ToBody PutBucketWebsite where
    toBody = toBody . encodeXML . _pbwrWebsiteConfiguration

data PutBucketWebsiteResponse = PutBucketWebsiteResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketWebsite where
    type Sv PutBucketWebsite = S3
    type Rs PutBucketWebsite = PutBucketWebsiteResponse

    request = put
    response _ = nullaryResponse PutBucketWebsiteResponse
