{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the website configuration for a bucket.
module Network.AWS.S3.PutBucketWebsite
    (
    -- * Request
      PutBucketWebsite
    -- ** Request constructor
    , putBucketWebsite
    -- ** Request lenses
    , pbwBucket
    , pbwContentMD5
    , pbwWebsiteConfiguration

    -- * Response
    , PutBucketWebsiteResponse
    -- ** Response constructor
    , putBucketWebsiteResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutBucketWebsite = PutBucketWebsite
    { _pbwBucket :: BucketName
    , _pbwContentMD5 :: Maybe Text
    , _pbwWebsiteConfiguration :: WebsiteConfiguration
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketWebsite' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @ContentMD5 ::@ @Maybe Text@
--
-- * @WebsiteConfiguration ::@ @WebsiteConfiguration@
--
putBucketWebsite :: BucketName -- ^ 'pbwBucket'
                 -> WebsiteConfiguration -- ^ 'pbwWebsiteConfiguration'
                 -> PutBucketWebsite
putBucketWebsite p1 p3 = PutBucketWebsite
    { _pbwBucket = p1
    , _pbwContentMD5 = Nothing
    , _pbwWebsiteConfiguration = p3
    }

pbwBucket :: Lens' PutBucketWebsite BucketName
pbwBucket = lens _pbwBucket (\s a -> s { _pbwBucket = a })

pbwContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwContentMD5 = lens _pbwContentMD5 (\s a -> s { _pbwContentMD5 = a })

pbwWebsiteConfiguration :: Lens' PutBucketWebsite WebsiteConfiguration
pbwWebsiteConfiguration =
    lens _pbwWebsiteConfiguration
         (\s a -> s { _pbwWebsiteConfiguration = a })

instance ToPath PutBucketWebsite

instance ToQuery PutBucketWebsite

instance ToHeaders PutBucketWebsite where
    toHeaders PutBucketWebsite{..} = concat
        [ "Content-MD5" =: _pbwContentMD5
        ]

instance ToBody PutBucketWebsite where
    toBody = toBody . encodeXML . _pbwWebsiteConfiguration

data PutBucketWebsiteResponse = PutBucketWebsiteResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketWebsiteResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
putBucketWebsiteResponse :: PutBucketWebsiteResponse
putBucketWebsiteResponse = PutBucketWebsiteResponse

instance AWSRequest PutBucketWebsite where
    type Sv PutBucketWebsite = S3
    type Rs PutBucketWebsite = PutBucketWebsiteResponse

    request = get
    response _ = nullaryResponse PutBucketWebsiteResponse
