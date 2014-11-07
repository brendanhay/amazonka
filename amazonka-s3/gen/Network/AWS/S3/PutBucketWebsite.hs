{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , pbwrBucket
    , pbwrContentMD5
    , pbwrWebsiteConfiguration

    -- * Response
    , PutBucketWebsiteResponse
    -- ** Response constructor
    , putBucketWebsiteResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data PutBucketWebsite = PutBucketWebsite
    { _pbwrBucket               :: Text
    , _pbwrContentMD5           :: Maybe Text
    , _pbwrWebsiteConfiguration :: WebsiteConfiguration
    } deriving (Eq, Show, Generic)

-- | 'PutBucketWebsite' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbwrBucket' @::@ 'Text'
--
-- * 'pbwrContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbwrWebsiteConfiguration' @::@ 'WebsiteConfiguration'
--
putBucketWebsite :: Text -- ^ 'pbwrBucket'
                 -> WebsiteConfiguration -- ^ 'pbwrWebsiteConfiguration'
                 -> PutBucketWebsite
putBucketWebsite p1 p2 = PutBucketWebsite
    { _pbwrBucket               = p1
    , _pbwrWebsiteConfiguration = p2
    , _pbwrContentMD5           = Nothing
    }

pbwrBucket :: Lens' PutBucketWebsite Text
pbwrBucket = lens _pbwrBucket (\s a -> s { _pbwrBucket = a })

pbwrContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwrContentMD5 = lens _pbwrContentMD5 (\s a -> s { _pbwrContentMD5 = a })

pbwrWebsiteConfiguration :: Lens' PutBucketWebsite WebsiteConfiguration
pbwrWebsiteConfiguration =
    lens _pbwrWebsiteConfiguration
        (\s a -> s { _pbwrWebsiteConfiguration = a })

instance ToPath PutBucketWebsite where
    toPath PutBucketWebsite{..} = mconcat
        [ "/"
        , toText _pbwrBucket
        ]

instance ToQuery PutBucketWebsite where
    toQuery = const "website"

instance ToHeaders PutBucketWebsite where
    toHeaders PutBucketWebsite{..} = mconcat
        [ "Content-MD5" =: _pbwrContentMD5
        ]

instance ToBody PutBucketWebsite where
    toBody = toBody . encodeXML . _pbwrWebsiteConfiguration

data PutBucketWebsiteResponse = PutBucketWebsiteResponse
-- | 'PutBucketWebsiteResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
putBucketWebsiteResponse :: PutBucketWebsiteResponse
putBucketWebsiteResponse = PutBucketWebsiteResponse

instance AWSRequest PutBucketWebsite where
    type Sv PutBucketWebsite = S3
    type Rs PutBucketWebsite = PutBucketWebsiteResponse

    request  = put'
    response = const (nullaryResponse PutBucketWebsiteResponse)
