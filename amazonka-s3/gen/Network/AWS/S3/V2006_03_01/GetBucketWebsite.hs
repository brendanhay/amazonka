{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the website configuration for a bucket.
module Network.AWS.S3.V2006_03_01.GetBucketWebsite
    (
    -- * Request
      GetBucketWebsite
    -- ** Request constructor
    , mkGetBucketWebsite
    -- ** Request lenses
    , gbwBucket

    -- * Response
    , GetBucketWebsiteResponse
    -- ** Response constructor
    , mkGetBucketWebsiteResponse
    -- ** Response lenses
    , gbwrRedirectAllRequestsTo
    , gbwrIndexDocument
    , gbwrErrorDocument
    , gbwrRoutingRules
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketWebsite = GetBucketWebsite
    { _gbwBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketWebsite' request.
mkGetBucketWebsite :: BucketName -- ^ 'gbwBucket'
                   -> GetBucketWebsite
mkGetBucketWebsite p1 = GetBucketWebsite
    { _gbwBucket = p1
    }

gbwBucket :: Lens' GetBucketWebsite BucketName
gbwBucket = lens _gbwBucket (\s a -> s { _gbwBucket = a })

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = mconcat
        [ "/"
        , toBS _gbwBucket
        ]

instance ToQuery GetBucketWebsite where
    toQuery GetBucketWebsite{..} = mconcat
        [ "website"
        ]

instance ToHeaders GetBucketWebsite

instance ToBody GetBucketWebsite

data GetBucketWebsiteResponse = GetBucketWebsiteResponse
    { _gbwrRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _gbwrIndexDocument :: Maybe IndexDocument
    , _gbwrErrorDocument :: Maybe ErrorDocument
    , _gbwrRoutingRules :: [RoutingRule]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketWebsiteResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkGetBucketWebsiteResponse :: GetBucketWebsiteResponse
mkGetBucketWebsiteResponse = GetBucketWebsiteResponse
    { _gbwrRedirectAllRequestsTo = Nothing
    , _gbwrIndexDocument = Nothing
    , _gbwrErrorDocument = Nothing
    , _gbwrRoutingRules = mempty
    }

gbwrRedirectAllRequestsTo :: Lens' GetBucketWebsiteResponse (Maybe RedirectAllRequestsTo)
gbwrRedirectAllRequestsTo =
    lens _gbwrRedirectAllRequestsTo
         (\s a -> s { _gbwrRedirectAllRequestsTo = a })

gbwrIndexDocument :: Lens' GetBucketWebsiteResponse (Maybe IndexDocument)
gbwrIndexDocument =
    lens _gbwrIndexDocument (\s a -> s { _gbwrIndexDocument = a })

gbwrErrorDocument :: Lens' GetBucketWebsiteResponse (Maybe ErrorDocument)
gbwrErrorDocument =
    lens _gbwrErrorDocument (\s a -> s { _gbwrErrorDocument = a })

gbwrRoutingRules :: Lens' GetBucketWebsiteResponse [RoutingRule]
gbwrRoutingRules =
    lens _gbwrRoutingRules (\s a -> s { _gbwrRoutingRules = a })

instance FromXML GetBucketWebsiteResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3
    type Rs GetBucketWebsite = GetBucketWebsiteResponse

    request = get
    response _ = xmlResponse
