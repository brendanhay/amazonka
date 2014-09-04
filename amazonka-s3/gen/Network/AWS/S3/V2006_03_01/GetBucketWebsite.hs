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
    , mkGetBucketWebsiteRequest
    -- ** Request lenses
    , gbwrBucket

    -- * Response
    , GetBucketWebsiteResponse
    -- ** Response lenses
    , gbwoRedirectAllRequestsTo
    , gbwoIndexDocument
    , gbwoErrorDocument
    , gbwoRoutingRules
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketWebsite' request.
mkGetBucketWebsiteRequest :: BucketName -- ^ 'gbwrBucket'
                          -> GetBucketWebsite
mkGetBucketWebsiteRequest p1 = GetBucketWebsite
    { _gbwrBucket = p1
    }
{-# INLINE mkGetBucketWebsiteRequest #-}

newtype GetBucketWebsite = GetBucketWebsite
    { _gbwrBucket :: BucketName
    } deriving (Show, Generic)

gbwrBucket :: Lens' GetBucketWebsite (BucketName)
gbwrBucket = lens _gbwrBucket (\s a -> s { _gbwrBucket = a })
{-# INLINE gbwrBucket #-}

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = mconcat
        [ "/"
        , toBS _gbwrBucket
        ]

instance ToQuery GetBucketWebsite where
    toQuery GetBucketWebsite{..} = mconcat
        [ "website"
        ]

instance ToHeaders GetBucketWebsite

instance ToBody GetBucketWebsite

data GetBucketWebsiteResponse = GetBucketWebsiteResponse
    { _gbwoRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _gbwoIndexDocument :: Maybe IndexDocument
    , _gbwoErrorDocument :: Maybe ErrorDocument
    , _gbwoRoutingRules :: [RoutingRule]
    } deriving (Show, Generic)

gbwoRedirectAllRequestsTo :: Lens' GetBucketWebsiteResponse (Maybe RedirectAllRequestsTo)
gbwoRedirectAllRequestsTo = lens _gbwoRedirectAllRequestsTo (\s a -> s { _gbwoRedirectAllRequestsTo = a })
{-# INLINE gbwoRedirectAllRequestsTo #-}

gbwoIndexDocument :: Lens' GetBucketWebsiteResponse (Maybe IndexDocument)
gbwoIndexDocument = lens _gbwoIndexDocument (\s a -> s { _gbwoIndexDocument = a })
{-# INLINE gbwoIndexDocument #-}

gbwoErrorDocument :: Lens' GetBucketWebsiteResponse (Maybe ErrorDocument)
gbwoErrorDocument = lens _gbwoErrorDocument (\s a -> s { _gbwoErrorDocument = a })
{-# INLINE gbwoErrorDocument #-}

gbwoRoutingRules :: Lens' GetBucketWebsiteResponse ([RoutingRule])
gbwoRoutingRules = lens _gbwoRoutingRules (\s a -> s { _gbwoRoutingRules = a })
{-# INLINE gbwoRoutingRules #-}

instance FromXML GetBucketWebsiteResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3
    type Rs GetBucketWebsite = GetBucketWebsiteResponse

    request = get
    response _ = xmlResponse
