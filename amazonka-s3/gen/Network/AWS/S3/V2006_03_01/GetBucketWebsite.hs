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
    , getBucketWebsite
    -- ** Request lenses
    , gbwrBucket

    -- * Response
    , GetBucketWebsiteResponse
    -- ** Response lenses
    , gbwoErrorDocument
    , gbwoIndexDocument
    , gbwoRedirectAllRequestsTo
    , gbwoRoutingRules
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetBucketWebsite' request.
getBucketWebsite :: BucketName -- ^ 'gbwrBucket'
                 -> GetBucketWebsite
getBucketWebsite p1 = GetBucketWebsite
    { _gbwrBucket = p1
    }

data GetBucketWebsite = GetBucketWebsite
    { _gbwrBucket :: BucketName
    } deriving (Show, Generic)

gbwrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> GetBucketWebsite
    -> f GetBucketWebsite
gbwrBucket f x =
    (\y -> x { _gbwrBucket = y })
       <$> f (_gbwrBucket x)
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
    { _gbwoErrorDocument :: Maybe ErrorDocument
    , _gbwoIndexDocument :: Maybe IndexDocument
    , _gbwoRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _gbwoRoutingRules :: [RoutingRule]
    } deriving (Show, Generic)

gbwoErrorDocument
    :: Functor f
    => (Maybe ErrorDocument
    -> f (Maybe ErrorDocument))
    -> GetBucketWebsiteResponse
    -> f GetBucketWebsiteResponse
gbwoErrorDocument f x =
    (\y -> x { _gbwoErrorDocument = y })
       <$> f (_gbwoErrorDocument x)
{-# INLINE gbwoErrorDocument #-}

gbwoIndexDocument
    :: Functor f
    => (Maybe IndexDocument
    -> f (Maybe IndexDocument))
    -> GetBucketWebsiteResponse
    -> f GetBucketWebsiteResponse
gbwoIndexDocument f x =
    (\y -> x { _gbwoIndexDocument = y })
       <$> f (_gbwoIndexDocument x)
{-# INLINE gbwoIndexDocument #-}

gbwoRedirectAllRequestsTo
    :: Functor f
    => (Maybe RedirectAllRequestsTo
    -> f (Maybe RedirectAllRequestsTo))
    -> GetBucketWebsiteResponse
    -> f GetBucketWebsiteResponse
gbwoRedirectAllRequestsTo f x =
    (\y -> x { _gbwoRedirectAllRequestsTo = y })
       <$> f (_gbwoRedirectAllRequestsTo x)
{-# INLINE gbwoRedirectAllRequestsTo #-}

gbwoRoutingRules
    :: Functor f
    => ([RoutingRule]
    -> f ([RoutingRule]))
    -> GetBucketWebsiteResponse
    -> f GetBucketWebsiteResponse
gbwoRoutingRules f x =
    (\y -> x { _gbwoRoutingRules = y })
       <$> f (_gbwoRoutingRules x)
{-# INLINE gbwoRoutingRules #-}

instance FromXML GetBucketWebsiteResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3
    type Rs GetBucketWebsite = GetBucketWebsiteResponse

    request = get
    response _ = xmlResponse
