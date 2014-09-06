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
    -- ** Response lenses
    , gbwrsRedirectAllRequestsTo
    , gbwrsIndexDocument
    , gbwrsErrorDocument
    , gbwrsRoutingRules
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
{-# INLINE mkGetBucketWebsite #-}

gbwBucket :: Lens' GetBucketWebsite BucketName
gbwBucket = lens _gbwBucket (\s a -> s { _gbwBucket = a })
{-# INLINE gbwBucket #-}

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
    { _gbwrsRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _gbwrsIndexDocument :: Maybe IndexDocument
    , _gbwrsErrorDocument :: Maybe ErrorDocument
    , _gbwrsRoutingRules :: [RoutingRule]
    } deriving (Show, Generic)

gbwrsRedirectAllRequestsTo :: Lens' GetBucketWebsiteResponse (Maybe RedirectAllRequestsTo)
gbwrsRedirectAllRequestsTo =
    lens _gbwrsRedirectAllRequestsTo
         (\s a -> s { _gbwrsRedirectAllRequestsTo = a })
{-# INLINE gbwrsRedirectAllRequestsTo #-}

gbwrsIndexDocument :: Lens' GetBucketWebsiteResponse (Maybe IndexDocument)
gbwrsIndexDocument =
    lens _gbwrsIndexDocument (\s a -> s { _gbwrsIndexDocument = a })
{-# INLINE gbwrsIndexDocument #-}

gbwrsErrorDocument :: Lens' GetBucketWebsiteResponse (Maybe ErrorDocument)
gbwrsErrorDocument =
    lens _gbwrsErrorDocument (\s a -> s { _gbwrsErrorDocument = a })
{-# INLINE gbwrsErrorDocument #-}

gbwrsRoutingRules :: Lens' GetBucketWebsiteResponse [RoutingRule]
gbwrsRoutingRules =
    lens _gbwrsRoutingRules (\s a -> s { _gbwrsRoutingRules = a })
{-# INLINE gbwrsRoutingRules #-}

instance FromXML GetBucketWebsiteResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3
    type Rs GetBucketWebsite = GetBucketWebsiteResponse

    request = get
    response _ = xmlResponse
