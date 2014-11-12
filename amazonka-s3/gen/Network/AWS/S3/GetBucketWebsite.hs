{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.S3.GetBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the website configuration for a bucket.
module Network.AWS.S3.GetBucketWebsite
    (
    -- * Request
      GetBucketWebsite
    -- ** Request constructor
    , getBucketWebsite
    -- ** Request lenses
    , gbwBucket

    -- * Response
    , GetBucketWebsiteOutput
    -- ** Response constructor
    , getBucketWebsiteResponse
    -- ** Response lenses
    , gbwoErrorDocument
    , gbwoIndexDocument
    , gbwoRedirectAllRequestsTo
    , gbwoRoutingRules
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketWebsite = GetBucketWebsite
    { _gbwBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetBucketWebsite' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbwBucket' @::@ 'Text'
--
getBucketWebsite :: Text -- ^ 'gbwBucket'
                 -> GetBucketWebsite
getBucketWebsite p1 = GetBucketWebsite
    { _gbwBucket = p1
    }

gbwBucket :: Lens' GetBucketWebsite Text
gbwBucket = lens _gbwBucket (\s a -> s { _gbwBucket = a })

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = mconcat
        [ "/"
        , toText _gbwBucket
        ]

instance ToQuery GetBucketWebsite where
    toQuery = const "website"

instance ToHeaders GetBucketWebsite

data GetBucketWebsiteOutput = GetBucketWebsiteOutput
    { _gbwoErrorDocument         :: Maybe ErrorDocument
    , _gbwoIndexDocument         :: Maybe IndexDocument
    , _gbwoRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _gbwoRoutingRules          :: [RoutingRule]
    } deriving (Eq, Show, Generic)

-- | 'GetBucketWebsiteOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbwoErrorDocument' @::@ 'Maybe' 'ErrorDocument'
--
-- * 'gbwoIndexDocument' @::@ 'Maybe' 'IndexDocument'
--
-- * 'gbwoRedirectAllRequestsTo' @::@ 'Maybe' 'RedirectAllRequestsTo'
--
-- * 'gbwoRoutingRules' @::@ ['RoutingRule']
--
getBucketWebsiteResponse :: GetBucketWebsiteOutput
getBucketWebsiteResponse = GetBucketWebsiteOutput
    { _gbwoRedirectAllRequestsTo = Nothing
    , _gbwoIndexDocument         = Nothing
    , _gbwoErrorDocument         = Nothing
    , _gbwoRoutingRules          = mempty
    }

gbwoErrorDocument :: Lens' GetBucketWebsiteOutput (Maybe ErrorDocument)
gbwoErrorDocument =
    lens _gbwoErrorDocument (\s a -> s { _gbwoErrorDocument = a })

gbwoIndexDocument :: Lens' GetBucketWebsiteOutput (Maybe IndexDocument)
gbwoIndexDocument =
    lens _gbwoIndexDocument (\s a -> s { _gbwoIndexDocument = a })

gbwoRedirectAllRequestsTo :: Lens' GetBucketWebsiteOutput (Maybe RedirectAllRequestsTo)
gbwoRedirectAllRequestsTo =
    lens _gbwoRedirectAllRequestsTo
        (\s a -> s { _gbwoRedirectAllRequestsTo = a })

gbwoRoutingRules :: Lens' GetBucketWebsiteOutput [RoutingRule]
gbwoRoutingRules = lens _gbwoRoutingRules (\s a -> s { _gbwoRoutingRules = a })

instance FromXML GetBucketWebsiteOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetBucketWebsiteOutput"
instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3
    type Rs GetBucketWebsite = GetBucketWebsiteOutput

    request  = get
    response = xmlResponse $ \h x -> GetBucketWebsiteOutput
        <$> x %| "ErrorDocument"
        <*> x %| "IndexDocument"
        <*> x %| "RedirectAllRequestsTo"
        <*> x %| "RoutingRules"
