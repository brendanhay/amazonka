{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketWebsite.html>
module Network.AWS.S3.GetBucketWebsite
    (
    -- * Request
      GetBucketWebsite
    -- ** Request constructor
    , getBucketWebsite
    -- ** Request lenses
    , gbwBucket

    -- * Response
    , GetBucketWebsiteResponse
    -- ** Response constructor
    , getBucketWebsiteResponse
    -- ** Response lenses
    , gbwrErrorDocument
    , gbwrIndexDocument
    , gbwrRedirectAllRequestsTo
    , gbwrRoutingRules
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

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

data GetBucketWebsiteResponse = GetBucketWebsiteResponse
    { _gbwrErrorDocument         :: Maybe ErrorDocument
    , _gbwrIndexDocument         :: Maybe IndexDocument
    , _gbwrRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _gbwrRoutingRules          :: [RoutingRule]
    } deriving (Eq, Show, Generic)

-- | 'GetBucketWebsiteResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbwrErrorDocument' @::@ 'Maybe' 'ErrorDocument'
--
-- * 'gbwrIndexDocument' @::@ 'Maybe' 'IndexDocument'
--
-- * 'gbwrRedirectAllRequestsTo' @::@ 'Maybe' 'RedirectAllRequestsTo'
--
-- * 'gbwrRoutingRules' @::@ ['RoutingRule']
--
getBucketWebsiteResponse :: GetBucketWebsiteResponse
getBucketWebsiteResponse = GetBucketWebsiteResponse
    { _gbwrRedirectAllRequestsTo = Nothing
    , _gbwrIndexDocument         = Nothing
    , _gbwrErrorDocument         = Nothing
    , _gbwrRoutingRules          = mempty
    }

gbwrErrorDocument :: Lens' GetBucketWebsiteResponse (Maybe ErrorDocument)
gbwrErrorDocument =
    lens _gbwrErrorDocument (\s a -> s { _gbwrErrorDocument = a })

gbwrIndexDocument :: Lens' GetBucketWebsiteResponse (Maybe IndexDocument)
gbwrIndexDocument =
    lens _gbwrIndexDocument (\s a -> s { _gbwrIndexDocument = a })

gbwrRedirectAllRequestsTo :: Lens' GetBucketWebsiteResponse (Maybe RedirectAllRequestsTo)
gbwrRedirectAllRequestsTo =
    lens _gbwrRedirectAllRequestsTo
        (\s a -> s { _gbwrRedirectAllRequestsTo = a })

gbwrRoutingRules :: Lens' GetBucketWebsiteResponse [RoutingRule]
gbwrRoutingRules = lens _gbwrRoutingRules (\s a -> s { _gbwrRoutingRules = a })

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = mconcat
        [ "/"
        , toText _gbwBucket
        ]

instance ToQuery GetBucketWebsite where
    toQuery = const "website"

instance ToHeaders GetBucketWebsite

instance ToXML GetBucketWebsite where
    toXML = const (node "GetBucketWebsite" [])

instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3
    type Rs GetBucketWebsite = GetBucketWebsiteResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketWebsiteResponse where
    parseXML c = GetBucketWebsiteResponse
        <$> c .: "ErrorDocument"
        <*> c .: "IndexDocument"
        <*> c .: "RedirectAllRequestsTo"
        <*> c .: "RoutingRules"
