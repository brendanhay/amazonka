{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketWebsite
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketWebsite = GetBucketWebsite
    { _gbwBucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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
    , _gbwrRoutingRules          :: List "RoutingRule" RoutingRule
    } deriving (Eq, Read, Show)

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
gbwrRoutingRules = lens _gbwrRoutingRules (\s a -> s { _gbwrRoutingRules = a }) . _List

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = mconcat
        [ "/"
        , toText _gbwBucket
        ]

instance ToQuery GetBucketWebsite where
    toQuery = const "website"

instance ToHeaders GetBucketWebsite

instance ToXMLRoot GetBucketWebsite where
    toXMLRoot = const (namespaced ns "GetBucketWebsite" [])

instance ToXML GetBucketWebsite

instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3
    type Rs GetBucketWebsite = GetBucketWebsiteResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketWebsiteResponse where
    parseXML x = GetBucketWebsiteResponse
        <$> x .@? "ErrorDocument"
        <*> x .@? "IndexDocument"
        <*> x .@? "RedirectAllRequestsTo"
        <*> x .@? "RoutingRules" .!@ mempty
