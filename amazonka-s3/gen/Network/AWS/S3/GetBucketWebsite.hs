{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , gbwrRedirectAllRequestsTo
    , gbwrErrorDocument
    , gbwrRoutingRules
    , gbwrIndexDocument
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.S3.Types

-- | /See:/ 'getBucketWebsite' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbwBucket'
newtype GetBucketWebsite = GetBucketWebsite'{_gbwBucket :: BucketName} deriving (Eq, Read, Show)

-- | 'GetBucketWebsite' smart constructor.
getBucketWebsite :: BucketName -> GetBucketWebsite
getBucketWebsite pBucket = GetBucketWebsite'{_gbwBucket = pBucket};

-- | FIXME: Undocumented member.
gbwBucket :: Lens' GetBucketWebsite BucketName
gbwBucket = lens _gbwBucket (\ s a -> s{_gbwBucket = a});

instance AWSRequest GetBucketWebsite where
        type Sv GetBucketWebsite = S3
        type Rs GetBucketWebsite = GetBucketWebsiteResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketWebsiteResponse' <$>
                   x .@? "RedirectAllRequestsTo" <*>
                     x .@? "ErrorDocument"
                     <*>
                     (x .@? "RoutingRules" .!@ mempty >>=
                        parseXMLList "RoutingRule")
                     <*> x .@? "IndexDocument")

instance ToHeaders GetBucketWebsite where
        toHeaders = const mempty

instance ToPath GetBucketWebsite where
        toPath GetBucketWebsite'{..}
          = mconcat ["/", toText _gbwBucket]

instance ToQuery GetBucketWebsite where
        toQuery = const (mconcat ["website"])

-- | /See:/ 'getBucketWebsiteResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbwrRedirectAllRequestsTo'
--
-- * 'gbwrErrorDocument'
--
-- * 'gbwrRoutingRules'
--
-- * 'gbwrIndexDocument'
data GetBucketWebsiteResponse = GetBucketWebsiteResponse'{_gbwrRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo, _gbwrErrorDocument :: Maybe ErrorDocument, _gbwrRoutingRules :: Maybe [RoutingRule], _gbwrIndexDocument :: Maybe IndexDocument} deriving (Eq, Read, Show)

-- | 'GetBucketWebsiteResponse' smart constructor.
getBucketWebsiteResponse :: GetBucketWebsiteResponse
getBucketWebsiteResponse = GetBucketWebsiteResponse'{_gbwrRedirectAllRequestsTo = Nothing, _gbwrErrorDocument = Nothing, _gbwrRoutingRules = Nothing, _gbwrIndexDocument = Nothing};

-- | FIXME: Undocumented member.
gbwrRedirectAllRequestsTo :: Lens' GetBucketWebsiteResponse (Maybe RedirectAllRequestsTo)
gbwrRedirectAllRequestsTo = lens _gbwrRedirectAllRequestsTo (\ s a -> s{_gbwrRedirectAllRequestsTo = a});

-- | FIXME: Undocumented member.
gbwrErrorDocument :: Lens' GetBucketWebsiteResponse (Maybe ErrorDocument)
gbwrErrorDocument = lens _gbwrErrorDocument (\ s a -> s{_gbwrErrorDocument = a});

-- | FIXME: Undocumented member.
gbwrRoutingRules :: Lens' GetBucketWebsiteResponse (Maybe [RoutingRule])
gbwrRoutingRules = lens _gbwrRoutingRules (\ s a -> s{_gbwrRoutingRules = a});

-- | FIXME: Undocumented member.
gbwrIndexDocument :: Lens' GetBucketWebsiteResponse (Maybe IndexDocument)
gbwrIndexDocument = lens _gbwrIndexDocument (\ s a -> s{_gbwrIndexDocument = a});
