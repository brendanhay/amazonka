{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketWebsite
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the website configuration for a bucket.
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
    , gbwrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketWebsite' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbwBucket'
newtype GetBucketWebsite = GetBucketWebsite'
    { _gbwBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketWebsite' smart constructor.
getBucketWebsite :: BucketName -> GetBucketWebsite
getBucketWebsite pBucket =
    GetBucketWebsite'
    { _gbwBucket = pBucket
    }

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
                   (x .@? "RedirectAllRequestsTo") <*>
                     (x .@? "ErrorDocument")
                     <*>
                     (x .@? "RoutingRules" .!@ mempty >>=
                        may (parseXMLList "RoutingRule"))
                     <*> (x .@? "IndexDocument")
                     <*> (pure (fromEnum s)))

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
--
-- * 'gbwrStatus'
data GetBucketWebsiteResponse = GetBucketWebsiteResponse'
    { _gbwrRedirectAllRequestsTo :: !(Maybe RedirectAllRequestsTo)
    , _gbwrErrorDocument         :: !(Maybe ErrorDocument)
    , _gbwrRoutingRules          :: !(Maybe [RoutingRule])
    , _gbwrIndexDocument         :: !(Maybe IndexDocument)
    , _gbwrStatus                :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketWebsiteResponse' smart constructor.
getBucketWebsiteResponse :: Int -> GetBucketWebsiteResponse
getBucketWebsiteResponse pStatus =
    GetBucketWebsiteResponse'
    { _gbwrRedirectAllRequestsTo = Nothing
    , _gbwrErrorDocument = Nothing
    , _gbwrRoutingRules = Nothing
    , _gbwrIndexDocument = Nothing
    , _gbwrStatus = pStatus
    }

-- | FIXME: Undocumented member.
gbwrRedirectAllRequestsTo :: Lens' GetBucketWebsiteResponse (Maybe RedirectAllRequestsTo)
gbwrRedirectAllRequestsTo = lens _gbwrRedirectAllRequestsTo (\ s a -> s{_gbwrRedirectAllRequestsTo = a});

-- | FIXME: Undocumented member.
gbwrErrorDocument :: Lens' GetBucketWebsiteResponse (Maybe ErrorDocument)
gbwrErrorDocument = lens _gbwrErrorDocument (\ s a -> s{_gbwrErrorDocument = a});

-- | FIXME: Undocumented member.
gbwrRoutingRules :: Lens' GetBucketWebsiteResponse [RoutingRule]
gbwrRoutingRules = lens _gbwrRoutingRules (\ s a -> s{_gbwrRoutingRules = a}) . _Default;

-- | FIXME: Undocumented member.
gbwrIndexDocument :: Lens' GetBucketWebsiteResponse (Maybe IndexDocument)
gbwrIndexDocument = lens _gbwrIndexDocument (\ s a -> s{_gbwrIndexDocument = a});

-- | FIXME: Undocumented member.
gbwrStatus :: Lens' GetBucketWebsiteResponse Int
gbwrStatus = lens _gbwrStatus (\ s a -> s{_gbwrStatus = a});
