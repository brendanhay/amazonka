{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBucketAnalyticsConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the analytics configurations for the bucket.
module Network.AWS.S3.ListBucketAnalyticsConfigurations
    (
    -- * Creating a Request
      listBucketAnalyticsConfigurations
    , ListBucketAnalyticsConfigurations
    -- * Request Lenses
    , lbacContinuationToken
    , lbacBucket

    -- * Destructuring the Response
    , listBucketAnalyticsConfigurationsResponse
    , ListBucketAnalyticsConfigurationsResponse
    -- * Response Lenses
    , lbacrsAnalyticsConfigurationList
    , lbacrsContinuationToken
    , lbacrsNextContinuationToken
    , lbacrsIsTruncated
    , lbacrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'listBucketAnalyticsConfigurations' smart constructor.
data ListBucketAnalyticsConfigurations = ListBucketAnalyticsConfigurations'
  { _lbacContinuationToken :: !(Maybe Text)
  , _lbacBucket            :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBucketAnalyticsConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbacContinuationToken' - The ContinuationToken that represents a placeholder from where this request should begin.
--
-- * 'lbacBucket' - The name of the bucket from which analytics configurations are retrieved.
listBucketAnalyticsConfigurations
    :: BucketName -- ^ 'lbacBucket'
    -> ListBucketAnalyticsConfigurations
listBucketAnalyticsConfigurations pBucket_ =
  ListBucketAnalyticsConfigurations'
    {_lbacContinuationToken = Nothing, _lbacBucket = pBucket_}


-- | The ContinuationToken that represents a placeholder from where this request should begin.
lbacContinuationToken :: Lens' ListBucketAnalyticsConfigurations (Maybe Text)
lbacContinuationToken = lens _lbacContinuationToken (\ s a -> s{_lbacContinuationToken = a})

-- | The name of the bucket from which analytics configurations are retrieved.
lbacBucket :: Lens' ListBucketAnalyticsConfigurations BucketName
lbacBucket = lens _lbacBucket (\ s a -> s{_lbacBucket = a})

instance AWSRequest ListBucketAnalyticsConfigurations
         where
        type Rs ListBucketAnalyticsConfigurations =
             ListBucketAnalyticsConfigurationsResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 ListBucketAnalyticsConfigurationsResponse' <$>
                   (may (parseXMLList "AnalyticsConfiguration") x) <*>
                     (x .@? "ContinuationToken")
                     <*> (x .@? "NextContinuationToken")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListBucketAnalyticsConfigurations
         where

instance NFData ListBucketAnalyticsConfigurations
         where

instance ToHeaders ListBucketAnalyticsConfigurations
         where
        toHeaders = const mempty

instance ToPath ListBucketAnalyticsConfigurations
         where
        toPath ListBucketAnalyticsConfigurations'{..}
          = mconcat ["/", toBS _lbacBucket]

instance ToQuery ListBucketAnalyticsConfigurations
         where
        toQuery ListBucketAnalyticsConfigurations'{..}
          = mconcat
              ["continuation-token" =: _lbacContinuationToken,
               "analytics"]

-- | /See:/ 'listBucketAnalyticsConfigurationsResponse' smart constructor.
data ListBucketAnalyticsConfigurationsResponse = ListBucketAnalyticsConfigurationsResponse'
  { _lbacrsAnalyticsConfigurationList :: !(Maybe [AnalyticsConfiguration])
  , _lbacrsContinuationToken          :: !(Maybe Text)
  , _lbacrsNextContinuationToken      :: !(Maybe Text)
  , _lbacrsIsTruncated                :: !(Maybe Bool)
  , _lbacrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBucketAnalyticsConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbacrsAnalyticsConfigurationList' - The list of analytics configurations for a bucket.
--
-- * 'lbacrsContinuationToken' - The ContinuationToken that represents where this request began.
--
-- * 'lbacrsNextContinuationToken' - NextContinuationToken is sent when isTruncated is true, which indicates that there are more analytics configurations to list. The next request must include this NextContinuationToken. The token is obfuscated and is not a usable value.
--
-- * 'lbacrsIsTruncated' - Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- * 'lbacrsResponseStatus' - -- | The response status code.
listBucketAnalyticsConfigurationsResponse
    :: Int -- ^ 'lbacrsResponseStatus'
    -> ListBucketAnalyticsConfigurationsResponse
listBucketAnalyticsConfigurationsResponse pResponseStatus_ =
  ListBucketAnalyticsConfigurationsResponse'
    { _lbacrsAnalyticsConfigurationList = Nothing
    , _lbacrsContinuationToken = Nothing
    , _lbacrsNextContinuationToken = Nothing
    , _lbacrsIsTruncated = Nothing
    , _lbacrsResponseStatus = pResponseStatus_
    }


-- | The list of analytics configurations for a bucket.
lbacrsAnalyticsConfigurationList :: Lens' ListBucketAnalyticsConfigurationsResponse [AnalyticsConfiguration]
lbacrsAnalyticsConfigurationList = lens _lbacrsAnalyticsConfigurationList (\ s a -> s{_lbacrsAnalyticsConfigurationList = a}) . _Default . _Coerce

-- | The ContinuationToken that represents where this request began.
lbacrsContinuationToken :: Lens' ListBucketAnalyticsConfigurationsResponse (Maybe Text)
lbacrsContinuationToken = lens _lbacrsContinuationToken (\ s a -> s{_lbacrsContinuationToken = a})

-- | NextContinuationToken is sent when isTruncated is true, which indicates that there are more analytics configurations to list. The next request must include this NextContinuationToken. The token is obfuscated and is not a usable value.
lbacrsNextContinuationToken :: Lens' ListBucketAnalyticsConfigurationsResponse (Maybe Text)
lbacrsNextContinuationToken = lens _lbacrsNextContinuationToken (\ s a -> s{_lbacrsNextContinuationToken = a})

-- | Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
lbacrsIsTruncated :: Lens' ListBucketAnalyticsConfigurationsResponse (Maybe Bool)
lbacrsIsTruncated = lens _lbacrsIsTruncated (\ s a -> s{_lbacrsIsTruncated = a})

-- | -- | The response status code.
lbacrsResponseStatus :: Lens' ListBucketAnalyticsConfigurationsResponse Int
lbacrsResponseStatus = lens _lbacrsResponseStatus (\ s a -> s{_lbacrsResponseStatus = a})

instance NFData
           ListBucketAnalyticsConfigurationsResponse
         where
