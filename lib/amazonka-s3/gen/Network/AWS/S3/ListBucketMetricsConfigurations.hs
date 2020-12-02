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
-- Module      : Network.AWS.S3.ListBucketMetricsConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the metrics configurations for the bucket.
module Network.AWS.S3.ListBucketMetricsConfigurations
    (
    -- * Creating a Request
      listBucketMetricsConfigurations
    , ListBucketMetricsConfigurations
    -- * Request Lenses
    , lbmcContinuationToken
    , lbmcBucket

    -- * Destructuring the Response
    , listBucketMetricsConfigurationsResponse
    , ListBucketMetricsConfigurationsResponse
    -- * Response Lenses
    , lbmcrsContinuationToken
    , lbmcrsMetricsConfigurationList
    , lbmcrsNextContinuationToken
    , lbmcrsIsTruncated
    , lbmcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'listBucketMetricsConfigurations' smart constructor.
data ListBucketMetricsConfigurations = ListBucketMetricsConfigurations'
  { _lbmcContinuationToken :: !(Maybe Text)
  , _lbmcBucket            :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBucketMetricsConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbmcContinuationToken' - The marker that is used to continue a metrics configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- * 'lbmcBucket' - The name of the bucket containing the metrics configurations to retrieve.
listBucketMetricsConfigurations
    :: BucketName -- ^ 'lbmcBucket'
    -> ListBucketMetricsConfigurations
listBucketMetricsConfigurations pBucket_ =
  ListBucketMetricsConfigurations'
    {_lbmcContinuationToken = Nothing, _lbmcBucket = pBucket_}


-- | The marker that is used to continue a metrics configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
lbmcContinuationToken :: Lens' ListBucketMetricsConfigurations (Maybe Text)
lbmcContinuationToken = lens _lbmcContinuationToken (\ s a -> s{_lbmcContinuationToken = a})

-- | The name of the bucket containing the metrics configurations to retrieve.
lbmcBucket :: Lens' ListBucketMetricsConfigurations BucketName
lbmcBucket = lens _lbmcBucket (\ s a -> s{_lbmcBucket = a})

instance AWSRequest ListBucketMetricsConfigurations
         where
        type Rs ListBucketMetricsConfigurations =
             ListBucketMetricsConfigurationsResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 ListBucketMetricsConfigurationsResponse' <$>
                   (x .@? "ContinuationToken") <*>
                     (may (parseXMLList "MetricsConfiguration") x)
                     <*> (x .@? "NextContinuationToken")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListBucketMetricsConfigurations
         where

instance NFData ListBucketMetricsConfigurations where

instance ToHeaders ListBucketMetricsConfigurations
         where
        toHeaders = const mempty

instance ToPath ListBucketMetricsConfigurations where
        toPath ListBucketMetricsConfigurations'{..}
          = mconcat ["/", toBS _lbmcBucket]

instance ToQuery ListBucketMetricsConfigurations
         where
        toQuery ListBucketMetricsConfigurations'{..}
          = mconcat
              ["continuation-token" =: _lbmcContinuationToken,
               "metrics"]

-- | /See:/ 'listBucketMetricsConfigurationsResponse' smart constructor.
data ListBucketMetricsConfigurationsResponse = ListBucketMetricsConfigurationsResponse'
  { _lbmcrsContinuationToken        :: !(Maybe Text)
  , _lbmcrsMetricsConfigurationList :: !(Maybe [MetricsConfiguration])
  , _lbmcrsNextContinuationToken    :: !(Maybe Text)
  , _lbmcrsIsTruncated              :: !(Maybe Bool)
  , _lbmcrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBucketMetricsConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbmcrsContinuationToken' - The marker that is used as a starting point for this metrics configuration list response. This value is present if it was sent in the request.
--
-- * 'lbmcrsMetricsConfigurationList' - The list of metrics configurations for a bucket.
--
-- * 'lbmcrsNextContinuationToken' - The marker used to continue a metrics configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- * 'lbmcrsIsTruncated' - Indicates whether the returned list of metrics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- * 'lbmcrsResponseStatus' - -- | The response status code.
listBucketMetricsConfigurationsResponse
    :: Int -- ^ 'lbmcrsResponseStatus'
    -> ListBucketMetricsConfigurationsResponse
listBucketMetricsConfigurationsResponse pResponseStatus_ =
  ListBucketMetricsConfigurationsResponse'
    { _lbmcrsContinuationToken = Nothing
    , _lbmcrsMetricsConfigurationList = Nothing
    , _lbmcrsNextContinuationToken = Nothing
    , _lbmcrsIsTruncated = Nothing
    , _lbmcrsResponseStatus = pResponseStatus_
    }


-- | The marker that is used as a starting point for this metrics configuration list response. This value is present if it was sent in the request.
lbmcrsContinuationToken :: Lens' ListBucketMetricsConfigurationsResponse (Maybe Text)
lbmcrsContinuationToken = lens _lbmcrsContinuationToken (\ s a -> s{_lbmcrsContinuationToken = a})

-- | The list of metrics configurations for a bucket.
lbmcrsMetricsConfigurationList :: Lens' ListBucketMetricsConfigurationsResponse [MetricsConfiguration]
lbmcrsMetricsConfigurationList = lens _lbmcrsMetricsConfigurationList (\ s a -> s{_lbmcrsMetricsConfigurationList = a}) . _Default . _Coerce

-- | The marker used to continue a metrics configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
lbmcrsNextContinuationToken :: Lens' ListBucketMetricsConfigurationsResponse (Maybe Text)
lbmcrsNextContinuationToken = lens _lbmcrsNextContinuationToken (\ s a -> s{_lbmcrsNextContinuationToken = a})

-- | Indicates whether the returned list of metrics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
lbmcrsIsTruncated :: Lens' ListBucketMetricsConfigurationsResponse (Maybe Bool)
lbmcrsIsTruncated = lens _lbmcrsIsTruncated (\ s a -> s{_lbmcrsIsTruncated = a})

-- | -- | The response status code.
lbmcrsResponseStatus :: Lens' ListBucketMetricsConfigurationsResponse Int
lbmcrsResponseStatus = lens _lbmcrsResponseStatus (\ s a -> s{_lbmcrsResponseStatus = a})

instance NFData
           ListBucketMetricsConfigurationsResponse
         where
