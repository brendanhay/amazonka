{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBucketAnalyticsConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the analytics configurations for the bucket. You can have up to 1,000 analytics configurations per bucket.
--
--
-- This operation supports list pagination and does not return more than 100 configurations at a time. You should always check the @IsTruncated@ element in the response. If there are no more configurations to list, @IsTruncated@ is set to false. If there are more configurations to list, @IsTruncated@ is set to true, and there will be a value in @NextContinuationToken@ . You use the @NextContinuationToken@ value to continue the pagination of the list by passing the value in continuation-token in the request to @GET@ the next page.
--
-- To use this operation, you must have permissions to perform the @s3:GetAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- For information about Amazon S3 analytics feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> .
--
-- The following operations are related to @ListBucketAnalyticsConfigurations@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.ListBucketAnalyticsConfigurations
  ( -- * Creating a Request
    listBucketAnalyticsConfigurations,
    ListBucketAnalyticsConfigurations,

    -- * Request Lenses
    lbacContinuationToken,
    lbacExpectedBucketOwner,
    lbacBucket,

    -- * Destructuring the Response
    listBucketAnalyticsConfigurationsResponse,
    ListBucketAnalyticsConfigurationsResponse,

    -- * Response Lenses
    lbacrsAnalyticsConfigurationList,
    lbacrsContinuationToken,
    lbacrsNextContinuationToken,
    lbacrsIsTruncated,
    lbacrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'listBucketAnalyticsConfigurations' smart constructor.
data ListBucketAnalyticsConfigurations = ListBucketAnalyticsConfigurations'
  { _lbacContinuationToken ::
      !(Maybe Text),
    _lbacExpectedBucketOwner ::
      !(Maybe Text),
    _lbacBucket ::
      !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBucketAnalyticsConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbacContinuationToken' - The ContinuationToken that represents a placeholder from where this request should begin.
--
-- * 'lbacExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'lbacBucket' - The name of the bucket from which analytics configurations are retrieved.
listBucketAnalyticsConfigurations ::
  -- | 'lbacBucket'
  BucketName ->
  ListBucketAnalyticsConfigurations
listBucketAnalyticsConfigurations pBucket_ =
  ListBucketAnalyticsConfigurations'
    { _lbacContinuationToken =
        Nothing,
      _lbacExpectedBucketOwner = Nothing,
      _lbacBucket = pBucket_
    }

-- | The ContinuationToken that represents a placeholder from where this request should begin.
lbacContinuationToken :: Lens' ListBucketAnalyticsConfigurations (Maybe Text)
lbacContinuationToken = lens _lbacContinuationToken (\s a -> s {_lbacContinuationToken = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
lbacExpectedBucketOwner :: Lens' ListBucketAnalyticsConfigurations (Maybe Text)
lbacExpectedBucketOwner = lens _lbacExpectedBucketOwner (\s a -> s {_lbacExpectedBucketOwner = a})

-- | The name of the bucket from which analytics configurations are retrieved.
lbacBucket :: Lens' ListBucketAnalyticsConfigurations BucketName
lbacBucket = lens _lbacBucket (\s a -> s {_lbacBucket = a})

instance AWSRequest ListBucketAnalyticsConfigurations where
  type
    Rs ListBucketAnalyticsConfigurations =
      ListBucketAnalyticsConfigurationsResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          ListBucketAnalyticsConfigurationsResponse'
            <$> (may (parseXMLList "AnalyticsConfiguration") x)
            <*> (x .@? "ContinuationToken")
            <*> (x .@? "NextContinuationToken")
            <*> (x .@? "IsTruncated")
            <*> (pure (fromEnum s))
      )

instance Hashable ListBucketAnalyticsConfigurations

instance NFData ListBucketAnalyticsConfigurations

instance ToHeaders ListBucketAnalyticsConfigurations where
  toHeaders ListBucketAnalyticsConfigurations' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _lbacExpectedBucketOwner]

instance ToPath ListBucketAnalyticsConfigurations where
  toPath ListBucketAnalyticsConfigurations' {..} =
    mconcat ["/", toBS _lbacBucket]

instance ToQuery ListBucketAnalyticsConfigurations where
  toQuery ListBucketAnalyticsConfigurations' {..} =
    mconcat
      ["continuation-token" =: _lbacContinuationToken, "analytics"]

-- | /See:/ 'listBucketAnalyticsConfigurationsResponse' smart constructor.
data ListBucketAnalyticsConfigurationsResponse = ListBucketAnalyticsConfigurationsResponse'
  { _lbacrsAnalyticsConfigurationList ::
      !( Maybe
           [AnalyticsConfiguration]
       ),
    _lbacrsContinuationToken ::
      !( Maybe
           Text
       ),
    _lbacrsNextContinuationToken ::
      !( Maybe
           Text
       ),
    _lbacrsIsTruncated ::
      !( Maybe
           Bool
       ),
    _lbacrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListBucketAnalyticsConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbacrsAnalyticsConfigurationList' - The list of analytics configurations for a bucket.
--
-- * 'lbacrsContinuationToken' - The marker that is used as a starting point for this analytics configuration list response. This value is present if it was sent in the request.
--
-- * 'lbacrsNextContinuationToken' - @NextContinuationToken@ is sent when @isTruncated@ is true, which indicates that there are more analytics configurations to list. The next request must include this @NextContinuationToken@ . The token is obfuscated and is not a usable value.
--
-- * 'lbacrsIsTruncated' - Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- * 'lbacrsResponseStatus' - -- | The response status code.
listBucketAnalyticsConfigurationsResponse ::
  -- | 'lbacrsResponseStatus'
  Int ->
  ListBucketAnalyticsConfigurationsResponse
listBucketAnalyticsConfigurationsResponse pResponseStatus_ =
  ListBucketAnalyticsConfigurationsResponse'
    { _lbacrsAnalyticsConfigurationList =
        Nothing,
      _lbacrsContinuationToken = Nothing,
      _lbacrsNextContinuationToken = Nothing,
      _lbacrsIsTruncated = Nothing,
      _lbacrsResponseStatus = pResponseStatus_
    }

-- | The list of analytics configurations for a bucket.
lbacrsAnalyticsConfigurationList :: Lens' ListBucketAnalyticsConfigurationsResponse [AnalyticsConfiguration]
lbacrsAnalyticsConfigurationList = lens _lbacrsAnalyticsConfigurationList (\s a -> s {_lbacrsAnalyticsConfigurationList = a}) . _Default . _Coerce

-- | The marker that is used as a starting point for this analytics configuration list response. This value is present if it was sent in the request.
lbacrsContinuationToken :: Lens' ListBucketAnalyticsConfigurationsResponse (Maybe Text)
lbacrsContinuationToken = lens _lbacrsContinuationToken (\s a -> s {_lbacrsContinuationToken = a})

-- | @NextContinuationToken@ is sent when @isTruncated@ is true, which indicates that there are more analytics configurations to list. The next request must include this @NextContinuationToken@ . The token is obfuscated and is not a usable value.
lbacrsNextContinuationToken :: Lens' ListBucketAnalyticsConfigurationsResponse (Maybe Text)
lbacrsNextContinuationToken = lens _lbacrsNextContinuationToken (\s a -> s {_lbacrsNextContinuationToken = a})

-- | Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
lbacrsIsTruncated :: Lens' ListBucketAnalyticsConfigurationsResponse (Maybe Bool)
lbacrsIsTruncated = lens _lbacrsIsTruncated (\s a -> s {_lbacrsIsTruncated = a})

-- | -- | The response status code.
lbacrsResponseStatus :: Lens' ListBucketAnalyticsConfigurationsResponse Int
lbacrsResponseStatus = lens _lbacrsResponseStatus (\s a -> s {_lbacrsResponseStatus = a})

instance NFData ListBucketAnalyticsConfigurationsResponse
