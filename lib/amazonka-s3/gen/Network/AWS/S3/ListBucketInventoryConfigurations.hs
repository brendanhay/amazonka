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
-- Module      : Network.AWS.S3.ListBucketInventoryConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of inventory configurations for the bucket. You can have up to 1,000 analytics configurations per bucket.
--
--
-- This operation supports list pagination and does not return more than 100 configurations at a time. Always check the @IsTruncated@ element in the response. If there are no more configurations to list, @IsTruncated@ is set to false. If there are more configurations to list, @IsTruncated@ is set to true, and there is a value in @NextContinuationToken@ . You use the @NextContinuationToken@ value to continue the pagination of the list by passing the value in continuation-token in the request to @GET@ the next page.
--
-- To use this operation, you must have permissions to perform the @s3:GetInventoryConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory>
--
-- The following operations are related to @ListBucketInventoryConfigurations@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
module Network.AWS.S3.ListBucketInventoryConfigurations
  ( -- * Creating a Request
    listBucketInventoryConfigurations,
    ListBucketInventoryConfigurations,

    -- * Request Lenses
    lbicContinuationToken,
    lbicExpectedBucketOwner,
    lbicBucket,

    -- * Destructuring the Response
    listBucketInventoryConfigurationsResponse,
    ListBucketInventoryConfigurationsResponse,

    -- * Response Lenses
    lbicrsContinuationToken,
    lbicrsInventoryConfigurationList,
    lbicrsNextContinuationToken,
    lbicrsIsTruncated,
    lbicrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'listBucketInventoryConfigurations' smart constructor.
data ListBucketInventoryConfigurations = ListBucketInventoryConfigurations'
  { _lbicContinuationToken ::
      !(Maybe Text),
    _lbicExpectedBucketOwner ::
      !(Maybe Text),
    _lbicBucket ::
      !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBucketInventoryConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbicContinuationToken' - The marker used to continue an inventory configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- * 'lbicExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'lbicBucket' - The name of the bucket containing the inventory configurations to retrieve.
listBucketInventoryConfigurations ::
  -- | 'lbicBucket'
  BucketName ->
  ListBucketInventoryConfigurations
listBucketInventoryConfigurations pBucket_ =
  ListBucketInventoryConfigurations'
    { _lbicContinuationToken =
        Nothing,
      _lbicExpectedBucketOwner = Nothing,
      _lbicBucket = pBucket_
    }

-- | The marker used to continue an inventory configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
lbicContinuationToken :: Lens' ListBucketInventoryConfigurations (Maybe Text)
lbicContinuationToken = lens _lbicContinuationToken (\s a -> s {_lbicContinuationToken = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
lbicExpectedBucketOwner :: Lens' ListBucketInventoryConfigurations (Maybe Text)
lbicExpectedBucketOwner = lens _lbicExpectedBucketOwner (\s a -> s {_lbicExpectedBucketOwner = a})

-- | The name of the bucket containing the inventory configurations to retrieve.
lbicBucket :: Lens' ListBucketInventoryConfigurations BucketName
lbicBucket = lens _lbicBucket (\s a -> s {_lbicBucket = a})

instance AWSRequest ListBucketInventoryConfigurations where
  type
    Rs ListBucketInventoryConfigurations =
      ListBucketInventoryConfigurationsResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          ListBucketInventoryConfigurationsResponse'
            <$> (x .@? "ContinuationToken")
            <*> (may (parseXMLList "InventoryConfiguration") x)
            <*> (x .@? "NextContinuationToken")
            <*> (x .@? "IsTruncated")
            <*> (pure (fromEnum s))
      )

instance Hashable ListBucketInventoryConfigurations

instance NFData ListBucketInventoryConfigurations

instance ToHeaders ListBucketInventoryConfigurations where
  toHeaders ListBucketInventoryConfigurations' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _lbicExpectedBucketOwner]

instance ToPath ListBucketInventoryConfigurations where
  toPath ListBucketInventoryConfigurations' {..} =
    mconcat ["/", toBS _lbicBucket]

instance ToQuery ListBucketInventoryConfigurations where
  toQuery ListBucketInventoryConfigurations' {..} =
    mconcat
      ["continuation-token" =: _lbicContinuationToken, "inventory"]

-- | /See:/ 'listBucketInventoryConfigurationsResponse' smart constructor.
data ListBucketInventoryConfigurationsResponse = ListBucketInventoryConfigurationsResponse'
  { _lbicrsContinuationToken ::
      !( Maybe
           Text
       ),
    _lbicrsInventoryConfigurationList ::
      !( Maybe
           [InventoryConfiguration]
       ),
    _lbicrsNextContinuationToken ::
      !( Maybe
           Text
       ),
    _lbicrsIsTruncated ::
      !( Maybe
           Bool
       ),
    _lbicrsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBucketInventoryConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbicrsContinuationToken' - If sent in the request, the marker that is used as a starting point for this inventory configuration list response.
--
-- * 'lbicrsInventoryConfigurationList' - The list of inventory configurations for a bucket.
--
-- * 'lbicrsNextContinuationToken' - The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
--
-- * 'lbicrsIsTruncated' - Tells whether the returned list of inventory configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken is provided for a subsequent request.
--
-- * 'lbicrsResponseStatus' - -- | The response status code.
listBucketInventoryConfigurationsResponse ::
  -- | 'lbicrsResponseStatus'
  Int ->
  ListBucketInventoryConfigurationsResponse
listBucketInventoryConfigurationsResponse pResponseStatus_ =
  ListBucketInventoryConfigurationsResponse'
    { _lbicrsContinuationToken =
        Nothing,
      _lbicrsInventoryConfigurationList = Nothing,
      _lbicrsNextContinuationToken = Nothing,
      _lbicrsIsTruncated = Nothing,
      _lbicrsResponseStatus = pResponseStatus_
    }

-- | If sent in the request, the marker that is used as a starting point for this inventory configuration list response.
lbicrsContinuationToken :: Lens' ListBucketInventoryConfigurationsResponse (Maybe Text)
lbicrsContinuationToken = lens _lbicrsContinuationToken (\s a -> s {_lbicrsContinuationToken = a})

-- | The list of inventory configurations for a bucket.
lbicrsInventoryConfigurationList :: Lens' ListBucketInventoryConfigurationsResponse [InventoryConfiguration]
lbicrsInventoryConfigurationList = lens _lbicrsInventoryConfigurationList (\s a -> s {_lbicrsInventoryConfigurationList = a}) . _Default . _Coerce

-- | The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
lbicrsNextContinuationToken :: Lens' ListBucketInventoryConfigurationsResponse (Maybe Text)
lbicrsNextContinuationToken = lens _lbicrsNextContinuationToken (\s a -> s {_lbicrsNextContinuationToken = a})

-- | Tells whether the returned list of inventory configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken is provided for a subsequent request.
lbicrsIsTruncated :: Lens' ListBucketInventoryConfigurationsResponse (Maybe Bool)
lbicrsIsTruncated = lens _lbicrsIsTruncated (\s a -> s {_lbicrsIsTruncated = a})

-- | -- | The response status code.
lbicrsResponseStatus :: Lens' ListBucketInventoryConfigurationsResponse Int
lbicrsResponseStatus = lens _lbicrsResponseStatus (\s a -> s {_lbicrsResponseStatus = a})

instance NFData ListBucketInventoryConfigurationsResponse
