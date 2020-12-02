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
-- Module      : Network.AWS.S3.GetBucketInventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an inventory configuration (identified by the inventory configuration ID) from the bucket.
--
--
-- To use this operation, you must have permissions to perform the @s3:GetInventoryConfiguration@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory> .
--
-- The following operations are related to @GetBucketInventoryConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
module Network.AWS.S3.GetBucketInventoryConfiguration
  ( -- * Creating a Request
    getBucketInventoryConfiguration,
    GetBucketInventoryConfiguration,

    -- * Request Lenses
    gbicExpectedBucketOwner,
    gbicBucket,
    gbicId,

    -- * Destructuring the Response
    getBucketInventoryConfigurationResponse,
    GetBucketInventoryConfigurationResponse,

    -- * Response Lenses
    gbicrsInventoryConfiguration,
    gbicrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketInventoryConfiguration' smart constructor.
data GetBucketInventoryConfiguration = GetBucketInventoryConfiguration'
  { _gbicExpectedBucketOwner ::
      !(Maybe Text),
    _gbicBucket :: !BucketName,
    _gbicId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketInventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbicExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbicBucket' - The name of the bucket containing the inventory configuration to retrieve.
--
-- * 'gbicId' - The ID used to identify the inventory configuration.
getBucketInventoryConfiguration ::
  -- | 'gbicBucket'
  BucketName ->
  -- | 'gbicId'
  Text ->
  GetBucketInventoryConfiguration
getBucketInventoryConfiguration pBucket_ pId_ =
  GetBucketInventoryConfiguration'
    { _gbicExpectedBucketOwner =
        Nothing,
      _gbicBucket = pBucket_,
      _gbicId = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbicExpectedBucketOwner :: Lens' GetBucketInventoryConfiguration (Maybe Text)
gbicExpectedBucketOwner = lens _gbicExpectedBucketOwner (\s a -> s {_gbicExpectedBucketOwner = a})

-- | The name of the bucket containing the inventory configuration to retrieve.
gbicBucket :: Lens' GetBucketInventoryConfiguration BucketName
gbicBucket = lens _gbicBucket (\s a -> s {_gbicBucket = a})

-- | The ID used to identify the inventory configuration.
gbicId :: Lens' GetBucketInventoryConfiguration Text
gbicId = lens _gbicId (\s a -> s {_gbicId = a})

instance AWSRequest GetBucketInventoryConfiguration where
  type
    Rs GetBucketInventoryConfiguration =
      GetBucketInventoryConfigurationResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketInventoryConfigurationResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetBucketInventoryConfiguration

instance NFData GetBucketInventoryConfiguration

instance ToHeaders GetBucketInventoryConfiguration where
  toHeaders GetBucketInventoryConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbicExpectedBucketOwner]

instance ToPath GetBucketInventoryConfiguration where
  toPath GetBucketInventoryConfiguration' {..} =
    mconcat ["/", toBS _gbicBucket]

instance ToQuery GetBucketInventoryConfiguration where
  toQuery GetBucketInventoryConfiguration' {..} =
    mconcat ["id" =: _gbicId, "inventory"]

-- | /See:/ 'getBucketInventoryConfigurationResponse' smart constructor.
data GetBucketInventoryConfigurationResponse = GetBucketInventoryConfigurationResponse'
  { _gbicrsInventoryConfiguration ::
      !( Maybe
           InventoryConfiguration
       ),
    _gbicrsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketInventoryConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbicrsInventoryConfiguration' - Specifies the inventory configuration.
--
-- * 'gbicrsResponseStatus' - -- | The response status code.
getBucketInventoryConfigurationResponse ::
  -- | 'gbicrsResponseStatus'
  Int ->
  GetBucketInventoryConfigurationResponse
getBucketInventoryConfigurationResponse pResponseStatus_ =
  GetBucketInventoryConfigurationResponse'
    { _gbicrsInventoryConfiguration =
        Nothing,
      _gbicrsResponseStatus = pResponseStatus_
    }

-- | Specifies the inventory configuration.
gbicrsInventoryConfiguration :: Lens' GetBucketInventoryConfigurationResponse (Maybe InventoryConfiguration)
gbicrsInventoryConfiguration = lens _gbicrsInventoryConfiguration (\s a -> s {_gbicrsInventoryConfiguration = a})

-- | -- | The response status code.
gbicrsResponseStatus :: Lens' GetBucketInventoryConfigurationResponse Int
gbicrsResponseStatus = lens _gbicrsResponseStatus (\s a -> s {_gbicrsResponseStatus = a})

instance NFData GetBucketInventoryConfigurationResponse
