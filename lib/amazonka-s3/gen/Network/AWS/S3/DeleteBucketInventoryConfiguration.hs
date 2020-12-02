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
-- Module      : Network.AWS.S3.DeleteBucketInventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an inventory configuration (identified by the inventory ID) from the bucket.
--
--
-- To use this operation, you must have permissions to perform the @s3:PutInventoryConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory> .
--
-- Operations related to @DeleteBucketInventoryConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
module Network.AWS.S3.DeleteBucketInventoryConfiguration
  ( -- * Creating a Request
    deleteBucketInventoryConfiguration,
    DeleteBucketInventoryConfiguration,

    -- * Request Lenses
    dbicExpectedBucketOwner,
    dbicBucket,
    dbicId,

    -- * Destructuring the Response
    deleteBucketInventoryConfigurationResponse,
    DeleteBucketInventoryConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketInventoryConfiguration' smart constructor.
data DeleteBucketInventoryConfiguration = DeleteBucketInventoryConfiguration'
  { _dbicExpectedBucketOwner ::
      !(Maybe Text),
    _dbicBucket ::
      !BucketName,
    _dbicId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketInventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbicExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbicBucket' - The name of the bucket containing the inventory configuration to delete.
--
-- * 'dbicId' - The ID used to identify the inventory configuration.
deleteBucketInventoryConfiguration ::
  -- | 'dbicBucket'
  BucketName ->
  -- | 'dbicId'
  Text ->
  DeleteBucketInventoryConfiguration
deleteBucketInventoryConfiguration pBucket_ pId_ =
  DeleteBucketInventoryConfiguration'
    { _dbicExpectedBucketOwner =
        Nothing,
      _dbicBucket = pBucket_,
      _dbicId = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbicExpectedBucketOwner :: Lens' DeleteBucketInventoryConfiguration (Maybe Text)
dbicExpectedBucketOwner = lens _dbicExpectedBucketOwner (\s a -> s {_dbicExpectedBucketOwner = a})

-- | The name of the bucket containing the inventory configuration to delete.
dbicBucket :: Lens' DeleteBucketInventoryConfiguration BucketName
dbicBucket = lens _dbicBucket (\s a -> s {_dbicBucket = a})

-- | The ID used to identify the inventory configuration.
dbicId :: Lens' DeleteBucketInventoryConfiguration Text
dbicId = lens _dbicId (\s a -> s {_dbicId = a})

instance AWSRequest DeleteBucketInventoryConfiguration where
  type
    Rs DeleteBucketInventoryConfiguration =
      DeleteBucketInventoryConfigurationResponse
  request = delete s3
  response = receiveNull DeleteBucketInventoryConfigurationResponse'

instance Hashable DeleteBucketInventoryConfiguration

instance NFData DeleteBucketInventoryConfiguration

instance ToHeaders DeleteBucketInventoryConfiguration where
  toHeaders DeleteBucketInventoryConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbicExpectedBucketOwner]

instance ToPath DeleteBucketInventoryConfiguration where
  toPath DeleteBucketInventoryConfiguration' {..} =
    mconcat ["/", toBS _dbicBucket]

instance ToQuery DeleteBucketInventoryConfiguration where
  toQuery DeleteBucketInventoryConfiguration' {..} =
    mconcat ["id" =: _dbicId, "inventory"]

-- | /See:/ 'deleteBucketInventoryConfigurationResponse' smart constructor.
data DeleteBucketInventoryConfigurationResponse = DeleteBucketInventoryConfigurationResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteBucketInventoryConfigurationResponse' with the minimum fields required to make a request.
deleteBucketInventoryConfigurationResponse ::
  DeleteBucketInventoryConfigurationResponse
deleteBucketInventoryConfigurationResponse =
  DeleteBucketInventoryConfigurationResponse'

instance NFData DeleteBucketInventoryConfigurationResponse
