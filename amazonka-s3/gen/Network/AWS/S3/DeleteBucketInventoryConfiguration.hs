{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketInventoryConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an inventory configuration (identified by the inventory ID) from
-- the bucket.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutInventoryConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For information about the Amazon S3 inventory feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory>.
--
-- Operations related to @DeleteBucketInventoryConfiguration@ include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
module Network.AWS.S3.DeleteBucketInventoryConfiguration
  ( -- * Creating a Request
    DeleteBucketInventoryConfiguration (..),
    newDeleteBucketInventoryConfiguration,

    -- * Request Lenses
    deleteBucketInventoryConfiguration_expectedBucketOwner,
    deleteBucketInventoryConfiguration_bucket,
    deleteBucketInventoryConfiguration_id,

    -- * Destructuring the Response
    DeleteBucketInventoryConfigurationResponse (..),
    newDeleteBucketInventoryConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketInventoryConfiguration' smart constructor.
data DeleteBucketInventoryConfiguration = DeleteBucketInventoryConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the inventory configuration to delete.
    bucket :: BucketName,
    -- | The ID used to identify the inventory configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketInventoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketInventoryConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucketInventoryConfiguration_bucket' - The name of the bucket containing the inventory configuration to delete.
--
-- 'id', 'deleteBucketInventoryConfiguration_id' - The ID used to identify the inventory configuration.
newDeleteBucketInventoryConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  DeleteBucketInventoryConfiguration
newDeleteBucketInventoryConfiguration pBucket_ pId_ =
  DeleteBucketInventoryConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketInventoryConfiguration_expectedBucketOwner :: Lens.Lens' DeleteBucketInventoryConfiguration (Prelude.Maybe Prelude.Text)
deleteBucketInventoryConfiguration_expectedBucketOwner = Lens.lens (\DeleteBucketInventoryConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketInventoryConfiguration' {} a -> s {expectedBucketOwner = a} :: DeleteBucketInventoryConfiguration)

-- | The name of the bucket containing the inventory configuration to delete.
deleteBucketInventoryConfiguration_bucket :: Lens.Lens' DeleteBucketInventoryConfiguration BucketName
deleteBucketInventoryConfiguration_bucket = Lens.lens (\DeleteBucketInventoryConfiguration' {bucket} -> bucket) (\s@DeleteBucketInventoryConfiguration' {} a -> s {bucket = a} :: DeleteBucketInventoryConfiguration)

-- | The ID used to identify the inventory configuration.
deleteBucketInventoryConfiguration_id :: Lens.Lens' DeleteBucketInventoryConfiguration Prelude.Text
deleteBucketInventoryConfiguration_id = Lens.lens (\DeleteBucketInventoryConfiguration' {id} -> id) (\s@DeleteBucketInventoryConfiguration' {} a -> s {id = a} :: DeleteBucketInventoryConfiguration)

instance
  Prelude.AWSRequest
    DeleteBucketInventoryConfiguration
  where
  type
    Rs DeleteBucketInventoryConfiguration =
      DeleteBucketInventoryConfigurationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteBucketInventoryConfigurationResponse'

instance
  Prelude.Hashable
    DeleteBucketInventoryConfiguration

instance
  Prelude.NFData
    DeleteBucketInventoryConfiguration

instance
  Prelude.ToHeaders
    DeleteBucketInventoryConfiguration
  where
  toHeaders DeleteBucketInventoryConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance
  Prelude.ToPath
    DeleteBucketInventoryConfiguration
  where
  toPath DeleteBucketInventoryConfiguration' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance
  Prelude.ToQuery
    DeleteBucketInventoryConfiguration
  where
  toQuery DeleteBucketInventoryConfiguration' {..} =
    Prelude.mconcat ["id" Prelude.=: id, "inventory"]

-- | /See:/ 'newDeleteBucketInventoryConfigurationResponse' smart constructor.
data DeleteBucketInventoryConfigurationResponse = DeleteBucketInventoryConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketInventoryConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketInventoryConfigurationResponse ::
  DeleteBucketInventoryConfigurationResponse
newDeleteBucketInventoryConfigurationResponse =
  DeleteBucketInventoryConfigurationResponse'

instance
  Prelude.NFData
    DeleteBucketInventoryConfigurationResponse
