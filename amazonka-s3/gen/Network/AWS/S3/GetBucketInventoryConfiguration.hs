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
-- Module      : Network.AWS.S3.GetBucketInventoryConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an inventory configuration (identified by the inventory
-- configuration ID) from the bucket.
--
-- To use this operation, you must have permissions to perform the
-- @s3:GetInventoryConfiguration@ action. The bucket owner has this
-- permission by default and can grant this permission to others. For more
-- information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For information about the Amazon S3 inventory feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory>.
--
-- The following operations are related to
-- @GetBucketInventoryConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
module Network.AWS.S3.GetBucketInventoryConfiguration
  ( -- * Creating a Request
    GetBucketInventoryConfiguration (..),
    newGetBucketInventoryConfiguration,

    -- * Request Lenses
    getBucketInventoryConfiguration_expectedBucketOwner,
    getBucketInventoryConfiguration_bucket,
    getBucketInventoryConfiguration_id,

    -- * Destructuring the Response
    GetBucketInventoryConfigurationResponse (..),
    newGetBucketInventoryConfigurationResponse,

    -- * Response Lenses
    getBucketInventoryConfigurationResponse_inventoryConfiguration,
    getBucketInventoryConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketInventoryConfiguration' smart constructor.
data GetBucketInventoryConfiguration = GetBucketInventoryConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the inventory configuration to
    -- retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the inventory configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketInventoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketInventoryConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketInventoryConfiguration_bucket' - The name of the bucket containing the inventory configuration to
-- retrieve.
--
-- 'id', 'getBucketInventoryConfiguration_id' - The ID used to identify the inventory configuration.
newGetBucketInventoryConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  GetBucketInventoryConfiguration
newGetBucketInventoryConfiguration pBucket_ pId_ =
  GetBucketInventoryConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketInventoryConfiguration_expectedBucketOwner :: Lens.Lens' GetBucketInventoryConfiguration (Prelude.Maybe Prelude.Text)
getBucketInventoryConfiguration_expectedBucketOwner = Lens.lens (\GetBucketInventoryConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketInventoryConfiguration' {} a -> s {expectedBucketOwner = a} :: GetBucketInventoryConfiguration)

-- | The name of the bucket containing the inventory configuration to
-- retrieve.
getBucketInventoryConfiguration_bucket :: Lens.Lens' GetBucketInventoryConfiguration BucketName
getBucketInventoryConfiguration_bucket = Lens.lens (\GetBucketInventoryConfiguration' {bucket} -> bucket) (\s@GetBucketInventoryConfiguration' {} a -> s {bucket = a} :: GetBucketInventoryConfiguration)

-- | The ID used to identify the inventory configuration.
getBucketInventoryConfiguration_id :: Lens.Lens' GetBucketInventoryConfiguration Prelude.Text
getBucketInventoryConfiguration_id = Lens.lens (\GetBucketInventoryConfiguration' {id} -> id) (\s@GetBucketInventoryConfiguration' {} a -> s {id = a} :: GetBucketInventoryConfiguration)

instance
  Prelude.AWSRequest
    GetBucketInventoryConfiguration
  where
  type
    Rs GetBucketInventoryConfiguration =
      GetBucketInventoryConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketInventoryConfigurationResponse'
            Prelude.<$> (Prelude.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetBucketInventoryConfiguration

instance
  Prelude.NFData
    GetBucketInventoryConfiguration

instance
  Prelude.ToHeaders
    GetBucketInventoryConfiguration
  where
  toHeaders GetBucketInventoryConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance
  Prelude.ToPath
    GetBucketInventoryConfiguration
  where
  toPath GetBucketInventoryConfiguration' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance
  Prelude.ToQuery
    GetBucketInventoryConfiguration
  where
  toQuery GetBucketInventoryConfiguration' {..} =
    Prelude.mconcat ["id" Prelude.=: id, "inventory"]

-- | /See:/ 'newGetBucketInventoryConfigurationResponse' smart constructor.
data GetBucketInventoryConfigurationResponse = GetBucketInventoryConfigurationResponse'
  { -- | Specifies the inventory configuration.
    inventoryConfiguration :: Prelude.Maybe InventoryConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketInventoryConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inventoryConfiguration', 'getBucketInventoryConfigurationResponse_inventoryConfiguration' - Specifies the inventory configuration.
--
-- 'httpStatus', 'getBucketInventoryConfigurationResponse_httpStatus' - The response's http status code.
newGetBucketInventoryConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketInventoryConfigurationResponse
newGetBucketInventoryConfigurationResponse
  pHttpStatus_ =
    GetBucketInventoryConfigurationResponse'
      { inventoryConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Specifies the inventory configuration.
getBucketInventoryConfigurationResponse_inventoryConfiguration :: Lens.Lens' GetBucketInventoryConfigurationResponse (Prelude.Maybe InventoryConfiguration)
getBucketInventoryConfigurationResponse_inventoryConfiguration = Lens.lens (\GetBucketInventoryConfigurationResponse' {inventoryConfiguration} -> inventoryConfiguration) (\s@GetBucketInventoryConfigurationResponse' {} a -> s {inventoryConfiguration = a} :: GetBucketInventoryConfigurationResponse)

-- | The response's http status code.
getBucketInventoryConfigurationResponse_httpStatus :: Lens.Lens' GetBucketInventoryConfigurationResponse Prelude.Int
getBucketInventoryConfigurationResponse_httpStatus = Lens.lens (\GetBucketInventoryConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBucketInventoryConfigurationResponse' {} a -> s {httpStatus = a} :: GetBucketInventoryConfigurationResponse)

instance
  Prelude.NFData
    GetBucketInventoryConfigurationResponse
