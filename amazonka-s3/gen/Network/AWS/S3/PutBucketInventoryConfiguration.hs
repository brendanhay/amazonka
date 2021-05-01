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
-- Module      : Network.AWS.S3.PutBucketInventoryConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @PUT@ operation adds an inventory
-- configuration (identified by the inventory ID) to the bucket. You can
-- have up to 1,000 inventory configurations per bucket.
--
-- Amazon S3 inventory generates inventories of the objects in the bucket
-- on a daily or weekly basis, and the results are published to a flat
-- file. The bucket that is inventoried is called the /source/ bucket, and
-- the bucket where the inventory flat file is stored is called the
-- /destination/ bucket. The /destination/ bucket must be in the same AWS
-- Region as the /source/ bucket.
--
-- When you configure an inventory for a /source/ bucket, you specify the
-- /destination/ bucket where you want the inventory to be stored, and
-- whether to generate the inventory daily or weekly. You can also
-- configure what object metadata to include and whether to inventory all
-- object versions or only current versions. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory>
-- in the Amazon Simple Storage Service Developer Guide.
--
-- You must create a bucket policy on the /destination/ bucket to grant
-- permissions to Amazon S3 to write objects to the bucket in the defined
-- location. For an example policy, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/example-bucket-policies.html#example-bucket-policies-use-case-9 Granting Permissions for Amazon S3 Inventory and Storage Class Analysis>.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutInventoryConfiguration@ action. The bucket owner has this
-- permission by default and can grant this permission to others. For more
-- information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>
-- in the Amazon Simple Storage Service Developer Guide.
--
-- __Special Errors__
--
-- -   __HTTP 400 Bad Request Error__
--
--     -   /Code:/ InvalidArgument
--
--     -   /Cause:/ Invalid Argument
--
-- -   __HTTP 400 Bad Request Error__
--
--     -   /Code:/ TooManyConfigurations
--
--     -   /Cause:/ You are attempting to create a new configuration but
--         have already reached the 1,000-configuration limit.
--
-- -   __HTTP 403 Forbidden Error__
--
--     -   /Code:/ AccessDenied
--
--     -   /Cause:/ You are not the owner of the specified bucket, or you
--         do not have the @s3:PutInventoryConfiguration@ bucket permission
--         to set the configuration on the bucket.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
module Network.AWS.S3.PutBucketInventoryConfiguration
  ( -- * Creating a Request
    PutBucketInventoryConfiguration (..),
    newPutBucketInventoryConfiguration,

    -- * Request Lenses
    putBucketInventoryConfiguration_expectedBucketOwner,
    putBucketInventoryConfiguration_bucket,
    putBucketInventoryConfiguration_id,
    putBucketInventoryConfiguration_inventoryConfiguration,

    -- * Destructuring the Response
    PutBucketInventoryConfigurationResponse (..),
    newPutBucketInventoryConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutBucketInventoryConfiguration' smart constructor.
data PutBucketInventoryConfiguration = PutBucketInventoryConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket where the inventory configuration will be stored.
    bucket :: BucketName,
    -- | The ID used to identify the inventory configuration.
    id :: Prelude.Text,
    -- | Specifies the inventory configuration.
    inventoryConfiguration :: InventoryConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketInventoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketInventoryConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'putBucketInventoryConfiguration_bucket' - The name of the bucket where the inventory configuration will be stored.
--
-- 'id', 'putBucketInventoryConfiguration_id' - The ID used to identify the inventory configuration.
--
-- 'inventoryConfiguration', 'putBucketInventoryConfiguration_inventoryConfiguration' - Specifies the inventory configuration.
newPutBucketInventoryConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  -- | 'inventoryConfiguration'
  InventoryConfiguration ->
  PutBucketInventoryConfiguration
newPutBucketInventoryConfiguration
  pBucket_
  pId_
  pInventoryConfiguration_ =
    PutBucketInventoryConfiguration'
      { expectedBucketOwner =
          Prelude.Nothing,
        bucket = pBucket_,
        id = pId_,
        inventoryConfiguration =
          pInventoryConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketInventoryConfiguration_expectedBucketOwner :: Lens.Lens' PutBucketInventoryConfiguration (Prelude.Maybe Prelude.Text)
putBucketInventoryConfiguration_expectedBucketOwner = Lens.lens (\PutBucketInventoryConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketInventoryConfiguration' {} a -> s {expectedBucketOwner = a} :: PutBucketInventoryConfiguration)

-- | The name of the bucket where the inventory configuration will be stored.
putBucketInventoryConfiguration_bucket :: Lens.Lens' PutBucketInventoryConfiguration BucketName
putBucketInventoryConfiguration_bucket = Lens.lens (\PutBucketInventoryConfiguration' {bucket} -> bucket) (\s@PutBucketInventoryConfiguration' {} a -> s {bucket = a} :: PutBucketInventoryConfiguration)

-- | The ID used to identify the inventory configuration.
putBucketInventoryConfiguration_id :: Lens.Lens' PutBucketInventoryConfiguration Prelude.Text
putBucketInventoryConfiguration_id = Lens.lens (\PutBucketInventoryConfiguration' {id} -> id) (\s@PutBucketInventoryConfiguration' {} a -> s {id = a} :: PutBucketInventoryConfiguration)

-- | Specifies the inventory configuration.
putBucketInventoryConfiguration_inventoryConfiguration :: Lens.Lens' PutBucketInventoryConfiguration InventoryConfiguration
putBucketInventoryConfiguration_inventoryConfiguration = Lens.lens (\PutBucketInventoryConfiguration' {inventoryConfiguration} -> inventoryConfiguration) (\s@PutBucketInventoryConfiguration' {} a -> s {inventoryConfiguration = a} :: PutBucketInventoryConfiguration)

instance
  Prelude.AWSRequest
    PutBucketInventoryConfiguration
  where
  type
    Rs PutBucketInventoryConfiguration =
      PutBucketInventoryConfigurationResponse
  request = Request.putXML defaultService
  response =
    Response.receiveNull
      PutBucketInventoryConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketInventoryConfiguration

instance
  Prelude.NFData
    PutBucketInventoryConfiguration

instance
  Prelude.ToElement
    PutBucketInventoryConfiguration
  where
  toElement PutBucketInventoryConfiguration' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}InventoryConfiguration"
      inventoryConfiguration

instance
  Prelude.ToHeaders
    PutBucketInventoryConfiguration
  where
  toHeaders PutBucketInventoryConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance
  Prelude.ToPath
    PutBucketInventoryConfiguration
  where
  toPath PutBucketInventoryConfiguration' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance
  Prelude.ToQuery
    PutBucketInventoryConfiguration
  where
  toQuery PutBucketInventoryConfiguration' {..} =
    Prelude.mconcat ["id" Prelude.=: id, "inventory"]

-- | /See:/ 'newPutBucketInventoryConfigurationResponse' smart constructor.
data PutBucketInventoryConfigurationResponse = PutBucketInventoryConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketInventoryConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketInventoryConfigurationResponse ::
  PutBucketInventoryConfigurationResponse
newPutBucketInventoryConfigurationResponse =
  PutBucketInventoryConfigurationResponse'

instance
  Prelude.NFData
    PutBucketInventoryConfigurationResponse
