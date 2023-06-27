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
-- Module      : Amazonka.S3.PutBucketInventoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @PUT@ action adds an inventory configuration
-- (identified by the inventory ID) to the bucket. You can have up to 1,000
-- inventory configurations per bucket.
--
-- Amazon S3 inventory generates inventories of the objects in the bucket
-- on a daily or weekly basis, and the results are published to a flat
-- file. The bucket that is inventoried is called the /source/ bucket, and
-- the bucket where the inventory flat file is stored is called the
-- /destination/ bucket. The /destination/ bucket must be in the same
-- Amazon Web Services Region as the /source/ bucket.
--
-- When you configure an inventory for a /source/ bucket, you specify the
-- /destination/ bucket where you want the inventory to be stored, and
-- whether to generate the inventory daily or weekly. You can also
-- configure what object metadata to include and whether to inventory all
-- object versions or only current versions. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory>
-- in the Amazon S3 User Guide.
--
-- You must create a bucket policy on the /destination/ bucket to grant
-- permissions to Amazon S3 to write objects to the bucket in the defined
-- location. For an example policy, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/example-bucket-policies.html#example-bucket-policies-use-case-9 Granting Permissions for Amazon S3 Inventory and Storage Class Analysis>.
--
-- [Permissions]
--     To use this operation, you must have permission to perform the
--     @s3:PutInventoryConfiguration@ action. The bucket owner has this
--     permission by default and can grant this permission to others.
--
--     The @s3:PutInventoryConfiguration@ permission allows a user to
--     create an
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/storage-inventory.html S3 Inventory>
--     report that includes all object metadata fields available and to
--     specify the destination bucket to store the inventory. A user with
--     read access to objects in the destination bucket can also access all
--     object metadata fields that are available in the inventory report.
--
--     To restrict access to an inventory report, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/example-bucket-policies.html#example-bucket-policies-use-case-10 Restricting access to an Amazon S3 Inventory report>
--     in the /Amazon S3 User Guide/. For more information about the
--     metadata fields available in S3 Inventory, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/storage-inventory.html#storage-inventory-contents Amazon S3 Inventory lists>
--     in the /Amazon S3 User Guide/. For more information about
--     permissions, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions related to bucket subresource operations>
--     and
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Identity and access management in Amazon S3>
--     in the /Amazon S3 User Guide/.
--
-- @PutBucketInventoryConfiguration@ has the following special errors:
--
-- [HTTP 400 Bad Request Error]
--     /Code:/ InvalidArgument
--
--     /Cause:/ Invalid Argument
--
-- [HTTP 400 Bad Request Error]
--     /Code:/ TooManyConfigurations
--
--     /Cause:/ You are attempting to create a new configuration but have
--     already reached the 1,000-configuration limit.
--
-- [HTTP 403 Forbidden Error]
--     /Cause:/ You are not the owner of the specified bucket, or you do
--     not have the @s3:PutInventoryConfiguration@ bucket permission to set
--     the configuration on the bucket.
--
-- The following operations are related to
-- @PutBucketInventoryConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
module Amazonka.S3.PutBucketInventoryConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketInventoryConfiguration' smart constructor.
data PutBucketInventoryConfiguration = PutBucketInventoryConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket where the inventory configuration will be stored.
    bucket :: BucketName,
    -- | The ID used to identify the inventory configuration.
    id :: Prelude.Text,
    -- | Specifies the inventory configuration.
    inventoryConfiguration :: InventoryConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketInventoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketInventoryConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
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

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
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
  Core.AWSRequest
    PutBucketInventoryConfiguration
  where
  type
    AWSResponse PutBucketInventoryConfiguration =
      PutBucketInventoryConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull
      PutBucketInventoryConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketInventoryConfiguration
  where
  hashWithSalt
    _salt
    PutBucketInventoryConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` inventoryConfiguration

instance
  Prelude.NFData
    PutBucketInventoryConfiguration
  where
  rnf PutBucketInventoryConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf inventoryConfiguration

instance
  Data.ToElement
    PutBucketInventoryConfiguration
  where
  toElement PutBucketInventoryConfiguration' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}InventoryConfiguration"
      inventoryConfiguration

instance
  Data.ToHeaders
    PutBucketInventoryConfiguration
  where
  toHeaders PutBucketInventoryConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketInventoryConfiguration where
  toPath PutBucketInventoryConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketInventoryConfiguration where
  toQuery PutBucketInventoryConfiguration' {..} =
    Prelude.mconcat ["id" Data.=: id, "inventory"]

-- | /See:/ 'newPutBucketInventoryConfigurationResponse' smart constructor.
data PutBucketInventoryConfigurationResponse = PutBucketInventoryConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
