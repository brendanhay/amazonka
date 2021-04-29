{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryS3BucketDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryS3BucketDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryEncryption
import Network.AWS.S3.Types.InventoryFormat

-- | Contains the bucket name, file format, bucket owner (optional), and
-- prefix (optional) where inventory results are published.
--
-- /See:/ 'newInventoryS3BucketDestination' smart constructor.
data InventoryS3BucketDestination = InventoryS3BucketDestination'
  { -- | The account ID that owns the destination S3 bucket. If no account ID is
    -- provided, the owner is not validated before exporting data.
    --
    -- Although this value is optional, we strongly recommend that you set it
    -- to help prevent problems if the destination bucket ownership changes.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The prefix that is prepended to all inventory results.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Contains the type of server-side encryption used to encrypt the
    -- inventory results.
    encryption :: Prelude.Maybe InventoryEncryption,
    -- | The Amazon Resource Name (ARN) of the bucket where inventory results
    -- will be published.
    bucket :: BucketName,
    -- | Specifies the output format of the inventory results.
    format :: InventoryFormat
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InventoryS3BucketDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'inventoryS3BucketDestination_accountId' - The account ID that owns the destination S3 bucket. If no account ID is
-- provided, the owner is not validated before exporting data.
--
-- Although this value is optional, we strongly recommend that you set it
-- to help prevent problems if the destination bucket ownership changes.
--
-- 'prefix', 'inventoryS3BucketDestination_prefix' - The prefix that is prepended to all inventory results.
--
-- 'encryption', 'inventoryS3BucketDestination_encryption' - Contains the type of server-side encryption used to encrypt the
-- inventory results.
--
-- 'bucket', 'inventoryS3BucketDestination_bucket' - The Amazon Resource Name (ARN) of the bucket where inventory results
-- will be published.
--
-- 'format', 'inventoryS3BucketDestination_format' - Specifies the output format of the inventory results.
newInventoryS3BucketDestination ::
  -- | 'bucket'
  BucketName ->
  -- | 'format'
  InventoryFormat ->
  InventoryS3BucketDestination
newInventoryS3BucketDestination pBucket_ pFormat_ =
  InventoryS3BucketDestination'
    { accountId =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      encryption = Prelude.Nothing,
      bucket = pBucket_,
      format = pFormat_
    }

-- | The account ID that owns the destination S3 bucket. If no account ID is
-- provided, the owner is not validated before exporting data.
--
-- Although this value is optional, we strongly recommend that you set it
-- to help prevent problems if the destination bucket ownership changes.
inventoryS3BucketDestination_accountId :: Lens.Lens' InventoryS3BucketDestination (Prelude.Maybe Prelude.Text)
inventoryS3BucketDestination_accountId = Lens.lens (\InventoryS3BucketDestination' {accountId} -> accountId) (\s@InventoryS3BucketDestination' {} a -> s {accountId = a} :: InventoryS3BucketDestination)

-- | The prefix that is prepended to all inventory results.
inventoryS3BucketDestination_prefix :: Lens.Lens' InventoryS3BucketDestination (Prelude.Maybe Prelude.Text)
inventoryS3BucketDestination_prefix = Lens.lens (\InventoryS3BucketDestination' {prefix} -> prefix) (\s@InventoryS3BucketDestination' {} a -> s {prefix = a} :: InventoryS3BucketDestination)

-- | Contains the type of server-side encryption used to encrypt the
-- inventory results.
inventoryS3BucketDestination_encryption :: Lens.Lens' InventoryS3BucketDestination (Prelude.Maybe InventoryEncryption)
inventoryS3BucketDestination_encryption = Lens.lens (\InventoryS3BucketDestination' {encryption} -> encryption) (\s@InventoryS3BucketDestination' {} a -> s {encryption = a} :: InventoryS3BucketDestination)

-- | The Amazon Resource Name (ARN) of the bucket where inventory results
-- will be published.
inventoryS3BucketDestination_bucket :: Lens.Lens' InventoryS3BucketDestination BucketName
inventoryS3BucketDestination_bucket = Lens.lens (\InventoryS3BucketDestination' {bucket} -> bucket) (\s@InventoryS3BucketDestination' {} a -> s {bucket = a} :: InventoryS3BucketDestination)

-- | Specifies the output format of the inventory results.
inventoryS3BucketDestination_format :: Lens.Lens' InventoryS3BucketDestination InventoryFormat
inventoryS3BucketDestination_format = Lens.lens (\InventoryS3BucketDestination' {format} -> format) (\s@InventoryS3BucketDestination' {} a -> s {format = a} :: InventoryS3BucketDestination)

instance Prelude.FromXML InventoryS3BucketDestination where
  parseXML x =
    InventoryS3BucketDestination'
      Prelude.<$> (x Prelude..@? "AccountId")
      Prelude.<*> (x Prelude..@? "Prefix")
      Prelude.<*> (x Prelude..@? "Encryption")
      Prelude.<*> (x Prelude..@ "Bucket")
      Prelude.<*> (x Prelude..@ "Format")

instance
  Prelude.Hashable
    InventoryS3BucketDestination

instance Prelude.NFData InventoryS3BucketDestination

instance Prelude.ToXML InventoryS3BucketDestination where
  toXML InventoryS3BucketDestination' {..} =
    Prelude.mconcat
      [ "AccountId" Prelude.@= accountId,
        "Prefix" Prelude.@= prefix,
        "Encryption" Prelude.@= encryption,
        "Bucket" Prelude.@= bucket,
        "Format" Prelude.@= format
      ]
