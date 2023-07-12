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
-- Module      : Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | S3-customer-managed; When you choose customer-managed storage, the
-- @retentionPeriod@ parameter is ignored. You can\'t change the choice of
-- Amazon S3 storage after your data store is created.
--
-- /See:/ 'newCustomerManagedDatastoreS3Storage' smart constructor.
data CustomerManagedDatastoreS3Storage = CustomerManagedDatastoreS3Storage'
  { -- | (Optional) The prefix used to create the keys of the data store data
    -- objects. Each object in an Amazon S3 bucket has a key that is its unique
    -- identifier in the bucket. Each object in a bucket has exactly one key.
    -- The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket where your data is stored.
    bucket :: Prelude.Text,
    -- | The ARN of the role that grants IoT Analytics permission to interact
    -- with your Amazon S3 resources.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedDatastoreS3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'customerManagedDatastoreS3Storage_keyPrefix' - (Optional) The prefix used to create the keys of the data store data
-- objects. Each object in an Amazon S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
--
-- 'bucket', 'customerManagedDatastoreS3Storage_bucket' - The name of the Amazon S3 bucket where your data is stored.
--
-- 'roleArn', 'customerManagedDatastoreS3Storage_roleArn' - The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 resources.
newCustomerManagedDatastoreS3Storage ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CustomerManagedDatastoreS3Storage
newCustomerManagedDatastoreS3Storage
  pBucket_
  pRoleArn_ =
    CustomerManagedDatastoreS3Storage'
      { keyPrefix =
          Prelude.Nothing,
        bucket = pBucket_,
        roleArn = pRoleArn_
      }

-- | (Optional) The prefix used to create the keys of the data store data
-- objects. Each object in an Amazon S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
customerManagedDatastoreS3Storage_keyPrefix :: Lens.Lens' CustomerManagedDatastoreS3Storage (Prelude.Maybe Prelude.Text)
customerManagedDatastoreS3Storage_keyPrefix = Lens.lens (\CustomerManagedDatastoreS3Storage' {keyPrefix} -> keyPrefix) (\s@CustomerManagedDatastoreS3Storage' {} a -> s {keyPrefix = a} :: CustomerManagedDatastoreS3Storage)

-- | The name of the Amazon S3 bucket where your data is stored.
customerManagedDatastoreS3Storage_bucket :: Lens.Lens' CustomerManagedDatastoreS3Storage Prelude.Text
customerManagedDatastoreS3Storage_bucket = Lens.lens (\CustomerManagedDatastoreS3Storage' {bucket} -> bucket) (\s@CustomerManagedDatastoreS3Storage' {} a -> s {bucket = a} :: CustomerManagedDatastoreS3Storage)

-- | The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 resources.
customerManagedDatastoreS3Storage_roleArn :: Lens.Lens' CustomerManagedDatastoreS3Storage Prelude.Text
customerManagedDatastoreS3Storage_roleArn = Lens.lens (\CustomerManagedDatastoreS3Storage' {roleArn} -> roleArn) (\s@CustomerManagedDatastoreS3Storage' {} a -> s {roleArn = a} :: CustomerManagedDatastoreS3Storage)

instance
  Data.FromJSON
    CustomerManagedDatastoreS3Storage
  where
  parseJSON =
    Data.withObject
      "CustomerManagedDatastoreS3Storage"
      ( \x ->
          CustomerManagedDatastoreS3Storage'
            Prelude.<$> (x Data..:? "keyPrefix")
            Prelude.<*> (x Data..: "bucket")
            Prelude.<*> (x Data..: "roleArn")
      )

instance
  Prelude.Hashable
    CustomerManagedDatastoreS3Storage
  where
  hashWithSalt
    _salt
    CustomerManagedDatastoreS3Storage' {..} =
      _salt
        `Prelude.hashWithSalt` keyPrefix
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CustomerManagedDatastoreS3Storage
  where
  rnf CustomerManagedDatastoreS3Storage' {..} =
    Prelude.rnf keyPrefix
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf roleArn

instance
  Data.ToJSON
    CustomerManagedDatastoreS3Storage
  where
  toJSON CustomerManagedDatastoreS3Storage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyPrefix" Data..=) Prelude.<$> keyPrefix,
            Prelude.Just ("bucket" Data..= bucket),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
