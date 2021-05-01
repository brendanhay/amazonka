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
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Use this to store data store data in an S3 bucket that you manage. When
-- customer-managed storage is selected, the @retentionPeriod@ parameter is
-- ignored. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the data store is created.
--
-- /See:/ 'newCustomerManagedDatastoreS3Storage' smart constructor.
data CustomerManagedDatastoreS3Storage = CustomerManagedDatastoreS3Storage'
  { -- | Optional. The prefix used to create the keys of the data store data
    -- objects. Each object in an S3 bucket has a key that is its unique
    -- identifier in the bucket. Each object in a bucket has exactly one key.
    -- The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket in which data store data is stored.
    bucket :: Prelude.Text,
    -- | The ARN of the role that grants AWS IoT Analytics permission to interact
    -- with your Amazon S3 resources.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedDatastoreS3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'customerManagedDatastoreS3Storage_keyPrefix' - Optional. The prefix used to create the keys of the data store data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
--
-- 'bucket', 'customerManagedDatastoreS3Storage_bucket' - The name of the S3 bucket in which data store data is stored.
--
-- 'roleArn', 'customerManagedDatastoreS3Storage_roleArn' - The ARN of the role that grants AWS IoT Analytics permission to interact
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

-- | Optional. The prefix used to create the keys of the data store data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
customerManagedDatastoreS3Storage_keyPrefix :: Lens.Lens' CustomerManagedDatastoreS3Storage (Prelude.Maybe Prelude.Text)
customerManagedDatastoreS3Storage_keyPrefix = Lens.lens (\CustomerManagedDatastoreS3Storage' {keyPrefix} -> keyPrefix) (\s@CustomerManagedDatastoreS3Storage' {} a -> s {keyPrefix = a} :: CustomerManagedDatastoreS3Storage)

-- | The name of the S3 bucket in which data store data is stored.
customerManagedDatastoreS3Storage_bucket :: Lens.Lens' CustomerManagedDatastoreS3Storage Prelude.Text
customerManagedDatastoreS3Storage_bucket = Lens.lens (\CustomerManagedDatastoreS3Storage' {bucket} -> bucket) (\s@CustomerManagedDatastoreS3Storage' {} a -> s {bucket = a} :: CustomerManagedDatastoreS3Storage)

-- | The ARN of the role that grants AWS IoT Analytics permission to interact
-- with your Amazon S3 resources.
customerManagedDatastoreS3Storage_roleArn :: Lens.Lens' CustomerManagedDatastoreS3Storage Prelude.Text
customerManagedDatastoreS3Storage_roleArn = Lens.lens (\CustomerManagedDatastoreS3Storage' {roleArn} -> roleArn) (\s@CustomerManagedDatastoreS3Storage' {} a -> s {roleArn = a} :: CustomerManagedDatastoreS3Storage)

instance
  Prelude.FromJSON
    CustomerManagedDatastoreS3Storage
  where
  parseJSON =
    Prelude.withObject
      "CustomerManagedDatastoreS3Storage"
      ( \x ->
          CustomerManagedDatastoreS3Storage'
            Prelude.<$> (x Prelude..:? "keyPrefix")
            Prelude.<*> (x Prelude..: "bucket")
            Prelude.<*> (x Prelude..: "roleArn")
      )

instance
  Prelude.Hashable
    CustomerManagedDatastoreS3Storage

instance
  Prelude.NFData
    CustomerManagedDatastoreS3Storage

instance
  Prelude.ToJSON
    CustomerManagedDatastoreS3Storage
  where
  toJSON CustomerManagedDatastoreS3Storage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("keyPrefix" Prelude..=) Prelude.<$> keyPrefix,
            Prelude.Just ("bucket" Prelude..= bucket),
            Prelude.Just ("roleArn" Prelude..= roleArn)
          ]
      )
