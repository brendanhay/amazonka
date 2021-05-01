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
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Use this to store channel data in an S3 bucket that you manage. If
-- customer managed storage is selected, the @retentionPeriod@ parameter is
-- ignored. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the channel is created.
--
-- /See:/ 'newCustomerManagedChannelS3Storage' smart constructor.
data CustomerManagedChannelS3Storage = CustomerManagedChannelS3Storage'
  { -- | Optional. The prefix used to create the keys of the channel data
    -- objects. Each object in an S3 bucket has a key that is its unique
    -- identifier in the bucket. Each object in a bucket has exactly one key.
    -- The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket in which channel data is stored.
    bucket :: Prelude.Text,
    -- | The ARN of the role that grants AWS IoT Analytics permission to interact
    -- with your Amazon S3 resources.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedChannelS3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'customerManagedChannelS3Storage_keyPrefix' - Optional. The prefix used to create the keys of the channel data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
--
-- 'bucket', 'customerManagedChannelS3Storage_bucket' - The name of the S3 bucket in which channel data is stored.
--
-- 'roleArn', 'customerManagedChannelS3Storage_roleArn' - The ARN of the role that grants AWS IoT Analytics permission to interact
-- with your Amazon S3 resources.
newCustomerManagedChannelS3Storage ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CustomerManagedChannelS3Storage
newCustomerManagedChannelS3Storage pBucket_ pRoleArn_ =
  CustomerManagedChannelS3Storage'
    { keyPrefix =
        Prelude.Nothing,
      bucket = pBucket_,
      roleArn = pRoleArn_
    }

-- | Optional. The prefix used to create the keys of the channel data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
customerManagedChannelS3Storage_keyPrefix :: Lens.Lens' CustomerManagedChannelS3Storage (Prelude.Maybe Prelude.Text)
customerManagedChannelS3Storage_keyPrefix = Lens.lens (\CustomerManagedChannelS3Storage' {keyPrefix} -> keyPrefix) (\s@CustomerManagedChannelS3Storage' {} a -> s {keyPrefix = a} :: CustomerManagedChannelS3Storage)

-- | The name of the S3 bucket in which channel data is stored.
customerManagedChannelS3Storage_bucket :: Lens.Lens' CustomerManagedChannelS3Storage Prelude.Text
customerManagedChannelS3Storage_bucket = Lens.lens (\CustomerManagedChannelS3Storage' {bucket} -> bucket) (\s@CustomerManagedChannelS3Storage' {} a -> s {bucket = a} :: CustomerManagedChannelS3Storage)

-- | The ARN of the role that grants AWS IoT Analytics permission to interact
-- with your Amazon S3 resources.
customerManagedChannelS3Storage_roleArn :: Lens.Lens' CustomerManagedChannelS3Storage Prelude.Text
customerManagedChannelS3Storage_roleArn = Lens.lens (\CustomerManagedChannelS3Storage' {roleArn} -> roleArn) (\s@CustomerManagedChannelS3Storage' {} a -> s {roleArn = a} :: CustomerManagedChannelS3Storage)

instance
  Prelude.FromJSON
    CustomerManagedChannelS3Storage
  where
  parseJSON =
    Prelude.withObject
      "CustomerManagedChannelS3Storage"
      ( \x ->
          CustomerManagedChannelS3Storage'
            Prelude.<$> (x Prelude..:? "keyPrefix")
            Prelude.<*> (x Prelude..: "bucket")
            Prelude.<*> (x Prelude..: "roleArn")
      )

instance
  Prelude.Hashable
    CustomerManagedChannelS3Storage

instance
  Prelude.NFData
    CustomerManagedChannelS3Storage

instance
  Prelude.ToJSON
    CustomerManagedChannelS3Storage
  where
  toJSON CustomerManagedChannelS3Storage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("keyPrefix" Prelude..=) Prelude.<$> keyPrefix,
            Prelude.Just ("bucket" Prelude..= bucket),
            Prelude.Just ("roleArn" Prelude..= roleArn)
          ]
      )
