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
-- Module      : Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3Storage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3Storage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used to store channel data in an S3 bucket that you manage. If
-- customer-managed storage is selected, the @retentionPeriod@ parameter is
-- ignored. You can\'t change the choice of S3 storage after the data store
-- is created.
--
-- /See:/ 'newCustomerManagedChannelS3Storage' smart constructor.
data CustomerManagedChannelS3Storage = CustomerManagedChannelS3Storage'
  { -- | (Optional) The prefix used to create the keys of the channel data
    -- objects. Each object in an S3 bucket has a key that is its unique
    -- identifier in the bucket. Each object in a bucket has exactly one key.
    -- The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket in which channel data is stored.
    bucket :: Prelude.Text,
    -- | The ARN of the role that grants IoT Analytics permission to interact
    -- with your Amazon S3 resources.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedChannelS3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'customerManagedChannelS3Storage_keyPrefix' - (Optional) The prefix used to create the keys of the channel data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
--
-- 'bucket', 'customerManagedChannelS3Storage_bucket' - The name of the S3 bucket in which channel data is stored.
--
-- 'roleArn', 'customerManagedChannelS3Storage_roleArn' - The ARN of the role that grants IoT Analytics permission to interact
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

-- | (Optional) The prefix used to create the keys of the channel data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
customerManagedChannelS3Storage_keyPrefix :: Lens.Lens' CustomerManagedChannelS3Storage (Prelude.Maybe Prelude.Text)
customerManagedChannelS3Storage_keyPrefix = Lens.lens (\CustomerManagedChannelS3Storage' {keyPrefix} -> keyPrefix) (\s@CustomerManagedChannelS3Storage' {} a -> s {keyPrefix = a} :: CustomerManagedChannelS3Storage)

-- | The name of the S3 bucket in which channel data is stored.
customerManagedChannelS3Storage_bucket :: Lens.Lens' CustomerManagedChannelS3Storage Prelude.Text
customerManagedChannelS3Storage_bucket = Lens.lens (\CustomerManagedChannelS3Storage' {bucket} -> bucket) (\s@CustomerManagedChannelS3Storage' {} a -> s {bucket = a} :: CustomerManagedChannelS3Storage)

-- | The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 resources.
customerManagedChannelS3Storage_roleArn :: Lens.Lens' CustomerManagedChannelS3Storage Prelude.Text
customerManagedChannelS3Storage_roleArn = Lens.lens (\CustomerManagedChannelS3Storage' {roleArn} -> roleArn) (\s@CustomerManagedChannelS3Storage' {} a -> s {roleArn = a} :: CustomerManagedChannelS3Storage)

instance
  Data.FromJSON
    CustomerManagedChannelS3Storage
  where
  parseJSON =
    Data.withObject
      "CustomerManagedChannelS3Storage"
      ( \x ->
          CustomerManagedChannelS3Storage'
            Prelude.<$> (x Data..:? "keyPrefix")
            Prelude.<*> (x Data..: "bucket")
            Prelude.<*> (x Data..: "roleArn")
      )

instance
  Prelude.Hashable
    CustomerManagedChannelS3Storage
  where
  hashWithSalt
    _salt
    CustomerManagedChannelS3Storage' {..} =
      _salt `Prelude.hashWithSalt` keyPrefix
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CustomerManagedChannelS3Storage
  where
  rnf CustomerManagedChannelS3Storage' {..} =
    Prelude.rnf keyPrefix
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON CustomerManagedChannelS3Storage where
  toJSON CustomerManagedChannelS3Storage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyPrefix" Data..=) Prelude.<$> keyPrefix,
            Prelude.Just ("bucket" Data..= bucket),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
