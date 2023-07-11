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
-- Module      : Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used to store channel data in an S3 bucket that you manage.
--
-- /See:/ 'newCustomerManagedChannelS3StorageSummary' smart constructor.
data CustomerManagedChannelS3StorageSummary = CustomerManagedChannelS3StorageSummary'
  { -- | The name of the S3 bucket in which channel data is stored.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The prefix used to create the keys of the channel data
    -- objects. Each object in an S3 bucket has a key that is its unique
    -- identifier within the bucket (each object in a bucket has exactly one
    -- key). The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that grants IoT Analytics permission to interact
    -- with your Amazon S3 resources.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedChannelS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'customerManagedChannelS3StorageSummary_bucket' - The name of the S3 bucket in which channel data is stored.
--
-- 'keyPrefix', 'customerManagedChannelS3StorageSummary_keyPrefix' - (Optional) The prefix used to create the keys of the channel data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier within the bucket (each object in a bucket has exactly one
-- key). The prefix must end with a forward slash (\/).
--
-- 'roleArn', 'customerManagedChannelS3StorageSummary_roleArn' - The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 resources.
newCustomerManagedChannelS3StorageSummary ::
  CustomerManagedChannelS3StorageSummary
newCustomerManagedChannelS3StorageSummary =
  CustomerManagedChannelS3StorageSummary'
    { bucket =
        Prelude.Nothing,
      keyPrefix = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The name of the S3 bucket in which channel data is stored.
customerManagedChannelS3StorageSummary_bucket :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedChannelS3StorageSummary_bucket = Lens.lens (\CustomerManagedChannelS3StorageSummary' {bucket} -> bucket) (\s@CustomerManagedChannelS3StorageSummary' {} a -> s {bucket = a} :: CustomerManagedChannelS3StorageSummary)

-- | (Optional) The prefix used to create the keys of the channel data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier within the bucket (each object in a bucket has exactly one
-- key). The prefix must end with a forward slash (\/).
customerManagedChannelS3StorageSummary_keyPrefix :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedChannelS3StorageSummary_keyPrefix = Lens.lens (\CustomerManagedChannelS3StorageSummary' {keyPrefix} -> keyPrefix) (\s@CustomerManagedChannelS3StorageSummary' {} a -> s {keyPrefix = a} :: CustomerManagedChannelS3StorageSummary)

-- | The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 resources.
customerManagedChannelS3StorageSummary_roleArn :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedChannelS3StorageSummary_roleArn = Lens.lens (\CustomerManagedChannelS3StorageSummary' {roleArn} -> roleArn) (\s@CustomerManagedChannelS3StorageSummary' {} a -> s {roleArn = a} :: CustomerManagedChannelS3StorageSummary)

instance
  Data.FromJSON
    CustomerManagedChannelS3StorageSummary
  where
  parseJSON =
    Data.withObject
      "CustomerManagedChannelS3StorageSummary"
      ( \x ->
          CustomerManagedChannelS3StorageSummary'
            Prelude.<$> (x Data..:? "bucket")
            Prelude.<*> (x Data..:? "keyPrefix")
            Prelude.<*> (x Data..:? "roleArn")
      )

instance
  Prelude.Hashable
    CustomerManagedChannelS3StorageSummary
  where
  hashWithSalt
    _salt
    CustomerManagedChannelS3StorageSummary' {..} =
      _salt
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` keyPrefix
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CustomerManagedChannelS3StorageSummary
  where
  rnf CustomerManagedChannelS3StorageSummary' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf keyPrefix
      `Prelude.seq` Prelude.rnf roleArn
