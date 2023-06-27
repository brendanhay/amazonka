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
-- Module      : Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the data store that you manage.
--
-- /See:/ 'newCustomerManagedDatastoreS3StorageSummary' smart constructor.
data CustomerManagedDatastoreS3StorageSummary = CustomerManagedDatastoreS3StorageSummary'
  { -- | The name of the Amazon S3 bucket where your data is stored.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The prefix used to create the keys of the data store data
    -- objects. Each object in an Amazon S3 bucket has a key that is its unique
    -- identifier in the bucket. Each object in a bucket has exactly one key.
    -- The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that grants IoT Analytics permission to interact
    -- with your Amazon S3 resources.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedDatastoreS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'customerManagedDatastoreS3StorageSummary_bucket' - The name of the Amazon S3 bucket where your data is stored.
--
-- 'keyPrefix', 'customerManagedDatastoreS3StorageSummary_keyPrefix' - (Optional) The prefix used to create the keys of the data store data
-- objects. Each object in an Amazon S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
--
-- 'roleArn', 'customerManagedDatastoreS3StorageSummary_roleArn' - The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 resources.
newCustomerManagedDatastoreS3StorageSummary ::
  CustomerManagedDatastoreS3StorageSummary
newCustomerManagedDatastoreS3StorageSummary =
  CustomerManagedDatastoreS3StorageSummary'
    { bucket =
        Prelude.Nothing,
      keyPrefix = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket where your data is stored.
customerManagedDatastoreS3StorageSummary_bucket :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedDatastoreS3StorageSummary_bucket = Lens.lens (\CustomerManagedDatastoreS3StorageSummary' {bucket} -> bucket) (\s@CustomerManagedDatastoreS3StorageSummary' {} a -> s {bucket = a} :: CustomerManagedDatastoreS3StorageSummary)

-- | (Optional) The prefix used to create the keys of the data store data
-- objects. Each object in an Amazon S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
customerManagedDatastoreS3StorageSummary_keyPrefix :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedDatastoreS3StorageSummary_keyPrefix = Lens.lens (\CustomerManagedDatastoreS3StorageSummary' {keyPrefix} -> keyPrefix) (\s@CustomerManagedDatastoreS3StorageSummary' {} a -> s {keyPrefix = a} :: CustomerManagedDatastoreS3StorageSummary)

-- | The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 resources.
customerManagedDatastoreS3StorageSummary_roleArn :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedDatastoreS3StorageSummary_roleArn = Lens.lens (\CustomerManagedDatastoreS3StorageSummary' {roleArn} -> roleArn) (\s@CustomerManagedDatastoreS3StorageSummary' {} a -> s {roleArn = a} :: CustomerManagedDatastoreS3StorageSummary)

instance
  Data.FromJSON
    CustomerManagedDatastoreS3StorageSummary
  where
  parseJSON =
    Data.withObject
      "CustomerManagedDatastoreS3StorageSummary"
      ( \x ->
          CustomerManagedDatastoreS3StorageSummary'
            Prelude.<$> (x Data..:? "bucket")
            Prelude.<*> (x Data..:? "keyPrefix")
            Prelude.<*> (x Data..:? "roleArn")
      )

instance
  Prelude.Hashable
    CustomerManagedDatastoreS3StorageSummary
  where
  hashWithSalt
    _salt
    CustomerManagedDatastoreS3StorageSummary' {..} =
      _salt
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` keyPrefix
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CustomerManagedDatastoreS3StorageSummary
  where
  rnf CustomerManagedDatastoreS3StorageSummary' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf keyPrefix
      `Prelude.seq` Prelude.rnf roleArn
