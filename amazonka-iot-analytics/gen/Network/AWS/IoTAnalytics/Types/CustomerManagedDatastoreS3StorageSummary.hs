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
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to store data store data in an S3 bucket that you manage.
--
-- /See:/ 'newCustomerManagedDatastoreS3StorageSummary' smart constructor.
data CustomerManagedDatastoreS3StorageSummary = CustomerManagedDatastoreS3StorageSummary'
  { -- | Optional. The prefix used to create the keys of the data store data
    -- objects. Each object in an S3 bucket has a key that is its unique
    -- identifier in the bucket. Each object in a bucket has exactly one key.
    -- The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that grants AWS IoT Analytics permission to interact
    -- with your Amazon S3 resources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket in which data store data is stored.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedDatastoreS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'customerManagedDatastoreS3StorageSummary_keyPrefix' - Optional. The prefix used to create the keys of the data store data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
--
-- 'roleArn', 'customerManagedDatastoreS3StorageSummary_roleArn' - The ARN of the role that grants AWS IoT Analytics permission to interact
-- with your Amazon S3 resources.
--
-- 'bucket', 'customerManagedDatastoreS3StorageSummary_bucket' - The name of the S3 bucket in which data store data is stored.
newCustomerManagedDatastoreS3StorageSummary ::
  CustomerManagedDatastoreS3StorageSummary
newCustomerManagedDatastoreS3StorageSummary =
  CustomerManagedDatastoreS3StorageSummary'
    { keyPrefix =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | Optional. The prefix used to create the keys of the data store data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
customerManagedDatastoreS3StorageSummary_keyPrefix :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedDatastoreS3StorageSummary_keyPrefix = Lens.lens (\CustomerManagedDatastoreS3StorageSummary' {keyPrefix} -> keyPrefix) (\s@CustomerManagedDatastoreS3StorageSummary' {} a -> s {keyPrefix = a} :: CustomerManagedDatastoreS3StorageSummary)

-- | The ARN of the role that grants AWS IoT Analytics permission to interact
-- with your Amazon S3 resources.
customerManagedDatastoreS3StorageSummary_roleArn :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedDatastoreS3StorageSummary_roleArn = Lens.lens (\CustomerManagedDatastoreS3StorageSummary' {roleArn} -> roleArn) (\s@CustomerManagedDatastoreS3StorageSummary' {} a -> s {roleArn = a} :: CustomerManagedDatastoreS3StorageSummary)

-- | The name of the S3 bucket in which data store data is stored.
customerManagedDatastoreS3StorageSummary_bucket :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedDatastoreS3StorageSummary_bucket = Lens.lens (\CustomerManagedDatastoreS3StorageSummary' {bucket} -> bucket) (\s@CustomerManagedDatastoreS3StorageSummary' {} a -> s {bucket = a} :: CustomerManagedDatastoreS3StorageSummary)

instance
  Prelude.FromJSON
    CustomerManagedDatastoreS3StorageSummary
  where
  parseJSON =
    Prelude.withObject
      "CustomerManagedDatastoreS3StorageSummary"
      ( \x ->
          CustomerManagedDatastoreS3StorageSummary'
            Prelude.<$> (x Prelude..:? "keyPrefix")
            Prelude.<*> (x Prelude..:? "roleArn")
            Prelude.<*> (x Prelude..:? "bucket")
      )

instance
  Prelude.Hashable
    CustomerManagedDatastoreS3StorageSummary

instance
  Prelude.NFData
    CustomerManagedDatastoreS3StorageSummary
