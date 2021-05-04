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
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to store channel data in an S3 bucket that you manage.
--
-- /See:/ 'newCustomerManagedChannelS3StorageSummary' smart constructor.
data CustomerManagedChannelS3StorageSummary = CustomerManagedChannelS3StorageSummary'
  { -- | Optional. The prefix used to create the keys of the channel data
    -- objects. Each object in an S3 bucket has a key that is its unique
    -- identifier within the bucket (each object in a bucket has exactly one
    -- key). The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that grants AWS IoT Analytics permission to interact
    -- with your Amazon S3 resources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket in which channel data is stored.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomerManagedChannelS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'customerManagedChannelS3StorageSummary_keyPrefix' - Optional. The prefix used to create the keys of the channel data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier within the bucket (each object in a bucket has exactly one
-- key). The prefix must end with a forward slash (\/).
--
-- 'roleArn', 'customerManagedChannelS3StorageSummary_roleArn' - The ARN of the role that grants AWS IoT Analytics permission to interact
-- with your Amazon S3 resources.
--
-- 'bucket', 'customerManagedChannelS3StorageSummary_bucket' - The name of the S3 bucket in which channel data is stored.
newCustomerManagedChannelS3StorageSummary ::
  CustomerManagedChannelS3StorageSummary
newCustomerManagedChannelS3StorageSummary =
  CustomerManagedChannelS3StorageSummary'
    { keyPrefix =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | Optional. The prefix used to create the keys of the channel data
-- objects. Each object in an S3 bucket has a key that is its unique
-- identifier within the bucket (each object in a bucket has exactly one
-- key). The prefix must end with a forward slash (\/).
customerManagedChannelS3StorageSummary_keyPrefix :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedChannelS3StorageSummary_keyPrefix = Lens.lens (\CustomerManagedChannelS3StorageSummary' {keyPrefix} -> keyPrefix) (\s@CustomerManagedChannelS3StorageSummary' {} a -> s {keyPrefix = a} :: CustomerManagedChannelS3StorageSummary)

-- | The ARN of the role that grants AWS IoT Analytics permission to interact
-- with your Amazon S3 resources.
customerManagedChannelS3StorageSummary_roleArn :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedChannelS3StorageSummary_roleArn = Lens.lens (\CustomerManagedChannelS3StorageSummary' {roleArn} -> roleArn) (\s@CustomerManagedChannelS3StorageSummary' {} a -> s {roleArn = a} :: CustomerManagedChannelS3StorageSummary)

-- | The name of the S3 bucket in which channel data is stored.
customerManagedChannelS3StorageSummary_bucket :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Prelude.Maybe Prelude.Text)
customerManagedChannelS3StorageSummary_bucket = Lens.lens (\CustomerManagedChannelS3StorageSummary' {bucket} -> bucket) (\s@CustomerManagedChannelS3StorageSummary' {} a -> s {bucket = a} :: CustomerManagedChannelS3StorageSummary)

instance
  Prelude.FromJSON
    CustomerManagedChannelS3StorageSummary
  where
  parseJSON =
    Prelude.withObject
      "CustomerManagedChannelS3StorageSummary"
      ( \x ->
          CustomerManagedChannelS3StorageSummary'
            Prelude.<$> (x Prelude..:? "keyPrefix")
            Prelude.<*> (x Prelude..:? "roleArn")
            Prelude.<*> (x Prelude..:? "bucket")
      )

instance
  Prelude.Hashable
    CustomerManagedChannelS3StorageSummary

instance
  Prelude.NFData
    CustomerManagedChannelS3StorageSummary
