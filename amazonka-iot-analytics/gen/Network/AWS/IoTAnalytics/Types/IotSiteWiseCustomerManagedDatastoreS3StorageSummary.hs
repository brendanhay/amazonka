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
-- Module      : Network.AWS.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3StorageSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the data store that you manage, which stores
-- data used by IoT SiteWise.
--
-- /See:/ 'newIotSiteWiseCustomerManagedDatastoreS3StorageSummary' smart constructor.
data IotSiteWiseCustomerManagedDatastoreS3StorageSummary = IotSiteWiseCustomerManagedDatastoreS3StorageSummary'
  { -- | (Optional) The prefix used to create the keys of the data store data
    -- objects. Each object in an Amazon S3 bucket has a key that is its unique
    -- identifier in the bucket. Each object in a bucket has exactly one key.
    -- The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket where your data is stored.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IotSiteWiseCustomerManagedDatastoreS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'iotSiteWiseCustomerManagedDatastoreS3StorageSummary_keyPrefix' - (Optional) The prefix used to create the keys of the data store data
-- objects. Each object in an Amazon S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
--
-- 'bucket', 'iotSiteWiseCustomerManagedDatastoreS3StorageSummary_bucket' - The name of the Amazon S3 bucket where your data is stored.
newIotSiteWiseCustomerManagedDatastoreS3StorageSummary ::
  IotSiteWiseCustomerManagedDatastoreS3StorageSummary
newIotSiteWiseCustomerManagedDatastoreS3StorageSummary =
  IotSiteWiseCustomerManagedDatastoreS3StorageSummary'
    { keyPrefix =
        Prelude.Nothing,
      bucket =
        Prelude.Nothing
    }

-- | (Optional) The prefix used to create the keys of the data store data
-- objects. Each object in an Amazon S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
iotSiteWiseCustomerManagedDatastoreS3StorageSummary_keyPrefix :: Lens.Lens' IotSiteWiseCustomerManagedDatastoreS3StorageSummary (Prelude.Maybe Prelude.Text)
iotSiteWiseCustomerManagedDatastoreS3StorageSummary_keyPrefix = Lens.lens (\IotSiteWiseCustomerManagedDatastoreS3StorageSummary' {keyPrefix} -> keyPrefix) (\s@IotSiteWiseCustomerManagedDatastoreS3StorageSummary' {} a -> s {keyPrefix = a} :: IotSiteWiseCustomerManagedDatastoreS3StorageSummary)

-- | The name of the Amazon S3 bucket where your data is stored.
iotSiteWiseCustomerManagedDatastoreS3StorageSummary_bucket :: Lens.Lens' IotSiteWiseCustomerManagedDatastoreS3StorageSummary (Prelude.Maybe Prelude.Text)
iotSiteWiseCustomerManagedDatastoreS3StorageSummary_bucket = Lens.lens (\IotSiteWiseCustomerManagedDatastoreS3StorageSummary' {bucket} -> bucket) (\s@IotSiteWiseCustomerManagedDatastoreS3StorageSummary' {} a -> s {bucket = a} :: IotSiteWiseCustomerManagedDatastoreS3StorageSummary)

instance
  Core.FromJSON
    IotSiteWiseCustomerManagedDatastoreS3StorageSummary
  where
  parseJSON =
    Core.withObject
      "IotSiteWiseCustomerManagedDatastoreS3StorageSummary"
      ( \x ->
          IotSiteWiseCustomerManagedDatastoreS3StorageSummary'
            Prelude.<$> (x Core..:? "keyPrefix")
              Prelude.<*> (x Core..:? "bucket")
      )

instance
  Prelude.Hashable
    IotSiteWiseCustomerManagedDatastoreS3StorageSummary

instance
  Prelude.NFData
    IotSiteWiseCustomerManagedDatastoreS3StorageSummary
