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
-- Module      : Amazonka.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3Storage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage. You can\'t change the choice of Amazon S3 storage after your
-- data store is created.
--
-- /See:/ 'newIotSiteWiseCustomerManagedDatastoreS3Storage' smart constructor.
data IotSiteWiseCustomerManagedDatastoreS3Storage = IotSiteWiseCustomerManagedDatastoreS3Storage'
  { -- | (Optional) The prefix used to create the keys of the data store data
    -- objects. Each object in an Amazon S3 bucket has a key that is its unique
    -- identifier in the bucket. Each object in a bucket has exactly one key.
    -- The prefix must end with a forward slash (\/).
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket where your data is stored.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IotSiteWiseCustomerManagedDatastoreS3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'iotSiteWiseCustomerManagedDatastoreS3Storage_keyPrefix' - (Optional) The prefix used to create the keys of the data store data
-- objects. Each object in an Amazon S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
--
-- 'bucket', 'iotSiteWiseCustomerManagedDatastoreS3Storage_bucket' - The name of the Amazon S3 bucket where your data is stored.
newIotSiteWiseCustomerManagedDatastoreS3Storage ::
  -- | 'bucket'
  Prelude.Text ->
  IotSiteWiseCustomerManagedDatastoreS3Storage
newIotSiteWiseCustomerManagedDatastoreS3Storage
  pBucket_ =
    IotSiteWiseCustomerManagedDatastoreS3Storage'
      { keyPrefix =
          Prelude.Nothing,
        bucket = pBucket_
      }

-- | (Optional) The prefix used to create the keys of the data store data
-- objects. Each object in an Amazon S3 bucket has a key that is its unique
-- identifier in the bucket. Each object in a bucket has exactly one key.
-- The prefix must end with a forward slash (\/).
iotSiteWiseCustomerManagedDatastoreS3Storage_keyPrefix :: Lens.Lens' IotSiteWiseCustomerManagedDatastoreS3Storage (Prelude.Maybe Prelude.Text)
iotSiteWiseCustomerManagedDatastoreS3Storage_keyPrefix = Lens.lens (\IotSiteWiseCustomerManagedDatastoreS3Storage' {keyPrefix} -> keyPrefix) (\s@IotSiteWiseCustomerManagedDatastoreS3Storage' {} a -> s {keyPrefix = a} :: IotSiteWiseCustomerManagedDatastoreS3Storage)

-- | The name of the Amazon S3 bucket where your data is stored.
iotSiteWiseCustomerManagedDatastoreS3Storage_bucket :: Lens.Lens' IotSiteWiseCustomerManagedDatastoreS3Storage Prelude.Text
iotSiteWiseCustomerManagedDatastoreS3Storage_bucket = Lens.lens (\IotSiteWiseCustomerManagedDatastoreS3Storage' {bucket} -> bucket) (\s@IotSiteWiseCustomerManagedDatastoreS3Storage' {} a -> s {bucket = a} :: IotSiteWiseCustomerManagedDatastoreS3Storage)

instance
  Data.FromJSON
    IotSiteWiseCustomerManagedDatastoreS3Storage
  where
  parseJSON =
    Data.withObject
      "IotSiteWiseCustomerManagedDatastoreS3Storage"
      ( \x ->
          IotSiteWiseCustomerManagedDatastoreS3Storage'
            Prelude.<$> (x Data..:? "keyPrefix")
            Prelude.<*> (x Data..: "bucket")
      )

instance
  Prelude.Hashable
    IotSiteWiseCustomerManagedDatastoreS3Storage
  where
  hashWithSalt
    _salt
    IotSiteWiseCustomerManagedDatastoreS3Storage' {..} =
      _salt
        `Prelude.hashWithSalt` keyPrefix
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    IotSiteWiseCustomerManagedDatastoreS3Storage
  where
  rnf IotSiteWiseCustomerManagedDatastoreS3Storage' {..} =
    Prelude.rnf keyPrefix
      `Prelude.seq` Prelude.rnf bucket

instance
  Data.ToJSON
    IotSiteWiseCustomerManagedDatastoreS3Storage
  where
  toJSON
    IotSiteWiseCustomerManagedDatastoreS3Storage' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("keyPrefix" Data..=) Prelude.<$> keyPrefix,
              Prelude.Just ("bucket" Data..= bucket)
            ]
        )
