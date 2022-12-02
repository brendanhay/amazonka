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
-- Module      : Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorageSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3StorageSummary
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the data store that you manage, which stores
-- data used by IoT SiteWise.
--
-- /See:/ 'newDatastoreIotSiteWiseMultiLayerStorageSummary' smart constructor.
data DatastoreIotSiteWiseMultiLayerStorageSummary = DatastoreIotSiteWiseMultiLayerStorageSummary'
  { -- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
    -- manage.
    customerManagedS3Storage :: Prelude.Maybe IotSiteWiseCustomerManagedDatastoreS3StorageSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreIotSiteWiseMultiLayerStorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerManagedS3Storage', 'datastoreIotSiteWiseMultiLayerStorageSummary_customerManagedS3Storage' - Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage.
newDatastoreIotSiteWiseMultiLayerStorageSummary ::
  DatastoreIotSiteWiseMultiLayerStorageSummary
newDatastoreIotSiteWiseMultiLayerStorageSummary =
  DatastoreIotSiteWiseMultiLayerStorageSummary'
    { customerManagedS3Storage =
        Prelude.Nothing
    }

-- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage.
datastoreIotSiteWiseMultiLayerStorageSummary_customerManagedS3Storage :: Lens.Lens' DatastoreIotSiteWiseMultiLayerStorageSummary (Prelude.Maybe IotSiteWiseCustomerManagedDatastoreS3StorageSummary)
datastoreIotSiteWiseMultiLayerStorageSummary_customerManagedS3Storage = Lens.lens (\DatastoreIotSiteWiseMultiLayerStorageSummary' {customerManagedS3Storage} -> customerManagedS3Storage) (\s@DatastoreIotSiteWiseMultiLayerStorageSummary' {} a -> s {customerManagedS3Storage = a} :: DatastoreIotSiteWiseMultiLayerStorageSummary)

instance
  Data.FromJSON
    DatastoreIotSiteWiseMultiLayerStorageSummary
  where
  parseJSON =
    Data.withObject
      "DatastoreIotSiteWiseMultiLayerStorageSummary"
      ( \x ->
          DatastoreIotSiteWiseMultiLayerStorageSummary'
            Prelude.<$> (x Data..:? "customerManagedS3Storage")
      )

instance
  Prelude.Hashable
    DatastoreIotSiteWiseMultiLayerStorageSummary
  where
  hashWithSalt
    _salt
    DatastoreIotSiteWiseMultiLayerStorageSummary' {..} =
      _salt
        `Prelude.hashWithSalt` customerManagedS3Storage

instance
  Prelude.NFData
    DatastoreIotSiteWiseMultiLayerStorageSummary
  where
  rnf DatastoreIotSiteWiseMultiLayerStorageSummary' {..} =
    Prelude.rnf customerManagedS3Storage
