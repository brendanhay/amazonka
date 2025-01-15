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
-- Module      : Amazonka.IoTAnalytics.Types.DatastoreStorageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastoreStorageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
import Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorageSummary
import Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
import qualified Amazonka.Prelude as Prelude

-- | Contains information about your data store.
--
-- /See:/ 'newDatastoreStorageSummary' smart constructor.
data DatastoreStorageSummary = DatastoreStorageSummary'
  { -- | Used to store data in an Amazon S3 bucket managed by IoT Analytics.
    customerManagedS3 :: Prelude.Maybe CustomerManagedDatastoreS3StorageSummary,
    -- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
    -- manage.
    iotSiteWiseMultiLayerStorage :: Prelude.Maybe DatastoreIotSiteWiseMultiLayerStorageSummary,
    -- | Used to store data in an Amazon S3 bucket managed by IoT Analytics.
    serviceManagedS3 :: Prelude.Maybe ServiceManagedDatastoreS3StorageSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreStorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerManagedS3', 'datastoreStorageSummary_customerManagedS3' - Used to store data in an Amazon S3 bucket managed by IoT Analytics.
--
-- 'iotSiteWiseMultiLayerStorage', 'datastoreStorageSummary_iotSiteWiseMultiLayerStorage' - Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage.
--
-- 'serviceManagedS3', 'datastoreStorageSummary_serviceManagedS3' - Used to store data in an Amazon S3 bucket managed by IoT Analytics.
newDatastoreStorageSummary ::
  DatastoreStorageSummary
newDatastoreStorageSummary =
  DatastoreStorageSummary'
    { customerManagedS3 =
        Prelude.Nothing,
      iotSiteWiseMultiLayerStorage = Prelude.Nothing,
      serviceManagedS3 = Prelude.Nothing
    }

-- | Used to store data in an Amazon S3 bucket managed by IoT Analytics.
datastoreStorageSummary_customerManagedS3 :: Lens.Lens' DatastoreStorageSummary (Prelude.Maybe CustomerManagedDatastoreS3StorageSummary)
datastoreStorageSummary_customerManagedS3 = Lens.lens (\DatastoreStorageSummary' {customerManagedS3} -> customerManagedS3) (\s@DatastoreStorageSummary' {} a -> s {customerManagedS3 = a} :: DatastoreStorageSummary)

-- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage.
datastoreStorageSummary_iotSiteWiseMultiLayerStorage :: Lens.Lens' DatastoreStorageSummary (Prelude.Maybe DatastoreIotSiteWiseMultiLayerStorageSummary)
datastoreStorageSummary_iotSiteWiseMultiLayerStorage = Lens.lens (\DatastoreStorageSummary' {iotSiteWiseMultiLayerStorage} -> iotSiteWiseMultiLayerStorage) (\s@DatastoreStorageSummary' {} a -> s {iotSiteWiseMultiLayerStorage = a} :: DatastoreStorageSummary)

-- | Used to store data in an Amazon S3 bucket managed by IoT Analytics.
datastoreStorageSummary_serviceManagedS3 :: Lens.Lens' DatastoreStorageSummary (Prelude.Maybe ServiceManagedDatastoreS3StorageSummary)
datastoreStorageSummary_serviceManagedS3 = Lens.lens (\DatastoreStorageSummary' {serviceManagedS3} -> serviceManagedS3) (\s@DatastoreStorageSummary' {} a -> s {serviceManagedS3 = a} :: DatastoreStorageSummary)

instance Data.FromJSON DatastoreStorageSummary where
  parseJSON =
    Data.withObject
      "DatastoreStorageSummary"
      ( \x ->
          DatastoreStorageSummary'
            Prelude.<$> (x Data..:? "customerManagedS3")
            Prelude.<*> (x Data..:? "iotSiteWiseMultiLayerStorage")
            Prelude.<*> (x Data..:? "serviceManagedS3")
      )

instance Prelude.Hashable DatastoreStorageSummary where
  hashWithSalt _salt DatastoreStorageSummary' {..} =
    _salt
      `Prelude.hashWithSalt` customerManagedS3
      `Prelude.hashWithSalt` iotSiteWiseMultiLayerStorage
      `Prelude.hashWithSalt` serviceManagedS3

instance Prelude.NFData DatastoreStorageSummary where
  rnf DatastoreStorageSummary' {..} =
    Prelude.rnf customerManagedS3 `Prelude.seq`
      Prelude.rnf iotSiteWiseMultiLayerStorage `Prelude.seq`
        Prelude.rnf serviceManagedS3
