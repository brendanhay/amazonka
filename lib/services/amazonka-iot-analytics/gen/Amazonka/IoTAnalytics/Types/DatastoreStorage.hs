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
-- Module      : Amazonka.IoTAnalytics.Types.DatastoreStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastoreStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
import Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorage
import Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
import qualified Amazonka.Prelude as Prelude

-- | Where data in a data store is stored.. You can choose @serviceManagedS3@
-- storage, @customerManagedS3@ storage, or @iotSiteWiseMultiLayerStorage@
-- storage. The default is @serviceManagedS3@. You can\'t change the choice
-- of Amazon S3 storage after your data store is created.
--
-- /See:/ 'newDatastoreStorage' smart constructor.
data DatastoreStorage = DatastoreStorage'
  { -- | S3-customer-managed; When you choose customer-managed storage, the
    -- @retentionPeriod@ parameter is ignored. You can\'t change the choice of
    -- Amazon S3 storage after your data store is created.
    customerManagedS3 :: Prelude.Maybe CustomerManagedDatastoreS3Storage,
    -- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
    -- manage. You can\'t change the choice of Amazon S3 storage after your
    -- data store is created.
    iotSiteWiseMultiLayerStorage :: Prelude.Maybe DatastoreIotSiteWiseMultiLayerStorage,
    -- | Used to store data in an Amazon S3 bucket managed by IoT Analytics. You
    -- can\'t change the choice of Amazon S3 storage after your data store is
    -- created.
    serviceManagedS3 :: Prelude.Maybe ServiceManagedDatastoreS3Storage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerManagedS3', 'datastoreStorage_customerManagedS3' - S3-customer-managed; When you choose customer-managed storage, the
-- @retentionPeriod@ parameter is ignored. You can\'t change the choice of
-- Amazon S3 storage after your data store is created.
--
-- 'iotSiteWiseMultiLayerStorage', 'datastoreStorage_iotSiteWiseMultiLayerStorage' - Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage. You can\'t change the choice of Amazon S3 storage after your
-- data store is created.
--
-- 'serviceManagedS3', 'datastoreStorage_serviceManagedS3' - Used to store data in an Amazon S3 bucket managed by IoT Analytics. You
-- can\'t change the choice of Amazon S3 storage after your data store is
-- created.
newDatastoreStorage ::
  DatastoreStorage
newDatastoreStorage =
  DatastoreStorage'
    { customerManagedS3 =
        Prelude.Nothing,
      iotSiteWiseMultiLayerStorage = Prelude.Nothing,
      serviceManagedS3 = Prelude.Nothing
    }

-- | S3-customer-managed; When you choose customer-managed storage, the
-- @retentionPeriod@ parameter is ignored. You can\'t change the choice of
-- Amazon S3 storage after your data store is created.
datastoreStorage_customerManagedS3 :: Lens.Lens' DatastoreStorage (Prelude.Maybe CustomerManagedDatastoreS3Storage)
datastoreStorage_customerManagedS3 = Lens.lens (\DatastoreStorage' {customerManagedS3} -> customerManagedS3) (\s@DatastoreStorage' {} a -> s {customerManagedS3 = a} :: DatastoreStorage)

-- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage. You can\'t change the choice of Amazon S3 storage after your
-- data store is created.
datastoreStorage_iotSiteWiseMultiLayerStorage :: Lens.Lens' DatastoreStorage (Prelude.Maybe DatastoreIotSiteWiseMultiLayerStorage)
datastoreStorage_iotSiteWiseMultiLayerStorage = Lens.lens (\DatastoreStorage' {iotSiteWiseMultiLayerStorage} -> iotSiteWiseMultiLayerStorage) (\s@DatastoreStorage' {} a -> s {iotSiteWiseMultiLayerStorage = a} :: DatastoreStorage)

-- | Used to store data in an Amazon S3 bucket managed by IoT Analytics. You
-- can\'t change the choice of Amazon S3 storage after your data store is
-- created.
datastoreStorage_serviceManagedS3 :: Lens.Lens' DatastoreStorage (Prelude.Maybe ServiceManagedDatastoreS3Storage)
datastoreStorage_serviceManagedS3 = Lens.lens (\DatastoreStorage' {serviceManagedS3} -> serviceManagedS3) (\s@DatastoreStorage' {} a -> s {serviceManagedS3 = a} :: DatastoreStorage)

instance Data.FromJSON DatastoreStorage where
  parseJSON =
    Data.withObject
      "DatastoreStorage"
      ( \x ->
          DatastoreStorage'
            Prelude.<$> (x Data..:? "customerManagedS3")
            Prelude.<*> (x Data..:? "iotSiteWiseMultiLayerStorage")
            Prelude.<*> (x Data..:? "serviceManagedS3")
      )

instance Prelude.Hashable DatastoreStorage where
  hashWithSalt _salt DatastoreStorage' {..} =
    _salt
      `Prelude.hashWithSalt` customerManagedS3
      `Prelude.hashWithSalt` iotSiteWiseMultiLayerStorage
      `Prelude.hashWithSalt` serviceManagedS3

instance Prelude.NFData DatastoreStorage where
  rnf DatastoreStorage' {..} =
    Prelude.rnf customerManagedS3
      `Prelude.seq` Prelude.rnf iotSiteWiseMultiLayerStorage
      `Prelude.seq` Prelude.rnf serviceManagedS3

instance Data.ToJSON DatastoreStorage where
  toJSON DatastoreStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("customerManagedS3" Data..=)
              Prelude.<$> customerManagedS3,
            ("iotSiteWiseMultiLayerStorage" Data..=)
              Prelude.<$> iotSiteWiseMultiLayerStorage,
            ("serviceManagedS3" Data..=)
              Prelude.<$> serviceManagedS3
          ]
      )
