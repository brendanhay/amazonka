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
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStorage where

import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Where data store data is stored. You can choose one of
-- @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the
-- default is @serviceManagedS3@. You cannot change this storage option
-- after the data store is created.
--
-- /See:/ 'newDatastoreStorage' smart constructor.
data DatastoreStorage = DatastoreStorage'
  { -- | Use this to store data store data in an S3 bucket managed by AWS IoT
    -- Analytics. You cannot change the choice of service-managed or
    -- customer-managed S3 storage after the data store is created.
    serviceManagedS3 :: Prelude.Maybe ServiceManagedDatastoreS3Storage,
    -- | Use this to store data store data in an S3 bucket that you manage. When
    -- customer managed storage is selected, the @retentionPeriod@ parameter is
    -- ignored. The choice of service-managed or customer-managed S3 storage
    -- cannot be changed after creation of the data store.
    customerManagedS3 :: Prelude.Maybe CustomerManagedDatastoreS3Storage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DatastoreStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceManagedS3', 'datastoreStorage_serviceManagedS3' - Use this to store data store data in an S3 bucket managed by AWS IoT
-- Analytics. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the data store is created.
--
-- 'customerManagedS3', 'datastoreStorage_customerManagedS3' - Use this to store data store data in an S3 bucket that you manage. When
-- customer managed storage is selected, the @retentionPeriod@ parameter is
-- ignored. The choice of service-managed or customer-managed S3 storage
-- cannot be changed after creation of the data store.
newDatastoreStorage ::
  DatastoreStorage
newDatastoreStorage =
  DatastoreStorage'
    { serviceManagedS3 =
        Prelude.Nothing,
      customerManagedS3 = Prelude.Nothing
    }

-- | Use this to store data store data in an S3 bucket managed by AWS IoT
-- Analytics. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the data store is created.
datastoreStorage_serviceManagedS3 :: Lens.Lens' DatastoreStorage (Prelude.Maybe ServiceManagedDatastoreS3Storage)
datastoreStorage_serviceManagedS3 = Lens.lens (\DatastoreStorage' {serviceManagedS3} -> serviceManagedS3) (\s@DatastoreStorage' {} a -> s {serviceManagedS3 = a} :: DatastoreStorage)

-- | Use this to store data store data in an S3 bucket that you manage. When
-- customer managed storage is selected, the @retentionPeriod@ parameter is
-- ignored. The choice of service-managed or customer-managed S3 storage
-- cannot be changed after creation of the data store.
datastoreStorage_customerManagedS3 :: Lens.Lens' DatastoreStorage (Prelude.Maybe CustomerManagedDatastoreS3Storage)
datastoreStorage_customerManagedS3 = Lens.lens (\DatastoreStorage' {customerManagedS3} -> customerManagedS3) (\s@DatastoreStorage' {} a -> s {customerManagedS3 = a} :: DatastoreStorage)

instance Prelude.FromJSON DatastoreStorage where
  parseJSON =
    Prelude.withObject
      "DatastoreStorage"
      ( \x ->
          DatastoreStorage'
            Prelude.<$> (x Prelude..:? "serviceManagedS3")
            Prelude.<*> (x Prelude..:? "customerManagedS3")
      )

instance Prelude.Hashable DatastoreStorage

instance Prelude.NFData DatastoreStorage

instance Prelude.ToJSON DatastoreStorage where
  toJSON DatastoreStorage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("serviceManagedS3" Prelude..=)
              Prelude.<$> serviceManagedS3,
            ("customerManagedS3" Prelude..=)
              Prelude.<$> customerManagedS3
          ]
      )
