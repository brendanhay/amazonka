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
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
import qualified Network.AWS.Lens as Lens

-- | Where data store data is stored.
--
-- /See:/ 'newDatastoreStorageSummary' smart constructor.
data DatastoreStorageSummary = DatastoreStorageSummary'
  { -- | Used to store data store data in an S3 bucket managed by AWS IoT
    -- Analytics.
    serviceManagedS3 :: Core.Maybe ServiceManagedDatastoreS3StorageSummary,
    -- | Used to store data store data in an S3 bucket that you manage.
    customerManagedS3 :: Core.Maybe CustomerManagedDatastoreS3StorageSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DatastoreStorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceManagedS3', 'datastoreStorageSummary_serviceManagedS3' - Used to store data store data in an S3 bucket managed by AWS IoT
-- Analytics.
--
-- 'customerManagedS3', 'datastoreStorageSummary_customerManagedS3' - Used to store data store data in an S3 bucket that you manage.
newDatastoreStorageSummary ::
  DatastoreStorageSummary
newDatastoreStorageSummary =
  DatastoreStorageSummary'
    { serviceManagedS3 =
        Core.Nothing,
      customerManagedS3 = Core.Nothing
    }

-- | Used to store data store data in an S3 bucket managed by AWS IoT
-- Analytics.
datastoreStorageSummary_serviceManagedS3 :: Lens.Lens' DatastoreStorageSummary (Core.Maybe ServiceManagedDatastoreS3StorageSummary)
datastoreStorageSummary_serviceManagedS3 = Lens.lens (\DatastoreStorageSummary' {serviceManagedS3} -> serviceManagedS3) (\s@DatastoreStorageSummary' {} a -> s {serviceManagedS3 = a} :: DatastoreStorageSummary)

-- | Used to store data store data in an S3 bucket that you manage.
datastoreStorageSummary_customerManagedS3 :: Lens.Lens' DatastoreStorageSummary (Core.Maybe CustomerManagedDatastoreS3StorageSummary)
datastoreStorageSummary_customerManagedS3 = Lens.lens (\DatastoreStorageSummary' {customerManagedS3} -> customerManagedS3) (\s@DatastoreStorageSummary' {} a -> s {customerManagedS3 = a} :: DatastoreStorageSummary)

instance Core.FromJSON DatastoreStorageSummary where
  parseJSON =
    Core.withObject
      "DatastoreStorageSummary"
      ( \x ->
          DatastoreStorageSummary'
            Core.<$> (x Core..:? "serviceManagedS3")
            Core.<*> (x Core..:? "customerManagedS3")
      )

instance Core.Hashable DatastoreStorageSummary

instance Core.NFData DatastoreStorageSummary
