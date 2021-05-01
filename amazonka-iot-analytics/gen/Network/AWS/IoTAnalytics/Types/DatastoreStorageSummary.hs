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
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary where

import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Where data store data is stored.
--
-- /See:/ 'newDatastoreStorageSummary' smart constructor.
data DatastoreStorageSummary = DatastoreStorageSummary'
  { -- | Used to store data store data in an S3 bucket managed by AWS IoT
    -- Analytics.
    serviceManagedS3 :: Prelude.Maybe ServiceManagedDatastoreS3StorageSummary,
    -- | Used to store data store data in an S3 bucket that you manage.
    customerManagedS3 :: Prelude.Maybe CustomerManagedDatastoreS3StorageSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      customerManagedS3 = Prelude.Nothing
    }

-- | Used to store data store data in an S3 bucket managed by AWS IoT
-- Analytics.
datastoreStorageSummary_serviceManagedS3 :: Lens.Lens' DatastoreStorageSummary (Prelude.Maybe ServiceManagedDatastoreS3StorageSummary)
datastoreStorageSummary_serviceManagedS3 = Lens.lens (\DatastoreStorageSummary' {serviceManagedS3} -> serviceManagedS3) (\s@DatastoreStorageSummary' {} a -> s {serviceManagedS3 = a} :: DatastoreStorageSummary)

-- | Used to store data store data in an S3 bucket that you manage.
datastoreStorageSummary_customerManagedS3 :: Lens.Lens' DatastoreStorageSummary (Prelude.Maybe CustomerManagedDatastoreS3StorageSummary)
datastoreStorageSummary_customerManagedS3 = Lens.lens (\DatastoreStorageSummary' {customerManagedS3} -> customerManagedS3) (\s@DatastoreStorageSummary' {} a -> s {customerManagedS3 = a} :: DatastoreStorageSummary)

instance Prelude.FromJSON DatastoreStorageSummary where
  parseJSON =
    Prelude.withObject
      "DatastoreStorageSummary"
      ( \x ->
          DatastoreStorageSummary'
            Prelude.<$> (x Prelude..:? "serviceManagedS3")
            Prelude.<*> (x Prelude..:? "customerManagedS3")
      )

instance Prelude.Hashable DatastoreStorageSummary

instance Prelude.NFData DatastoreStorageSummary
