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
-- Module      : Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3Storage
import qualified Amazonka.Prelude as Prelude

-- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage. You can\'t change the choice of Amazon S3 storage after your
-- data store is created.
--
-- /See:/ 'newDatastoreIotSiteWiseMultiLayerStorage' smart constructor.
data DatastoreIotSiteWiseMultiLayerStorage = DatastoreIotSiteWiseMultiLayerStorage'
  { -- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
    -- manage.
    customerManagedS3Storage :: IotSiteWiseCustomerManagedDatastoreS3Storage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreIotSiteWiseMultiLayerStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerManagedS3Storage', 'datastoreIotSiteWiseMultiLayerStorage_customerManagedS3Storage' - Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage.
newDatastoreIotSiteWiseMultiLayerStorage ::
  -- | 'customerManagedS3Storage'
  IotSiteWiseCustomerManagedDatastoreS3Storage ->
  DatastoreIotSiteWiseMultiLayerStorage
newDatastoreIotSiteWiseMultiLayerStorage
  pCustomerManagedS3Storage_ =
    DatastoreIotSiteWiseMultiLayerStorage'
      { customerManagedS3Storage =
          pCustomerManagedS3Storage_
      }

-- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage.
datastoreIotSiteWiseMultiLayerStorage_customerManagedS3Storage :: Lens.Lens' DatastoreIotSiteWiseMultiLayerStorage IotSiteWiseCustomerManagedDatastoreS3Storage
datastoreIotSiteWiseMultiLayerStorage_customerManagedS3Storage = Lens.lens (\DatastoreIotSiteWiseMultiLayerStorage' {customerManagedS3Storage} -> customerManagedS3Storage) (\s@DatastoreIotSiteWiseMultiLayerStorage' {} a -> s {customerManagedS3Storage = a} :: DatastoreIotSiteWiseMultiLayerStorage)

instance
  Core.FromJSON
    DatastoreIotSiteWiseMultiLayerStorage
  where
  parseJSON =
    Core.withObject
      "DatastoreIotSiteWiseMultiLayerStorage"
      ( \x ->
          DatastoreIotSiteWiseMultiLayerStorage'
            Prelude.<$> (x Core..: "customerManagedS3Storage")
      )

instance
  Prelude.Hashable
    DatastoreIotSiteWiseMultiLayerStorage
  where
  hashWithSalt
    _salt
    DatastoreIotSiteWiseMultiLayerStorage' {..} =
      _salt
        `Prelude.hashWithSalt` customerManagedS3Storage

instance
  Prelude.NFData
    DatastoreIotSiteWiseMultiLayerStorage
  where
  rnf DatastoreIotSiteWiseMultiLayerStorage' {..} =
    Prelude.rnf customerManagedS3Storage

instance
  Core.ToJSON
    DatastoreIotSiteWiseMultiLayerStorage
  where
  toJSON DatastoreIotSiteWiseMultiLayerStorage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "customerManagedS3Storage"
                  Core..= customerManagedS3Storage
              )
          ]
      )
