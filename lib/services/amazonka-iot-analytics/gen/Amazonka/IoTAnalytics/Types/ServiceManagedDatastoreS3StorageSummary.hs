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
-- Module      : Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the data store that is managed by IoT
-- Analytics.
--
-- /See:/ 'newServiceManagedDatastoreS3StorageSummary' smart constructor.
data ServiceManagedDatastoreS3StorageSummary = ServiceManagedDatastoreS3StorageSummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceManagedDatastoreS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newServiceManagedDatastoreS3StorageSummary ::
  ServiceManagedDatastoreS3StorageSummary
newServiceManagedDatastoreS3StorageSummary =
  ServiceManagedDatastoreS3StorageSummary'

instance
  Data.FromJSON
    ServiceManagedDatastoreS3StorageSummary
  where
  parseJSON =
    Data.withObject
      "ServiceManagedDatastoreS3StorageSummary"
      ( \x ->
          Prelude.pure
            ServiceManagedDatastoreS3StorageSummary'
      )

instance
  Prelude.Hashable
    ServiceManagedDatastoreS3StorageSummary
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    ServiceManagedDatastoreS3StorageSummary
  where
  rnf _ = ()
