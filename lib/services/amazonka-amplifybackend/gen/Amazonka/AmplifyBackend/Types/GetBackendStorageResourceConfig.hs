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
-- Module      : Amazonka.AmplifyBackend.Types.GetBackendStorageResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.GetBackendStorageResourceConfig where

import Amazonka.AmplifyBackend.Types.BackendStoragePermissions
import Amazonka.AmplifyBackend.Types.ServiceName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details for a backend storage resource.
--
-- /See:/ 'newGetBackendStorageResourceConfig' smart constructor.
data GetBackendStorageResourceConfig = GetBackendStorageResourceConfig'
  { -- | The name of the S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The authorization configuration for the storage S3 bucket.
    permissions :: Prelude.Maybe BackendStoragePermissions,
    -- | The name of the storage service.
    serviceName :: ServiceName,
    -- | Returns True if the storage resource has been imported.
    imported :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendStorageResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'getBackendStorageResourceConfig_bucketName' - The name of the S3 bucket.
--
-- 'permissions', 'getBackendStorageResourceConfig_permissions' - The authorization configuration for the storage S3 bucket.
--
-- 'serviceName', 'getBackendStorageResourceConfig_serviceName' - The name of the storage service.
--
-- 'imported', 'getBackendStorageResourceConfig_imported' - Returns True if the storage resource has been imported.
newGetBackendStorageResourceConfig ::
  -- | 'serviceName'
  ServiceName ->
  -- | 'imported'
  Prelude.Bool ->
  GetBackendStorageResourceConfig
newGetBackendStorageResourceConfig
  pServiceName_
  pImported_ =
    GetBackendStorageResourceConfig'
      { bucketName =
          Prelude.Nothing,
        permissions = Prelude.Nothing,
        serviceName = pServiceName_,
        imported = pImported_
      }

-- | The name of the S3 bucket.
getBackendStorageResourceConfig_bucketName :: Lens.Lens' GetBackendStorageResourceConfig (Prelude.Maybe Prelude.Text)
getBackendStorageResourceConfig_bucketName = Lens.lens (\GetBackendStorageResourceConfig' {bucketName} -> bucketName) (\s@GetBackendStorageResourceConfig' {} a -> s {bucketName = a} :: GetBackendStorageResourceConfig)

-- | The authorization configuration for the storage S3 bucket.
getBackendStorageResourceConfig_permissions :: Lens.Lens' GetBackendStorageResourceConfig (Prelude.Maybe BackendStoragePermissions)
getBackendStorageResourceConfig_permissions = Lens.lens (\GetBackendStorageResourceConfig' {permissions} -> permissions) (\s@GetBackendStorageResourceConfig' {} a -> s {permissions = a} :: GetBackendStorageResourceConfig)

-- | The name of the storage service.
getBackendStorageResourceConfig_serviceName :: Lens.Lens' GetBackendStorageResourceConfig ServiceName
getBackendStorageResourceConfig_serviceName = Lens.lens (\GetBackendStorageResourceConfig' {serviceName} -> serviceName) (\s@GetBackendStorageResourceConfig' {} a -> s {serviceName = a} :: GetBackendStorageResourceConfig)

-- | Returns True if the storage resource has been imported.
getBackendStorageResourceConfig_imported :: Lens.Lens' GetBackendStorageResourceConfig Prelude.Bool
getBackendStorageResourceConfig_imported = Lens.lens (\GetBackendStorageResourceConfig' {imported} -> imported) (\s@GetBackendStorageResourceConfig' {} a -> s {imported = a} :: GetBackendStorageResourceConfig)

instance
  Data.FromJSON
    GetBackendStorageResourceConfig
  where
  parseJSON =
    Data.withObject
      "GetBackendStorageResourceConfig"
      ( \x ->
          GetBackendStorageResourceConfig'
            Prelude.<$> (x Data..:? "bucketName")
            Prelude.<*> (x Data..:? "permissions")
            Prelude.<*> (x Data..: "serviceName")
            Prelude.<*> (x Data..: "imported")
      )

instance
  Prelude.Hashable
    GetBackendStorageResourceConfig
  where
  hashWithSalt
    _salt
    GetBackendStorageResourceConfig' {..} =
      _salt
        `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` permissions
        `Prelude.hashWithSalt` serviceName
        `Prelude.hashWithSalt` imported

instance
  Prelude.NFData
    GetBackendStorageResourceConfig
  where
  rnf GetBackendStorageResourceConfig' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf imported
