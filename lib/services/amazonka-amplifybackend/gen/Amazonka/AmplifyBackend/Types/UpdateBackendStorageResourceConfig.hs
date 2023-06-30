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
-- Module      : Amazonka.AmplifyBackend.Types.UpdateBackendStorageResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UpdateBackendStorageResourceConfig where

import Amazonka.AmplifyBackend.Types.BackendStoragePermissions
import Amazonka.AmplifyBackend.Types.ServiceName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The resource configuration for updating backend storage.
--
-- /See:/ 'newUpdateBackendStorageResourceConfig' smart constructor.
data UpdateBackendStorageResourceConfig = UpdateBackendStorageResourceConfig'
  { -- | The name of the storage service.
    serviceName :: ServiceName,
    -- | The authorization configuration for the storage S3 bucket.
    permissions :: BackendStoragePermissions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendStorageResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'updateBackendStorageResourceConfig_serviceName' - The name of the storage service.
--
-- 'permissions', 'updateBackendStorageResourceConfig_permissions' - The authorization configuration for the storage S3 bucket.
newUpdateBackendStorageResourceConfig ::
  -- | 'serviceName'
  ServiceName ->
  -- | 'permissions'
  BackendStoragePermissions ->
  UpdateBackendStorageResourceConfig
newUpdateBackendStorageResourceConfig
  pServiceName_
  pPermissions_ =
    UpdateBackendStorageResourceConfig'
      { serviceName =
          pServiceName_,
        permissions = pPermissions_
      }

-- | The name of the storage service.
updateBackendStorageResourceConfig_serviceName :: Lens.Lens' UpdateBackendStorageResourceConfig ServiceName
updateBackendStorageResourceConfig_serviceName = Lens.lens (\UpdateBackendStorageResourceConfig' {serviceName} -> serviceName) (\s@UpdateBackendStorageResourceConfig' {} a -> s {serviceName = a} :: UpdateBackendStorageResourceConfig)

-- | The authorization configuration for the storage S3 bucket.
updateBackendStorageResourceConfig_permissions :: Lens.Lens' UpdateBackendStorageResourceConfig BackendStoragePermissions
updateBackendStorageResourceConfig_permissions = Lens.lens (\UpdateBackendStorageResourceConfig' {permissions} -> permissions) (\s@UpdateBackendStorageResourceConfig' {} a -> s {permissions = a} :: UpdateBackendStorageResourceConfig)

instance
  Prelude.Hashable
    UpdateBackendStorageResourceConfig
  where
  hashWithSalt
    _salt
    UpdateBackendStorageResourceConfig' {..} =
      _salt
        `Prelude.hashWithSalt` serviceName
        `Prelude.hashWithSalt` permissions

instance
  Prelude.NFData
    UpdateBackendStorageResourceConfig
  where
  rnf UpdateBackendStorageResourceConfig' {..} =
    Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf permissions

instance
  Data.ToJSON
    UpdateBackendStorageResourceConfig
  where
  toJSON UpdateBackendStorageResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just ("permissions" Data..= permissions)
          ]
      )
