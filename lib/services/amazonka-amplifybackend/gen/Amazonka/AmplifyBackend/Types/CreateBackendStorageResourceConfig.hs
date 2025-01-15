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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendStorageResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendStorageResourceConfig where

import Amazonka.AmplifyBackend.Types.BackendStoragePermissions
import Amazonka.AmplifyBackend.Types.ServiceName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The resource configuration for creating backend storage.
--
-- /See:/ 'newCreateBackendStorageResourceConfig' smart constructor.
data CreateBackendStorageResourceConfig = CreateBackendStorageResourceConfig'
  { -- | The name of the S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The name of the storage service.
    serviceName :: ServiceName,
    -- | The authorization configuration for the storage S3 bucket.
    permissions :: BackendStoragePermissions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendStorageResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'createBackendStorageResourceConfig_bucketName' - The name of the S3 bucket.
--
-- 'serviceName', 'createBackendStorageResourceConfig_serviceName' - The name of the storage service.
--
-- 'permissions', 'createBackendStorageResourceConfig_permissions' - The authorization configuration for the storage S3 bucket.
newCreateBackendStorageResourceConfig ::
  -- | 'serviceName'
  ServiceName ->
  -- | 'permissions'
  BackendStoragePermissions ->
  CreateBackendStorageResourceConfig
newCreateBackendStorageResourceConfig
  pServiceName_
  pPermissions_ =
    CreateBackendStorageResourceConfig'
      { bucketName =
          Prelude.Nothing,
        serviceName = pServiceName_,
        permissions = pPermissions_
      }

-- | The name of the S3 bucket.
createBackendStorageResourceConfig_bucketName :: Lens.Lens' CreateBackendStorageResourceConfig (Prelude.Maybe Prelude.Text)
createBackendStorageResourceConfig_bucketName = Lens.lens (\CreateBackendStorageResourceConfig' {bucketName} -> bucketName) (\s@CreateBackendStorageResourceConfig' {} a -> s {bucketName = a} :: CreateBackendStorageResourceConfig)

-- | The name of the storage service.
createBackendStorageResourceConfig_serviceName :: Lens.Lens' CreateBackendStorageResourceConfig ServiceName
createBackendStorageResourceConfig_serviceName = Lens.lens (\CreateBackendStorageResourceConfig' {serviceName} -> serviceName) (\s@CreateBackendStorageResourceConfig' {} a -> s {serviceName = a} :: CreateBackendStorageResourceConfig)

-- | The authorization configuration for the storage S3 bucket.
createBackendStorageResourceConfig_permissions :: Lens.Lens' CreateBackendStorageResourceConfig BackendStoragePermissions
createBackendStorageResourceConfig_permissions = Lens.lens (\CreateBackendStorageResourceConfig' {permissions} -> permissions) (\s@CreateBackendStorageResourceConfig' {} a -> s {permissions = a} :: CreateBackendStorageResourceConfig)

instance
  Prelude.Hashable
    CreateBackendStorageResourceConfig
  where
  hashWithSalt
    _salt
    CreateBackendStorageResourceConfig' {..} =
      _salt
        `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` serviceName
        `Prelude.hashWithSalt` permissions

instance
  Prelude.NFData
    CreateBackendStorageResourceConfig
  where
  rnf CreateBackendStorageResourceConfig' {..} =
    Prelude.rnf bucketName `Prelude.seq`
      Prelude.rnf serviceName `Prelude.seq`
        Prelude.rnf permissions

instance
  Data.ToJSON
    CreateBackendStorageResourceConfig
  where
  toJSON CreateBackendStorageResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketName" Data..=) Prelude.<$> bucketName,
            Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just ("permissions" Data..= permissions)
          ]
      )
