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
-- Module      : Amazonka.Proton.Types.ServiceSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceSyncConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.RepositoryProvider

-- | Detailed data of the service sync configuration.
--
-- /See:/ 'newServiceSyncConfig' smart constructor.
data ServiceSyncConfig = ServiceSyncConfig'
  { -- | The name of the code repository branch that holds the service code
    -- Proton will sync with.
    branch :: Prelude.Text,
    -- | The file path to the service sync configuration file.
    filePath :: Prelude.Text,
    -- | The name of the code repository that holds the service code Proton will
    -- sync with.
    repositoryName :: Prelude.Text,
    -- | The name of the repository provider that holds the repository Proton
    -- will sync with.
    repositoryProvider :: RepositoryProvider,
    -- | The name of the service that the service instance is added to.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'serviceSyncConfig_branch' - The name of the code repository branch that holds the service code
-- Proton will sync with.
--
-- 'filePath', 'serviceSyncConfig_filePath' - The file path to the service sync configuration file.
--
-- 'repositoryName', 'serviceSyncConfig_repositoryName' - The name of the code repository that holds the service code Proton will
-- sync with.
--
-- 'repositoryProvider', 'serviceSyncConfig_repositoryProvider' - The name of the repository provider that holds the repository Proton
-- will sync with.
--
-- 'serviceName', 'serviceSyncConfig_serviceName' - The name of the service that the service instance is added to.
newServiceSyncConfig ::
  -- | 'branch'
  Prelude.Text ->
  -- | 'filePath'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'repositoryProvider'
  RepositoryProvider ->
  -- | 'serviceName'
  Prelude.Text ->
  ServiceSyncConfig
newServiceSyncConfig
  pBranch_
  pFilePath_
  pRepositoryName_
  pRepositoryProvider_
  pServiceName_ =
    ServiceSyncConfig'
      { branch = pBranch_,
        filePath = pFilePath_,
        repositoryName = pRepositoryName_,
        repositoryProvider = pRepositoryProvider_,
        serviceName = pServiceName_
      }

-- | The name of the code repository branch that holds the service code
-- Proton will sync with.
serviceSyncConfig_branch :: Lens.Lens' ServiceSyncConfig Prelude.Text
serviceSyncConfig_branch = Lens.lens (\ServiceSyncConfig' {branch} -> branch) (\s@ServiceSyncConfig' {} a -> s {branch = a} :: ServiceSyncConfig)

-- | The file path to the service sync configuration file.
serviceSyncConfig_filePath :: Lens.Lens' ServiceSyncConfig Prelude.Text
serviceSyncConfig_filePath = Lens.lens (\ServiceSyncConfig' {filePath} -> filePath) (\s@ServiceSyncConfig' {} a -> s {filePath = a} :: ServiceSyncConfig)

-- | The name of the code repository that holds the service code Proton will
-- sync with.
serviceSyncConfig_repositoryName :: Lens.Lens' ServiceSyncConfig Prelude.Text
serviceSyncConfig_repositoryName = Lens.lens (\ServiceSyncConfig' {repositoryName} -> repositoryName) (\s@ServiceSyncConfig' {} a -> s {repositoryName = a} :: ServiceSyncConfig)

-- | The name of the repository provider that holds the repository Proton
-- will sync with.
serviceSyncConfig_repositoryProvider :: Lens.Lens' ServiceSyncConfig RepositoryProvider
serviceSyncConfig_repositoryProvider = Lens.lens (\ServiceSyncConfig' {repositoryProvider} -> repositoryProvider) (\s@ServiceSyncConfig' {} a -> s {repositoryProvider = a} :: ServiceSyncConfig)

-- | The name of the service that the service instance is added to.
serviceSyncConfig_serviceName :: Lens.Lens' ServiceSyncConfig Prelude.Text
serviceSyncConfig_serviceName = Lens.lens (\ServiceSyncConfig' {serviceName} -> serviceName) (\s@ServiceSyncConfig' {} a -> s {serviceName = a} :: ServiceSyncConfig)

instance Data.FromJSON ServiceSyncConfig where
  parseJSON =
    Data.withObject
      "ServiceSyncConfig"
      ( \x ->
          ServiceSyncConfig'
            Prelude.<$> (x Data..: "branch")
            Prelude.<*> (x Data..: "filePath")
            Prelude.<*> (x Data..: "repositoryName")
            Prelude.<*> (x Data..: "repositoryProvider")
            Prelude.<*> (x Data..: "serviceName")
      )

instance Prelude.Hashable ServiceSyncConfig where
  hashWithSalt _salt ServiceSyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryProvider
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ServiceSyncConfig where
  rnf ServiceSyncConfig' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryProvider
      `Prelude.seq` Prelude.rnf serviceName
