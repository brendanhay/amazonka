{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Proton.UpdateServiceSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the Proton Ops config file.
module Amazonka.Proton.UpdateServiceSyncConfig
  ( -- * Creating a Request
    UpdateServiceSyncConfig (..),
    newUpdateServiceSyncConfig,

    -- * Request Lenses
    updateServiceSyncConfig_branch,
    updateServiceSyncConfig_filePath,
    updateServiceSyncConfig_repositoryName,
    updateServiceSyncConfig_repositoryProvider,
    updateServiceSyncConfig_serviceName,

    -- * Destructuring the Response
    UpdateServiceSyncConfigResponse (..),
    newUpdateServiceSyncConfigResponse,

    -- * Response Lenses
    updateServiceSyncConfigResponse_serviceSyncConfig,
    updateServiceSyncConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceSyncConfig' smart constructor.
data UpdateServiceSyncConfig = UpdateServiceSyncConfig'
  { -- | The name of the code repository branch where the Proton Ops file is
    -- found.
    branch :: Prelude.Text,
    -- | The path to the Proton Ops file.
    filePath :: Prelude.Text,
    -- | The name of the repository where the Proton Ops file is found.
    repositoryName :: Prelude.Text,
    -- | The name of the repository provider where the Proton Ops file is found.
    repositoryProvider :: RepositoryProvider,
    -- | The name of the service the Proton Ops file is for.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'updateServiceSyncConfig_branch' - The name of the code repository branch where the Proton Ops file is
-- found.
--
-- 'filePath', 'updateServiceSyncConfig_filePath' - The path to the Proton Ops file.
--
-- 'repositoryName', 'updateServiceSyncConfig_repositoryName' - The name of the repository where the Proton Ops file is found.
--
-- 'repositoryProvider', 'updateServiceSyncConfig_repositoryProvider' - The name of the repository provider where the Proton Ops file is found.
--
-- 'serviceName', 'updateServiceSyncConfig_serviceName' - The name of the service the Proton Ops file is for.
newUpdateServiceSyncConfig ::
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
  UpdateServiceSyncConfig
newUpdateServiceSyncConfig
  pBranch_
  pFilePath_
  pRepositoryName_
  pRepositoryProvider_
  pServiceName_ =
    UpdateServiceSyncConfig'
      { branch = pBranch_,
        filePath = pFilePath_,
        repositoryName = pRepositoryName_,
        repositoryProvider = pRepositoryProvider_,
        serviceName = pServiceName_
      }

-- | The name of the code repository branch where the Proton Ops file is
-- found.
updateServiceSyncConfig_branch :: Lens.Lens' UpdateServiceSyncConfig Prelude.Text
updateServiceSyncConfig_branch = Lens.lens (\UpdateServiceSyncConfig' {branch} -> branch) (\s@UpdateServiceSyncConfig' {} a -> s {branch = a} :: UpdateServiceSyncConfig)

-- | The path to the Proton Ops file.
updateServiceSyncConfig_filePath :: Lens.Lens' UpdateServiceSyncConfig Prelude.Text
updateServiceSyncConfig_filePath = Lens.lens (\UpdateServiceSyncConfig' {filePath} -> filePath) (\s@UpdateServiceSyncConfig' {} a -> s {filePath = a} :: UpdateServiceSyncConfig)

-- | The name of the repository where the Proton Ops file is found.
updateServiceSyncConfig_repositoryName :: Lens.Lens' UpdateServiceSyncConfig Prelude.Text
updateServiceSyncConfig_repositoryName = Lens.lens (\UpdateServiceSyncConfig' {repositoryName} -> repositoryName) (\s@UpdateServiceSyncConfig' {} a -> s {repositoryName = a} :: UpdateServiceSyncConfig)

-- | The name of the repository provider where the Proton Ops file is found.
updateServiceSyncConfig_repositoryProvider :: Lens.Lens' UpdateServiceSyncConfig RepositoryProvider
updateServiceSyncConfig_repositoryProvider = Lens.lens (\UpdateServiceSyncConfig' {repositoryProvider} -> repositoryProvider) (\s@UpdateServiceSyncConfig' {} a -> s {repositoryProvider = a} :: UpdateServiceSyncConfig)

-- | The name of the service the Proton Ops file is for.
updateServiceSyncConfig_serviceName :: Lens.Lens' UpdateServiceSyncConfig Prelude.Text
updateServiceSyncConfig_serviceName = Lens.lens (\UpdateServiceSyncConfig' {serviceName} -> serviceName) (\s@UpdateServiceSyncConfig' {} a -> s {serviceName = a} :: UpdateServiceSyncConfig)

instance Core.AWSRequest UpdateServiceSyncConfig where
  type
    AWSResponse UpdateServiceSyncConfig =
      UpdateServiceSyncConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceSyncConfigResponse'
            Prelude.<$> (x Data..?> "serviceSyncConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceSyncConfig where
  hashWithSalt _salt UpdateServiceSyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryProvider
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData UpdateServiceSyncConfig where
  rnf UpdateServiceSyncConfig' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryProvider
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders UpdateServiceSyncConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateServiceSyncConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServiceSyncConfig where
  toJSON UpdateServiceSyncConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("branch" Data..= branch),
            Prelude.Just ("filePath" Data..= filePath),
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ("repositoryProvider" Data..= repositoryProvider),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath UpdateServiceSyncConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateServiceSyncConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceSyncConfigResponse' smart constructor.
data UpdateServiceSyncConfigResponse = UpdateServiceSyncConfigResponse'
  { -- | The detailed data of the Proton Ops file.
    serviceSyncConfig :: Prelude.Maybe ServiceSyncConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSyncConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSyncConfig', 'updateServiceSyncConfigResponse_serviceSyncConfig' - The detailed data of the Proton Ops file.
--
-- 'httpStatus', 'updateServiceSyncConfigResponse_httpStatus' - The response's http status code.
newUpdateServiceSyncConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceSyncConfigResponse
newUpdateServiceSyncConfigResponse pHttpStatus_ =
  UpdateServiceSyncConfigResponse'
    { serviceSyncConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the Proton Ops file.
updateServiceSyncConfigResponse_serviceSyncConfig :: Lens.Lens' UpdateServiceSyncConfigResponse (Prelude.Maybe ServiceSyncConfig)
updateServiceSyncConfigResponse_serviceSyncConfig = Lens.lens (\UpdateServiceSyncConfigResponse' {serviceSyncConfig} -> serviceSyncConfig) (\s@UpdateServiceSyncConfigResponse' {} a -> s {serviceSyncConfig = a} :: UpdateServiceSyncConfigResponse)

-- | The response's http status code.
updateServiceSyncConfigResponse_httpStatus :: Lens.Lens' UpdateServiceSyncConfigResponse Prelude.Int
updateServiceSyncConfigResponse_httpStatus = Lens.lens (\UpdateServiceSyncConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceSyncConfigResponse' {} a -> s {httpStatus = a} :: UpdateServiceSyncConfigResponse)

instance
  Prelude.NFData
    UpdateServiceSyncConfigResponse
  where
  rnf UpdateServiceSyncConfigResponse' {..} =
    Prelude.rnf serviceSyncConfig
      `Prelude.seq` Prelude.rnf httpStatus
