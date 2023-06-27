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
-- Module      : Amazonka.Proton.CreateServiceSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create the Proton Ops configuration file.
module Amazonka.Proton.CreateServiceSyncConfig
  ( -- * Creating a Request
    CreateServiceSyncConfig (..),
    newCreateServiceSyncConfig,

    -- * Request Lenses
    createServiceSyncConfig_branch,
    createServiceSyncConfig_filePath,
    createServiceSyncConfig_repositoryName,
    createServiceSyncConfig_repositoryProvider,
    createServiceSyncConfig_serviceName,

    -- * Destructuring the Response
    CreateServiceSyncConfigResponse (..),
    newCreateServiceSyncConfigResponse,

    -- * Response Lenses
    createServiceSyncConfigResponse_serviceSyncConfig,
    createServiceSyncConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateServiceSyncConfig' smart constructor.
data CreateServiceSyncConfig = CreateServiceSyncConfig'
  { -- | The repository branch for your Proton Ops file.
    branch :: Prelude.Text,
    -- | The path to the Proton Ops file.
    filePath :: Prelude.Text,
    -- | The repository name.
    repositoryName :: Prelude.Text,
    -- | The provider type for your repository.
    repositoryProvider :: RepositoryProvider,
    -- | The name of the service the Proton Ops file is for.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'createServiceSyncConfig_branch' - The repository branch for your Proton Ops file.
--
-- 'filePath', 'createServiceSyncConfig_filePath' - The path to the Proton Ops file.
--
-- 'repositoryName', 'createServiceSyncConfig_repositoryName' - The repository name.
--
-- 'repositoryProvider', 'createServiceSyncConfig_repositoryProvider' - The provider type for your repository.
--
-- 'serviceName', 'createServiceSyncConfig_serviceName' - The name of the service the Proton Ops file is for.
newCreateServiceSyncConfig ::
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
  CreateServiceSyncConfig
newCreateServiceSyncConfig
  pBranch_
  pFilePath_
  pRepositoryName_
  pRepositoryProvider_
  pServiceName_ =
    CreateServiceSyncConfig'
      { branch = pBranch_,
        filePath = pFilePath_,
        repositoryName = pRepositoryName_,
        repositoryProvider = pRepositoryProvider_,
        serviceName = pServiceName_
      }

-- | The repository branch for your Proton Ops file.
createServiceSyncConfig_branch :: Lens.Lens' CreateServiceSyncConfig Prelude.Text
createServiceSyncConfig_branch = Lens.lens (\CreateServiceSyncConfig' {branch} -> branch) (\s@CreateServiceSyncConfig' {} a -> s {branch = a} :: CreateServiceSyncConfig)

-- | The path to the Proton Ops file.
createServiceSyncConfig_filePath :: Lens.Lens' CreateServiceSyncConfig Prelude.Text
createServiceSyncConfig_filePath = Lens.lens (\CreateServiceSyncConfig' {filePath} -> filePath) (\s@CreateServiceSyncConfig' {} a -> s {filePath = a} :: CreateServiceSyncConfig)

-- | The repository name.
createServiceSyncConfig_repositoryName :: Lens.Lens' CreateServiceSyncConfig Prelude.Text
createServiceSyncConfig_repositoryName = Lens.lens (\CreateServiceSyncConfig' {repositoryName} -> repositoryName) (\s@CreateServiceSyncConfig' {} a -> s {repositoryName = a} :: CreateServiceSyncConfig)

-- | The provider type for your repository.
createServiceSyncConfig_repositoryProvider :: Lens.Lens' CreateServiceSyncConfig RepositoryProvider
createServiceSyncConfig_repositoryProvider = Lens.lens (\CreateServiceSyncConfig' {repositoryProvider} -> repositoryProvider) (\s@CreateServiceSyncConfig' {} a -> s {repositoryProvider = a} :: CreateServiceSyncConfig)

-- | The name of the service the Proton Ops file is for.
createServiceSyncConfig_serviceName :: Lens.Lens' CreateServiceSyncConfig Prelude.Text
createServiceSyncConfig_serviceName = Lens.lens (\CreateServiceSyncConfig' {serviceName} -> serviceName) (\s@CreateServiceSyncConfig' {} a -> s {serviceName = a} :: CreateServiceSyncConfig)

instance Core.AWSRequest CreateServiceSyncConfig where
  type
    AWSResponse CreateServiceSyncConfig =
      CreateServiceSyncConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceSyncConfigResponse'
            Prelude.<$> (x Data..?> "serviceSyncConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateServiceSyncConfig where
  hashWithSalt _salt CreateServiceSyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryProvider
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData CreateServiceSyncConfig where
  rnf CreateServiceSyncConfig' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryProvider
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders CreateServiceSyncConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateServiceSyncConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateServiceSyncConfig where
  toJSON CreateServiceSyncConfig' {..} =
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

instance Data.ToPath CreateServiceSyncConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateServiceSyncConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceSyncConfigResponse' smart constructor.
data CreateServiceSyncConfigResponse = CreateServiceSyncConfigResponse'
  { -- | The detailed data of the Proton Ops file.
    serviceSyncConfig :: Prelude.Maybe ServiceSyncConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceSyncConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSyncConfig', 'createServiceSyncConfigResponse_serviceSyncConfig' - The detailed data of the Proton Ops file.
--
-- 'httpStatus', 'createServiceSyncConfigResponse_httpStatus' - The response's http status code.
newCreateServiceSyncConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceSyncConfigResponse
newCreateServiceSyncConfigResponse pHttpStatus_ =
  CreateServiceSyncConfigResponse'
    { serviceSyncConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the Proton Ops file.
createServiceSyncConfigResponse_serviceSyncConfig :: Lens.Lens' CreateServiceSyncConfigResponse (Prelude.Maybe ServiceSyncConfig)
createServiceSyncConfigResponse_serviceSyncConfig = Lens.lens (\CreateServiceSyncConfigResponse' {serviceSyncConfig} -> serviceSyncConfig) (\s@CreateServiceSyncConfigResponse' {} a -> s {serviceSyncConfig = a} :: CreateServiceSyncConfigResponse)

-- | The response's http status code.
createServiceSyncConfigResponse_httpStatus :: Lens.Lens' CreateServiceSyncConfigResponse Prelude.Int
createServiceSyncConfigResponse_httpStatus = Lens.lens (\CreateServiceSyncConfigResponse' {httpStatus} -> httpStatus) (\s@CreateServiceSyncConfigResponse' {} a -> s {httpStatus = a} :: CreateServiceSyncConfigResponse)

instance
  Prelude.NFData
    CreateServiceSyncConfigResponse
  where
  rnf CreateServiceSyncConfigResponse' {..} =
    Prelude.rnf serviceSyncConfig
      `Prelude.seq` Prelude.rnf httpStatus
