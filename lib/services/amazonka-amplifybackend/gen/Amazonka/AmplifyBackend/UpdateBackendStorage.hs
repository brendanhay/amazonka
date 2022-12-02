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
-- Module      : Amazonka.AmplifyBackend.UpdateBackendStorage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing backend storage resource.
module Amazonka.AmplifyBackend.UpdateBackendStorage
  ( -- * Creating a Request
    UpdateBackendStorage (..),
    newUpdateBackendStorage,

    -- * Request Lenses
    updateBackendStorage_appId,
    updateBackendStorage_backendEnvironmentName,
    updateBackendStorage_resourceName,
    updateBackendStorage_resourceConfig,

    -- * Destructuring the Response
    UpdateBackendStorageResponse (..),
    newUpdateBackendStorageResponse,

    -- * Response Lenses
    updateBackendStorageResponse_jobId,
    updateBackendStorageResponse_status,
    updateBackendStorageResponse_appId,
    updateBackendStorageResponse_backendEnvironmentName,
    updateBackendStorageResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for UpdateBackendStorage.
--
-- /See:/ 'newUpdateBackendStorage' smart constructor.
data UpdateBackendStorage = UpdateBackendStorage'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of the storage resource.
    resourceName :: Prelude.Text,
    -- | The resource configuration for updating backend storage.
    resourceConfig :: UpdateBackendStorageResourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'updateBackendStorage_appId' - The app ID.
--
-- 'backendEnvironmentName', 'updateBackendStorage_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'updateBackendStorage_resourceName' - The name of the storage resource.
--
-- 'resourceConfig', 'updateBackendStorage_resourceConfig' - The resource configuration for updating backend storage.
newUpdateBackendStorage ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  -- | 'resourceConfig'
  UpdateBackendStorageResourceConfig ->
  UpdateBackendStorage
newUpdateBackendStorage
  pAppId_
  pBackendEnvironmentName_
  pResourceName_
  pResourceConfig_ =
    UpdateBackendStorage'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_,
        resourceConfig = pResourceConfig_
      }

-- | The app ID.
updateBackendStorage_appId :: Lens.Lens' UpdateBackendStorage Prelude.Text
updateBackendStorage_appId = Lens.lens (\UpdateBackendStorage' {appId} -> appId) (\s@UpdateBackendStorage' {} a -> s {appId = a} :: UpdateBackendStorage)

-- | The name of the backend environment.
updateBackendStorage_backendEnvironmentName :: Lens.Lens' UpdateBackendStorage Prelude.Text
updateBackendStorage_backendEnvironmentName = Lens.lens (\UpdateBackendStorage' {backendEnvironmentName} -> backendEnvironmentName) (\s@UpdateBackendStorage' {} a -> s {backendEnvironmentName = a} :: UpdateBackendStorage)

-- | The name of the storage resource.
updateBackendStorage_resourceName :: Lens.Lens' UpdateBackendStorage Prelude.Text
updateBackendStorage_resourceName = Lens.lens (\UpdateBackendStorage' {resourceName} -> resourceName) (\s@UpdateBackendStorage' {} a -> s {resourceName = a} :: UpdateBackendStorage)

-- | The resource configuration for updating backend storage.
updateBackendStorage_resourceConfig :: Lens.Lens' UpdateBackendStorage UpdateBackendStorageResourceConfig
updateBackendStorage_resourceConfig = Lens.lens (\UpdateBackendStorage' {resourceConfig} -> resourceConfig) (\s@UpdateBackendStorage' {} a -> s {resourceConfig = a} :: UpdateBackendStorage)

instance Core.AWSRequest UpdateBackendStorage where
  type
    AWSResponse UpdateBackendStorage =
      UpdateBackendStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBackendStorageResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBackendStorage where
  hashWithSalt _salt UpdateBackendStorage' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceConfig

instance Prelude.NFData UpdateBackendStorage where
  rnf UpdateBackendStorage' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceConfig

instance Data.ToHeaders UpdateBackendStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBackendStorage where
  toJSON UpdateBackendStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceName" Data..= resourceName),
            Prelude.Just
              ("resourceConfig" Data..= resourceConfig)
          ]
      )

instance Data.ToPath UpdateBackendStorage where
  toPath UpdateBackendStorage' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/storage/",
        Data.toBS backendEnvironmentName
      ]

instance Data.ToQuery UpdateBackendStorage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBackendStorageResponse' smart constructor.
data UpdateBackendStorageResponse = UpdateBackendStorageResponse'
  { -- | The ID for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'updateBackendStorageResponse_jobId' - The ID for the job.
--
-- 'status', 'updateBackendStorageResponse_status' - The current status of the request.
--
-- 'appId', 'updateBackendStorageResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'updateBackendStorageResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'httpStatus', 'updateBackendStorageResponse_httpStatus' - The response's http status code.
newUpdateBackendStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBackendStorageResponse
newUpdateBackendStorageResponse pHttpStatus_ =
  UpdateBackendStorageResponse'
    { jobId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the job.
updateBackendStorageResponse_jobId :: Lens.Lens' UpdateBackendStorageResponse (Prelude.Maybe Prelude.Text)
updateBackendStorageResponse_jobId = Lens.lens (\UpdateBackendStorageResponse' {jobId} -> jobId) (\s@UpdateBackendStorageResponse' {} a -> s {jobId = a} :: UpdateBackendStorageResponse)

-- | The current status of the request.
updateBackendStorageResponse_status :: Lens.Lens' UpdateBackendStorageResponse (Prelude.Maybe Prelude.Text)
updateBackendStorageResponse_status = Lens.lens (\UpdateBackendStorageResponse' {status} -> status) (\s@UpdateBackendStorageResponse' {} a -> s {status = a} :: UpdateBackendStorageResponse)

-- | The app ID.
updateBackendStorageResponse_appId :: Lens.Lens' UpdateBackendStorageResponse (Prelude.Maybe Prelude.Text)
updateBackendStorageResponse_appId = Lens.lens (\UpdateBackendStorageResponse' {appId} -> appId) (\s@UpdateBackendStorageResponse' {} a -> s {appId = a} :: UpdateBackendStorageResponse)

-- | The name of the backend environment.
updateBackendStorageResponse_backendEnvironmentName :: Lens.Lens' UpdateBackendStorageResponse (Prelude.Maybe Prelude.Text)
updateBackendStorageResponse_backendEnvironmentName = Lens.lens (\UpdateBackendStorageResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@UpdateBackendStorageResponse' {} a -> s {backendEnvironmentName = a} :: UpdateBackendStorageResponse)

-- | The response's http status code.
updateBackendStorageResponse_httpStatus :: Lens.Lens' UpdateBackendStorageResponse Prelude.Int
updateBackendStorageResponse_httpStatus = Lens.lens (\UpdateBackendStorageResponse' {httpStatus} -> httpStatus) (\s@UpdateBackendStorageResponse' {} a -> s {httpStatus = a} :: UpdateBackendStorageResponse)

instance Prelude.NFData UpdateBackendStorageResponse where
  rnf UpdateBackendStorageResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf httpStatus
