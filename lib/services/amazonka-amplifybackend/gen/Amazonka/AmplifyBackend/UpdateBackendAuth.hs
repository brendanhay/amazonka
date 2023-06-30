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
-- Module      : Amazonka.AmplifyBackend.UpdateBackendAuth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing backend authentication resource.
module Amazonka.AmplifyBackend.UpdateBackendAuth
  ( -- * Creating a Request
    UpdateBackendAuth (..),
    newUpdateBackendAuth,

    -- * Request Lenses
    updateBackendAuth_appId,
    updateBackendAuth_backendEnvironmentName,
    updateBackendAuth_resourceName,
    updateBackendAuth_resourceConfig,

    -- * Destructuring the Response
    UpdateBackendAuthResponse (..),
    newUpdateBackendAuthResponse,

    -- * Response Lenses
    updateBackendAuthResponse_appId,
    updateBackendAuthResponse_backendEnvironmentName,
    updateBackendAuthResponse_error,
    updateBackendAuthResponse_jobId,
    updateBackendAuthResponse_operation,
    updateBackendAuthResponse_status,
    updateBackendAuthResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for UpdateBackendAuth.
--
-- /See:/ 'newUpdateBackendAuth' smart constructor.
data UpdateBackendAuth = UpdateBackendAuth'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of this resource.
    resourceName :: Prelude.Text,
    -- | The resource configuration for this request object.
    resourceConfig :: UpdateBackendAuthResourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'updateBackendAuth_appId' - The app ID.
--
-- 'backendEnvironmentName', 'updateBackendAuth_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'updateBackendAuth_resourceName' - The name of this resource.
--
-- 'resourceConfig', 'updateBackendAuth_resourceConfig' - The resource configuration for this request object.
newUpdateBackendAuth ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  -- | 'resourceConfig'
  UpdateBackendAuthResourceConfig ->
  UpdateBackendAuth
newUpdateBackendAuth
  pAppId_
  pBackendEnvironmentName_
  pResourceName_
  pResourceConfig_ =
    UpdateBackendAuth'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_,
        resourceConfig = pResourceConfig_
      }

-- | The app ID.
updateBackendAuth_appId :: Lens.Lens' UpdateBackendAuth Prelude.Text
updateBackendAuth_appId = Lens.lens (\UpdateBackendAuth' {appId} -> appId) (\s@UpdateBackendAuth' {} a -> s {appId = a} :: UpdateBackendAuth)

-- | The name of the backend environment.
updateBackendAuth_backendEnvironmentName :: Lens.Lens' UpdateBackendAuth Prelude.Text
updateBackendAuth_backendEnvironmentName = Lens.lens (\UpdateBackendAuth' {backendEnvironmentName} -> backendEnvironmentName) (\s@UpdateBackendAuth' {} a -> s {backendEnvironmentName = a} :: UpdateBackendAuth)

-- | The name of this resource.
updateBackendAuth_resourceName :: Lens.Lens' UpdateBackendAuth Prelude.Text
updateBackendAuth_resourceName = Lens.lens (\UpdateBackendAuth' {resourceName} -> resourceName) (\s@UpdateBackendAuth' {} a -> s {resourceName = a} :: UpdateBackendAuth)

-- | The resource configuration for this request object.
updateBackendAuth_resourceConfig :: Lens.Lens' UpdateBackendAuth UpdateBackendAuthResourceConfig
updateBackendAuth_resourceConfig = Lens.lens (\UpdateBackendAuth' {resourceConfig} -> resourceConfig) (\s@UpdateBackendAuth' {} a -> s {resourceConfig = a} :: UpdateBackendAuth)

instance Core.AWSRequest UpdateBackendAuth where
  type
    AWSResponse UpdateBackendAuth =
      UpdateBackendAuthResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBackendAuthResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBackendAuth where
  hashWithSalt _salt UpdateBackendAuth' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceConfig

instance Prelude.NFData UpdateBackendAuth where
  rnf UpdateBackendAuth' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceConfig

instance Data.ToHeaders UpdateBackendAuth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBackendAuth where
  toJSON UpdateBackendAuth' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceName" Data..= resourceName),
            Prelude.Just
              ("resourceConfig" Data..= resourceConfig)
          ]
      )

instance Data.ToPath UpdateBackendAuth where
  toPath UpdateBackendAuth' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/auth/",
        Data.toBS backendEnvironmentName
      ]

instance Data.ToQuery UpdateBackendAuth where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBackendAuthResponse' smart constructor.
data UpdateBackendAuthResponse = UpdateBackendAuthResponse'
  { -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | The ID for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name of the operation.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The current status of the request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'updateBackendAuthResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'updateBackendAuthResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'error', 'updateBackendAuthResponse_error' - If the request fails, this error is returned.
--
-- 'jobId', 'updateBackendAuthResponse_jobId' - The ID for the job.
--
-- 'operation', 'updateBackendAuthResponse_operation' - The name of the operation.
--
-- 'status', 'updateBackendAuthResponse_status' - The current status of the request.
--
-- 'httpStatus', 'updateBackendAuthResponse_httpStatus' - The response's http status code.
newUpdateBackendAuthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBackendAuthResponse
newUpdateBackendAuthResponse pHttpStatus_ =
  UpdateBackendAuthResponse'
    { appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      error = Prelude.Nothing,
      jobId = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
updateBackendAuthResponse_appId :: Lens.Lens' UpdateBackendAuthResponse (Prelude.Maybe Prelude.Text)
updateBackendAuthResponse_appId = Lens.lens (\UpdateBackendAuthResponse' {appId} -> appId) (\s@UpdateBackendAuthResponse' {} a -> s {appId = a} :: UpdateBackendAuthResponse)

-- | The name of the backend environment.
updateBackendAuthResponse_backendEnvironmentName :: Lens.Lens' UpdateBackendAuthResponse (Prelude.Maybe Prelude.Text)
updateBackendAuthResponse_backendEnvironmentName = Lens.lens (\UpdateBackendAuthResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@UpdateBackendAuthResponse' {} a -> s {backendEnvironmentName = a} :: UpdateBackendAuthResponse)

-- | If the request fails, this error is returned.
updateBackendAuthResponse_error :: Lens.Lens' UpdateBackendAuthResponse (Prelude.Maybe Prelude.Text)
updateBackendAuthResponse_error = Lens.lens (\UpdateBackendAuthResponse' {error} -> error) (\s@UpdateBackendAuthResponse' {} a -> s {error = a} :: UpdateBackendAuthResponse)

-- | The ID for the job.
updateBackendAuthResponse_jobId :: Lens.Lens' UpdateBackendAuthResponse (Prelude.Maybe Prelude.Text)
updateBackendAuthResponse_jobId = Lens.lens (\UpdateBackendAuthResponse' {jobId} -> jobId) (\s@UpdateBackendAuthResponse' {} a -> s {jobId = a} :: UpdateBackendAuthResponse)

-- | The name of the operation.
updateBackendAuthResponse_operation :: Lens.Lens' UpdateBackendAuthResponse (Prelude.Maybe Prelude.Text)
updateBackendAuthResponse_operation = Lens.lens (\UpdateBackendAuthResponse' {operation} -> operation) (\s@UpdateBackendAuthResponse' {} a -> s {operation = a} :: UpdateBackendAuthResponse)

-- | The current status of the request.
updateBackendAuthResponse_status :: Lens.Lens' UpdateBackendAuthResponse (Prelude.Maybe Prelude.Text)
updateBackendAuthResponse_status = Lens.lens (\UpdateBackendAuthResponse' {status} -> status) (\s@UpdateBackendAuthResponse' {} a -> s {status = a} :: UpdateBackendAuthResponse)

-- | The response's http status code.
updateBackendAuthResponse_httpStatus :: Lens.Lens' UpdateBackendAuthResponse Prelude.Int
updateBackendAuthResponse_httpStatus = Lens.lens (\UpdateBackendAuthResponse' {httpStatus} -> httpStatus) (\s@UpdateBackendAuthResponse' {} a -> s {httpStatus = a} :: UpdateBackendAuthResponse)

instance Prelude.NFData UpdateBackendAuthResponse where
  rnf UpdateBackendAuthResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
