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
-- Module      : Amazonka.AmplifyBackend.UpdateBackendAPI
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing backend API resource.
module Amazonka.AmplifyBackend.UpdateBackendAPI
  ( -- * Creating a Request
    UpdateBackendAPI (..),
    newUpdateBackendAPI,

    -- * Request Lenses
    updateBackendAPI_resourceConfig,
    updateBackendAPI_appId,
    updateBackendAPI_backendEnvironmentName,
    updateBackendAPI_resourceName,

    -- * Destructuring the Response
    UpdateBackendAPIResponse (..),
    newUpdateBackendAPIResponse,

    -- * Response Lenses
    updateBackendAPIResponse_jobId,
    updateBackendAPIResponse_status,
    updateBackendAPIResponse_error,
    updateBackendAPIResponse_operation,
    updateBackendAPIResponse_appId,
    updateBackendAPIResponse_backendEnvironmentName,
    updateBackendAPIResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for UpdateBackendAPI.
--
-- /See:/ 'newUpdateBackendAPI' smart constructor.
data UpdateBackendAPI = UpdateBackendAPI'
  { -- | Defines the resource configuration for the data model in your Amplify
    -- project.
    resourceConfig :: Prelude.Maybe BackendAPIResourceConfig,
    -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of this resource.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAPI' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceConfig', 'updateBackendAPI_resourceConfig' - Defines the resource configuration for the data model in your Amplify
-- project.
--
-- 'appId', 'updateBackendAPI_appId' - The app ID.
--
-- 'backendEnvironmentName', 'updateBackendAPI_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'updateBackendAPI_resourceName' - The name of this resource.
newUpdateBackendAPI ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  UpdateBackendAPI
newUpdateBackendAPI
  pAppId_
  pBackendEnvironmentName_
  pResourceName_ =
    UpdateBackendAPI'
      { resourceConfig = Prelude.Nothing,
        appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_
      }

-- | Defines the resource configuration for the data model in your Amplify
-- project.
updateBackendAPI_resourceConfig :: Lens.Lens' UpdateBackendAPI (Prelude.Maybe BackendAPIResourceConfig)
updateBackendAPI_resourceConfig = Lens.lens (\UpdateBackendAPI' {resourceConfig} -> resourceConfig) (\s@UpdateBackendAPI' {} a -> s {resourceConfig = a} :: UpdateBackendAPI)

-- | The app ID.
updateBackendAPI_appId :: Lens.Lens' UpdateBackendAPI Prelude.Text
updateBackendAPI_appId = Lens.lens (\UpdateBackendAPI' {appId} -> appId) (\s@UpdateBackendAPI' {} a -> s {appId = a} :: UpdateBackendAPI)

-- | The name of the backend environment.
updateBackendAPI_backendEnvironmentName :: Lens.Lens' UpdateBackendAPI Prelude.Text
updateBackendAPI_backendEnvironmentName = Lens.lens (\UpdateBackendAPI' {backendEnvironmentName} -> backendEnvironmentName) (\s@UpdateBackendAPI' {} a -> s {backendEnvironmentName = a} :: UpdateBackendAPI)

-- | The name of this resource.
updateBackendAPI_resourceName :: Lens.Lens' UpdateBackendAPI Prelude.Text
updateBackendAPI_resourceName = Lens.lens (\UpdateBackendAPI' {resourceName} -> resourceName) (\s@UpdateBackendAPI' {} a -> s {resourceName = a} :: UpdateBackendAPI)

instance Core.AWSRequest UpdateBackendAPI where
  type
    AWSResponse UpdateBackendAPI =
      UpdateBackendAPIResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBackendAPIResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBackendAPI where
  hashWithSalt _salt UpdateBackendAPI' {..} =
    _salt `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData UpdateBackendAPI where
  rnf UpdateBackendAPI' {..} =
    Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceName

instance Data.ToHeaders UpdateBackendAPI where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBackendAPI where
  toJSON UpdateBackendAPI' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceConfig" Data..=)
              Prelude.<$> resourceConfig,
            Prelude.Just ("resourceName" Data..= resourceName)
          ]
      )

instance Data.ToPath UpdateBackendAPI where
  toPath UpdateBackendAPI' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/api/",
        Data.toBS backendEnvironmentName
      ]

instance Data.ToQuery UpdateBackendAPI where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBackendAPIResponse' smart constructor.
data UpdateBackendAPIResponse = UpdateBackendAPIResponse'
  { -- | The ID for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the request.
    status :: Prelude.Maybe Prelude.Text,
    -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | The name of the operation.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAPIResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'updateBackendAPIResponse_jobId' - The ID for the job.
--
-- 'status', 'updateBackendAPIResponse_status' - The current status of the request.
--
-- 'error', 'updateBackendAPIResponse_error' - If the request fails, this error is returned.
--
-- 'operation', 'updateBackendAPIResponse_operation' - The name of the operation.
--
-- 'appId', 'updateBackendAPIResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'updateBackendAPIResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'httpStatus', 'updateBackendAPIResponse_httpStatus' - The response's http status code.
newUpdateBackendAPIResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBackendAPIResponse
newUpdateBackendAPIResponse pHttpStatus_ =
  UpdateBackendAPIResponse'
    { jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      error = Prelude.Nothing,
      operation = Prelude.Nothing,
      appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the job.
updateBackendAPIResponse_jobId :: Lens.Lens' UpdateBackendAPIResponse (Prelude.Maybe Prelude.Text)
updateBackendAPIResponse_jobId = Lens.lens (\UpdateBackendAPIResponse' {jobId} -> jobId) (\s@UpdateBackendAPIResponse' {} a -> s {jobId = a} :: UpdateBackendAPIResponse)

-- | The current status of the request.
updateBackendAPIResponse_status :: Lens.Lens' UpdateBackendAPIResponse (Prelude.Maybe Prelude.Text)
updateBackendAPIResponse_status = Lens.lens (\UpdateBackendAPIResponse' {status} -> status) (\s@UpdateBackendAPIResponse' {} a -> s {status = a} :: UpdateBackendAPIResponse)

-- | If the request fails, this error is returned.
updateBackendAPIResponse_error :: Lens.Lens' UpdateBackendAPIResponse (Prelude.Maybe Prelude.Text)
updateBackendAPIResponse_error = Lens.lens (\UpdateBackendAPIResponse' {error} -> error) (\s@UpdateBackendAPIResponse' {} a -> s {error = a} :: UpdateBackendAPIResponse)

-- | The name of the operation.
updateBackendAPIResponse_operation :: Lens.Lens' UpdateBackendAPIResponse (Prelude.Maybe Prelude.Text)
updateBackendAPIResponse_operation = Lens.lens (\UpdateBackendAPIResponse' {operation} -> operation) (\s@UpdateBackendAPIResponse' {} a -> s {operation = a} :: UpdateBackendAPIResponse)

-- | The app ID.
updateBackendAPIResponse_appId :: Lens.Lens' UpdateBackendAPIResponse (Prelude.Maybe Prelude.Text)
updateBackendAPIResponse_appId = Lens.lens (\UpdateBackendAPIResponse' {appId} -> appId) (\s@UpdateBackendAPIResponse' {} a -> s {appId = a} :: UpdateBackendAPIResponse)

-- | The name of the backend environment.
updateBackendAPIResponse_backendEnvironmentName :: Lens.Lens' UpdateBackendAPIResponse (Prelude.Maybe Prelude.Text)
updateBackendAPIResponse_backendEnvironmentName = Lens.lens (\UpdateBackendAPIResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@UpdateBackendAPIResponse' {} a -> s {backendEnvironmentName = a} :: UpdateBackendAPIResponse)

-- | The response's http status code.
updateBackendAPIResponse_httpStatus :: Lens.Lens' UpdateBackendAPIResponse Prelude.Int
updateBackendAPIResponse_httpStatus = Lens.lens (\UpdateBackendAPIResponse' {httpStatus} -> httpStatus) (\s@UpdateBackendAPIResponse' {} a -> s {httpStatus = a} :: UpdateBackendAPIResponse)

instance Prelude.NFData UpdateBackendAPIResponse where
  rnf UpdateBackendAPIResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf httpStatus
