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
-- Module      : Amazonka.AmplifyBackend.CreateBackendAPI
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new backend API resource.
module Amazonka.AmplifyBackend.CreateBackendAPI
  ( -- * Creating a Request
    CreateBackendAPI (..),
    newCreateBackendAPI,

    -- * Request Lenses
    createBackendAPI_appId,
    createBackendAPI_resourceName,
    createBackendAPI_backendEnvironmentName,
    createBackendAPI_resourceConfig,

    -- * Destructuring the Response
    CreateBackendAPIResponse (..),
    newCreateBackendAPIResponse,

    -- * Response Lenses
    createBackendAPIResponse_appId,
    createBackendAPIResponse_backendEnvironmentName,
    createBackendAPIResponse_error,
    createBackendAPIResponse_jobId,
    createBackendAPIResponse_operation,
    createBackendAPIResponse_status,
    createBackendAPIResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for CreateBackendAPI.
--
-- /See:/ 'newCreateBackendAPI' smart constructor.
data CreateBackendAPI = CreateBackendAPI'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of this resource.
    resourceName :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The resource configuration for this request.
    resourceConfig :: BackendAPIResourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAPI' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createBackendAPI_appId' - The app ID.
--
-- 'resourceName', 'createBackendAPI_resourceName' - The name of this resource.
--
-- 'backendEnvironmentName', 'createBackendAPI_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceConfig', 'createBackendAPI_resourceConfig' - The resource configuration for this request.
newCreateBackendAPI ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceConfig'
  BackendAPIResourceConfig ->
  CreateBackendAPI
newCreateBackendAPI
  pAppId_
  pResourceName_
  pBackendEnvironmentName_
  pResourceConfig_ =
    CreateBackendAPI'
      { appId = pAppId_,
        resourceName = pResourceName_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceConfig = pResourceConfig_
      }

-- | The app ID.
createBackendAPI_appId :: Lens.Lens' CreateBackendAPI Prelude.Text
createBackendAPI_appId = Lens.lens (\CreateBackendAPI' {appId} -> appId) (\s@CreateBackendAPI' {} a -> s {appId = a} :: CreateBackendAPI)

-- | The name of this resource.
createBackendAPI_resourceName :: Lens.Lens' CreateBackendAPI Prelude.Text
createBackendAPI_resourceName = Lens.lens (\CreateBackendAPI' {resourceName} -> resourceName) (\s@CreateBackendAPI' {} a -> s {resourceName = a} :: CreateBackendAPI)

-- | The name of the backend environment.
createBackendAPI_backendEnvironmentName :: Lens.Lens' CreateBackendAPI Prelude.Text
createBackendAPI_backendEnvironmentName = Lens.lens (\CreateBackendAPI' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendAPI' {} a -> s {backendEnvironmentName = a} :: CreateBackendAPI)

-- | The resource configuration for this request.
createBackendAPI_resourceConfig :: Lens.Lens' CreateBackendAPI BackendAPIResourceConfig
createBackendAPI_resourceConfig = Lens.lens (\CreateBackendAPI' {resourceConfig} -> resourceConfig) (\s@CreateBackendAPI' {} a -> s {resourceConfig = a} :: CreateBackendAPI)

instance Core.AWSRequest CreateBackendAPI where
  type
    AWSResponse CreateBackendAPI =
      CreateBackendAPIResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackendAPIResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackendAPI where
  hashWithSalt _salt CreateBackendAPI' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceConfig

instance Prelude.NFData CreateBackendAPI where
  rnf CreateBackendAPI' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceConfig

instance Data.ToHeaders CreateBackendAPI where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackendAPI where
  toJSON CreateBackendAPI' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceName" Data..= resourceName),
            Prelude.Just
              ( "backendEnvironmentName"
                  Data..= backendEnvironmentName
              ),
            Prelude.Just
              ("resourceConfig" Data..= resourceConfig)
          ]
      )

instance Data.ToPath CreateBackendAPI where
  toPath CreateBackendAPI' {..} =
    Prelude.mconcat
      ["/backend/", Data.toBS appId, "/api"]

instance Data.ToQuery CreateBackendAPI where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackendAPIResponse' smart constructor.
data CreateBackendAPIResponse = CreateBackendAPIResponse'
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
-- Create a value of 'CreateBackendAPIResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createBackendAPIResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'createBackendAPIResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'error', 'createBackendAPIResponse_error' - If the request fails, this error is returned.
--
-- 'jobId', 'createBackendAPIResponse_jobId' - The ID for the job.
--
-- 'operation', 'createBackendAPIResponse_operation' - The name of the operation.
--
-- 'status', 'createBackendAPIResponse_status' - The current status of the request.
--
-- 'httpStatus', 'createBackendAPIResponse_httpStatus' - The response's http status code.
newCreateBackendAPIResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackendAPIResponse
newCreateBackendAPIResponse pHttpStatus_ =
  CreateBackendAPIResponse'
    { appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      error = Prelude.Nothing,
      jobId = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
createBackendAPIResponse_appId :: Lens.Lens' CreateBackendAPIResponse (Prelude.Maybe Prelude.Text)
createBackendAPIResponse_appId = Lens.lens (\CreateBackendAPIResponse' {appId} -> appId) (\s@CreateBackendAPIResponse' {} a -> s {appId = a} :: CreateBackendAPIResponse)

-- | The name of the backend environment.
createBackendAPIResponse_backendEnvironmentName :: Lens.Lens' CreateBackendAPIResponse (Prelude.Maybe Prelude.Text)
createBackendAPIResponse_backendEnvironmentName = Lens.lens (\CreateBackendAPIResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendAPIResponse' {} a -> s {backendEnvironmentName = a} :: CreateBackendAPIResponse)

-- | If the request fails, this error is returned.
createBackendAPIResponse_error :: Lens.Lens' CreateBackendAPIResponse (Prelude.Maybe Prelude.Text)
createBackendAPIResponse_error = Lens.lens (\CreateBackendAPIResponse' {error} -> error) (\s@CreateBackendAPIResponse' {} a -> s {error = a} :: CreateBackendAPIResponse)

-- | The ID for the job.
createBackendAPIResponse_jobId :: Lens.Lens' CreateBackendAPIResponse (Prelude.Maybe Prelude.Text)
createBackendAPIResponse_jobId = Lens.lens (\CreateBackendAPIResponse' {jobId} -> jobId) (\s@CreateBackendAPIResponse' {} a -> s {jobId = a} :: CreateBackendAPIResponse)

-- | The name of the operation.
createBackendAPIResponse_operation :: Lens.Lens' CreateBackendAPIResponse (Prelude.Maybe Prelude.Text)
createBackendAPIResponse_operation = Lens.lens (\CreateBackendAPIResponse' {operation} -> operation) (\s@CreateBackendAPIResponse' {} a -> s {operation = a} :: CreateBackendAPIResponse)

-- | The current status of the request.
createBackendAPIResponse_status :: Lens.Lens' CreateBackendAPIResponse (Prelude.Maybe Prelude.Text)
createBackendAPIResponse_status = Lens.lens (\CreateBackendAPIResponse' {status} -> status) (\s@CreateBackendAPIResponse' {} a -> s {status = a} :: CreateBackendAPIResponse)

-- | The response's http status code.
createBackendAPIResponse_httpStatus :: Lens.Lens' CreateBackendAPIResponse Prelude.Int
createBackendAPIResponse_httpStatus = Lens.lens (\CreateBackendAPIResponse' {httpStatus} -> httpStatus) (\s@CreateBackendAPIResponse' {} a -> s {httpStatus = a} :: CreateBackendAPIResponse)

instance Prelude.NFData CreateBackendAPIResponse where
  rnf CreateBackendAPIResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
