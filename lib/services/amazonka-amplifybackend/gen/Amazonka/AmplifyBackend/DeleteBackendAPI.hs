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
-- Module      : Amazonka.AmplifyBackend.DeleteBackendAPI
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing backend API resource.
module Amazonka.AmplifyBackend.DeleteBackendAPI
  ( -- * Creating a Request
    DeleteBackendAPI (..),
    newDeleteBackendAPI,

    -- * Request Lenses
    deleteBackendAPI_resourceConfig,
    deleteBackendAPI_appId,
    deleteBackendAPI_backendEnvironmentName,
    deleteBackendAPI_resourceName,

    -- * Destructuring the Response
    DeleteBackendAPIResponse (..),
    newDeleteBackendAPIResponse,

    -- * Response Lenses
    deleteBackendAPIResponse_appId,
    deleteBackendAPIResponse_backendEnvironmentName,
    deleteBackendAPIResponse_error,
    deleteBackendAPIResponse_jobId,
    deleteBackendAPIResponse_operation,
    deleteBackendAPIResponse_status,
    deleteBackendAPIResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for DeleteBackendAPI.
--
-- /See:/ 'newDeleteBackendAPI' smart constructor.
data DeleteBackendAPI = DeleteBackendAPI'
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
-- Create a value of 'DeleteBackendAPI' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceConfig', 'deleteBackendAPI_resourceConfig' - Defines the resource configuration for the data model in your Amplify
-- project.
--
-- 'appId', 'deleteBackendAPI_appId' - The app ID.
--
-- 'backendEnvironmentName', 'deleteBackendAPI_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'deleteBackendAPI_resourceName' - The name of this resource.
newDeleteBackendAPI ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  DeleteBackendAPI
newDeleteBackendAPI
  pAppId_
  pBackendEnvironmentName_
  pResourceName_ =
    DeleteBackendAPI'
      { resourceConfig = Prelude.Nothing,
        appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_
      }

-- | Defines the resource configuration for the data model in your Amplify
-- project.
deleteBackendAPI_resourceConfig :: Lens.Lens' DeleteBackendAPI (Prelude.Maybe BackendAPIResourceConfig)
deleteBackendAPI_resourceConfig = Lens.lens (\DeleteBackendAPI' {resourceConfig} -> resourceConfig) (\s@DeleteBackendAPI' {} a -> s {resourceConfig = a} :: DeleteBackendAPI)

-- | The app ID.
deleteBackendAPI_appId :: Lens.Lens' DeleteBackendAPI Prelude.Text
deleteBackendAPI_appId = Lens.lens (\DeleteBackendAPI' {appId} -> appId) (\s@DeleteBackendAPI' {} a -> s {appId = a} :: DeleteBackendAPI)

-- | The name of the backend environment.
deleteBackendAPI_backendEnvironmentName :: Lens.Lens' DeleteBackendAPI Prelude.Text
deleteBackendAPI_backendEnvironmentName = Lens.lens (\DeleteBackendAPI' {backendEnvironmentName} -> backendEnvironmentName) (\s@DeleteBackendAPI' {} a -> s {backendEnvironmentName = a} :: DeleteBackendAPI)

-- | The name of this resource.
deleteBackendAPI_resourceName :: Lens.Lens' DeleteBackendAPI Prelude.Text
deleteBackendAPI_resourceName = Lens.lens (\DeleteBackendAPI' {resourceName} -> resourceName) (\s@DeleteBackendAPI' {} a -> s {resourceName = a} :: DeleteBackendAPI)

instance Core.AWSRequest DeleteBackendAPI where
  type
    AWSResponse DeleteBackendAPI =
      DeleteBackendAPIResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBackendAPIResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBackendAPI where
  hashWithSalt _salt DeleteBackendAPI' {..} =
    _salt `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData DeleteBackendAPI where
  rnf DeleteBackendAPI' {..} =
    Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceName

instance Data.ToHeaders DeleteBackendAPI where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBackendAPI where
  toJSON DeleteBackendAPI' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceConfig" Data..=)
              Prelude.<$> resourceConfig,
            Prelude.Just ("resourceName" Data..= resourceName)
          ]
      )

instance Data.ToPath DeleteBackendAPI where
  toPath DeleteBackendAPI' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/api/",
        Data.toBS backendEnvironmentName,
        "/remove"
      ]

instance Data.ToQuery DeleteBackendAPI where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackendAPIResponse' smart constructor.
data DeleteBackendAPIResponse = DeleteBackendAPIResponse'
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
-- Create a value of 'DeleteBackendAPIResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteBackendAPIResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'deleteBackendAPIResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'error', 'deleteBackendAPIResponse_error' - If the request fails, this error is returned.
--
-- 'jobId', 'deleteBackendAPIResponse_jobId' - The ID for the job.
--
-- 'operation', 'deleteBackendAPIResponse_operation' - The name of the operation.
--
-- 'status', 'deleteBackendAPIResponse_status' - The current status of the request.
--
-- 'httpStatus', 'deleteBackendAPIResponse_httpStatus' - The response's http status code.
newDeleteBackendAPIResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBackendAPIResponse
newDeleteBackendAPIResponse pHttpStatus_ =
  DeleteBackendAPIResponse'
    { appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      error = Prelude.Nothing,
      jobId = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
deleteBackendAPIResponse_appId :: Lens.Lens' DeleteBackendAPIResponse (Prelude.Maybe Prelude.Text)
deleteBackendAPIResponse_appId = Lens.lens (\DeleteBackendAPIResponse' {appId} -> appId) (\s@DeleteBackendAPIResponse' {} a -> s {appId = a} :: DeleteBackendAPIResponse)

-- | The name of the backend environment.
deleteBackendAPIResponse_backendEnvironmentName :: Lens.Lens' DeleteBackendAPIResponse (Prelude.Maybe Prelude.Text)
deleteBackendAPIResponse_backendEnvironmentName = Lens.lens (\DeleteBackendAPIResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@DeleteBackendAPIResponse' {} a -> s {backendEnvironmentName = a} :: DeleteBackendAPIResponse)

-- | If the request fails, this error is returned.
deleteBackendAPIResponse_error :: Lens.Lens' DeleteBackendAPIResponse (Prelude.Maybe Prelude.Text)
deleteBackendAPIResponse_error = Lens.lens (\DeleteBackendAPIResponse' {error} -> error) (\s@DeleteBackendAPIResponse' {} a -> s {error = a} :: DeleteBackendAPIResponse)

-- | The ID for the job.
deleteBackendAPIResponse_jobId :: Lens.Lens' DeleteBackendAPIResponse (Prelude.Maybe Prelude.Text)
deleteBackendAPIResponse_jobId = Lens.lens (\DeleteBackendAPIResponse' {jobId} -> jobId) (\s@DeleteBackendAPIResponse' {} a -> s {jobId = a} :: DeleteBackendAPIResponse)

-- | The name of the operation.
deleteBackendAPIResponse_operation :: Lens.Lens' DeleteBackendAPIResponse (Prelude.Maybe Prelude.Text)
deleteBackendAPIResponse_operation = Lens.lens (\DeleteBackendAPIResponse' {operation} -> operation) (\s@DeleteBackendAPIResponse' {} a -> s {operation = a} :: DeleteBackendAPIResponse)

-- | The current status of the request.
deleteBackendAPIResponse_status :: Lens.Lens' DeleteBackendAPIResponse (Prelude.Maybe Prelude.Text)
deleteBackendAPIResponse_status = Lens.lens (\DeleteBackendAPIResponse' {status} -> status) (\s@DeleteBackendAPIResponse' {} a -> s {status = a} :: DeleteBackendAPIResponse)

-- | The response's http status code.
deleteBackendAPIResponse_httpStatus :: Lens.Lens' DeleteBackendAPIResponse Prelude.Int
deleteBackendAPIResponse_httpStatus = Lens.lens (\DeleteBackendAPIResponse' {httpStatus} -> httpStatus) (\s@DeleteBackendAPIResponse' {} a -> s {httpStatus = a} :: DeleteBackendAPIResponse)

instance Prelude.NFData DeleteBackendAPIResponse where
  rnf DeleteBackendAPIResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
