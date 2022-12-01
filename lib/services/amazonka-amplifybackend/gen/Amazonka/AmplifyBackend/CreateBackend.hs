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
-- Module      : Amazonka.AmplifyBackend.CreateBackend
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a backend for an Amplify app. Backends are
-- automatically created at the time of app creation.
module Amazonka.AmplifyBackend.CreateBackend
  ( -- * Creating a Request
    CreateBackend (..),
    newCreateBackend,

    -- * Request Lenses
    createBackend_resourceName,
    createBackend_resourceConfig,
    createBackend_appId,
    createBackend_backendEnvironmentName,
    createBackend_appName,

    -- * Destructuring the Response
    CreateBackendResponse (..),
    newCreateBackendResponse,

    -- * Response Lenses
    createBackendResponse_jobId,
    createBackendResponse_status,
    createBackendResponse_error,
    createBackendResponse_operation,
    createBackendResponse_appId,
    createBackendResponse_backendEnvironmentName,
    createBackendResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for CreateBackend.
--
-- /See:/ 'newCreateBackend' smart constructor.
data CreateBackend = CreateBackend'
  { -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The resource configuration for creating a backend.
    resourceConfig :: Prelude.Maybe ResourceConfig,
    -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of the app.
    appName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'createBackend_resourceName' - The name of the resource.
--
-- 'resourceConfig', 'createBackend_resourceConfig' - The resource configuration for creating a backend.
--
-- 'appId', 'createBackend_appId' - The app ID.
--
-- 'backendEnvironmentName', 'createBackend_backendEnvironmentName' - The name of the backend environment.
--
-- 'appName', 'createBackend_appName' - The name of the app.
newCreateBackend ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'appName'
  Prelude.Text ->
  CreateBackend
newCreateBackend
  pAppId_
  pBackendEnvironmentName_
  pAppName_ =
    CreateBackend'
      { resourceName = Prelude.Nothing,
        resourceConfig = Prelude.Nothing,
        appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        appName = pAppName_
      }

-- | The name of the resource.
createBackend_resourceName :: Lens.Lens' CreateBackend (Prelude.Maybe Prelude.Text)
createBackend_resourceName = Lens.lens (\CreateBackend' {resourceName} -> resourceName) (\s@CreateBackend' {} a -> s {resourceName = a} :: CreateBackend)

-- | The resource configuration for creating a backend.
createBackend_resourceConfig :: Lens.Lens' CreateBackend (Prelude.Maybe ResourceConfig)
createBackend_resourceConfig = Lens.lens (\CreateBackend' {resourceConfig} -> resourceConfig) (\s@CreateBackend' {} a -> s {resourceConfig = a} :: CreateBackend)

-- | The app ID.
createBackend_appId :: Lens.Lens' CreateBackend Prelude.Text
createBackend_appId = Lens.lens (\CreateBackend' {appId} -> appId) (\s@CreateBackend' {} a -> s {appId = a} :: CreateBackend)

-- | The name of the backend environment.
createBackend_backendEnvironmentName :: Lens.Lens' CreateBackend Prelude.Text
createBackend_backendEnvironmentName = Lens.lens (\CreateBackend' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackend' {} a -> s {backendEnvironmentName = a} :: CreateBackend)

-- | The name of the app.
createBackend_appName :: Lens.Lens' CreateBackend Prelude.Text
createBackend_appName = Lens.lens (\CreateBackend' {appName} -> appName) (\s@CreateBackend' {} a -> s {appName = a} :: CreateBackend)

instance Core.AWSRequest CreateBackend where
  type
    AWSResponse CreateBackend =
      CreateBackendResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackendResponse'
            Prelude.<$> (x Core..?> "jobId")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "error")
            Prelude.<*> (x Core..?> "operation")
            Prelude.<*> (x Core..?> "appId")
            Prelude.<*> (x Core..?> "backendEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackend where
  hashWithSalt _salt CreateBackend' {..} =
    _salt `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` appName

instance Prelude.NFData CreateBackend where
  rnf CreateBackend' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf appName

instance Core.ToHeaders CreateBackend where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBackend where
  toJSON CreateBackend' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resourceName" Core..=) Prelude.<$> resourceName,
            ("resourceConfig" Core..=)
              Prelude.<$> resourceConfig,
            Prelude.Just ("appId" Core..= appId),
            Prelude.Just
              ( "backendEnvironmentName"
                  Core..= backendEnvironmentName
              ),
            Prelude.Just ("appName" Core..= appName)
          ]
      )

instance Core.ToPath CreateBackend where
  toPath = Prelude.const "/backend"

instance Core.ToQuery CreateBackend where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackendResponse' smart constructor.
data CreateBackendResponse = CreateBackendResponse'
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
-- Create a value of 'CreateBackendResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'createBackendResponse_jobId' - The ID for the job.
--
-- 'status', 'createBackendResponse_status' - The current status of the request.
--
-- 'error', 'createBackendResponse_error' - If the request fails, this error is returned.
--
-- 'operation', 'createBackendResponse_operation' - The name of the operation.
--
-- 'appId', 'createBackendResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'createBackendResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'httpStatus', 'createBackendResponse_httpStatus' - The response's http status code.
newCreateBackendResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackendResponse
newCreateBackendResponse pHttpStatus_ =
  CreateBackendResponse'
    { jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      error = Prelude.Nothing,
      operation = Prelude.Nothing,
      appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the job.
createBackendResponse_jobId :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_jobId = Lens.lens (\CreateBackendResponse' {jobId} -> jobId) (\s@CreateBackendResponse' {} a -> s {jobId = a} :: CreateBackendResponse)

-- | The current status of the request.
createBackendResponse_status :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_status = Lens.lens (\CreateBackendResponse' {status} -> status) (\s@CreateBackendResponse' {} a -> s {status = a} :: CreateBackendResponse)

-- | If the request fails, this error is returned.
createBackendResponse_error :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_error = Lens.lens (\CreateBackendResponse' {error} -> error) (\s@CreateBackendResponse' {} a -> s {error = a} :: CreateBackendResponse)

-- | The name of the operation.
createBackendResponse_operation :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_operation = Lens.lens (\CreateBackendResponse' {operation} -> operation) (\s@CreateBackendResponse' {} a -> s {operation = a} :: CreateBackendResponse)

-- | The app ID.
createBackendResponse_appId :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_appId = Lens.lens (\CreateBackendResponse' {appId} -> appId) (\s@CreateBackendResponse' {} a -> s {appId = a} :: CreateBackendResponse)

-- | The name of the backend environment.
createBackendResponse_backendEnvironmentName :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_backendEnvironmentName = Lens.lens (\CreateBackendResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendResponse' {} a -> s {backendEnvironmentName = a} :: CreateBackendResponse)

-- | The response's http status code.
createBackendResponse_httpStatus :: Lens.Lens' CreateBackendResponse Prelude.Int
createBackendResponse_httpStatus = Lens.lens (\CreateBackendResponse' {httpStatus} -> httpStatus) (\s@CreateBackendResponse' {} a -> s {httpStatus = a} :: CreateBackendResponse)

instance Prelude.NFData CreateBackendResponse where
  rnf CreateBackendResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf httpStatus
