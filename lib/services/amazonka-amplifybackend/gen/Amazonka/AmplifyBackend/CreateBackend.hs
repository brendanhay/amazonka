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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createBackend_resourceConfig,
    createBackend_resourceName,
    createBackend_appId,
    createBackend_backendEnvironmentName,
    createBackend_appName,

    -- * Destructuring the Response
    CreateBackendResponse (..),
    newCreateBackendResponse,

    -- * Response Lenses
    createBackendResponse_appId,
    createBackendResponse_backendEnvironmentName,
    createBackendResponse_error,
    createBackendResponse_jobId,
    createBackendResponse_operation,
    createBackendResponse_status,
    createBackendResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for CreateBackend.
--
-- /See:/ 'newCreateBackend' smart constructor.
data CreateBackend = CreateBackend'
  { -- | The resource configuration for creating a backend.
    resourceConfig :: Prelude.Maybe ResourceConfig,
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
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
-- 'resourceConfig', 'createBackend_resourceConfig' - The resource configuration for creating a backend.
--
-- 'resourceName', 'createBackend_resourceName' - The name of the resource.
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
      { resourceConfig = Prelude.Nothing,
        resourceName = Prelude.Nothing,
        appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        appName = pAppName_
      }

-- | The resource configuration for creating a backend.
createBackend_resourceConfig :: Lens.Lens' CreateBackend (Prelude.Maybe ResourceConfig)
createBackend_resourceConfig = Lens.lens (\CreateBackend' {resourceConfig} -> resourceConfig) (\s@CreateBackend' {} a -> s {resourceConfig = a} :: CreateBackend)

-- | The name of the resource.
createBackend_resourceName :: Lens.Lens' CreateBackend (Prelude.Maybe Prelude.Text)
createBackend_resourceName = Lens.lens (\CreateBackend' {resourceName} -> resourceName) (\s@CreateBackend' {} a -> s {resourceName = a} :: CreateBackend)

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
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackend where
  hashWithSalt _salt CreateBackend' {..} =
    _salt
      `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` appName

instance Prelude.NFData CreateBackend where
  rnf CreateBackend' {..} =
    Prelude.rnf resourceConfig `Prelude.seq`
      Prelude.rnf resourceName `Prelude.seq`
        Prelude.rnf appId `Prelude.seq`
          Prelude.rnf backendEnvironmentName `Prelude.seq`
            Prelude.rnf appName

instance Data.ToHeaders CreateBackend where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackend where
  toJSON CreateBackend' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceConfig" Data..=)
              Prelude.<$> resourceConfig,
            ("resourceName" Data..=) Prelude.<$> resourceName,
            Prelude.Just ("appId" Data..= appId),
            Prelude.Just
              ( "backendEnvironmentName"
                  Data..= backendEnvironmentName
              ),
            Prelude.Just ("appName" Data..= appName)
          ]
      )

instance Data.ToPath CreateBackend where
  toPath = Prelude.const "/backend"

instance Data.ToQuery CreateBackend where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackendResponse' smart constructor.
data CreateBackendResponse = CreateBackendResponse'
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
-- Create a value of 'CreateBackendResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createBackendResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'createBackendResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'error', 'createBackendResponse_error' - If the request fails, this error is returned.
--
-- 'jobId', 'createBackendResponse_jobId' - The ID for the job.
--
-- 'operation', 'createBackendResponse_operation' - The name of the operation.
--
-- 'status', 'createBackendResponse_status' - The current status of the request.
--
-- 'httpStatus', 'createBackendResponse_httpStatus' - The response's http status code.
newCreateBackendResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackendResponse
newCreateBackendResponse pHttpStatus_ =
  CreateBackendResponse'
    { appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      error = Prelude.Nothing,
      jobId = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
createBackendResponse_appId :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_appId = Lens.lens (\CreateBackendResponse' {appId} -> appId) (\s@CreateBackendResponse' {} a -> s {appId = a} :: CreateBackendResponse)

-- | The name of the backend environment.
createBackendResponse_backendEnvironmentName :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_backendEnvironmentName = Lens.lens (\CreateBackendResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendResponse' {} a -> s {backendEnvironmentName = a} :: CreateBackendResponse)

-- | If the request fails, this error is returned.
createBackendResponse_error :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_error = Lens.lens (\CreateBackendResponse' {error} -> error) (\s@CreateBackendResponse' {} a -> s {error = a} :: CreateBackendResponse)

-- | The ID for the job.
createBackendResponse_jobId :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_jobId = Lens.lens (\CreateBackendResponse' {jobId} -> jobId) (\s@CreateBackendResponse' {} a -> s {jobId = a} :: CreateBackendResponse)

-- | The name of the operation.
createBackendResponse_operation :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_operation = Lens.lens (\CreateBackendResponse' {operation} -> operation) (\s@CreateBackendResponse' {} a -> s {operation = a} :: CreateBackendResponse)

-- | The current status of the request.
createBackendResponse_status :: Lens.Lens' CreateBackendResponse (Prelude.Maybe Prelude.Text)
createBackendResponse_status = Lens.lens (\CreateBackendResponse' {status} -> status) (\s@CreateBackendResponse' {} a -> s {status = a} :: CreateBackendResponse)

-- | The response's http status code.
createBackendResponse_httpStatus :: Lens.Lens' CreateBackendResponse Prelude.Int
createBackendResponse_httpStatus = Lens.lens (\CreateBackendResponse' {httpStatus} -> httpStatus) (\s@CreateBackendResponse' {} a -> s {httpStatus = a} :: CreateBackendResponse)

instance Prelude.NFData CreateBackendResponse where
  rnf CreateBackendResponse' {..} =
    Prelude.rnf appId `Prelude.seq`
      Prelude.rnf backendEnvironmentName `Prelude.seq`
        Prelude.rnf error `Prelude.seq`
          Prelude.rnf jobId `Prelude.seq`
            Prelude.rnf operation `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf httpStatus
