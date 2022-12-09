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
-- Module      : Amazonka.AmplifyBackend.CreateBackendAuth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new backend authentication resource.
module Amazonka.AmplifyBackend.CreateBackendAuth
  ( -- * Creating a Request
    CreateBackendAuth (..),
    newCreateBackendAuth,

    -- * Request Lenses
    createBackendAuth_appId,
    createBackendAuth_resourceName,
    createBackendAuth_backendEnvironmentName,
    createBackendAuth_resourceConfig,

    -- * Destructuring the Response
    CreateBackendAuthResponse (..),
    newCreateBackendAuthResponse,

    -- * Response Lenses
    createBackendAuthResponse_appId,
    createBackendAuthResponse_backendEnvironmentName,
    createBackendAuthResponse_error,
    createBackendAuthResponse_jobId,
    createBackendAuthResponse_operation,
    createBackendAuthResponse_status,
    createBackendAuthResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for CreateBackendAuth.
--
-- /See:/ 'newCreateBackendAuth' smart constructor.
data CreateBackendAuth = CreateBackendAuth'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of this resource.
    resourceName :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The resource configuration for this request object.
    resourceConfig :: CreateBackendAuthResourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createBackendAuth_appId' - The app ID.
--
-- 'resourceName', 'createBackendAuth_resourceName' - The name of this resource.
--
-- 'backendEnvironmentName', 'createBackendAuth_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceConfig', 'createBackendAuth_resourceConfig' - The resource configuration for this request object.
newCreateBackendAuth ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceConfig'
  CreateBackendAuthResourceConfig ->
  CreateBackendAuth
newCreateBackendAuth
  pAppId_
  pResourceName_
  pBackendEnvironmentName_
  pResourceConfig_ =
    CreateBackendAuth'
      { appId = pAppId_,
        resourceName = pResourceName_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceConfig = pResourceConfig_
      }

-- | The app ID.
createBackendAuth_appId :: Lens.Lens' CreateBackendAuth Prelude.Text
createBackendAuth_appId = Lens.lens (\CreateBackendAuth' {appId} -> appId) (\s@CreateBackendAuth' {} a -> s {appId = a} :: CreateBackendAuth)

-- | The name of this resource.
createBackendAuth_resourceName :: Lens.Lens' CreateBackendAuth Prelude.Text
createBackendAuth_resourceName = Lens.lens (\CreateBackendAuth' {resourceName} -> resourceName) (\s@CreateBackendAuth' {} a -> s {resourceName = a} :: CreateBackendAuth)

-- | The name of the backend environment.
createBackendAuth_backendEnvironmentName :: Lens.Lens' CreateBackendAuth Prelude.Text
createBackendAuth_backendEnvironmentName = Lens.lens (\CreateBackendAuth' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendAuth' {} a -> s {backendEnvironmentName = a} :: CreateBackendAuth)

-- | The resource configuration for this request object.
createBackendAuth_resourceConfig :: Lens.Lens' CreateBackendAuth CreateBackendAuthResourceConfig
createBackendAuth_resourceConfig = Lens.lens (\CreateBackendAuth' {resourceConfig} -> resourceConfig) (\s@CreateBackendAuth' {} a -> s {resourceConfig = a} :: CreateBackendAuth)

instance Core.AWSRequest CreateBackendAuth where
  type
    AWSResponse CreateBackendAuth =
      CreateBackendAuthResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackendAuthResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackendAuth where
  hashWithSalt _salt CreateBackendAuth' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceConfig

instance Prelude.NFData CreateBackendAuth where
  rnf CreateBackendAuth' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceConfig

instance Data.ToHeaders CreateBackendAuth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackendAuth where
  toJSON CreateBackendAuth' {..} =
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

instance Data.ToPath CreateBackendAuth where
  toPath CreateBackendAuth' {..} =
    Prelude.mconcat
      ["/backend/", Data.toBS appId, "/auth"]

instance Data.ToQuery CreateBackendAuth where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackendAuthResponse' smart constructor.
data CreateBackendAuthResponse = CreateBackendAuthResponse'
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
-- Create a value of 'CreateBackendAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createBackendAuthResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'createBackendAuthResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'error', 'createBackendAuthResponse_error' - If the request fails, this error is returned.
--
-- 'jobId', 'createBackendAuthResponse_jobId' - The ID for the job.
--
-- 'operation', 'createBackendAuthResponse_operation' - The name of the operation.
--
-- 'status', 'createBackendAuthResponse_status' - The current status of the request.
--
-- 'httpStatus', 'createBackendAuthResponse_httpStatus' - The response's http status code.
newCreateBackendAuthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackendAuthResponse
newCreateBackendAuthResponse pHttpStatus_ =
  CreateBackendAuthResponse'
    { appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      error = Prelude.Nothing,
      jobId = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
createBackendAuthResponse_appId :: Lens.Lens' CreateBackendAuthResponse (Prelude.Maybe Prelude.Text)
createBackendAuthResponse_appId = Lens.lens (\CreateBackendAuthResponse' {appId} -> appId) (\s@CreateBackendAuthResponse' {} a -> s {appId = a} :: CreateBackendAuthResponse)

-- | The name of the backend environment.
createBackendAuthResponse_backendEnvironmentName :: Lens.Lens' CreateBackendAuthResponse (Prelude.Maybe Prelude.Text)
createBackendAuthResponse_backendEnvironmentName = Lens.lens (\CreateBackendAuthResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendAuthResponse' {} a -> s {backendEnvironmentName = a} :: CreateBackendAuthResponse)

-- | If the request fails, this error is returned.
createBackendAuthResponse_error :: Lens.Lens' CreateBackendAuthResponse (Prelude.Maybe Prelude.Text)
createBackendAuthResponse_error = Lens.lens (\CreateBackendAuthResponse' {error} -> error) (\s@CreateBackendAuthResponse' {} a -> s {error = a} :: CreateBackendAuthResponse)

-- | The ID for the job.
createBackendAuthResponse_jobId :: Lens.Lens' CreateBackendAuthResponse (Prelude.Maybe Prelude.Text)
createBackendAuthResponse_jobId = Lens.lens (\CreateBackendAuthResponse' {jobId} -> jobId) (\s@CreateBackendAuthResponse' {} a -> s {jobId = a} :: CreateBackendAuthResponse)

-- | The name of the operation.
createBackendAuthResponse_operation :: Lens.Lens' CreateBackendAuthResponse (Prelude.Maybe Prelude.Text)
createBackendAuthResponse_operation = Lens.lens (\CreateBackendAuthResponse' {operation} -> operation) (\s@CreateBackendAuthResponse' {} a -> s {operation = a} :: CreateBackendAuthResponse)

-- | The current status of the request.
createBackendAuthResponse_status :: Lens.Lens' CreateBackendAuthResponse (Prelude.Maybe Prelude.Text)
createBackendAuthResponse_status = Lens.lens (\CreateBackendAuthResponse' {status} -> status) (\s@CreateBackendAuthResponse' {} a -> s {status = a} :: CreateBackendAuthResponse)

-- | The response's http status code.
createBackendAuthResponse_httpStatus :: Lens.Lens' CreateBackendAuthResponse Prelude.Int
createBackendAuthResponse_httpStatus = Lens.lens (\CreateBackendAuthResponse' {httpStatus} -> httpStatus) (\s@CreateBackendAuthResponse' {} a -> s {httpStatus = a} :: CreateBackendAuthResponse)

instance Prelude.NFData CreateBackendAuthResponse where
  rnf CreateBackendAuthResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
