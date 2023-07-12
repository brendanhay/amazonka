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
-- Module      : Amazonka.AmplifyBackend.GenerateBackendAPIModels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a model schema for an existing backend API resource.
module Amazonka.AmplifyBackend.GenerateBackendAPIModels
  ( -- * Creating a Request
    GenerateBackendAPIModels (..),
    newGenerateBackendAPIModels,

    -- * Request Lenses
    generateBackendAPIModels_appId,
    generateBackendAPIModels_backendEnvironmentName,
    generateBackendAPIModels_resourceName,

    -- * Destructuring the Response
    GenerateBackendAPIModelsResponse (..),
    newGenerateBackendAPIModelsResponse,

    -- * Response Lenses
    generateBackendAPIModelsResponse_appId,
    generateBackendAPIModelsResponse_backendEnvironmentName,
    generateBackendAPIModelsResponse_error,
    generateBackendAPIModelsResponse_jobId,
    generateBackendAPIModelsResponse_operation,
    generateBackendAPIModelsResponse_status,
    generateBackendAPIModelsResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for GenerateBackendAPIModels.
--
-- /See:/ 'newGenerateBackendAPIModels' smart constructor.
data GenerateBackendAPIModels = GenerateBackendAPIModels'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of this resource.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateBackendAPIModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'generateBackendAPIModels_appId' - The app ID.
--
-- 'backendEnvironmentName', 'generateBackendAPIModels_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'generateBackendAPIModels_resourceName' - The name of this resource.
newGenerateBackendAPIModels ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  GenerateBackendAPIModels
newGenerateBackendAPIModels
  pAppId_
  pBackendEnvironmentName_
  pResourceName_ =
    GenerateBackendAPIModels'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_
      }

-- | The app ID.
generateBackendAPIModels_appId :: Lens.Lens' GenerateBackendAPIModels Prelude.Text
generateBackendAPIModels_appId = Lens.lens (\GenerateBackendAPIModels' {appId} -> appId) (\s@GenerateBackendAPIModels' {} a -> s {appId = a} :: GenerateBackendAPIModels)

-- | The name of the backend environment.
generateBackendAPIModels_backendEnvironmentName :: Lens.Lens' GenerateBackendAPIModels Prelude.Text
generateBackendAPIModels_backendEnvironmentName = Lens.lens (\GenerateBackendAPIModels' {backendEnvironmentName} -> backendEnvironmentName) (\s@GenerateBackendAPIModels' {} a -> s {backendEnvironmentName = a} :: GenerateBackendAPIModels)

-- | The name of this resource.
generateBackendAPIModels_resourceName :: Lens.Lens' GenerateBackendAPIModels Prelude.Text
generateBackendAPIModels_resourceName = Lens.lens (\GenerateBackendAPIModels' {resourceName} -> resourceName) (\s@GenerateBackendAPIModels' {} a -> s {resourceName = a} :: GenerateBackendAPIModels)

instance Core.AWSRequest GenerateBackendAPIModels where
  type
    AWSResponse GenerateBackendAPIModels =
      GenerateBackendAPIModelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateBackendAPIModelsResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateBackendAPIModels where
  hashWithSalt _salt GenerateBackendAPIModels' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData GenerateBackendAPIModels where
  rnf GenerateBackendAPIModels' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceName

instance Data.ToHeaders GenerateBackendAPIModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateBackendAPIModels where
  toJSON GenerateBackendAPIModels' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceName" Data..= resourceName)]
      )

instance Data.ToPath GenerateBackendAPIModels where
  toPath GenerateBackendAPIModels' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/api/",
        Data.toBS backendEnvironmentName,
        "/generateModels"
      ]

instance Data.ToQuery GenerateBackendAPIModels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateBackendAPIModelsResponse' smart constructor.
data GenerateBackendAPIModelsResponse = GenerateBackendAPIModelsResponse'
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
-- Create a value of 'GenerateBackendAPIModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'generateBackendAPIModelsResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'generateBackendAPIModelsResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'error', 'generateBackendAPIModelsResponse_error' - If the request fails, this error is returned.
--
-- 'jobId', 'generateBackendAPIModelsResponse_jobId' - The ID for the job.
--
-- 'operation', 'generateBackendAPIModelsResponse_operation' - The name of the operation.
--
-- 'status', 'generateBackendAPIModelsResponse_status' - The current status of the request.
--
-- 'httpStatus', 'generateBackendAPIModelsResponse_httpStatus' - The response's http status code.
newGenerateBackendAPIModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateBackendAPIModelsResponse
newGenerateBackendAPIModelsResponse pHttpStatus_ =
  GenerateBackendAPIModelsResponse'
    { appId =
        Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      error = Prelude.Nothing,
      jobId = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
generateBackendAPIModelsResponse_appId :: Lens.Lens' GenerateBackendAPIModelsResponse (Prelude.Maybe Prelude.Text)
generateBackendAPIModelsResponse_appId = Lens.lens (\GenerateBackendAPIModelsResponse' {appId} -> appId) (\s@GenerateBackendAPIModelsResponse' {} a -> s {appId = a} :: GenerateBackendAPIModelsResponse)

-- | The name of the backend environment.
generateBackendAPIModelsResponse_backendEnvironmentName :: Lens.Lens' GenerateBackendAPIModelsResponse (Prelude.Maybe Prelude.Text)
generateBackendAPIModelsResponse_backendEnvironmentName = Lens.lens (\GenerateBackendAPIModelsResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@GenerateBackendAPIModelsResponse' {} a -> s {backendEnvironmentName = a} :: GenerateBackendAPIModelsResponse)

-- | If the request fails, this error is returned.
generateBackendAPIModelsResponse_error :: Lens.Lens' GenerateBackendAPIModelsResponse (Prelude.Maybe Prelude.Text)
generateBackendAPIModelsResponse_error = Lens.lens (\GenerateBackendAPIModelsResponse' {error} -> error) (\s@GenerateBackendAPIModelsResponse' {} a -> s {error = a} :: GenerateBackendAPIModelsResponse)

-- | The ID for the job.
generateBackendAPIModelsResponse_jobId :: Lens.Lens' GenerateBackendAPIModelsResponse (Prelude.Maybe Prelude.Text)
generateBackendAPIModelsResponse_jobId = Lens.lens (\GenerateBackendAPIModelsResponse' {jobId} -> jobId) (\s@GenerateBackendAPIModelsResponse' {} a -> s {jobId = a} :: GenerateBackendAPIModelsResponse)

-- | The name of the operation.
generateBackendAPIModelsResponse_operation :: Lens.Lens' GenerateBackendAPIModelsResponse (Prelude.Maybe Prelude.Text)
generateBackendAPIModelsResponse_operation = Lens.lens (\GenerateBackendAPIModelsResponse' {operation} -> operation) (\s@GenerateBackendAPIModelsResponse' {} a -> s {operation = a} :: GenerateBackendAPIModelsResponse)

-- | The current status of the request.
generateBackendAPIModelsResponse_status :: Lens.Lens' GenerateBackendAPIModelsResponse (Prelude.Maybe Prelude.Text)
generateBackendAPIModelsResponse_status = Lens.lens (\GenerateBackendAPIModelsResponse' {status} -> status) (\s@GenerateBackendAPIModelsResponse' {} a -> s {status = a} :: GenerateBackendAPIModelsResponse)

-- | The response's http status code.
generateBackendAPIModelsResponse_httpStatus :: Lens.Lens' GenerateBackendAPIModelsResponse Prelude.Int
generateBackendAPIModelsResponse_httpStatus = Lens.lens (\GenerateBackendAPIModelsResponse' {httpStatus} -> httpStatus) (\s@GenerateBackendAPIModelsResponse' {} a -> s {httpStatus = a} :: GenerateBackendAPIModelsResponse)

instance
  Prelude.NFData
    GenerateBackendAPIModelsResponse
  where
  rnf GenerateBackendAPIModelsResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
