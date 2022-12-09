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
-- Module      : Amazonka.AmplifyBackend.CreateBackendStorage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a backend storage resource.
module Amazonka.AmplifyBackend.CreateBackendStorage
  ( -- * Creating a Request
    CreateBackendStorage (..),
    newCreateBackendStorage,

    -- * Request Lenses
    createBackendStorage_appId,
    createBackendStorage_resourceName,
    createBackendStorage_backendEnvironmentName,
    createBackendStorage_resourceConfig,

    -- * Destructuring the Response
    CreateBackendStorageResponse (..),
    newCreateBackendStorageResponse,

    -- * Response Lenses
    createBackendStorageResponse_appId,
    createBackendStorageResponse_backendEnvironmentName,
    createBackendStorageResponse_jobId,
    createBackendStorageResponse_status,
    createBackendStorageResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for CreateBackendStorage.
--
-- /See:/ 'newCreateBackendStorage' smart constructor.
data CreateBackendStorage = CreateBackendStorage'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the storage resource.
    resourceName :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The resource configuration for creating backend storage.
    resourceConfig :: CreateBackendStorageResourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createBackendStorage_appId' - The app ID.
--
-- 'resourceName', 'createBackendStorage_resourceName' - The name of the storage resource.
--
-- 'backendEnvironmentName', 'createBackendStorage_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceConfig', 'createBackendStorage_resourceConfig' - The resource configuration for creating backend storage.
newCreateBackendStorage ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceConfig'
  CreateBackendStorageResourceConfig ->
  CreateBackendStorage
newCreateBackendStorage
  pAppId_
  pResourceName_
  pBackendEnvironmentName_
  pResourceConfig_ =
    CreateBackendStorage'
      { appId = pAppId_,
        resourceName = pResourceName_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceConfig = pResourceConfig_
      }

-- | The app ID.
createBackendStorage_appId :: Lens.Lens' CreateBackendStorage Prelude.Text
createBackendStorage_appId = Lens.lens (\CreateBackendStorage' {appId} -> appId) (\s@CreateBackendStorage' {} a -> s {appId = a} :: CreateBackendStorage)

-- | The name of the storage resource.
createBackendStorage_resourceName :: Lens.Lens' CreateBackendStorage Prelude.Text
createBackendStorage_resourceName = Lens.lens (\CreateBackendStorage' {resourceName} -> resourceName) (\s@CreateBackendStorage' {} a -> s {resourceName = a} :: CreateBackendStorage)

-- | The name of the backend environment.
createBackendStorage_backendEnvironmentName :: Lens.Lens' CreateBackendStorage Prelude.Text
createBackendStorage_backendEnvironmentName = Lens.lens (\CreateBackendStorage' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendStorage' {} a -> s {backendEnvironmentName = a} :: CreateBackendStorage)

-- | The resource configuration for creating backend storage.
createBackendStorage_resourceConfig :: Lens.Lens' CreateBackendStorage CreateBackendStorageResourceConfig
createBackendStorage_resourceConfig = Lens.lens (\CreateBackendStorage' {resourceConfig} -> resourceConfig) (\s@CreateBackendStorage' {} a -> s {resourceConfig = a} :: CreateBackendStorage)

instance Core.AWSRequest CreateBackendStorage where
  type
    AWSResponse CreateBackendStorage =
      CreateBackendStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackendStorageResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackendStorage where
  hashWithSalt _salt CreateBackendStorage' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceConfig

instance Prelude.NFData CreateBackendStorage where
  rnf CreateBackendStorage' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceConfig

instance Data.ToHeaders CreateBackendStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackendStorage where
  toJSON CreateBackendStorage' {..} =
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

instance Data.ToPath CreateBackendStorage where
  toPath CreateBackendStorage' {..} =
    Prelude.mconcat
      ["/backend/", Data.toBS appId, "/storage"]

instance Data.ToQuery CreateBackendStorage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackendStorageResponse' smart constructor.
data CreateBackendStorageResponse = CreateBackendStorageResponse'
  { -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The ID for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createBackendStorageResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'createBackendStorageResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'jobId', 'createBackendStorageResponse_jobId' - The ID for the job.
--
-- 'status', 'createBackendStorageResponse_status' - The current status of the request.
--
-- 'httpStatus', 'createBackendStorageResponse_httpStatus' - The response's http status code.
newCreateBackendStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackendStorageResponse
newCreateBackendStorageResponse pHttpStatus_ =
  CreateBackendStorageResponse'
    { appId =
        Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
createBackendStorageResponse_appId :: Lens.Lens' CreateBackendStorageResponse (Prelude.Maybe Prelude.Text)
createBackendStorageResponse_appId = Lens.lens (\CreateBackendStorageResponse' {appId} -> appId) (\s@CreateBackendStorageResponse' {} a -> s {appId = a} :: CreateBackendStorageResponse)

-- | The name of the backend environment.
createBackendStorageResponse_backendEnvironmentName :: Lens.Lens' CreateBackendStorageResponse (Prelude.Maybe Prelude.Text)
createBackendStorageResponse_backendEnvironmentName = Lens.lens (\CreateBackendStorageResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendStorageResponse' {} a -> s {backendEnvironmentName = a} :: CreateBackendStorageResponse)

-- | The ID for the job.
createBackendStorageResponse_jobId :: Lens.Lens' CreateBackendStorageResponse (Prelude.Maybe Prelude.Text)
createBackendStorageResponse_jobId = Lens.lens (\CreateBackendStorageResponse' {jobId} -> jobId) (\s@CreateBackendStorageResponse' {} a -> s {jobId = a} :: CreateBackendStorageResponse)

-- | The current status of the request.
createBackendStorageResponse_status :: Lens.Lens' CreateBackendStorageResponse (Prelude.Maybe Prelude.Text)
createBackendStorageResponse_status = Lens.lens (\CreateBackendStorageResponse' {status} -> status) (\s@CreateBackendStorageResponse' {} a -> s {status = a} :: CreateBackendStorageResponse)

-- | The response's http status code.
createBackendStorageResponse_httpStatus :: Lens.Lens' CreateBackendStorageResponse Prelude.Int
createBackendStorageResponse_httpStatus = Lens.lens (\CreateBackendStorageResponse' {httpStatus} -> httpStatus) (\s@CreateBackendStorageResponse' {} a -> s {httpStatus = a} :: CreateBackendStorageResponse)

instance Prelude.NFData CreateBackendStorageResponse where
  rnf CreateBackendStorageResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
