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
-- Module      : Amazonka.AmplifyBackend.DeleteBackendStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified backend storage resource.
module Amazonka.AmplifyBackend.DeleteBackendStorage
  ( -- * Creating a Request
    DeleteBackendStorage (..),
    newDeleteBackendStorage,

    -- * Request Lenses
    deleteBackendStorage_appId,
    deleteBackendStorage_backendEnvironmentName,
    deleteBackendStorage_serviceName,
    deleteBackendStorage_resourceName,

    -- * Destructuring the Response
    DeleteBackendStorageResponse (..),
    newDeleteBackendStorageResponse,

    -- * Response Lenses
    deleteBackendStorageResponse_appId,
    deleteBackendStorageResponse_backendEnvironmentName,
    deleteBackendStorageResponse_jobId,
    deleteBackendStorageResponse_status,
    deleteBackendStorageResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for DeleteBackendStorage.
--
-- /See:/ 'newDeleteBackendStorage' smart constructor.
data DeleteBackendStorage = DeleteBackendStorage'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of the storage service.
    serviceName :: ServiceName,
    -- | The name of the storage resource.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackendStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteBackendStorage_appId' - The app ID.
--
-- 'backendEnvironmentName', 'deleteBackendStorage_backendEnvironmentName' - The name of the backend environment.
--
-- 'serviceName', 'deleteBackendStorage_serviceName' - The name of the storage service.
--
-- 'resourceName', 'deleteBackendStorage_resourceName' - The name of the storage resource.
newDeleteBackendStorage ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'serviceName'
  ServiceName ->
  -- | 'resourceName'
  Prelude.Text ->
  DeleteBackendStorage
newDeleteBackendStorage
  pAppId_
  pBackendEnvironmentName_
  pServiceName_
  pResourceName_ =
    DeleteBackendStorage'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        serviceName = pServiceName_,
        resourceName = pResourceName_
      }

-- | The app ID.
deleteBackendStorage_appId :: Lens.Lens' DeleteBackendStorage Prelude.Text
deleteBackendStorage_appId = Lens.lens (\DeleteBackendStorage' {appId} -> appId) (\s@DeleteBackendStorage' {} a -> s {appId = a} :: DeleteBackendStorage)

-- | The name of the backend environment.
deleteBackendStorage_backendEnvironmentName :: Lens.Lens' DeleteBackendStorage Prelude.Text
deleteBackendStorage_backendEnvironmentName = Lens.lens (\DeleteBackendStorage' {backendEnvironmentName} -> backendEnvironmentName) (\s@DeleteBackendStorage' {} a -> s {backendEnvironmentName = a} :: DeleteBackendStorage)

-- | The name of the storage service.
deleteBackendStorage_serviceName :: Lens.Lens' DeleteBackendStorage ServiceName
deleteBackendStorage_serviceName = Lens.lens (\DeleteBackendStorage' {serviceName} -> serviceName) (\s@DeleteBackendStorage' {} a -> s {serviceName = a} :: DeleteBackendStorage)

-- | The name of the storage resource.
deleteBackendStorage_resourceName :: Lens.Lens' DeleteBackendStorage Prelude.Text
deleteBackendStorage_resourceName = Lens.lens (\DeleteBackendStorage' {resourceName} -> resourceName) (\s@DeleteBackendStorage' {} a -> s {resourceName = a} :: DeleteBackendStorage)

instance Core.AWSRequest DeleteBackendStorage where
  type
    AWSResponse DeleteBackendStorage =
      DeleteBackendStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBackendStorageResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBackendStorage where
  hashWithSalt _salt DeleteBackendStorage' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData DeleteBackendStorage where
  rnf DeleteBackendStorage' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf resourceName

instance Data.ToHeaders DeleteBackendStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBackendStorage where
  toJSON DeleteBackendStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just ("resourceName" Data..= resourceName)
          ]
      )

instance Data.ToPath DeleteBackendStorage where
  toPath DeleteBackendStorage' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/storage/",
        Data.toBS backendEnvironmentName,
        "/remove"
      ]

instance Data.ToQuery DeleteBackendStorage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackendStorageResponse' smart constructor.
data DeleteBackendStorageResponse = DeleteBackendStorageResponse'
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
-- Create a value of 'DeleteBackendStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteBackendStorageResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'deleteBackendStorageResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'jobId', 'deleteBackendStorageResponse_jobId' - The ID for the job.
--
-- 'status', 'deleteBackendStorageResponse_status' - The current status of the request.
--
-- 'httpStatus', 'deleteBackendStorageResponse_httpStatus' - The response's http status code.
newDeleteBackendStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBackendStorageResponse
newDeleteBackendStorageResponse pHttpStatus_ =
  DeleteBackendStorageResponse'
    { appId =
        Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
deleteBackendStorageResponse_appId :: Lens.Lens' DeleteBackendStorageResponse (Prelude.Maybe Prelude.Text)
deleteBackendStorageResponse_appId = Lens.lens (\DeleteBackendStorageResponse' {appId} -> appId) (\s@DeleteBackendStorageResponse' {} a -> s {appId = a} :: DeleteBackendStorageResponse)

-- | The name of the backend environment.
deleteBackendStorageResponse_backendEnvironmentName :: Lens.Lens' DeleteBackendStorageResponse (Prelude.Maybe Prelude.Text)
deleteBackendStorageResponse_backendEnvironmentName = Lens.lens (\DeleteBackendStorageResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@DeleteBackendStorageResponse' {} a -> s {backendEnvironmentName = a} :: DeleteBackendStorageResponse)

-- | The ID for the job.
deleteBackendStorageResponse_jobId :: Lens.Lens' DeleteBackendStorageResponse (Prelude.Maybe Prelude.Text)
deleteBackendStorageResponse_jobId = Lens.lens (\DeleteBackendStorageResponse' {jobId} -> jobId) (\s@DeleteBackendStorageResponse' {} a -> s {jobId = a} :: DeleteBackendStorageResponse)

-- | The current status of the request.
deleteBackendStorageResponse_status :: Lens.Lens' DeleteBackendStorageResponse (Prelude.Maybe Prelude.Text)
deleteBackendStorageResponse_status = Lens.lens (\DeleteBackendStorageResponse' {status} -> status) (\s@DeleteBackendStorageResponse' {} a -> s {status = a} :: DeleteBackendStorageResponse)

-- | The response's http status code.
deleteBackendStorageResponse_httpStatus :: Lens.Lens' DeleteBackendStorageResponse Prelude.Int
deleteBackendStorageResponse_httpStatus = Lens.lens (\DeleteBackendStorageResponse' {httpStatus} -> httpStatus) (\s@DeleteBackendStorageResponse' {} a -> s {httpStatus = a} :: DeleteBackendStorageResponse)

instance Prelude.NFData DeleteBackendStorageResponse where
  rnf DeleteBackendStorageResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
