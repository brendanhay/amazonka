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
-- Module      : Amazonka.AmplifyBackend.ImportBackendStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an existing backend storage resource.
module Amazonka.AmplifyBackend.ImportBackendStorage
  ( -- * Creating a Request
    ImportBackendStorage (..),
    newImportBackendStorage,

    -- * Request Lenses
    importBackendStorage_bucketName,
    importBackendStorage_appId,
    importBackendStorage_backendEnvironmentName,
    importBackendStorage_serviceName,

    -- * Destructuring the Response
    ImportBackendStorageResponse (..),
    newImportBackendStorageResponse,

    -- * Response Lenses
    importBackendStorageResponse_appId,
    importBackendStorageResponse_backendEnvironmentName,
    importBackendStorageResponse_jobId,
    importBackendStorageResponse_status,
    importBackendStorageResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for ImportBackendStorage.
--
-- /See:/ 'newImportBackendStorage' smart constructor.
data ImportBackendStorage = ImportBackendStorage'
  { -- | The name of the S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of the storage service.
    serviceName :: ServiceName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportBackendStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'importBackendStorage_bucketName' - The name of the S3 bucket.
--
-- 'appId', 'importBackendStorage_appId' - The app ID.
--
-- 'backendEnvironmentName', 'importBackendStorage_backendEnvironmentName' - The name of the backend environment.
--
-- 'serviceName', 'importBackendStorage_serviceName' - The name of the storage service.
newImportBackendStorage ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'serviceName'
  ServiceName ->
  ImportBackendStorage
newImportBackendStorage
  pAppId_
  pBackendEnvironmentName_
  pServiceName_ =
    ImportBackendStorage'
      { bucketName = Prelude.Nothing,
        appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        serviceName = pServiceName_
      }

-- | The name of the S3 bucket.
importBackendStorage_bucketName :: Lens.Lens' ImportBackendStorage (Prelude.Maybe Prelude.Text)
importBackendStorage_bucketName = Lens.lens (\ImportBackendStorage' {bucketName} -> bucketName) (\s@ImportBackendStorage' {} a -> s {bucketName = a} :: ImportBackendStorage)

-- | The app ID.
importBackendStorage_appId :: Lens.Lens' ImportBackendStorage Prelude.Text
importBackendStorage_appId = Lens.lens (\ImportBackendStorage' {appId} -> appId) (\s@ImportBackendStorage' {} a -> s {appId = a} :: ImportBackendStorage)

-- | The name of the backend environment.
importBackendStorage_backendEnvironmentName :: Lens.Lens' ImportBackendStorage Prelude.Text
importBackendStorage_backendEnvironmentName = Lens.lens (\ImportBackendStorage' {backendEnvironmentName} -> backendEnvironmentName) (\s@ImportBackendStorage' {} a -> s {backendEnvironmentName = a} :: ImportBackendStorage)

-- | The name of the storage service.
importBackendStorage_serviceName :: Lens.Lens' ImportBackendStorage ServiceName
importBackendStorage_serviceName = Lens.lens (\ImportBackendStorage' {serviceName} -> serviceName) (\s@ImportBackendStorage' {} a -> s {serviceName = a} :: ImportBackendStorage)

instance Core.AWSRequest ImportBackendStorage where
  type
    AWSResponse ImportBackendStorage =
      ImportBackendStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportBackendStorageResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportBackendStorage where
  hashWithSalt _salt ImportBackendStorage' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ImportBackendStorage where
  rnf ImportBackendStorage' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders ImportBackendStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportBackendStorage where
  toJSON ImportBackendStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketName" Data..=) Prelude.<$> bucketName,
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath ImportBackendStorage where
  toPath ImportBackendStorage' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/storage/",
        Data.toBS backendEnvironmentName,
        "/import"
      ]

instance Data.ToQuery ImportBackendStorage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportBackendStorageResponse' smart constructor.
data ImportBackendStorageResponse = ImportBackendStorageResponse'
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
-- Create a value of 'ImportBackendStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'importBackendStorageResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'importBackendStorageResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'jobId', 'importBackendStorageResponse_jobId' - The ID for the job.
--
-- 'status', 'importBackendStorageResponse_status' - The current status of the request.
--
-- 'httpStatus', 'importBackendStorageResponse_httpStatus' - The response's http status code.
newImportBackendStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportBackendStorageResponse
newImportBackendStorageResponse pHttpStatus_ =
  ImportBackendStorageResponse'
    { appId =
        Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
importBackendStorageResponse_appId :: Lens.Lens' ImportBackendStorageResponse (Prelude.Maybe Prelude.Text)
importBackendStorageResponse_appId = Lens.lens (\ImportBackendStorageResponse' {appId} -> appId) (\s@ImportBackendStorageResponse' {} a -> s {appId = a} :: ImportBackendStorageResponse)

-- | The name of the backend environment.
importBackendStorageResponse_backendEnvironmentName :: Lens.Lens' ImportBackendStorageResponse (Prelude.Maybe Prelude.Text)
importBackendStorageResponse_backendEnvironmentName = Lens.lens (\ImportBackendStorageResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@ImportBackendStorageResponse' {} a -> s {backendEnvironmentName = a} :: ImportBackendStorageResponse)

-- | The ID for the job.
importBackendStorageResponse_jobId :: Lens.Lens' ImportBackendStorageResponse (Prelude.Maybe Prelude.Text)
importBackendStorageResponse_jobId = Lens.lens (\ImportBackendStorageResponse' {jobId} -> jobId) (\s@ImportBackendStorageResponse' {} a -> s {jobId = a} :: ImportBackendStorageResponse)

-- | The current status of the request.
importBackendStorageResponse_status :: Lens.Lens' ImportBackendStorageResponse (Prelude.Maybe Prelude.Text)
importBackendStorageResponse_status = Lens.lens (\ImportBackendStorageResponse' {status} -> status) (\s@ImportBackendStorageResponse' {} a -> s {status = a} :: ImportBackendStorageResponse)

-- | The response's http status code.
importBackendStorageResponse_httpStatus :: Lens.Lens' ImportBackendStorageResponse Prelude.Int
importBackendStorageResponse_httpStatus = Lens.lens (\ImportBackendStorageResponse' {httpStatus} -> httpStatus) (\s@ImportBackendStorageResponse' {} a -> s {httpStatus = a} :: ImportBackendStorageResponse)

instance Prelude.NFData ImportBackendStorageResponse where
  rnf ImportBackendStorageResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
