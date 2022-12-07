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
-- Module      : Amazonka.AmplifyBackend.ImportBackendAuth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an existing backend authentication resource.
module Amazonka.AmplifyBackend.ImportBackendAuth
  ( -- * Creating a Request
    ImportBackendAuth (..),
    newImportBackendAuth,

    -- * Request Lenses
    importBackendAuth_identityPoolId,
    importBackendAuth_appId,
    importBackendAuth_backendEnvironmentName,
    importBackendAuth_userPoolId,
    importBackendAuth_nativeClientId,
    importBackendAuth_webClientId,

    -- * Destructuring the Response
    ImportBackendAuthResponse (..),
    newImportBackendAuthResponse,

    -- * Response Lenses
    importBackendAuthResponse_jobId,
    importBackendAuthResponse_status,
    importBackendAuthResponse_error,
    importBackendAuthResponse_operation,
    importBackendAuthResponse_appId,
    importBackendAuthResponse_backendEnvironmentName,
    importBackendAuthResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for ImportBackendAuth.
--
-- /See:/ 'newImportBackendAuth' smart constructor.
data ImportBackendAuth = ImportBackendAuth'
  { -- | The ID of the Amazon Cognito identity pool.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The ID of the Amazon Cognito user pool.
    userPoolId :: Prelude.Text,
    -- | The ID of the Amazon Cognito native client.
    nativeClientId :: Prelude.Text,
    -- | The ID of the Amazon Cognito web client.
    webClientId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportBackendAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'importBackendAuth_identityPoolId' - The ID of the Amazon Cognito identity pool.
--
-- 'appId', 'importBackendAuth_appId' - The app ID.
--
-- 'backendEnvironmentName', 'importBackendAuth_backendEnvironmentName' - The name of the backend environment.
--
-- 'userPoolId', 'importBackendAuth_userPoolId' - The ID of the Amazon Cognito user pool.
--
-- 'nativeClientId', 'importBackendAuth_nativeClientId' - The ID of the Amazon Cognito native client.
--
-- 'webClientId', 'importBackendAuth_webClientId' - The ID of the Amazon Cognito web client.
newImportBackendAuth ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'nativeClientId'
  Prelude.Text ->
  -- | 'webClientId'
  Prelude.Text ->
  ImportBackendAuth
newImportBackendAuth
  pAppId_
  pBackendEnvironmentName_
  pUserPoolId_
  pNativeClientId_
  pWebClientId_ =
    ImportBackendAuth'
      { identityPoolId =
          Prelude.Nothing,
        appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        userPoolId = pUserPoolId_,
        nativeClientId = pNativeClientId_,
        webClientId = pWebClientId_
      }

-- | The ID of the Amazon Cognito identity pool.
importBackendAuth_identityPoolId :: Lens.Lens' ImportBackendAuth (Prelude.Maybe Prelude.Text)
importBackendAuth_identityPoolId = Lens.lens (\ImportBackendAuth' {identityPoolId} -> identityPoolId) (\s@ImportBackendAuth' {} a -> s {identityPoolId = a} :: ImportBackendAuth)

-- | The app ID.
importBackendAuth_appId :: Lens.Lens' ImportBackendAuth Prelude.Text
importBackendAuth_appId = Lens.lens (\ImportBackendAuth' {appId} -> appId) (\s@ImportBackendAuth' {} a -> s {appId = a} :: ImportBackendAuth)

-- | The name of the backend environment.
importBackendAuth_backendEnvironmentName :: Lens.Lens' ImportBackendAuth Prelude.Text
importBackendAuth_backendEnvironmentName = Lens.lens (\ImportBackendAuth' {backendEnvironmentName} -> backendEnvironmentName) (\s@ImportBackendAuth' {} a -> s {backendEnvironmentName = a} :: ImportBackendAuth)

-- | The ID of the Amazon Cognito user pool.
importBackendAuth_userPoolId :: Lens.Lens' ImportBackendAuth Prelude.Text
importBackendAuth_userPoolId = Lens.lens (\ImportBackendAuth' {userPoolId} -> userPoolId) (\s@ImportBackendAuth' {} a -> s {userPoolId = a} :: ImportBackendAuth)

-- | The ID of the Amazon Cognito native client.
importBackendAuth_nativeClientId :: Lens.Lens' ImportBackendAuth Prelude.Text
importBackendAuth_nativeClientId = Lens.lens (\ImportBackendAuth' {nativeClientId} -> nativeClientId) (\s@ImportBackendAuth' {} a -> s {nativeClientId = a} :: ImportBackendAuth)

-- | The ID of the Amazon Cognito web client.
importBackendAuth_webClientId :: Lens.Lens' ImportBackendAuth Prelude.Text
importBackendAuth_webClientId = Lens.lens (\ImportBackendAuth' {webClientId} -> webClientId) (\s@ImportBackendAuth' {} a -> s {webClientId = a} :: ImportBackendAuth)

instance Core.AWSRequest ImportBackendAuth where
  type
    AWSResponse ImportBackendAuth =
      ImportBackendAuthResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportBackendAuthResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportBackendAuth where
  hashWithSalt _salt ImportBackendAuth' {..} =
    _salt `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` nativeClientId
      `Prelude.hashWithSalt` webClientId

instance Prelude.NFData ImportBackendAuth where
  rnf ImportBackendAuth' {..} =
    Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf nativeClientId
      `Prelude.seq` Prelude.rnf webClientId

instance Data.ToHeaders ImportBackendAuth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportBackendAuth where
  toJSON ImportBackendAuth' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("identityPoolId" Data..=)
              Prelude.<$> identityPoolId,
            Prelude.Just ("userPoolId" Data..= userPoolId),
            Prelude.Just
              ("nativeClientId" Data..= nativeClientId),
            Prelude.Just ("webClientId" Data..= webClientId)
          ]
      )

instance Data.ToPath ImportBackendAuth where
  toPath ImportBackendAuth' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/auth/",
        Data.toBS backendEnvironmentName,
        "/import"
      ]

instance Data.ToQuery ImportBackendAuth where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportBackendAuthResponse' smart constructor.
data ImportBackendAuthResponse = ImportBackendAuthResponse'
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
-- Create a value of 'ImportBackendAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'importBackendAuthResponse_jobId' - The ID for the job.
--
-- 'status', 'importBackendAuthResponse_status' - The current status of the request.
--
-- 'error', 'importBackendAuthResponse_error' - If the request fails, this error is returned.
--
-- 'operation', 'importBackendAuthResponse_operation' - The name of the operation.
--
-- 'appId', 'importBackendAuthResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'importBackendAuthResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'httpStatus', 'importBackendAuthResponse_httpStatus' - The response's http status code.
newImportBackendAuthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportBackendAuthResponse
newImportBackendAuthResponse pHttpStatus_ =
  ImportBackendAuthResponse'
    { jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      error = Prelude.Nothing,
      operation = Prelude.Nothing,
      appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the job.
importBackendAuthResponse_jobId :: Lens.Lens' ImportBackendAuthResponse (Prelude.Maybe Prelude.Text)
importBackendAuthResponse_jobId = Lens.lens (\ImportBackendAuthResponse' {jobId} -> jobId) (\s@ImportBackendAuthResponse' {} a -> s {jobId = a} :: ImportBackendAuthResponse)

-- | The current status of the request.
importBackendAuthResponse_status :: Lens.Lens' ImportBackendAuthResponse (Prelude.Maybe Prelude.Text)
importBackendAuthResponse_status = Lens.lens (\ImportBackendAuthResponse' {status} -> status) (\s@ImportBackendAuthResponse' {} a -> s {status = a} :: ImportBackendAuthResponse)

-- | If the request fails, this error is returned.
importBackendAuthResponse_error :: Lens.Lens' ImportBackendAuthResponse (Prelude.Maybe Prelude.Text)
importBackendAuthResponse_error = Lens.lens (\ImportBackendAuthResponse' {error} -> error) (\s@ImportBackendAuthResponse' {} a -> s {error = a} :: ImportBackendAuthResponse)

-- | The name of the operation.
importBackendAuthResponse_operation :: Lens.Lens' ImportBackendAuthResponse (Prelude.Maybe Prelude.Text)
importBackendAuthResponse_operation = Lens.lens (\ImportBackendAuthResponse' {operation} -> operation) (\s@ImportBackendAuthResponse' {} a -> s {operation = a} :: ImportBackendAuthResponse)

-- | The app ID.
importBackendAuthResponse_appId :: Lens.Lens' ImportBackendAuthResponse (Prelude.Maybe Prelude.Text)
importBackendAuthResponse_appId = Lens.lens (\ImportBackendAuthResponse' {appId} -> appId) (\s@ImportBackendAuthResponse' {} a -> s {appId = a} :: ImportBackendAuthResponse)

-- | The name of the backend environment.
importBackendAuthResponse_backendEnvironmentName :: Lens.Lens' ImportBackendAuthResponse (Prelude.Maybe Prelude.Text)
importBackendAuthResponse_backendEnvironmentName = Lens.lens (\ImportBackendAuthResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@ImportBackendAuthResponse' {} a -> s {backendEnvironmentName = a} :: ImportBackendAuthResponse)

-- | The response's http status code.
importBackendAuthResponse_httpStatus :: Lens.Lens' ImportBackendAuthResponse Prelude.Int
importBackendAuthResponse_httpStatus = Lens.lens (\ImportBackendAuthResponse' {httpStatus} -> httpStatus) (\s@ImportBackendAuthResponse' {} a -> s {httpStatus = a} :: ImportBackendAuthResponse)

instance Prelude.NFData ImportBackendAuthResponse where
  rnf ImportBackendAuthResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf httpStatus
