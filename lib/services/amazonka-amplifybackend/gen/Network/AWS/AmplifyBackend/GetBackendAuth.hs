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
-- Module      : Network.AWS.AmplifyBackend.GetBackendAuth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a backend auth details.
module Network.AWS.AmplifyBackend.GetBackendAuth
  ( -- * Creating a Request
    GetBackendAuth (..),
    newGetBackendAuth,

    -- * Request Lenses
    getBackendAuth_appId,
    getBackendAuth_backendEnvironmentName,
    getBackendAuth_resourceName,

    -- * Destructuring the Response
    GetBackendAuthResponse (..),
    newGetBackendAuthResponse,

    -- * Response Lenses
    getBackendAuthResponse_resourceName,
    getBackendAuthResponse_error,
    getBackendAuthResponse_appId,
    getBackendAuthResponse_resourceConfig,
    getBackendAuthResponse_backendEnvironmentName,
    getBackendAuthResponse_httpStatus,
  )
where

import Network.AWS.AmplifyBackend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request body for GetBackendAuth.
--
-- /See:/ 'newGetBackendAuth' smart constructor.
data GetBackendAuth = GetBackendAuth'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of this resource.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getBackendAuth_appId' - The app ID.
--
-- 'backendEnvironmentName', 'getBackendAuth_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'getBackendAuth_resourceName' - The name of this resource.
newGetBackendAuth ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  GetBackendAuth
newGetBackendAuth
  pAppId_
  pBackendEnvironmentName_
  pResourceName_ =
    GetBackendAuth'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_
      }

-- | The app ID.
getBackendAuth_appId :: Lens.Lens' GetBackendAuth Prelude.Text
getBackendAuth_appId = Lens.lens (\GetBackendAuth' {appId} -> appId) (\s@GetBackendAuth' {} a -> s {appId = a} :: GetBackendAuth)

-- | The name of the backend environment.
getBackendAuth_backendEnvironmentName :: Lens.Lens' GetBackendAuth Prelude.Text
getBackendAuth_backendEnvironmentName = Lens.lens (\GetBackendAuth' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendAuth' {} a -> s {backendEnvironmentName = a} :: GetBackendAuth)

-- | The name of this resource.
getBackendAuth_resourceName :: Lens.Lens' GetBackendAuth Prelude.Text
getBackendAuth_resourceName = Lens.lens (\GetBackendAuth' {resourceName} -> resourceName) (\s@GetBackendAuth' {} a -> s {resourceName = a} :: GetBackendAuth)

instance Core.AWSRequest GetBackendAuth where
  type
    AWSResponse GetBackendAuth =
      GetBackendAuthResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackendAuthResponse'
            Prelude.<$> (x Core..?> "resourceName")
            Prelude.<*> (x Core..?> "error")
            Prelude.<*> (x Core..?> "appId")
            Prelude.<*> (x Core..?> "resourceConfig")
            Prelude.<*> (x Core..?> "backendEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackendAuth

instance Prelude.NFData GetBackendAuth

instance Core.ToHeaders GetBackendAuth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBackendAuth where
  toJSON GetBackendAuth' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceName" Core..= resourceName)]
      )

instance Core.ToPath GetBackendAuth where
  toPath GetBackendAuth' {..} =
    Prelude.mconcat
      [ "/backend/",
        Core.toBS appId,
        "/auth/",
        Core.toBS backendEnvironmentName,
        "/details"
      ]

instance Core.ToQuery GetBackendAuth where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackendAuthResponse' smart constructor.
data GetBackendAuthResponse = GetBackendAuthResponse'
  { -- | The name of this resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The resource configuration for authorization requests to the backend of
    -- your Amplify project.
    resourceConfig :: Prelude.Maybe CreateBackendAuthResourceConfig,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'getBackendAuthResponse_resourceName' - The name of this resource.
--
-- 'error', 'getBackendAuthResponse_error' - If the request fails, this error is returned.
--
-- 'appId', 'getBackendAuthResponse_appId' - The app ID.
--
-- 'resourceConfig', 'getBackendAuthResponse_resourceConfig' - The resource configuration for authorization requests to the backend of
-- your Amplify project.
--
-- 'backendEnvironmentName', 'getBackendAuthResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'httpStatus', 'getBackendAuthResponse_httpStatus' - The response's http status code.
newGetBackendAuthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackendAuthResponse
newGetBackendAuthResponse pHttpStatus_ =
  GetBackendAuthResponse'
    { resourceName =
        Prelude.Nothing,
      error = Prelude.Nothing,
      appId = Prelude.Nothing,
      resourceConfig = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of this resource.
getBackendAuthResponse_resourceName :: Lens.Lens' GetBackendAuthResponse (Prelude.Maybe Prelude.Text)
getBackendAuthResponse_resourceName = Lens.lens (\GetBackendAuthResponse' {resourceName} -> resourceName) (\s@GetBackendAuthResponse' {} a -> s {resourceName = a} :: GetBackendAuthResponse)

-- | If the request fails, this error is returned.
getBackendAuthResponse_error :: Lens.Lens' GetBackendAuthResponse (Prelude.Maybe Prelude.Text)
getBackendAuthResponse_error = Lens.lens (\GetBackendAuthResponse' {error} -> error) (\s@GetBackendAuthResponse' {} a -> s {error = a} :: GetBackendAuthResponse)

-- | The app ID.
getBackendAuthResponse_appId :: Lens.Lens' GetBackendAuthResponse (Prelude.Maybe Prelude.Text)
getBackendAuthResponse_appId = Lens.lens (\GetBackendAuthResponse' {appId} -> appId) (\s@GetBackendAuthResponse' {} a -> s {appId = a} :: GetBackendAuthResponse)

-- | The resource configuration for authorization requests to the backend of
-- your Amplify project.
getBackendAuthResponse_resourceConfig :: Lens.Lens' GetBackendAuthResponse (Prelude.Maybe CreateBackendAuthResourceConfig)
getBackendAuthResponse_resourceConfig = Lens.lens (\GetBackendAuthResponse' {resourceConfig} -> resourceConfig) (\s@GetBackendAuthResponse' {} a -> s {resourceConfig = a} :: GetBackendAuthResponse)

-- | The name of the backend environment.
getBackendAuthResponse_backendEnvironmentName :: Lens.Lens' GetBackendAuthResponse (Prelude.Maybe Prelude.Text)
getBackendAuthResponse_backendEnvironmentName = Lens.lens (\GetBackendAuthResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendAuthResponse' {} a -> s {backendEnvironmentName = a} :: GetBackendAuthResponse)

-- | The response's http status code.
getBackendAuthResponse_httpStatus :: Lens.Lens' GetBackendAuthResponse Prelude.Int
getBackendAuthResponse_httpStatus = Lens.lens (\GetBackendAuthResponse' {httpStatus} -> httpStatus) (\s@GetBackendAuthResponse' {} a -> s {httpStatus = a} :: GetBackendAuthResponse)

instance Prelude.NFData GetBackendAuthResponse
