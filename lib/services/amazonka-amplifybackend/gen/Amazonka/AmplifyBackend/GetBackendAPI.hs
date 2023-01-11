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
-- Module      : Amazonka.AmplifyBackend.GetBackendAPI
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details for a backend API.
module Amazonka.AmplifyBackend.GetBackendAPI
  ( -- * Creating a Request
    GetBackendAPI (..),
    newGetBackendAPI,

    -- * Request Lenses
    getBackendAPI_resourceConfig,
    getBackendAPI_appId,
    getBackendAPI_backendEnvironmentName,
    getBackendAPI_resourceName,

    -- * Destructuring the Response
    GetBackendAPIResponse (..),
    newGetBackendAPIResponse,

    -- * Response Lenses
    getBackendAPIResponse_appId,
    getBackendAPIResponse_backendEnvironmentName,
    getBackendAPIResponse_error,
    getBackendAPIResponse_resourceConfig,
    getBackendAPIResponse_resourceName,
    getBackendAPIResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for GetBackendAPI.
--
-- /See:/ 'newGetBackendAPI' smart constructor.
data GetBackendAPI = GetBackendAPI'
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
-- Create a value of 'GetBackendAPI' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceConfig', 'getBackendAPI_resourceConfig' - Defines the resource configuration for the data model in your Amplify
-- project.
--
-- 'appId', 'getBackendAPI_appId' - The app ID.
--
-- 'backendEnvironmentName', 'getBackendAPI_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'getBackendAPI_resourceName' - The name of this resource.
newGetBackendAPI ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  GetBackendAPI
newGetBackendAPI
  pAppId_
  pBackendEnvironmentName_
  pResourceName_ =
    GetBackendAPI'
      { resourceConfig = Prelude.Nothing,
        appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_
      }

-- | Defines the resource configuration for the data model in your Amplify
-- project.
getBackendAPI_resourceConfig :: Lens.Lens' GetBackendAPI (Prelude.Maybe BackendAPIResourceConfig)
getBackendAPI_resourceConfig = Lens.lens (\GetBackendAPI' {resourceConfig} -> resourceConfig) (\s@GetBackendAPI' {} a -> s {resourceConfig = a} :: GetBackendAPI)

-- | The app ID.
getBackendAPI_appId :: Lens.Lens' GetBackendAPI Prelude.Text
getBackendAPI_appId = Lens.lens (\GetBackendAPI' {appId} -> appId) (\s@GetBackendAPI' {} a -> s {appId = a} :: GetBackendAPI)

-- | The name of the backend environment.
getBackendAPI_backendEnvironmentName :: Lens.Lens' GetBackendAPI Prelude.Text
getBackendAPI_backendEnvironmentName = Lens.lens (\GetBackendAPI' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendAPI' {} a -> s {backendEnvironmentName = a} :: GetBackendAPI)

-- | The name of this resource.
getBackendAPI_resourceName :: Lens.Lens' GetBackendAPI Prelude.Text
getBackendAPI_resourceName = Lens.lens (\GetBackendAPI' {resourceName} -> resourceName) (\s@GetBackendAPI' {} a -> s {resourceName = a} :: GetBackendAPI)

instance Core.AWSRequest GetBackendAPI where
  type
    AWSResponse GetBackendAPI =
      GetBackendAPIResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackendAPIResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "resourceConfig")
            Prelude.<*> (x Data..?> "resourceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackendAPI where
  hashWithSalt _salt GetBackendAPI' {..} =
    _salt `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData GetBackendAPI where
  rnf GetBackendAPI' {..} =
    Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceName

instance Data.ToHeaders GetBackendAPI where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBackendAPI where
  toJSON GetBackendAPI' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceConfig" Data..=)
              Prelude.<$> resourceConfig,
            Prelude.Just ("resourceName" Data..= resourceName)
          ]
      )

instance Data.ToPath GetBackendAPI where
  toPath GetBackendAPI' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/api/",
        Data.toBS backendEnvironmentName,
        "/details"
      ]

instance Data.ToQuery GetBackendAPI where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackendAPIResponse' smart constructor.
data GetBackendAPIResponse = GetBackendAPIResponse'
  { -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | The resource configuration for this response object.
    resourceConfig :: Prelude.Maybe BackendAPIResourceConfig,
    -- | The name of this resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendAPIResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getBackendAPIResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'getBackendAPIResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'error', 'getBackendAPIResponse_error' - If the request fails, this error is returned.
--
-- 'resourceConfig', 'getBackendAPIResponse_resourceConfig' - The resource configuration for this response object.
--
-- 'resourceName', 'getBackendAPIResponse_resourceName' - The name of this resource.
--
-- 'httpStatus', 'getBackendAPIResponse_httpStatus' - The response's http status code.
newGetBackendAPIResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackendAPIResponse
newGetBackendAPIResponse pHttpStatus_ =
  GetBackendAPIResponse'
    { appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      error = Prelude.Nothing,
      resourceConfig = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
getBackendAPIResponse_appId :: Lens.Lens' GetBackendAPIResponse (Prelude.Maybe Prelude.Text)
getBackendAPIResponse_appId = Lens.lens (\GetBackendAPIResponse' {appId} -> appId) (\s@GetBackendAPIResponse' {} a -> s {appId = a} :: GetBackendAPIResponse)

-- | The name of the backend environment.
getBackendAPIResponse_backendEnvironmentName :: Lens.Lens' GetBackendAPIResponse (Prelude.Maybe Prelude.Text)
getBackendAPIResponse_backendEnvironmentName = Lens.lens (\GetBackendAPIResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendAPIResponse' {} a -> s {backendEnvironmentName = a} :: GetBackendAPIResponse)

-- | If the request fails, this error is returned.
getBackendAPIResponse_error :: Lens.Lens' GetBackendAPIResponse (Prelude.Maybe Prelude.Text)
getBackendAPIResponse_error = Lens.lens (\GetBackendAPIResponse' {error} -> error) (\s@GetBackendAPIResponse' {} a -> s {error = a} :: GetBackendAPIResponse)

-- | The resource configuration for this response object.
getBackendAPIResponse_resourceConfig :: Lens.Lens' GetBackendAPIResponse (Prelude.Maybe BackendAPIResourceConfig)
getBackendAPIResponse_resourceConfig = Lens.lens (\GetBackendAPIResponse' {resourceConfig} -> resourceConfig) (\s@GetBackendAPIResponse' {} a -> s {resourceConfig = a} :: GetBackendAPIResponse)

-- | The name of this resource.
getBackendAPIResponse_resourceName :: Lens.Lens' GetBackendAPIResponse (Prelude.Maybe Prelude.Text)
getBackendAPIResponse_resourceName = Lens.lens (\GetBackendAPIResponse' {resourceName} -> resourceName) (\s@GetBackendAPIResponse' {} a -> s {resourceName = a} :: GetBackendAPIResponse)

-- | The response's http status code.
getBackendAPIResponse_httpStatus :: Lens.Lens' GetBackendAPIResponse Prelude.Int
getBackendAPIResponse_httpStatus = Lens.lens (\GetBackendAPIResponse' {httpStatus} -> httpStatus) (\s@GetBackendAPIResponse' {} a -> s {httpStatus = a} :: GetBackendAPIResponse)

instance Prelude.NFData GetBackendAPIResponse where
  rnf GetBackendAPIResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf httpStatus
