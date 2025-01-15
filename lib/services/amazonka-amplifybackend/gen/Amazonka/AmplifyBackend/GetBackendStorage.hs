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
-- Module      : Amazonka.AmplifyBackend.GetBackendStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details for a backend storage resource.
module Amazonka.AmplifyBackend.GetBackendStorage
  ( -- * Creating a Request
    GetBackendStorage (..),
    newGetBackendStorage,

    -- * Request Lenses
    getBackendStorage_appId,
    getBackendStorage_backendEnvironmentName,
    getBackendStorage_resourceName,

    -- * Destructuring the Response
    GetBackendStorageResponse (..),
    newGetBackendStorageResponse,

    -- * Response Lenses
    getBackendStorageResponse_appId,
    getBackendStorageResponse_backendEnvironmentName,
    getBackendStorageResponse_resourceConfig,
    getBackendStorageResponse_resourceName,
    getBackendStorageResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for GetBackendStorage.
--
-- /See:/ 'newGetBackendStorage' smart constructor.
data GetBackendStorage = GetBackendStorage'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of the storage resource.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getBackendStorage_appId' - The app ID.
--
-- 'backendEnvironmentName', 'getBackendStorage_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'getBackendStorage_resourceName' - The name of the storage resource.
newGetBackendStorage ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  GetBackendStorage
newGetBackendStorage
  pAppId_
  pBackendEnvironmentName_
  pResourceName_ =
    GetBackendStorage'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_
      }

-- | The app ID.
getBackendStorage_appId :: Lens.Lens' GetBackendStorage Prelude.Text
getBackendStorage_appId = Lens.lens (\GetBackendStorage' {appId} -> appId) (\s@GetBackendStorage' {} a -> s {appId = a} :: GetBackendStorage)

-- | The name of the backend environment.
getBackendStorage_backendEnvironmentName :: Lens.Lens' GetBackendStorage Prelude.Text
getBackendStorage_backendEnvironmentName = Lens.lens (\GetBackendStorage' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendStorage' {} a -> s {backendEnvironmentName = a} :: GetBackendStorage)

-- | The name of the storage resource.
getBackendStorage_resourceName :: Lens.Lens' GetBackendStorage Prelude.Text
getBackendStorage_resourceName = Lens.lens (\GetBackendStorage' {resourceName} -> resourceName) (\s@GetBackendStorage' {} a -> s {resourceName = a} :: GetBackendStorage)

instance Core.AWSRequest GetBackendStorage where
  type
    AWSResponse GetBackendStorage =
      GetBackendStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackendStorageResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "resourceConfig")
            Prelude.<*> (x Data..?> "resourceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackendStorage where
  hashWithSalt _salt GetBackendStorage' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData GetBackendStorage where
  rnf GetBackendStorage' {..} =
    Prelude.rnf appId `Prelude.seq`
      Prelude.rnf backendEnvironmentName `Prelude.seq`
        Prelude.rnf resourceName

instance Data.ToHeaders GetBackendStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBackendStorage where
  toJSON GetBackendStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceName" Data..= resourceName)]
      )

instance Data.ToPath GetBackendStorage where
  toPath GetBackendStorage' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/storage/",
        Data.toBS backendEnvironmentName,
        "/details"
      ]

instance Data.ToQuery GetBackendStorage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackendStorageResponse' smart constructor.
data GetBackendStorageResponse = GetBackendStorageResponse'
  { -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The resource configuration for the backend storage resource.
    resourceConfig :: Prelude.Maybe GetBackendStorageResourceConfig,
    -- | The name of the storage resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getBackendStorageResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'getBackendStorageResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceConfig', 'getBackendStorageResponse_resourceConfig' - The resource configuration for the backend storage resource.
--
-- 'resourceName', 'getBackendStorageResponse_resourceName' - The name of the storage resource.
--
-- 'httpStatus', 'getBackendStorageResponse_httpStatus' - The response's http status code.
newGetBackendStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackendStorageResponse
newGetBackendStorageResponse pHttpStatus_ =
  GetBackendStorageResponse'
    { appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      resourceConfig = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
getBackendStorageResponse_appId :: Lens.Lens' GetBackendStorageResponse (Prelude.Maybe Prelude.Text)
getBackendStorageResponse_appId = Lens.lens (\GetBackendStorageResponse' {appId} -> appId) (\s@GetBackendStorageResponse' {} a -> s {appId = a} :: GetBackendStorageResponse)

-- | The name of the backend environment.
getBackendStorageResponse_backendEnvironmentName :: Lens.Lens' GetBackendStorageResponse (Prelude.Maybe Prelude.Text)
getBackendStorageResponse_backendEnvironmentName = Lens.lens (\GetBackendStorageResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendStorageResponse' {} a -> s {backendEnvironmentName = a} :: GetBackendStorageResponse)

-- | The resource configuration for the backend storage resource.
getBackendStorageResponse_resourceConfig :: Lens.Lens' GetBackendStorageResponse (Prelude.Maybe GetBackendStorageResourceConfig)
getBackendStorageResponse_resourceConfig = Lens.lens (\GetBackendStorageResponse' {resourceConfig} -> resourceConfig) (\s@GetBackendStorageResponse' {} a -> s {resourceConfig = a} :: GetBackendStorageResponse)

-- | The name of the storage resource.
getBackendStorageResponse_resourceName :: Lens.Lens' GetBackendStorageResponse (Prelude.Maybe Prelude.Text)
getBackendStorageResponse_resourceName = Lens.lens (\GetBackendStorageResponse' {resourceName} -> resourceName) (\s@GetBackendStorageResponse' {} a -> s {resourceName = a} :: GetBackendStorageResponse)

-- | The response's http status code.
getBackendStorageResponse_httpStatus :: Lens.Lens' GetBackendStorageResponse Prelude.Int
getBackendStorageResponse_httpStatus = Lens.lens (\GetBackendStorageResponse' {httpStatus} -> httpStatus) (\s@GetBackendStorageResponse' {} a -> s {httpStatus = a} :: GetBackendStorageResponse)

instance Prelude.NFData GetBackendStorageResponse where
  rnf GetBackendStorageResponse' {..} =
    Prelude.rnf appId `Prelude.seq`
      Prelude.rnf backendEnvironmentName `Prelude.seq`
        Prelude.rnf resourceConfig `Prelude.seq`
          Prelude.rnf resourceName `Prelude.seq`
            Prelude.rnf httpStatus
