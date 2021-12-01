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
-- Module      : Amazonka.AmplifyBackend.GetBackend
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides project-level details for your Amplify UI project.
module Amazonka.AmplifyBackend.GetBackend
  ( -- * Creating a Request
    GetBackend (..),
    newGetBackend,

    -- * Request Lenses
    getBackend_backendEnvironmentName,
    getBackend_appId,

    -- * Destructuring the Response
    GetBackendResponse (..),
    newGetBackendResponse,

    -- * Response Lenses
    getBackendResponse_error,
    getBackendResponse_appName,
    getBackendResponse_amplifyMetaConfig,
    getBackendResponse_appId,
    getBackendResponse_amplifyFeatureFlags,
    getBackendResponse_backendEnvironmentList,
    getBackendResponse_backendEnvironmentName,
    getBackendResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for GetBackend.
--
-- /See:/ 'newGetBackend' smart constructor.
data GetBackend = GetBackend'
  { -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backendEnvironmentName', 'getBackend_backendEnvironmentName' - The name of the backend environment.
--
-- 'appId', 'getBackend_appId' - The app ID.
newGetBackend ::
  -- | 'appId'
  Prelude.Text ->
  GetBackend
newGetBackend pAppId_ =
  GetBackend'
    { backendEnvironmentName =
        Prelude.Nothing,
      appId = pAppId_
    }

-- | The name of the backend environment.
getBackend_backendEnvironmentName :: Lens.Lens' GetBackend (Prelude.Maybe Prelude.Text)
getBackend_backendEnvironmentName = Lens.lens (\GetBackend' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackend' {} a -> s {backendEnvironmentName = a} :: GetBackend)

-- | The app ID.
getBackend_appId :: Lens.Lens' GetBackend Prelude.Text
getBackend_appId = Lens.lens (\GetBackend' {appId} -> appId) (\s@GetBackend' {} a -> s {appId = a} :: GetBackend)

instance Core.AWSRequest GetBackend where
  type AWSResponse GetBackend = GetBackendResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackendResponse'
            Prelude.<$> (x Core..?> "error")
            Prelude.<*> (x Core..?> "appName")
            Prelude.<*> (x Core..?> "amplifyMetaConfig")
            Prelude.<*> (x Core..?> "appId")
            Prelude.<*> (x Core..?> "amplifyFeatureFlags")
            Prelude.<*> ( x Core..?> "backendEnvironmentList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "backendEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackend where
  hashWithSalt salt' GetBackend' {..} =
    salt' `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName

instance Prelude.NFData GetBackend where
  rnf GetBackend' {..} =
    Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf appId

instance Core.ToHeaders GetBackend where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBackend where
  toJSON GetBackend' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("backendEnvironmentName" Core..=)
              Prelude.<$> backendEnvironmentName
          ]
      )

instance Core.ToPath GetBackend where
  toPath GetBackend' {..} =
    Prelude.mconcat
      ["/backend/", Core.toBS appId, "/details"]

instance Core.ToQuery GetBackend where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackendResponse' smart constructor.
data GetBackendResponse = GetBackendResponse'
  { -- | If the request failed, this is the returned error.
    error :: Prelude.Maybe Prelude.Text,
    -- | The name of the app.
    appName :: Prelude.Maybe Prelude.Text,
    -- | A stringified version of the current configs for your Amplify project.
    amplifyMetaConfig :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | A stringified version of the cli.json file for your Amplify project.
    amplifyFeatureFlags :: Prelude.Maybe Prelude.Text,
    -- | A list of backend environments in an array.
    backendEnvironmentList :: Prelude.Maybe [Prelude.Text],
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'getBackendResponse_error' - If the request failed, this is the returned error.
--
-- 'appName', 'getBackendResponse_appName' - The name of the app.
--
-- 'amplifyMetaConfig', 'getBackendResponse_amplifyMetaConfig' - A stringified version of the current configs for your Amplify project.
--
-- 'appId', 'getBackendResponse_appId' - The app ID.
--
-- 'amplifyFeatureFlags', 'getBackendResponse_amplifyFeatureFlags' - A stringified version of the cli.json file for your Amplify project.
--
-- 'backendEnvironmentList', 'getBackendResponse_backendEnvironmentList' - A list of backend environments in an array.
--
-- 'backendEnvironmentName', 'getBackendResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'httpStatus', 'getBackendResponse_httpStatus' - The response's http status code.
newGetBackendResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackendResponse
newGetBackendResponse pHttpStatus_ =
  GetBackendResponse'
    { error = Prelude.Nothing,
      appName = Prelude.Nothing,
      amplifyMetaConfig = Prelude.Nothing,
      appId = Prelude.Nothing,
      amplifyFeatureFlags = Prelude.Nothing,
      backendEnvironmentList = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request failed, this is the returned error.
getBackendResponse_error :: Lens.Lens' GetBackendResponse (Prelude.Maybe Prelude.Text)
getBackendResponse_error = Lens.lens (\GetBackendResponse' {error} -> error) (\s@GetBackendResponse' {} a -> s {error = a} :: GetBackendResponse)

-- | The name of the app.
getBackendResponse_appName :: Lens.Lens' GetBackendResponse (Prelude.Maybe Prelude.Text)
getBackendResponse_appName = Lens.lens (\GetBackendResponse' {appName} -> appName) (\s@GetBackendResponse' {} a -> s {appName = a} :: GetBackendResponse)

-- | A stringified version of the current configs for your Amplify project.
getBackendResponse_amplifyMetaConfig :: Lens.Lens' GetBackendResponse (Prelude.Maybe Prelude.Text)
getBackendResponse_amplifyMetaConfig = Lens.lens (\GetBackendResponse' {amplifyMetaConfig} -> amplifyMetaConfig) (\s@GetBackendResponse' {} a -> s {amplifyMetaConfig = a} :: GetBackendResponse)

-- | The app ID.
getBackendResponse_appId :: Lens.Lens' GetBackendResponse (Prelude.Maybe Prelude.Text)
getBackendResponse_appId = Lens.lens (\GetBackendResponse' {appId} -> appId) (\s@GetBackendResponse' {} a -> s {appId = a} :: GetBackendResponse)

-- | A stringified version of the cli.json file for your Amplify project.
getBackendResponse_amplifyFeatureFlags :: Lens.Lens' GetBackendResponse (Prelude.Maybe Prelude.Text)
getBackendResponse_amplifyFeatureFlags = Lens.lens (\GetBackendResponse' {amplifyFeatureFlags} -> amplifyFeatureFlags) (\s@GetBackendResponse' {} a -> s {amplifyFeatureFlags = a} :: GetBackendResponse)

-- | A list of backend environments in an array.
getBackendResponse_backendEnvironmentList :: Lens.Lens' GetBackendResponse (Prelude.Maybe [Prelude.Text])
getBackendResponse_backendEnvironmentList = Lens.lens (\GetBackendResponse' {backendEnvironmentList} -> backendEnvironmentList) (\s@GetBackendResponse' {} a -> s {backendEnvironmentList = a} :: GetBackendResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the backend environment.
getBackendResponse_backendEnvironmentName :: Lens.Lens' GetBackendResponse (Prelude.Maybe Prelude.Text)
getBackendResponse_backendEnvironmentName = Lens.lens (\GetBackendResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendResponse' {} a -> s {backendEnvironmentName = a} :: GetBackendResponse)

-- | The response's http status code.
getBackendResponse_httpStatus :: Lens.Lens' GetBackendResponse Prelude.Int
getBackendResponse_httpStatus = Lens.lens (\GetBackendResponse' {httpStatus} -> httpStatus) (\s@GetBackendResponse' {} a -> s {httpStatus = a} :: GetBackendResponse)

instance Prelude.NFData GetBackendResponse where
  rnf GetBackendResponse' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf backendEnvironmentList
      `Prelude.seq` Prelude.rnf amplifyFeatureFlags
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf amplifyMetaConfig
      `Prelude.seq` Prelude.rnf appName
