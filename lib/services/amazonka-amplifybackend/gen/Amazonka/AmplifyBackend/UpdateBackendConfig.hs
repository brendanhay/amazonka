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
-- Module      : Amazonka.AmplifyBackend.UpdateBackendConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the AWS resources required to access the Amplify Admin UI.
module Amazonka.AmplifyBackend.UpdateBackendConfig
  ( -- * Creating a Request
    UpdateBackendConfig (..),
    newUpdateBackendConfig,

    -- * Request Lenses
    updateBackendConfig_loginAuthConfig,
    updateBackendConfig_appId,

    -- * Destructuring the Response
    UpdateBackendConfigResponse (..),
    newUpdateBackendConfigResponse,

    -- * Response Lenses
    updateBackendConfigResponse_appId,
    updateBackendConfigResponse_backendManagerAppId,
    updateBackendConfigResponse_error,
    updateBackendConfigResponse_loginAuthConfig,
    updateBackendConfigResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for UpdateBackendConfig.
--
-- /See:/ 'newUpdateBackendConfig' smart constructor.
data UpdateBackendConfig = UpdateBackendConfig'
  { -- | Describes the Amazon Cognito configuration for Admin UI access.
    loginAuthConfig :: Prelude.Maybe LoginAuthConfigReqObj,
    -- | The app ID.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loginAuthConfig', 'updateBackendConfig_loginAuthConfig' - Describes the Amazon Cognito configuration for Admin UI access.
--
-- 'appId', 'updateBackendConfig_appId' - The app ID.
newUpdateBackendConfig ::
  -- | 'appId'
  Prelude.Text ->
  UpdateBackendConfig
newUpdateBackendConfig pAppId_ =
  UpdateBackendConfig'
    { loginAuthConfig =
        Prelude.Nothing,
      appId = pAppId_
    }

-- | Describes the Amazon Cognito configuration for Admin UI access.
updateBackendConfig_loginAuthConfig :: Lens.Lens' UpdateBackendConfig (Prelude.Maybe LoginAuthConfigReqObj)
updateBackendConfig_loginAuthConfig = Lens.lens (\UpdateBackendConfig' {loginAuthConfig} -> loginAuthConfig) (\s@UpdateBackendConfig' {} a -> s {loginAuthConfig = a} :: UpdateBackendConfig)

-- | The app ID.
updateBackendConfig_appId :: Lens.Lens' UpdateBackendConfig Prelude.Text
updateBackendConfig_appId = Lens.lens (\UpdateBackendConfig' {appId} -> appId) (\s@UpdateBackendConfig' {} a -> s {appId = a} :: UpdateBackendConfig)

instance Core.AWSRequest UpdateBackendConfig where
  type
    AWSResponse UpdateBackendConfig =
      UpdateBackendConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBackendConfigResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendManagerAppId")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "loginAuthConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBackendConfig where
  hashWithSalt _salt UpdateBackendConfig' {..} =
    _salt
      `Prelude.hashWithSalt` loginAuthConfig
      `Prelude.hashWithSalt` appId

instance Prelude.NFData UpdateBackendConfig where
  rnf UpdateBackendConfig' {..} =
    Prelude.rnf loginAuthConfig
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders UpdateBackendConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBackendConfig where
  toJSON UpdateBackendConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("loginAuthConfig" Data..=)
              Prelude.<$> loginAuthConfig
          ]
      )

instance Data.ToPath UpdateBackendConfig where
  toPath UpdateBackendConfig' {..} =
    Prelude.mconcat
      ["/backend/", Data.toBS appId, "/config/update"]

instance Data.ToQuery UpdateBackendConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBackendConfigResponse' smart constructor.
data UpdateBackendConfigResponse = UpdateBackendConfigResponse'
  { -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The app ID for the backend manager.
    backendManagerAppId :: Prelude.Maybe Prelude.Text,
    -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | Describes the Amazon Cognito configurations for the Admin UI auth
    -- resource to log in with.
    loginAuthConfig :: Prelude.Maybe LoginAuthConfigReqObj,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'updateBackendConfigResponse_appId' - The app ID.
--
-- 'backendManagerAppId', 'updateBackendConfigResponse_backendManagerAppId' - The app ID for the backend manager.
--
-- 'error', 'updateBackendConfigResponse_error' - If the request fails, this error is returned.
--
-- 'loginAuthConfig', 'updateBackendConfigResponse_loginAuthConfig' - Describes the Amazon Cognito configurations for the Admin UI auth
-- resource to log in with.
--
-- 'httpStatus', 'updateBackendConfigResponse_httpStatus' - The response's http status code.
newUpdateBackendConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBackendConfigResponse
newUpdateBackendConfigResponse pHttpStatus_ =
  UpdateBackendConfigResponse'
    { appId =
        Prelude.Nothing,
      backendManagerAppId = Prelude.Nothing,
      error = Prelude.Nothing,
      loginAuthConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
updateBackendConfigResponse_appId :: Lens.Lens' UpdateBackendConfigResponse (Prelude.Maybe Prelude.Text)
updateBackendConfigResponse_appId = Lens.lens (\UpdateBackendConfigResponse' {appId} -> appId) (\s@UpdateBackendConfigResponse' {} a -> s {appId = a} :: UpdateBackendConfigResponse)

-- | The app ID for the backend manager.
updateBackendConfigResponse_backendManagerAppId :: Lens.Lens' UpdateBackendConfigResponse (Prelude.Maybe Prelude.Text)
updateBackendConfigResponse_backendManagerAppId = Lens.lens (\UpdateBackendConfigResponse' {backendManagerAppId} -> backendManagerAppId) (\s@UpdateBackendConfigResponse' {} a -> s {backendManagerAppId = a} :: UpdateBackendConfigResponse)

-- | If the request fails, this error is returned.
updateBackendConfigResponse_error :: Lens.Lens' UpdateBackendConfigResponse (Prelude.Maybe Prelude.Text)
updateBackendConfigResponse_error = Lens.lens (\UpdateBackendConfigResponse' {error} -> error) (\s@UpdateBackendConfigResponse' {} a -> s {error = a} :: UpdateBackendConfigResponse)

-- | Describes the Amazon Cognito configurations for the Admin UI auth
-- resource to log in with.
updateBackendConfigResponse_loginAuthConfig :: Lens.Lens' UpdateBackendConfigResponse (Prelude.Maybe LoginAuthConfigReqObj)
updateBackendConfigResponse_loginAuthConfig = Lens.lens (\UpdateBackendConfigResponse' {loginAuthConfig} -> loginAuthConfig) (\s@UpdateBackendConfigResponse' {} a -> s {loginAuthConfig = a} :: UpdateBackendConfigResponse)

-- | The response's http status code.
updateBackendConfigResponse_httpStatus :: Lens.Lens' UpdateBackendConfigResponse Prelude.Int
updateBackendConfigResponse_httpStatus = Lens.lens (\UpdateBackendConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateBackendConfigResponse' {} a -> s {httpStatus = a} :: UpdateBackendConfigResponse)

instance Prelude.NFData UpdateBackendConfigResponse where
  rnf UpdateBackendConfigResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendManagerAppId
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf loginAuthConfig
      `Prelude.seq` Prelude.rnf httpStatus
