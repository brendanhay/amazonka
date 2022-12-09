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
-- Module      : Amazonka.AmplifyBackend.CreateBackendConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a config object for a backend.
module Amazonka.AmplifyBackend.CreateBackendConfig
  ( -- * Creating a Request
    CreateBackendConfig (..),
    newCreateBackendConfig,

    -- * Request Lenses
    createBackendConfig_backendManagerAppId,
    createBackendConfig_appId,

    -- * Destructuring the Response
    CreateBackendConfigResponse (..),
    newCreateBackendConfigResponse,

    -- * Response Lenses
    createBackendConfigResponse_appId,
    createBackendConfigResponse_backendEnvironmentName,
    createBackendConfigResponse_jobId,
    createBackendConfigResponse_status,
    createBackendConfigResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for CreateBackendConfig.
--
-- /See:/ 'newCreateBackendConfig' smart constructor.
data CreateBackendConfig = CreateBackendConfig'
  { -- | The app ID for the backend manager.
    backendManagerAppId :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backendManagerAppId', 'createBackendConfig_backendManagerAppId' - The app ID for the backend manager.
--
-- 'appId', 'createBackendConfig_appId' - The app ID.
newCreateBackendConfig ::
  -- | 'appId'
  Prelude.Text ->
  CreateBackendConfig
newCreateBackendConfig pAppId_ =
  CreateBackendConfig'
    { backendManagerAppId =
        Prelude.Nothing,
      appId = pAppId_
    }

-- | The app ID for the backend manager.
createBackendConfig_backendManagerAppId :: Lens.Lens' CreateBackendConfig (Prelude.Maybe Prelude.Text)
createBackendConfig_backendManagerAppId = Lens.lens (\CreateBackendConfig' {backendManagerAppId} -> backendManagerAppId) (\s@CreateBackendConfig' {} a -> s {backendManagerAppId = a} :: CreateBackendConfig)

-- | The app ID.
createBackendConfig_appId :: Lens.Lens' CreateBackendConfig Prelude.Text
createBackendConfig_appId = Lens.lens (\CreateBackendConfig' {appId} -> appId) (\s@CreateBackendConfig' {} a -> s {appId = a} :: CreateBackendConfig)

instance Core.AWSRequest CreateBackendConfig where
  type
    AWSResponse CreateBackendConfig =
      CreateBackendConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackendConfigResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackendConfig where
  hashWithSalt _salt CreateBackendConfig' {..} =
    _salt `Prelude.hashWithSalt` backendManagerAppId
      `Prelude.hashWithSalt` appId

instance Prelude.NFData CreateBackendConfig where
  rnf CreateBackendConfig' {..} =
    Prelude.rnf backendManagerAppId
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders CreateBackendConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackendConfig where
  toJSON CreateBackendConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("backendManagerAppId" Data..=)
              Prelude.<$> backendManagerAppId
          ]
      )

instance Data.ToPath CreateBackendConfig where
  toPath CreateBackendConfig' {..} =
    Prelude.mconcat
      ["/backend/", Data.toBS appId, "/config"]

instance Data.ToQuery CreateBackendConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackendConfigResponse' smart constructor.
data CreateBackendConfigResponse = CreateBackendConfigResponse'
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
-- Create a value of 'CreateBackendConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createBackendConfigResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'createBackendConfigResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'jobId', 'createBackendConfigResponse_jobId' - The ID for the job.
--
-- 'status', 'createBackendConfigResponse_status' - The current status of the request.
--
-- 'httpStatus', 'createBackendConfigResponse_httpStatus' - The response's http status code.
newCreateBackendConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackendConfigResponse
newCreateBackendConfigResponse pHttpStatus_ =
  CreateBackendConfigResponse'
    { appId =
        Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
createBackendConfigResponse_appId :: Lens.Lens' CreateBackendConfigResponse (Prelude.Maybe Prelude.Text)
createBackendConfigResponse_appId = Lens.lens (\CreateBackendConfigResponse' {appId} -> appId) (\s@CreateBackendConfigResponse' {} a -> s {appId = a} :: CreateBackendConfigResponse)

-- | The name of the backend environment.
createBackendConfigResponse_backendEnvironmentName :: Lens.Lens' CreateBackendConfigResponse (Prelude.Maybe Prelude.Text)
createBackendConfigResponse_backendEnvironmentName = Lens.lens (\CreateBackendConfigResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@CreateBackendConfigResponse' {} a -> s {backendEnvironmentName = a} :: CreateBackendConfigResponse)

-- | The ID for the job.
createBackendConfigResponse_jobId :: Lens.Lens' CreateBackendConfigResponse (Prelude.Maybe Prelude.Text)
createBackendConfigResponse_jobId = Lens.lens (\CreateBackendConfigResponse' {jobId} -> jobId) (\s@CreateBackendConfigResponse' {} a -> s {jobId = a} :: CreateBackendConfigResponse)

-- | The current status of the request.
createBackendConfigResponse_status :: Lens.Lens' CreateBackendConfigResponse (Prelude.Maybe Prelude.Text)
createBackendConfigResponse_status = Lens.lens (\CreateBackendConfigResponse' {status} -> status) (\s@CreateBackendConfigResponse' {} a -> s {status = a} :: CreateBackendConfigResponse)

-- | The response's http status code.
createBackendConfigResponse_httpStatus :: Lens.Lens' CreateBackendConfigResponse Prelude.Int
createBackendConfigResponse_httpStatus = Lens.lens (\CreateBackendConfigResponse' {httpStatus} -> httpStatus) (\s@CreateBackendConfigResponse' {} a -> s {httpStatus = a} :: CreateBackendConfigResponse)

instance Prelude.NFData CreateBackendConfigResponse where
  rnf CreateBackendConfigResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
