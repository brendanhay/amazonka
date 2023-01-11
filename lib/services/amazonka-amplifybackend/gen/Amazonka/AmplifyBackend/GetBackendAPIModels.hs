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
-- Module      : Amazonka.AmplifyBackend.GetBackendAPIModels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a model introspection schema for an existing backend API resource.
module Amazonka.AmplifyBackend.GetBackendAPIModels
  ( -- * Creating a Request
    GetBackendAPIModels (..),
    newGetBackendAPIModels,

    -- * Request Lenses
    getBackendAPIModels_appId,
    getBackendAPIModels_backendEnvironmentName,
    getBackendAPIModels_resourceName,

    -- * Destructuring the Response
    GetBackendAPIModelsResponse (..),
    newGetBackendAPIModelsResponse,

    -- * Response Lenses
    getBackendAPIModelsResponse_modelIntrospectionSchema,
    getBackendAPIModelsResponse_models,
    getBackendAPIModelsResponse_status,
    getBackendAPIModelsResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for GetBackendAPIModels.
--
-- /See:/ 'newGetBackendAPIModels' smart constructor.
data GetBackendAPIModels = GetBackendAPIModels'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of this resource.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendAPIModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getBackendAPIModels_appId' - The app ID.
--
-- 'backendEnvironmentName', 'getBackendAPIModels_backendEnvironmentName' - The name of the backend environment.
--
-- 'resourceName', 'getBackendAPIModels_resourceName' - The name of this resource.
newGetBackendAPIModels ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'resourceName'
  Prelude.Text ->
  GetBackendAPIModels
newGetBackendAPIModels
  pAppId_
  pBackendEnvironmentName_
  pResourceName_ =
    GetBackendAPIModels'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        resourceName = pResourceName_
      }

-- | The app ID.
getBackendAPIModels_appId :: Lens.Lens' GetBackendAPIModels Prelude.Text
getBackendAPIModels_appId = Lens.lens (\GetBackendAPIModels' {appId} -> appId) (\s@GetBackendAPIModels' {} a -> s {appId = a} :: GetBackendAPIModels)

-- | The name of the backend environment.
getBackendAPIModels_backendEnvironmentName :: Lens.Lens' GetBackendAPIModels Prelude.Text
getBackendAPIModels_backendEnvironmentName = Lens.lens (\GetBackendAPIModels' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendAPIModels' {} a -> s {backendEnvironmentName = a} :: GetBackendAPIModels)

-- | The name of this resource.
getBackendAPIModels_resourceName :: Lens.Lens' GetBackendAPIModels Prelude.Text
getBackendAPIModels_resourceName = Lens.lens (\GetBackendAPIModels' {resourceName} -> resourceName) (\s@GetBackendAPIModels' {} a -> s {resourceName = a} :: GetBackendAPIModels)

instance Core.AWSRequest GetBackendAPIModels where
  type
    AWSResponse GetBackendAPIModels =
      GetBackendAPIModelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackendAPIModelsResponse'
            Prelude.<$> (x Data..?> "modelIntrospectionSchema")
            Prelude.<*> (x Data..?> "models")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackendAPIModels where
  hashWithSalt _salt GetBackendAPIModels' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData GetBackendAPIModels where
  rnf GetBackendAPIModels' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf resourceName

instance Data.ToHeaders GetBackendAPIModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBackendAPIModels where
  toJSON GetBackendAPIModels' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceName" Data..= resourceName)]
      )

instance Data.ToPath GetBackendAPIModels where
  toPath GetBackendAPIModels' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/api/",
        Data.toBS backendEnvironmentName,
        "/getModels"
      ]

instance Data.ToQuery GetBackendAPIModels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackendAPIModelsResponse' smart constructor.
data GetBackendAPIModelsResponse = GetBackendAPIModelsResponse'
  { -- | Stringified JSON of the model introspection schema for an existing
    -- backend API resource.
    modelIntrospectionSchema :: Prelude.Maybe Prelude.Text,
    -- | Stringified JSON of the datastore model.
    models :: Prelude.Maybe Prelude.Text,
    -- | The current status of the request.
    status :: Prelude.Maybe Status,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendAPIModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelIntrospectionSchema', 'getBackendAPIModelsResponse_modelIntrospectionSchema' - Stringified JSON of the model introspection schema for an existing
-- backend API resource.
--
-- 'models', 'getBackendAPIModelsResponse_models' - Stringified JSON of the datastore model.
--
-- 'status', 'getBackendAPIModelsResponse_status' - The current status of the request.
--
-- 'httpStatus', 'getBackendAPIModelsResponse_httpStatus' - The response's http status code.
newGetBackendAPIModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackendAPIModelsResponse
newGetBackendAPIModelsResponse pHttpStatus_ =
  GetBackendAPIModelsResponse'
    { modelIntrospectionSchema =
        Prelude.Nothing,
      models = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Stringified JSON of the model introspection schema for an existing
-- backend API resource.
getBackendAPIModelsResponse_modelIntrospectionSchema :: Lens.Lens' GetBackendAPIModelsResponse (Prelude.Maybe Prelude.Text)
getBackendAPIModelsResponse_modelIntrospectionSchema = Lens.lens (\GetBackendAPIModelsResponse' {modelIntrospectionSchema} -> modelIntrospectionSchema) (\s@GetBackendAPIModelsResponse' {} a -> s {modelIntrospectionSchema = a} :: GetBackendAPIModelsResponse)

-- | Stringified JSON of the datastore model.
getBackendAPIModelsResponse_models :: Lens.Lens' GetBackendAPIModelsResponse (Prelude.Maybe Prelude.Text)
getBackendAPIModelsResponse_models = Lens.lens (\GetBackendAPIModelsResponse' {models} -> models) (\s@GetBackendAPIModelsResponse' {} a -> s {models = a} :: GetBackendAPIModelsResponse)

-- | The current status of the request.
getBackendAPIModelsResponse_status :: Lens.Lens' GetBackendAPIModelsResponse (Prelude.Maybe Status)
getBackendAPIModelsResponse_status = Lens.lens (\GetBackendAPIModelsResponse' {status} -> status) (\s@GetBackendAPIModelsResponse' {} a -> s {status = a} :: GetBackendAPIModelsResponse)

-- | The response's http status code.
getBackendAPIModelsResponse_httpStatus :: Lens.Lens' GetBackendAPIModelsResponse Prelude.Int
getBackendAPIModelsResponse_httpStatus = Lens.lens (\GetBackendAPIModelsResponse' {httpStatus} -> httpStatus) (\s@GetBackendAPIModelsResponse' {} a -> s {httpStatus = a} :: GetBackendAPIModelsResponse)

instance Prelude.NFData GetBackendAPIModelsResponse where
  rnf GetBackendAPIModelsResponse' {..} =
    Prelude.rnf modelIntrospectionSchema
      `Prelude.seq` Prelude.rnf models
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
