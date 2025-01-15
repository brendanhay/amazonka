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
-- Module      : Amazonka.Amplify.GetBackendEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a backend environment for an Amplify app.
module Amazonka.Amplify.GetBackendEnvironment
  ( -- * Creating a Request
    GetBackendEnvironment (..),
    newGetBackendEnvironment,

    -- * Request Lenses
    getBackendEnvironment_appId,
    getBackendEnvironment_environmentName,

    -- * Destructuring the Response
    GetBackendEnvironmentResponse (..),
    newGetBackendEnvironmentResponse,

    -- * Response Lenses
    getBackendEnvironmentResponse_httpStatus,
    getBackendEnvironmentResponse_backendEnvironment,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the get backend environment request.
--
-- /See:/ 'newGetBackendEnvironment' smart constructor.
data GetBackendEnvironment = GetBackendEnvironment'
  { -- | The unique id for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the backend environment.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getBackendEnvironment_appId' - The unique id for an Amplify app.
--
-- 'environmentName', 'getBackendEnvironment_environmentName' - The name for the backend environment.
newGetBackendEnvironment ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  GetBackendEnvironment
newGetBackendEnvironment pAppId_ pEnvironmentName_ =
  GetBackendEnvironment'
    { appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The unique id for an Amplify app.
getBackendEnvironment_appId :: Lens.Lens' GetBackendEnvironment Prelude.Text
getBackendEnvironment_appId = Lens.lens (\GetBackendEnvironment' {appId} -> appId) (\s@GetBackendEnvironment' {} a -> s {appId = a} :: GetBackendEnvironment)

-- | The name for the backend environment.
getBackendEnvironment_environmentName :: Lens.Lens' GetBackendEnvironment Prelude.Text
getBackendEnvironment_environmentName = Lens.lens (\GetBackendEnvironment' {environmentName} -> environmentName) (\s@GetBackendEnvironment' {} a -> s {environmentName = a} :: GetBackendEnvironment)

instance Core.AWSRequest GetBackendEnvironment where
  type
    AWSResponse GetBackendEnvironment =
      GetBackendEnvironmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackendEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "backendEnvironment")
      )

instance Prelude.Hashable GetBackendEnvironment where
  hashWithSalt _salt GetBackendEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData GetBackendEnvironment where
  rnf GetBackendEnvironment' {..} =
    Prelude.rnf appId `Prelude.seq`
      Prelude.rnf environmentName

instance Data.ToHeaders GetBackendEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBackendEnvironment where
  toPath GetBackendEnvironment' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/backendenvironments/",
        Data.toBS environmentName
      ]

instance Data.ToQuery GetBackendEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the get backend environment result.
--
-- /See:/ 'newGetBackendEnvironmentResponse' smart constructor.
data GetBackendEnvironmentResponse = GetBackendEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes the backend environment for an Amplify app.
    backendEnvironment :: BackendEnvironment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBackendEnvironmentResponse_httpStatus' - The response's http status code.
--
-- 'backendEnvironment', 'getBackendEnvironmentResponse_backendEnvironment' - Describes the backend environment for an Amplify app.
newGetBackendEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'backendEnvironment'
  BackendEnvironment ->
  GetBackendEnvironmentResponse
newGetBackendEnvironmentResponse
  pHttpStatus_
  pBackendEnvironment_ =
    GetBackendEnvironmentResponse'
      { httpStatus =
          pHttpStatus_,
        backendEnvironment = pBackendEnvironment_
      }

-- | The response's http status code.
getBackendEnvironmentResponse_httpStatus :: Lens.Lens' GetBackendEnvironmentResponse Prelude.Int
getBackendEnvironmentResponse_httpStatus = Lens.lens (\GetBackendEnvironmentResponse' {httpStatus} -> httpStatus) (\s@GetBackendEnvironmentResponse' {} a -> s {httpStatus = a} :: GetBackendEnvironmentResponse)

-- | Describes the backend environment for an Amplify app.
getBackendEnvironmentResponse_backendEnvironment :: Lens.Lens' GetBackendEnvironmentResponse BackendEnvironment
getBackendEnvironmentResponse_backendEnvironment = Lens.lens (\GetBackendEnvironmentResponse' {backendEnvironment} -> backendEnvironment) (\s@GetBackendEnvironmentResponse' {} a -> s {backendEnvironment = a} :: GetBackendEnvironmentResponse)

instance Prelude.NFData GetBackendEnvironmentResponse where
  rnf GetBackendEnvironmentResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf backendEnvironment
