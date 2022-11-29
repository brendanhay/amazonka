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
-- Module      : Amazonka.AmplifyUiBuilder.GetComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an existing component for an Amplify app.
module Amazonka.AmplifyUiBuilder.GetComponent
  ( -- * Creating a Request
    GetComponent (..),
    newGetComponent,

    -- * Request Lenses
    getComponent_appId,
    getComponent_environmentName,
    getComponent_id,

    -- * Destructuring the Response
    GetComponentResponse (..),
    newGetComponentResponse,

    -- * Response Lenses
    getComponentResponse_component,
    getComponentResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComponent' smart constructor.
data GetComponent = GetComponent'
  { -- | The unique ID of the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID of the component.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getComponent_appId' - The unique ID of the Amplify app.
--
-- 'environmentName', 'getComponent_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'id', 'getComponent_id' - The unique ID of the component.
newGetComponent ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetComponent
newGetComponent pAppId_ pEnvironmentName_ pId_ =
  GetComponent'
    { appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_
    }

-- | The unique ID of the Amplify app.
getComponent_appId :: Lens.Lens' GetComponent Prelude.Text
getComponent_appId = Lens.lens (\GetComponent' {appId} -> appId) (\s@GetComponent' {} a -> s {appId = a} :: GetComponent)

-- | The name of the backend environment that is part of the Amplify app.
getComponent_environmentName :: Lens.Lens' GetComponent Prelude.Text
getComponent_environmentName = Lens.lens (\GetComponent' {environmentName} -> environmentName) (\s@GetComponent' {} a -> s {environmentName = a} :: GetComponent)

-- | The unique ID of the component.
getComponent_id :: Lens.Lens' GetComponent Prelude.Text
getComponent_id = Lens.lens (\GetComponent' {id} -> id) (\s@GetComponent' {} a -> s {id = a} :: GetComponent)

instance Core.AWSRequest GetComponent where
  type AWSResponse GetComponent = GetComponentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComponentResponse'
            Prelude.<$> (Core.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComponent where
  hashWithSalt _salt GetComponent' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetComponent where
  rnf GetComponent' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders GetComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetComponent where
  toPath GetComponent' {..} =
    Prelude.mconcat
      [ "/app/",
        Core.toBS appId,
        "/environment/",
        Core.toBS environmentName,
        "/components/",
        Core.toBS id
      ]

instance Core.ToQuery GetComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetComponentResponse' smart constructor.
data GetComponentResponse = GetComponentResponse'
  { -- | Represents the configuration settings for the component.
    component :: Prelude.Maybe Component,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'component', 'getComponentResponse_component' - Represents the configuration settings for the component.
--
-- 'httpStatus', 'getComponentResponse_httpStatus' - The response's http status code.
newGetComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComponentResponse
newGetComponentResponse pHttpStatus_ =
  GetComponentResponse'
    { component = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the configuration settings for the component.
getComponentResponse_component :: Lens.Lens' GetComponentResponse (Prelude.Maybe Component)
getComponentResponse_component = Lens.lens (\GetComponentResponse' {component} -> component) (\s@GetComponentResponse' {} a -> s {component = a} :: GetComponentResponse)

-- | The response's http status code.
getComponentResponse_httpStatus :: Lens.Lens' GetComponentResponse Prelude.Int
getComponentResponse_httpStatus = Lens.lens (\GetComponentResponse' {httpStatus} -> httpStatus) (\s@GetComponentResponse' {} a -> s {httpStatus = a} :: GetComponentResponse)

instance Prelude.NFData GetComponentResponse where
  rnf GetComponentResponse' {..} =
    Prelude.rnf component
      `Prelude.seq` Prelude.rnf httpStatus
