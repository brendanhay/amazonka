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
-- Module      : Amazonka.AmplifyUiBuilder.UpdateComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing component.
module Amazonka.AmplifyUiBuilder.UpdateComponent
  ( -- * Creating a Request
    UpdateComponent (..),
    newUpdateComponent,

    -- * Request Lenses
    updateComponent_clientToken,
    updateComponent_appId,
    updateComponent_environmentName,
    updateComponent_id,
    updateComponent_updatedComponent,

    -- * Destructuring the Response
    UpdateComponentResponse (..),
    newUpdateComponentResponse,

    -- * Response Lenses
    updateComponentResponse_entity,
    updateComponentResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateComponent' smart constructor.
data UpdateComponent = UpdateComponent'
  { -- | The unique client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID for the component.
    id :: Prelude.Text,
    -- | The configuration of the updated component.
    updatedComponent :: UpdateComponentData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateComponent_clientToken' - The unique client token.
--
-- 'appId', 'updateComponent_appId' - The unique ID for the Amplify app.
--
-- 'environmentName', 'updateComponent_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'id', 'updateComponent_id' - The unique ID for the component.
--
-- 'updatedComponent', 'updateComponent_updatedComponent' - The configuration of the updated component.
newUpdateComponent ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'updatedComponent'
  UpdateComponentData ->
  UpdateComponent
newUpdateComponent
  pAppId_
  pEnvironmentName_
  pId_
  pUpdatedComponent_ =
    UpdateComponent'
      { clientToken = Prelude.Nothing,
        appId = pAppId_,
        environmentName = pEnvironmentName_,
        id = pId_,
        updatedComponent = pUpdatedComponent_
      }

-- | The unique client token.
updateComponent_clientToken :: Lens.Lens' UpdateComponent (Prelude.Maybe Prelude.Text)
updateComponent_clientToken = Lens.lens (\UpdateComponent' {clientToken} -> clientToken) (\s@UpdateComponent' {} a -> s {clientToken = a} :: UpdateComponent)

-- | The unique ID for the Amplify app.
updateComponent_appId :: Lens.Lens' UpdateComponent Prelude.Text
updateComponent_appId = Lens.lens (\UpdateComponent' {appId} -> appId) (\s@UpdateComponent' {} a -> s {appId = a} :: UpdateComponent)

-- | The name of the backend environment that is part of the Amplify app.
updateComponent_environmentName :: Lens.Lens' UpdateComponent Prelude.Text
updateComponent_environmentName = Lens.lens (\UpdateComponent' {environmentName} -> environmentName) (\s@UpdateComponent' {} a -> s {environmentName = a} :: UpdateComponent)

-- | The unique ID for the component.
updateComponent_id :: Lens.Lens' UpdateComponent Prelude.Text
updateComponent_id = Lens.lens (\UpdateComponent' {id} -> id) (\s@UpdateComponent' {} a -> s {id = a} :: UpdateComponent)

-- | The configuration of the updated component.
updateComponent_updatedComponent :: Lens.Lens' UpdateComponent UpdateComponentData
updateComponent_updatedComponent = Lens.lens (\UpdateComponent' {updatedComponent} -> updatedComponent) (\s@UpdateComponent' {} a -> s {updatedComponent = a} :: UpdateComponent)

instance Core.AWSRequest UpdateComponent where
  type
    AWSResponse UpdateComponent =
      UpdateComponentResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateComponentResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateComponent where
  hashWithSalt _salt UpdateComponent' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` updatedComponent

instance Prelude.NFData UpdateComponent where
  rnf UpdateComponent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf updatedComponent

instance Data.ToHeaders UpdateComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateComponent where
  toJSON UpdateComponent' {..} =
    Data.toJSON updatedComponent

instance Data.ToPath UpdateComponent where
  toPath UpdateComponent' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/components/",
        Data.toBS id
      ]

instance Data.ToQuery UpdateComponent where
  toQuery UpdateComponent' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newUpdateComponentResponse' smart constructor.
data UpdateComponentResponse = UpdateComponentResponse'
  { -- | Describes the configuration of the updated component.
    entity :: Prelude.Maybe Component,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entity', 'updateComponentResponse_entity' - Describes the configuration of the updated component.
--
-- 'httpStatus', 'updateComponentResponse_httpStatus' - The response's http status code.
newUpdateComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateComponentResponse
newUpdateComponentResponse pHttpStatus_ =
  UpdateComponentResponse'
    { entity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the configuration of the updated component.
updateComponentResponse_entity :: Lens.Lens' UpdateComponentResponse (Prelude.Maybe Component)
updateComponentResponse_entity = Lens.lens (\UpdateComponentResponse' {entity} -> entity) (\s@UpdateComponentResponse' {} a -> s {entity = a} :: UpdateComponentResponse)

-- | The response's http status code.
updateComponentResponse_httpStatus :: Lens.Lens' UpdateComponentResponse Prelude.Int
updateComponentResponse_httpStatus = Lens.lens (\UpdateComponentResponse' {httpStatus} -> httpStatus) (\s@UpdateComponentResponse' {} a -> s {httpStatus = a} :: UpdateComponentResponse)

instance Prelude.NFData UpdateComponentResponse where
  rnf UpdateComponentResponse' {..} =
    Prelude.rnf entity
      `Prelude.seq` Prelude.rnf httpStatus
