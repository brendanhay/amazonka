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
-- Module      : Amazonka.AmplifyUiBuilder.CreateComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new component for an Amplify app.
module Amazonka.AmplifyUiBuilder.CreateComponent
  ( -- * Creating a Request
    CreateComponent (..),
    newCreateComponent,

    -- * Request Lenses
    createComponent_clientToken,
    createComponent_appId,
    createComponent_componentToCreate,
    createComponent_environmentName,

    -- * Destructuring the Response
    CreateComponentResponse (..),
    newCreateComponentResponse,

    -- * Response Lenses
    createComponentResponse_entity,
    createComponentResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateComponent' smart constructor.
data CreateComponent = CreateComponent'
  { -- | The unique client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the Amplify app to associate with the component.
    appId :: Prelude.Text,
    -- | Represents the configuration of the component to create.
    componentToCreate :: CreateComponentData,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createComponent_clientToken' - The unique client token.
--
-- 'appId', 'createComponent_appId' - The unique ID of the Amplify app to associate with the component.
--
-- 'componentToCreate', 'createComponent_componentToCreate' - Represents the configuration of the component to create.
--
-- 'environmentName', 'createComponent_environmentName' - The name of the backend environment that is a part of the Amplify app.
newCreateComponent ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'componentToCreate'
  CreateComponentData ->
  -- | 'environmentName'
  Prelude.Text ->
  CreateComponent
newCreateComponent
  pAppId_
  pComponentToCreate_
  pEnvironmentName_ =
    CreateComponent'
      { clientToken = Prelude.Nothing,
        appId = pAppId_,
        componentToCreate = pComponentToCreate_,
        environmentName = pEnvironmentName_
      }

-- | The unique client token.
createComponent_clientToken :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_clientToken = Lens.lens (\CreateComponent' {clientToken} -> clientToken) (\s@CreateComponent' {} a -> s {clientToken = a} :: CreateComponent)

-- | The unique ID of the Amplify app to associate with the component.
createComponent_appId :: Lens.Lens' CreateComponent Prelude.Text
createComponent_appId = Lens.lens (\CreateComponent' {appId} -> appId) (\s@CreateComponent' {} a -> s {appId = a} :: CreateComponent)

-- | Represents the configuration of the component to create.
createComponent_componentToCreate :: Lens.Lens' CreateComponent CreateComponentData
createComponent_componentToCreate = Lens.lens (\CreateComponent' {componentToCreate} -> componentToCreate) (\s@CreateComponent' {} a -> s {componentToCreate = a} :: CreateComponent)

-- | The name of the backend environment that is a part of the Amplify app.
createComponent_environmentName :: Lens.Lens' CreateComponent Prelude.Text
createComponent_environmentName = Lens.lens (\CreateComponent' {environmentName} -> environmentName) (\s@CreateComponent' {} a -> s {environmentName = a} :: CreateComponent)

instance Core.AWSRequest CreateComponent where
  type
    AWSResponse CreateComponent =
      CreateComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateComponentResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateComponent where
  hashWithSalt _salt CreateComponent' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` componentToCreate
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData CreateComponent where
  rnf CreateComponent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf componentToCreate
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders CreateComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateComponent where
  toJSON CreateComponent' {..} =
    Data.toJSON componentToCreate

instance Data.ToPath CreateComponent where
  toPath CreateComponent' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/components"
      ]

instance Data.ToQuery CreateComponent where
  toQuery CreateComponent' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newCreateComponentResponse' smart constructor.
data CreateComponentResponse = CreateComponentResponse'
  { -- | Describes the configuration of the new component.
    entity :: Prelude.Maybe Component,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entity', 'createComponentResponse_entity' - Describes the configuration of the new component.
--
-- 'httpStatus', 'createComponentResponse_httpStatus' - The response's http status code.
newCreateComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateComponentResponse
newCreateComponentResponse pHttpStatus_ =
  CreateComponentResponse'
    { entity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the configuration of the new component.
createComponentResponse_entity :: Lens.Lens' CreateComponentResponse (Prelude.Maybe Component)
createComponentResponse_entity = Lens.lens (\CreateComponentResponse' {entity} -> entity) (\s@CreateComponentResponse' {} a -> s {entity = a} :: CreateComponentResponse)

-- | The response's http status code.
createComponentResponse_httpStatus :: Lens.Lens' CreateComponentResponse Prelude.Int
createComponentResponse_httpStatus = Lens.lens (\CreateComponentResponse' {httpStatus} -> httpStatus) (\s@CreateComponentResponse' {} a -> s {httpStatus = a} :: CreateComponentResponse)

instance Prelude.NFData CreateComponentResponse where
  rnf CreateComponentResponse' {..} =
    Prelude.rnf entity
      `Prelude.seq` Prelude.rnf httpStatus
