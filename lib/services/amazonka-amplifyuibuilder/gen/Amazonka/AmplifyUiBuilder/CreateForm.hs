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
-- Module      : Amazonka.AmplifyUiBuilder.CreateForm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new form for an Amplify app.
module Amazonka.AmplifyUiBuilder.CreateForm
  ( -- * Creating a Request
    CreateForm (..),
    newCreateForm,

    -- * Request Lenses
    createForm_clientToken,
    createForm_appId,
    createForm_environmentName,
    createForm_formToCreate,

    -- * Destructuring the Response
    CreateFormResponse (..),
    newCreateFormResponse,

    -- * Response Lenses
    createFormResponse_entity,
    createFormResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateForm' smart constructor.
data CreateForm = CreateForm'
  { -- | The unique client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the Amplify app to associate with the form.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | Represents the configuration of the form to create.
    formToCreate :: CreateFormData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createForm_clientToken' - The unique client token.
--
-- 'appId', 'createForm_appId' - The unique ID of the Amplify app to associate with the form.
--
-- 'environmentName', 'createForm_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'formToCreate', 'createForm_formToCreate' - Represents the configuration of the form to create.
newCreateForm ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'formToCreate'
  CreateFormData ->
  CreateForm
newCreateForm
  pAppId_
  pEnvironmentName_
  pFormToCreate_ =
    CreateForm'
      { clientToken = Prelude.Nothing,
        appId = pAppId_,
        environmentName = pEnvironmentName_,
        formToCreate = pFormToCreate_
      }

-- | The unique client token.
createForm_clientToken :: Lens.Lens' CreateForm (Prelude.Maybe Prelude.Text)
createForm_clientToken = Lens.lens (\CreateForm' {clientToken} -> clientToken) (\s@CreateForm' {} a -> s {clientToken = a} :: CreateForm)

-- | The unique ID of the Amplify app to associate with the form.
createForm_appId :: Lens.Lens' CreateForm Prelude.Text
createForm_appId = Lens.lens (\CreateForm' {appId} -> appId) (\s@CreateForm' {} a -> s {appId = a} :: CreateForm)

-- | The name of the backend environment that is a part of the Amplify app.
createForm_environmentName :: Lens.Lens' CreateForm Prelude.Text
createForm_environmentName = Lens.lens (\CreateForm' {environmentName} -> environmentName) (\s@CreateForm' {} a -> s {environmentName = a} :: CreateForm)

-- | Represents the configuration of the form to create.
createForm_formToCreate :: Lens.Lens' CreateForm CreateFormData
createForm_formToCreate = Lens.lens (\CreateForm' {formToCreate} -> formToCreate) (\s@CreateForm' {} a -> s {formToCreate = a} :: CreateForm)

instance Core.AWSRequest CreateForm where
  type AWSResponse CreateForm = CreateFormResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFormResponse'
            Prelude.<$> (Core.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateForm where
  hashWithSalt _salt CreateForm' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` formToCreate

instance Prelude.NFData CreateForm where
  rnf CreateForm' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf formToCreate

instance Core.ToHeaders CreateForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateForm where
  toJSON CreateForm' {..} = Core.toJSON formToCreate

instance Core.ToPath CreateForm where
  toPath CreateForm' {..} =
    Prelude.mconcat
      [ "/app/",
        Core.toBS appId,
        "/environment/",
        Core.toBS environmentName,
        "/forms"
      ]

instance Core.ToQuery CreateForm where
  toQuery CreateForm' {..} =
    Prelude.mconcat ["clientToken" Core.=: clientToken]

-- | /See:/ 'newCreateFormResponse' smart constructor.
data CreateFormResponse = CreateFormResponse'
  { -- | Describes the configuration of the new form.
    entity :: Prelude.Maybe Form,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entity', 'createFormResponse_entity' - Describes the configuration of the new form.
--
-- 'httpStatus', 'createFormResponse_httpStatus' - The response's http status code.
newCreateFormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFormResponse
newCreateFormResponse pHttpStatus_ =
  CreateFormResponse'
    { entity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the configuration of the new form.
createFormResponse_entity :: Lens.Lens' CreateFormResponse (Prelude.Maybe Form)
createFormResponse_entity = Lens.lens (\CreateFormResponse' {entity} -> entity) (\s@CreateFormResponse' {} a -> s {entity = a} :: CreateFormResponse)

-- | The response's http status code.
createFormResponse_httpStatus :: Lens.Lens' CreateFormResponse Prelude.Int
createFormResponse_httpStatus = Lens.lens (\CreateFormResponse' {httpStatus} -> httpStatus) (\s@CreateFormResponse' {} a -> s {httpStatus = a} :: CreateFormResponse)

instance Prelude.NFData CreateFormResponse where
  rnf CreateFormResponse' {..} =
    Prelude.rnf entity
      `Prelude.seq` Prelude.rnf httpStatus
