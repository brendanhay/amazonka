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
-- Module      : Amazonka.AmplifyUiBuilder.UpdateForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing form.
module Amazonka.AmplifyUiBuilder.UpdateForm
  ( -- * Creating a Request
    UpdateForm (..),
    newUpdateForm,

    -- * Request Lenses
    updateForm_clientToken,
    updateForm_appId,
    updateForm_environmentName,
    updateForm_id,
    updateForm_updatedForm,

    -- * Destructuring the Response
    UpdateFormResponse (..),
    newUpdateFormResponse,

    -- * Response Lenses
    updateFormResponse_entity,
    updateFormResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateForm' smart constructor.
data UpdateForm = UpdateForm'
  { -- | The unique client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID for the form.
    id :: Prelude.Text,
    -- | The request accepts the following data in JSON format.
    updatedForm :: UpdateFormData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateForm_clientToken' - The unique client token.
--
-- 'appId', 'updateForm_appId' - The unique ID for the Amplify app.
--
-- 'environmentName', 'updateForm_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'id', 'updateForm_id' - The unique ID for the form.
--
-- 'updatedForm', 'updateForm_updatedForm' - The request accepts the following data in JSON format.
newUpdateForm ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'updatedForm'
  UpdateFormData ->
  UpdateForm
newUpdateForm
  pAppId_
  pEnvironmentName_
  pId_
  pUpdatedForm_ =
    UpdateForm'
      { clientToken = Prelude.Nothing,
        appId = pAppId_,
        environmentName = pEnvironmentName_,
        id = pId_,
        updatedForm = pUpdatedForm_
      }

-- | The unique client token.
updateForm_clientToken :: Lens.Lens' UpdateForm (Prelude.Maybe Prelude.Text)
updateForm_clientToken = Lens.lens (\UpdateForm' {clientToken} -> clientToken) (\s@UpdateForm' {} a -> s {clientToken = a} :: UpdateForm)

-- | The unique ID for the Amplify app.
updateForm_appId :: Lens.Lens' UpdateForm Prelude.Text
updateForm_appId = Lens.lens (\UpdateForm' {appId} -> appId) (\s@UpdateForm' {} a -> s {appId = a} :: UpdateForm)

-- | The name of the backend environment that is part of the Amplify app.
updateForm_environmentName :: Lens.Lens' UpdateForm Prelude.Text
updateForm_environmentName = Lens.lens (\UpdateForm' {environmentName} -> environmentName) (\s@UpdateForm' {} a -> s {environmentName = a} :: UpdateForm)

-- | The unique ID for the form.
updateForm_id :: Lens.Lens' UpdateForm Prelude.Text
updateForm_id = Lens.lens (\UpdateForm' {id} -> id) (\s@UpdateForm' {} a -> s {id = a} :: UpdateForm)

-- | The request accepts the following data in JSON format.
updateForm_updatedForm :: Lens.Lens' UpdateForm UpdateFormData
updateForm_updatedForm = Lens.lens (\UpdateForm' {updatedForm} -> updatedForm) (\s@UpdateForm' {} a -> s {updatedForm = a} :: UpdateForm)

instance Core.AWSRequest UpdateForm where
  type AWSResponse UpdateForm = UpdateFormResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFormResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateForm where
  hashWithSalt _salt UpdateForm' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` updatedForm

instance Prelude.NFData UpdateForm where
  rnf UpdateForm' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf appId `Prelude.seq`
        Prelude.rnf environmentName `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf updatedForm

instance Data.ToHeaders UpdateForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateForm where
  toJSON UpdateForm' {..} = Data.toJSON updatedForm

instance Data.ToPath UpdateForm where
  toPath UpdateForm' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/forms/",
        Data.toBS id
      ]

instance Data.ToQuery UpdateForm where
  toQuery UpdateForm' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newUpdateFormResponse' smart constructor.
data UpdateFormResponse = UpdateFormResponse'
  { -- | Describes the configuration of the updated form.
    entity :: Prelude.Maybe Form,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entity', 'updateFormResponse_entity' - Describes the configuration of the updated form.
--
-- 'httpStatus', 'updateFormResponse_httpStatus' - The response's http status code.
newUpdateFormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFormResponse
newUpdateFormResponse pHttpStatus_ =
  UpdateFormResponse'
    { entity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the configuration of the updated form.
updateFormResponse_entity :: Lens.Lens' UpdateFormResponse (Prelude.Maybe Form)
updateFormResponse_entity = Lens.lens (\UpdateFormResponse' {entity} -> entity) (\s@UpdateFormResponse' {} a -> s {entity = a} :: UpdateFormResponse)

-- | The response's http status code.
updateFormResponse_httpStatus :: Lens.Lens' UpdateFormResponse Prelude.Int
updateFormResponse_httpStatus = Lens.lens (\UpdateFormResponse' {httpStatus} -> httpStatus) (\s@UpdateFormResponse' {} a -> s {httpStatus = a} :: UpdateFormResponse)

instance Prelude.NFData UpdateFormResponse where
  rnf UpdateFormResponse' {..} =
    Prelude.rnf entity `Prelude.seq`
      Prelude.rnf httpStatus
