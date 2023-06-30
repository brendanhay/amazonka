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
-- Module      : Amazonka.AmplifyUiBuilder.GetForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an existing form for an Amplify app.
module Amazonka.AmplifyUiBuilder.GetForm
  ( -- * Creating a Request
    GetForm (..),
    newGetForm,

    -- * Request Lenses
    getForm_appId,
    getForm_environmentName,
    getForm_id,

    -- * Destructuring the Response
    GetFormResponse (..),
    newGetFormResponse,

    -- * Response Lenses
    getFormResponse_form,
    getFormResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetForm' smart constructor.
data GetForm = GetForm'
  { -- | The unique ID of the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID of the form.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getForm_appId' - The unique ID of the Amplify app.
--
-- 'environmentName', 'getForm_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'id', 'getForm_id' - The unique ID of the form.
newGetForm ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetForm
newGetForm pAppId_ pEnvironmentName_ pId_ =
  GetForm'
    { appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_
    }

-- | The unique ID of the Amplify app.
getForm_appId :: Lens.Lens' GetForm Prelude.Text
getForm_appId = Lens.lens (\GetForm' {appId} -> appId) (\s@GetForm' {} a -> s {appId = a} :: GetForm)

-- | The name of the backend environment that is part of the Amplify app.
getForm_environmentName :: Lens.Lens' GetForm Prelude.Text
getForm_environmentName = Lens.lens (\GetForm' {environmentName} -> environmentName) (\s@GetForm' {} a -> s {environmentName = a} :: GetForm)

-- | The unique ID of the form.
getForm_id :: Lens.Lens' GetForm Prelude.Text
getForm_id = Lens.lens (\GetForm' {id} -> id) (\s@GetForm' {} a -> s {id = a} :: GetForm)

instance Core.AWSRequest GetForm where
  type AWSResponse GetForm = GetFormResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFormResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetForm where
  hashWithSalt _salt GetForm' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetForm where
  rnf GetForm' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetForm where
  toPath GetForm' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/forms/",
        Data.toBS id
      ]

instance Data.ToQuery GetForm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFormResponse' smart constructor.
data GetFormResponse = GetFormResponse'
  { -- | Represents the configuration settings for the form.
    form :: Prelude.Maybe Form,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'form', 'getFormResponse_form' - Represents the configuration settings for the form.
--
-- 'httpStatus', 'getFormResponse_httpStatus' - The response's http status code.
newGetFormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFormResponse
newGetFormResponse pHttpStatus_ =
  GetFormResponse'
    { form = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the configuration settings for the form.
getFormResponse_form :: Lens.Lens' GetFormResponse (Prelude.Maybe Form)
getFormResponse_form = Lens.lens (\GetFormResponse' {form} -> form) (\s@GetFormResponse' {} a -> s {form = a} :: GetFormResponse)

-- | The response's http status code.
getFormResponse_httpStatus :: Lens.Lens' GetFormResponse Prelude.Int
getFormResponse_httpStatus = Lens.lens (\GetFormResponse' {httpStatus} -> httpStatus) (\s@GetFormResponse' {} a -> s {httpStatus = a} :: GetFormResponse)

instance Prelude.NFData GetFormResponse where
  rnf GetFormResponse' {..} =
    Prelude.rnf form
      `Prelude.seq` Prelude.rnf httpStatus
