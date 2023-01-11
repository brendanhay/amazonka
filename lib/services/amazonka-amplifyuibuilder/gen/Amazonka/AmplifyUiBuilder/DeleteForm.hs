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
-- Module      : Amazonka.AmplifyUiBuilder.DeleteForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a form from an Amplify app.
module Amazonka.AmplifyUiBuilder.DeleteForm
  ( -- * Creating a Request
    DeleteForm (..),
    newDeleteForm,

    -- * Request Lenses
    deleteForm_appId,
    deleteForm_environmentName,
    deleteForm_id,

    -- * Destructuring the Response
    DeleteFormResponse (..),
    newDeleteFormResponse,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteForm' smart constructor.
data DeleteForm = DeleteForm'
  { -- | The unique ID of the Amplify app associated with the form to delete.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID of the form to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteForm_appId' - The unique ID of the Amplify app associated with the form to delete.
--
-- 'environmentName', 'deleteForm_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'id', 'deleteForm_id' - The unique ID of the form to delete.
newDeleteForm ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteForm
newDeleteForm pAppId_ pEnvironmentName_ pId_ =
  DeleteForm'
    { appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_
    }

-- | The unique ID of the Amplify app associated with the form to delete.
deleteForm_appId :: Lens.Lens' DeleteForm Prelude.Text
deleteForm_appId = Lens.lens (\DeleteForm' {appId} -> appId) (\s@DeleteForm' {} a -> s {appId = a} :: DeleteForm)

-- | The name of the backend environment that is a part of the Amplify app.
deleteForm_environmentName :: Lens.Lens' DeleteForm Prelude.Text
deleteForm_environmentName = Lens.lens (\DeleteForm' {environmentName} -> environmentName) (\s@DeleteForm' {} a -> s {environmentName = a} :: DeleteForm)

-- | The unique ID of the form to delete.
deleteForm_id :: Lens.Lens' DeleteForm Prelude.Text
deleteForm_id = Lens.lens (\DeleteForm' {id} -> id) (\s@DeleteForm' {} a -> s {id = a} :: DeleteForm)

instance Core.AWSRequest DeleteForm where
  type AWSResponse DeleteForm = DeleteFormResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteFormResponse'

instance Prelude.Hashable DeleteForm where
  hashWithSalt _salt DeleteForm' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteForm where
  rnf DeleteForm' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteForm where
  toPath DeleteForm' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/forms/",
        Data.toBS id
      ]

instance Data.ToQuery DeleteForm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFormResponse' smart constructor.
data DeleteFormResponse = DeleteFormResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFormResponse ::
  DeleteFormResponse
newDeleteFormResponse = DeleteFormResponse'

instance Prelude.NFData DeleteFormResponse where
  rnf _ = ()
