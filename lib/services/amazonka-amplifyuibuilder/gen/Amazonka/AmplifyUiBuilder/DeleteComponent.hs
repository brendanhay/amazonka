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
-- Module      : Amazonka.AmplifyUiBuilder.DeleteComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a component from an Amplify app.
module Amazonka.AmplifyUiBuilder.DeleteComponent
  ( -- * Creating a Request
    DeleteComponent (..),
    newDeleteComponent,

    -- * Request Lenses
    deleteComponent_appId,
    deleteComponent_environmentName,
    deleteComponent_id,

    -- * Destructuring the Response
    DeleteComponentResponse (..),
    newDeleteComponentResponse,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteComponent' smart constructor.
data DeleteComponent = DeleteComponent'
  { -- | The unique ID of the Amplify app associated with the component to
    -- delete.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID of the component to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteComponent_appId' - The unique ID of the Amplify app associated with the component to
-- delete.
--
-- 'environmentName', 'deleteComponent_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'id', 'deleteComponent_id' - The unique ID of the component to delete.
newDeleteComponent ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteComponent
newDeleteComponent pAppId_ pEnvironmentName_ pId_ =
  DeleteComponent'
    { appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_
    }

-- | The unique ID of the Amplify app associated with the component to
-- delete.
deleteComponent_appId :: Lens.Lens' DeleteComponent Prelude.Text
deleteComponent_appId = Lens.lens (\DeleteComponent' {appId} -> appId) (\s@DeleteComponent' {} a -> s {appId = a} :: DeleteComponent)

-- | The name of the backend environment that is a part of the Amplify app.
deleteComponent_environmentName :: Lens.Lens' DeleteComponent Prelude.Text
deleteComponent_environmentName = Lens.lens (\DeleteComponent' {environmentName} -> environmentName) (\s@DeleteComponent' {} a -> s {environmentName = a} :: DeleteComponent)

-- | The unique ID of the component to delete.
deleteComponent_id :: Lens.Lens' DeleteComponent Prelude.Text
deleteComponent_id = Lens.lens (\DeleteComponent' {id} -> id) (\s@DeleteComponent' {} a -> s {id = a} :: DeleteComponent)

instance Core.AWSRequest DeleteComponent where
  type
    AWSResponse DeleteComponent =
      DeleteComponentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteComponentResponse'

instance Prelude.Hashable DeleteComponent where
  hashWithSalt _salt DeleteComponent' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteComponent where
  rnf DeleteComponent' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteComponent where
  toPath DeleteComponent' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/components/",
        Data.toBS id
      ]

instance Data.ToQuery DeleteComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteComponentResponse' smart constructor.
data DeleteComponentResponse = DeleteComponentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteComponentResponse ::
  DeleteComponentResponse
newDeleteComponentResponse = DeleteComponentResponse'

instance Prelude.NFData DeleteComponentResponse where
  rnf _ = ()
