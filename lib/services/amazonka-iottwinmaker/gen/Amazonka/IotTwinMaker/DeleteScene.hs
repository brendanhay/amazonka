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
-- Module      : Amazonka.IotTwinMaker.DeleteScene
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scene.
module Amazonka.IotTwinMaker.DeleteScene
  ( -- * Creating a Request
    DeleteScene (..),
    newDeleteScene,

    -- * Request Lenses
    deleteScene_workspaceId,
    deleteScene_sceneId,

    -- * Destructuring the Response
    DeleteSceneResponse (..),
    newDeleteSceneResponse,

    -- * Response Lenses
    deleteSceneResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteScene' smart constructor.
data DeleteScene = DeleteScene'
  { -- | The ID of the workspace.
    workspaceId :: Prelude.Text,
    -- | The ID of the scene to delete.
    sceneId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScene' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'deleteScene_workspaceId' - The ID of the workspace.
--
-- 'sceneId', 'deleteScene_sceneId' - The ID of the scene to delete.
newDeleteScene ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'sceneId'
  Prelude.Text ->
  DeleteScene
newDeleteScene pWorkspaceId_ pSceneId_ =
  DeleteScene'
    { workspaceId = pWorkspaceId_,
      sceneId = pSceneId_
    }

-- | The ID of the workspace.
deleteScene_workspaceId :: Lens.Lens' DeleteScene Prelude.Text
deleteScene_workspaceId = Lens.lens (\DeleteScene' {workspaceId} -> workspaceId) (\s@DeleteScene' {} a -> s {workspaceId = a} :: DeleteScene)

-- | The ID of the scene to delete.
deleteScene_sceneId :: Lens.Lens' DeleteScene Prelude.Text
deleteScene_sceneId = Lens.lens (\DeleteScene' {sceneId} -> sceneId) (\s@DeleteScene' {} a -> s {sceneId = a} :: DeleteScene)

instance Core.AWSRequest DeleteScene where
  type AWSResponse DeleteScene = DeleteSceneResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSceneResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteScene where
  hashWithSalt _salt DeleteScene' {..} =
    _salt
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` sceneId

instance Prelude.NFData DeleteScene where
  rnf DeleteScene' {..} =
    Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf sceneId

instance Data.ToHeaders DeleteScene where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteScene where
  toPath DeleteScene' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/scenes/",
        Data.toBS sceneId
      ]

instance Data.ToQuery DeleteScene where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSceneResponse' smart constructor.
data DeleteSceneResponse = DeleteSceneResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSceneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSceneResponse_httpStatus' - The response's http status code.
newDeleteSceneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSceneResponse
newDeleteSceneResponse pHttpStatus_ =
  DeleteSceneResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteSceneResponse_httpStatus :: Lens.Lens' DeleteSceneResponse Prelude.Int
deleteSceneResponse_httpStatus = Lens.lens (\DeleteSceneResponse' {httpStatus} -> httpStatus) (\s@DeleteSceneResponse' {} a -> s {httpStatus = a} :: DeleteSceneResponse)

instance Prelude.NFData DeleteSceneResponse where
  rnf DeleteSceneResponse' {..} = Prelude.rnf httpStatus
