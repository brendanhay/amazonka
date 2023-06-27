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
-- Module      : Amazonka.IotTwinMaker.DeleteEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entity.
module Amazonka.IotTwinMaker.DeleteEntity
  ( -- * Creating a Request
    DeleteEntity (..),
    newDeleteEntity,

    -- * Request Lenses
    deleteEntity_isRecursive,
    deleteEntity_workspaceId,
    deleteEntity_entityId,

    -- * Destructuring the Response
    DeleteEntityResponse (..),
    newDeleteEntityResponse,

    -- * Response Lenses
    deleteEntityResponse_httpStatus,
    deleteEntityResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEntity' smart constructor.
data DeleteEntity = DeleteEntity'
  { -- | A Boolean value that specifies whether the operation deletes child
    -- entities.
    isRecursive :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the workspace that contains the entity to delete.
    workspaceId :: Prelude.Text,
    -- | The ID of the entity to delete.
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isRecursive', 'deleteEntity_isRecursive' - A Boolean value that specifies whether the operation deletes child
-- entities.
--
-- 'workspaceId', 'deleteEntity_workspaceId' - The ID of the workspace that contains the entity to delete.
--
-- 'entityId', 'deleteEntity_entityId' - The ID of the entity to delete.
newDeleteEntity ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  DeleteEntity
newDeleteEntity pWorkspaceId_ pEntityId_ =
  DeleteEntity'
    { isRecursive = Prelude.Nothing,
      workspaceId = pWorkspaceId_,
      entityId = pEntityId_
    }

-- | A Boolean value that specifies whether the operation deletes child
-- entities.
deleteEntity_isRecursive :: Lens.Lens' DeleteEntity (Prelude.Maybe Prelude.Bool)
deleteEntity_isRecursive = Lens.lens (\DeleteEntity' {isRecursive} -> isRecursive) (\s@DeleteEntity' {} a -> s {isRecursive = a} :: DeleteEntity)

-- | The ID of the workspace that contains the entity to delete.
deleteEntity_workspaceId :: Lens.Lens' DeleteEntity Prelude.Text
deleteEntity_workspaceId = Lens.lens (\DeleteEntity' {workspaceId} -> workspaceId) (\s@DeleteEntity' {} a -> s {workspaceId = a} :: DeleteEntity)

-- | The ID of the entity to delete.
deleteEntity_entityId :: Lens.Lens' DeleteEntity Prelude.Text
deleteEntity_entityId = Lens.lens (\DeleteEntity' {entityId} -> entityId) (\s@DeleteEntity' {} a -> s {entityId = a} :: DeleteEntity)

instance Core.AWSRequest DeleteEntity where
  type AWSResponse DeleteEntity = DeleteEntityResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEntityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable DeleteEntity where
  hashWithSalt _salt DeleteEntity' {..} =
    _salt
      `Prelude.hashWithSalt` isRecursive
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData DeleteEntity where
  rnf DeleteEntity' {..} =
    Prelude.rnf isRecursive
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf entityId

instance Data.ToHeaders DeleteEntity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEntity where
  toPath DeleteEntity' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/entities/",
        Data.toBS entityId
      ]

instance Data.ToQuery DeleteEntity where
  toQuery DeleteEntity' {..} =
    Prelude.mconcat ["isRecursive" Data.=: isRecursive]

-- | /See:/ 'newDeleteEntityResponse' smart constructor.
data DeleteEntityResponse = DeleteEntityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current state of the deleted entity.
    state :: State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEntityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEntityResponse_httpStatus' - The response's http status code.
--
-- 'state', 'deleteEntityResponse_state' - The current state of the deleted entity.
newDeleteEntityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'state'
  State ->
  DeleteEntityResponse
newDeleteEntityResponse pHttpStatus_ pState_ =
  DeleteEntityResponse'
    { httpStatus = pHttpStatus_,
      state = pState_
    }

-- | The response's http status code.
deleteEntityResponse_httpStatus :: Lens.Lens' DeleteEntityResponse Prelude.Int
deleteEntityResponse_httpStatus = Lens.lens (\DeleteEntityResponse' {httpStatus} -> httpStatus) (\s@DeleteEntityResponse' {} a -> s {httpStatus = a} :: DeleteEntityResponse)

-- | The current state of the deleted entity.
deleteEntityResponse_state :: Lens.Lens' DeleteEntityResponse State
deleteEntityResponse_state = Lens.lens (\DeleteEntityResponse' {state} -> state) (\s@DeleteEntityResponse' {} a -> s {state = a} :: DeleteEntityResponse)

instance Prelude.NFData DeleteEntityResponse where
  rnf DeleteEntityResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf state
