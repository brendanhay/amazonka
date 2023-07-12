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
-- Module      : Amazonka.IotTwinMaker.DeleteComponentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a component type.
module Amazonka.IotTwinMaker.DeleteComponentType
  ( -- * Creating a Request
    DeleteComponentType (..),
    newDeleteComponentType,

    -- * Request Lenses
    deleteComponentType_workspaceId,
    deleteComponentType_componentTypeId,

    -- * Destructuring the Response
    DeleteComponentTypeResponse (..),
    newDeleteComponentTypeResponse,

    -- * Response Lenses
    deleteComponentTypeResponse_httpStatus,
    deleteComponentTypeResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteComponentType' smart constructor.
data DeleteComponentType = DeleteComponentType'
  { -- | The ID of the workspace that contains the component type.
    workspaceId :: Prelude.Text,
    -- | The ID of the component type to delete.
    componentTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponentType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'deleteComponentType_workspaceId' - The ID of the workspace that contains the component type.
--
-- 'componentTypeId', 'deleteComponentType_componentTypeId' - The ID of the component type to delete.
newDeleteComponentType ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'componentTypeId'
  Prelude.Text ->
  DeleteComponentType
newDeleteComponentType
  pWorkspaceId_
  pComponentTypeId_ =
    DeleteComponentType'
      { workspaceId = pWorkspaceId_,
        componentTypeId = pComponentTypeId_
      }

-- | The ID of the workspace that contains the component type.
deleteComponentType_workspaceId :: Lens.Lens' DeleteComponentType Prelude.Text
deleteComponentType_workspaceId = Lens.lens (\DeleteComponentType' {workspaceId} -> workspaceId) (\s@DeleteComponentType' {} a -> s {workspaceId = a} :: DeleteComponentType)

-- | The ID of the component type to delete.
deleteComponentType_componentTypeId :: Lens.Lens' DeleteComponentType Prelude.Text
deleteComponentType_componentTypeId = Lens.lens (\DeleteComponentType' {componentTypeId} -> componentTypeId) (\s@DeleteComponentType' {} a -> s {componentTypeId = a} :: DeleteComponentType)

instance Core.AWSRequest DeleteComponentType where
  type
    AWSResponse DeleteComponentType =
      DeleteComponentTypeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteComponentTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable DeleteComponentType where
  hashWithSalt _salt DeleteComponentType' {..} =
    _salt
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` componentTypeId

instance Prelude.NFData DeleteComponentType where
  rnf DeleteComponentType' {..} =
    Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf componentTypeId

instance Data.ToHeaders DeleteComponentType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteComponentType where
  toPath DeleteComponentType' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/component-types/",
        Data.toBS componentTypeId
      ]

instance Data.ToQuery DeleteComponentType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteComponentTypeResponse' smart constructor.
data DeleteComponentTypeResponse = DeleteComponentTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current state of the component type to be deleted.
    state :: State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponentTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteComponentTypeResponse_httpStatus' - The response's http status code.
--
-- 'state', 'deleteComponentTypeResponse_state' - The current state of the component type to be deleted.
newDeleteComponentTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'state'
  State ->
  DeleteComponentTypeResponse
newDeleteComponentTypeResponse pHttpStatus_ pState_ =
  DeleteComponentTypeResponse'
    { httpStatus =
        pHttpStatus_,
      state = pState_
    }

-- | The response's http status code.
deleteComponentTypeResponse_httpStatus :: Lens.Lens' DeleteComponentTypeResponse Prelude.Int
deleteComponentTypeResponse_httpStatus = Lens.lens (\DeleteComponentTypeResponse' {httpStatus} -> httpStatus) (\s@DeleteComponentTypeResponse' {} a -> s {httpStatus = a} :: DeleteComponentTypeResponse)

-- | The current state of the component type to be deleted.
deleteComponentTypeResponse_state :: Lens.Lens' DeleteComponentTypeResponse State
deleteComponentTypeResponse_state = Lens.lens (\DeleteComponentTypeResponse' {state} -> state) (\s@DeleteComponentTypeResponse' {} a -> s {state = a} :: DeleteComponentTypeResponse)

instance Prelude.NFData DeleteComponentTypeResponse where
  rnf DeleteComponentTypeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf state
