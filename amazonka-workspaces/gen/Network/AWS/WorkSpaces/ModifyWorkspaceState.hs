{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the state of the specified WorkSpace.
--
-- To maintain a WorkSpace without being interrupted, set the WorkSpace
-- state to @ADMIN_MAINTENANCE@. WorkSpaces in this state do not respond to
-- requests to reboot, stop, start, rebuild, or restore. An AutoStop
-- WorkSpace in this state is not stopped. Users cannot log into a
-- WorkSpace in the @ADMIN_MAINTENANCE@ state.
module Network.AWS.WorkSpaces.ModifyWorkspaceState
  ( -- * Creating a Request
    ModifyWorkspaceState (..),
    newModifyWorkspaceState,

    -- * Request Lenses
    modifyWorkspaceState_workspaceId,
    modifyWorkspaceState_workspaceState,

    -- * Destructuring the Response
    ModifyWorkspaceStateResponse (..),
    newModifyWorkspaceStateResponse,

    -- * Response Lenses
    modifyWorkspaceStateResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyWorkspaceState' smart constructor.
data ModifyWorkspaceState = ModifyWorkspaceState'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Text,
    -- | The WorkSpace state.
    workspaceState :: TargetWorkspaceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyWorkspaceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'modifyWorkspaceState_workspaceId' - The identifier of the WorkSpace.
--
-- 'workspaceState', 'modifyWorkspaceState_workspaceState' - The WorkSpace state.
newModifyWorkspaceState ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'workspaceState'
  TargetWorkspaceState ->
  ModifyWorkspaceState
newModifyWorkspaceState
  pWorkspaceId_
  pWorkspaceState_ =
    ModifyWorkspaceState'
      { workspaceId = pWorkspaceId_,
        workspaceState = pWorkspaceState_
      }

-- | The identifier of the WorkSpace.
modifyWorkspaceState_workspaceId :: Lens.Lens' ModifyWorkspaceState Prelude.Text
modifyWorkspaceState_workspaceId = Lens.lens (\ModifyWorkspaceState' {workspaceId} -> workspaceId) (\s@ModifyWorkspaceState' {} a -> s {workspaceId = a} :: ModifyWorkspaceState)

-- | The WorkSpace state.
modifyWorkspaceState_workspaceState :: Lens.Lens' ModifyWorkspaceState TargetWorkspaceState
modifyWorkspaceState_workspaceState = Lens.lens (\ModifyWorkspaceState' {workspaceState} -> workspaceState) (\s@ModifyWorkspaceState' {} a -> s {workspaceState = a} :: ModifyWorkspaceState)

instance Prelude.AWSRequest ModifyWorkspaceState where
  type
    Rs ModifyWorkspaceState =
      ModifyWorkspaceStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyWorkspaceState

instance Prelude.NFData ModifyWorkspaceState

instance Prelude.ToHeaders ModifyWorkspaceState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.ModifyWorkspaceState" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ModifyWorkspaceState where
  toJSON ModifyWorkspaceState' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WorkspaceId" Prelude..= workspaceId),
            Prelude.Just
              ("WorkspaceState" Prelude..= workspaceState)
          ]
      )

instance Prelude.ToPath ModifyWorkspaceState where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyWorkspaceState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyWorkspaceStateResponse' smart constructor.
data ModifyWorkspaceStateResponse = ModifyWorkspaceStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyWorkspaceStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyWorkspaceStateResponse_httpStatus' - The response's http status code.
newModifyWorkspaceStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyWorkspaceStateResponse
newModifyWorkspaceStateResponse pHttpStatus_ =
  ModifyWorkspaceStateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifyWorkspaceStateResponse_httpStatus :: Lens.Lens' ModifyWorkspaceStateResponse Prelude.Int
modifyWorkspaceStateResponse_httpStatus = Lens.lens (\ModifyWorkspaceStateResponse' {httpStatus} -> httpStatus) (\s@ModifyWorkspaceStateResponse' {} a -> s {httpStatus = a} :: ModifyWorkspaceStateResponse)

instance Prelude.NFData ModifyWorkspaceStateResponse
