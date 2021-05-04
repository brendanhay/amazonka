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
-- Module      : Network.AWS.WorkSpaces.RestoreWorkspace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the specified WorkSpace to its last known healthy state.
--
-- You cannot restore a WorkSpace unless its state is @ AVAILABLE@,
-- @ERROR@, @UNHEALTHY@, or @STOPPED@.
--
-- Restoring a WorkSpace is a potentially destructive action that can
-- result in the loss of data. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/restore-workspace.html Restore a WorkSpace>.
--
-- This operation is asynchronous and returns before the WorkSpace is
-- completely restored.
module Network.AWS.WorkSpaces.RestoreWorkspace
  ( -- * Creating a Request
    RestoreWorkspace (..),
    newRestoreWorkspace,

    -- * Request Lenses
    restoreWorkspace_workspaceId,

    -- * Destructuring the Response
    RestoreWorkspaceResponse (..),
    newRestoreWorkspaceResponse,

    -- * Response Lenses
    restoreWorkspaceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newRestoreWorkspace' smart constructor.
data RestoreWorkspace = RestoreWorkspace'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'restoreWorkspace_workspaceId' - The identifier of the WorkSpace.
newRestoreWorkspace ::
  -- | 'workspaceId'
  Prelude.Text ->
  RestoreWorkspace
newRestoreWorkspace pWorkspaceId_ =
  RestoreWorkspace' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
restoreWorkspace_workspaceId :: Lens.Lens' RestoreWorkspace Prelude.Text
restoreWorkspace_workspaceId = Lens.lens (\RestoreWorkspace' {workspaceId} -> workspaceId) (\s@RestoreWorkspace' {} a -> s {workspaceId = a} :: RestoreWorkspace)

instance Prelude.AWSRequest RestoreWorkspace where
  type Rs RestoreWorkspace = RestoreWorkspaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RestoreWorkspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreWorkspace

instance Prelude.NFData RestoreWorkspace

instance Prelude.ToHeaders RestoreWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.RestoreWorkspace" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RestoreWorkspace where
  toJSON RestoreWorkspace' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WorkspaceId" Prelude..= workspaceId)
          ]
      )

instance Prelude.ToPath RestoreWorkspace where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RestoreWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreWorkspaceResponse' smart constructor.
data RestoreWorkspaceResponse = RestoreWorkspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'restoreWorkspaceResponse_httpStatus' - The response's http status code.
newRestoreWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreWorkspaceResponse
newRestoreWorkspaceResponse pHttpStatus_ =
  RestoreWorkspaceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
restoreWorkspaceResponse_httpStatus :: Lens.Lens' RestoreWorkspaceResponse Prelude.Int
restoreWorkspaceResponse_httpStatus = Lens.lens (\RestoreWorkspaceResponse' {httpStatus} -> httpStatus) (\s@RestoreWorkspaceResponse' {} a -> s {httpStatus = a} :: RestoreWorkspaceResponse)

instance Prelude.NFData RestoreWorkspaceResponse
