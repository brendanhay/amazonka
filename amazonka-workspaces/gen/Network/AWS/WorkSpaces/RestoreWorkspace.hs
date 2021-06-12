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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newRestoreWorkspace' smart constructor.
data RestoreWorkspace = RestoreWorkspace'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  RestoreWorkspace
newRestoreWorkspace pWorkspaceId_ =
  RestoreWorkspace' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
restoreWorkspace_workspaceId :: Lens.Lens' RestoreWorkspace Core.Text
restoreWorkspace_workspaceId = Lens.lens (\RestoreWorkspace' {workspaceId} -> workspaceId) (\s@RestoreWorkspace' {} a -> s {workspaceId = a} :: RestoreWorkspace)

instance Core.AWSRequest RestoreWorkspace where
  type
    AWSResponse RestoreWorkspace =
      RestoreWorkspaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RestoreWorkspaceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RestoreWorkspace

instance Core.NFData RestoreWorkspace

instance Core.ToHeaders RestoreWorkspace where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.RestoreWorkspace" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RestoreWorkspace where
  toJSON RestoreWorkspace' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WorkspaceId" Core..= workspaceId)]
      )

instance Core.ToPath RestoreWorkspace where
  toPath = Core.const "/"

instance Core.ToQuery RestoreWorkspace where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRestoreWorkspaceResponse' smart constructor.
data RestoreWorkspaceResponse = RestoreWorkspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RestoreWorkspaceResponse
newRestoreWorkspaceResponse pHttpStatus_ =
  RestoreWorkspaceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
restoreWorkspaceResponse_httpStatus :: Lens.Lens' RestoreWorkspaceResponse Core.Int
restoreWorkspaceResponse_httpStatus = Lens.lens (\RestoreWorkspaceResponse' {httpStatus} -> httpStatus) (\s@RestoreWorkspaceResponse' {} a -> s {httpStatus = a} :: RestoreWorkspaceResponse)

instance Core.NFData RestoreWorkspaceResponse
