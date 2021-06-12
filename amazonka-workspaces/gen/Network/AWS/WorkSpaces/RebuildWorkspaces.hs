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
-- Module      : Network.AWS.WorkSpaces.RebuildWorkspaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rebuilds the specified WorkSpace.
--
-- You cannot rebuild a WorkSpace unless its state is @AVAILABLE@, @ERROR@,
-- @UNHEALTHY@, @STOPPED@, or @REBOOTING@.
--
-- Rebuilding a WorkSpace is a potentially destructive action that can
-- result in the loss of data. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/reset-workspace.html Rebuild a WorkSpace>.
--
-- This operation is asynchronous and returns before the WorkSpaces have
-- been completely rebuilt.
module Network.AWS.WorkSpaces.RebuildWorkspaces
  ( -- * Creating a Request
    RebuildWorkspaces (..),
    newRebuildWorkspaces,

    -- * Request Lenses
    rebuildWorkspaces_rebuildWorkspaceRequests,

    -- * Destructuring the Response
    RebuildWorkspacesResponse (..),
    newRebuildWorkspacesResponse,

    -- * Response Lenses
    rebuildWorkspacesResponse_failedRequests,
    rebuildWorkspacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newRebuildWorkspaces' smart constructor.
data RebuildWorkspaces = RebuildWorkspaces'
  { -- | The WorkSpace to rebuild. You can specify a single WorkSpace.
    rebuildWorkspaceRequests :: Core.NonEmpty RebuildRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebuildWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rebuildWorkspaceRequests', 'rebuildWorkspaces_rebuildWorkspaceRequests' - The WorkSpace to rebuild. You can specify a single WorkSpace.
newRebuildWorkspaces ::
  -- | 'rebuildWorkspaceRequests'
  Core.NonEmpty RebuildRequest ->
  RebuildWorkspaces
newRebuildWorkspaces pRebuildWorkspaceRequests_ =
  RebuildWorkspaces'
    { rebuildWorkspaceRequests =
        Lens._Coerce Lens.# pRebuildWorkspaceRequests_
    }

-- | The WorkSpace to rebuild. You can specify a single WorkSpace.
rebuildWorkspaces_rebuildWorkspaceRequests :: Lens.Lens' RebuildWorkspaces (Core.NonEmpty RebuildRequest)
rebuildWorkspaces_rebuildWorkspaceRequests = Lens.lens (\RebuildWorkspaces' {rebuildWorkspaceRequests} -> rebuildWorkspaceRequests) (\s@RebuildWorkspaces' {} a -> s {rebuildWorkspaceRequests = a} :: RebuildWorkspaces) Core.. Lens._Coerce

instance Core.AWSRequest RebuildWorkspaces where
  type
    AWSResponse RebuildWorkspaces =
      RebuildWorkspacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RebuildWorkspacesResponse'
            Core.<$> (x Core..?> "FailedRequests" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RebuildWorkspaces

instance Core.NFData RebuildWorkspaces

instance Core.ToHeaders RebuildWorkspaces where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.RebuildWorkspaces" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RebuildWorkspaces where
  toJSON RebuildWorkspaces' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "RebuildWorkspaceRequests"
                  Core..= rebuildWorkspaceRequests
              )
          ]
      )

instance Core.ToPath RebuildWorkspaces where
  toPath = Core.const "/"

instance Core.ToQuery RebuildWorkspaces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRebuildWorkspacesResponse' smart constructor.
data RebuildWorkspacesResponse = RebuildWorkspacesResponse'
  { -- | Information about the WorkSpace that could not be rebuilt.
    failedRequests :: Core.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebuildWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'rebuildWorkspacesResponse_failedRequests' - Information about the WorkSpace that could not be rebuilt.
--
-- 'httpStatus', 'rebuildWorkspacesResponse_httpStatus' - The response's http status code.
newRebuildWorkspacesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RebuildWorkspacesResponse
newRebuildWorkspacesResponse pHttpStatus_ =
  RebuildWorkspacesResponse'
    { failedRequests =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpace that could not be rebuilt.
rebuildWorkspacesResponse_failedRequests :: Lens.Lens' RebuildWorkspacesResponse (Core.Maybe [FailedWorkspaceChangeRequest])
rebuildWorkspacesResponse_failedRequests = Lens.lens (\RebuildWorkspacesResponse' {failedRequests} -> failedRequests) (\s@RebuildWorkspacesResponse' {} a -> s {failedRequests = a} :: RebuildWorkspacesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
rebuildWorkspacesResponse_httpStatus :: Lens.Lens' RebuildWorkspacesResponse Core.Int
rebuildWorkspacesResponse_httpStatus = Lens.lens (\RebuildWorkspacesResponse' {httpStatus} -> httpStatus) (\s@RebuildWorkspacesResponse' {} a -> s {httpStatus = a} :: RebuildWorkspacesResponse)

instance Core.NFData RebuildWorkspacesResponse
