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
-- Module      : Network.AWS.WorkSpaces.StopWorkspaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified WorkSpaces.
--
-- You cannot stop a WorkSpace unless it has a running mode of @AutoStop@
-- and a state of @AVAILABLE@, @IMPAIRED@, @UNHEALTHY@, or @ERROR@.
module Network.AWS.WorkSpaces.StopWorkspaces
  ( -- * Creating a Request
    StopWorkspaces (..),
    newStopWorkspaces,

    -- * Request Lenses
    stopWorkspaces_stopWorkspaceRequests,

    -- * Destructuring the Response
    StopWorkspacesResponse (..),
    newStopWorkspacesResponse,

    -- * Response Lenses
    stopWorkspacesResponse_failedRequests,
    stopWorkspacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newStopWorkspaces' smart constructor.
data StopWorkspaces = StopWorkspaces'
  { -- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
    stopWorkspaceRequests :: Core.NonEmpty StopRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopWorkspaceRequests', 'stopWorkspaces_stopWorkspaceRequests' - The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
newStopWorkspaces ::
  -- | 'stopWorkspaceRequests'
  Core.NonEmpty StopRequest ->
  StopWorkspaces
newStopWorkspaces pStopWorkspaceRequests_ =
  StopWorkspaces'
    { stopWorkspaceRequests =
        Lens._Coerce Lens.# pStopWorkspaceRequests_
    }

-- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
stopWorkspaces_stopWorkspaceRequests :: Lens.Lens' StopWorkspaces (Core.NonEmpty StopRequest)
stopWorkspaces_stopWorkspaceRequests = Lens.lens (\StopWorkspaces' {stopWorkspaceRequests} -> stopWorkspaceRequests) (\s@StopWorkspaces' {} a -> s {stopWorkspaceRequests = a} :: StopWorkspaces) Core.. Lens._Coerce

instance Core.AWSRequest StopWorkspaces where
  type
    AWSResponse StopWorkspaces =
      StopWorkspacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopWorkspacesResponse'
            Core.<$> (x Core..?> "FailedRequests" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopWorkspaces

instance Core.NFData StopWorkspaces

instance Core.ToHeaders StopWorkspaces where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.StopWorkspaces" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopWorkspaces where
  toJSON StopWorkspaces' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "StopWorkspaceRequests"
                  Core..= stopWorkspaceRequests
              )
          ]
      )

instance Core.ToPath StopWorkspaces where
  toPath = Core.const "/"

instance Core.ToQuery StopWorkspaces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopWorkspacesResponse' smart constructor.
data StopWorkspacesResponse = StopWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be stopped.
    failedRequests :: Core.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'stopWorkspacesResponse_failedRequests' - Information about the WorkSpaces that could not be stopped.
--
-- 'httpStatus', 'stopWorkspacesResponse_httpStatus' - The response's http status code.
newStopWorkspacesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopWorkspacesResponse
newStopWorkspacesResponse pHttpStatus_ =
  StopWorkspacesResponse'
    { failedRequests =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpaces that could not be stopped.
stopWorkspacesResponse_failedRequests :: Lens.Lens' StopWorkspacesResponse (Core.Maybe [FailedWorkspaceChangeRequest])
stopWorkspacesResponse_failedRequests = Lens.lens (\StopWorkspacesResponse' {failedRequests} -> failedRequests) (\s@StopWorkspacesResponse' {} a -> s {failedRequests = a} :: StopWorkspacesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
stopWorkspacesResponse_httpStatus :: Lens.Lens' StopWorkspacesResponse Core.Int
stopWorkspacesResponse_httpStatus = Lens.lens (\StopWorkspacesResponse' {httpStatus} -> httpStatus) (\s@StopWorkspacesResponse' {} a -> s {httpStatus = a} :: StopWorkspacesResponse)

instance Core.NFData StopWorkspacesResponse
