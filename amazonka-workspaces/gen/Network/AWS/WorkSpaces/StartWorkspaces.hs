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
-- Module      : Network.AWS.WorkSpaces.StartWorkspaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified WorkSpaces.
--
-- You cannot start a WorkSpace unless it has a running mode of @AutoStop@
-- and a state of @STOPPED@.
module Network.AWS.WorkSpaces.StartWorkspaces
  ( -- * Creating a Request
    StartWorkspaces (..),
    newStartWorkspaces,

    -- * Request Lenses
    startWorkspaces_startWorkspaceRequests,

    -- * Destructuring the Response
    StartWorkspacesResponse (..),
    newStartWorkspacesResponse,

    -- * Response Lenses
    startWorkspacesResponse_failedRequests,
    startWorkspacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newStartWorkspaces' smart constructor.
data StartWorkspaces = StartWorkspaces'
  { -- | The WorkSpaces to start. You can specify up to 25 WorkSpaces.
    startWorkspaceRequests :: Core.NonEmpty StartRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startWorkspaceRequests', 'startWorkspaces_startWorkspaceRequests' - The WorkSpaces to start. You can specify up to 25 WorkSpaces.
newStartWorkspaces ::
  -- | 'startWorkspaceRequests'
  Core.NonEmpty StartRequest ->
  StartWorkspaces
newStartWorkspaces pStartWorkspaceRequests_ =
  StartWorkspaces'
    { startWorkspaceRequests =
        Lens._Coerce Lens.# pStartWorkspaceRequests_
    }

-- | The WorkSpaces to start. You can specify up to 25 WorkSpaces.
startWorkspaces_startWorkspaceRequests :: Lens.Lens' StartWorkspaces (Core.NonEmpty StartRequest)
startWorkspaces_startWorkspaceRequests = Lens.lens (\StartWorkspaces' {startWorkspaceRequests} -> startWorkspaceRequests) (\s@StartWorkspaces' {} a -> s {startWorkspaceRequests = a} :: StartWorkspaces) Core.. Lens._Coerce

instance Core.AWSRequest StartWorkspaces where
  type
    AWSResponse StartWorkspaces =
      StartWorkspacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkspacesResponse'
            Core.<$> (x Core..?> "FailedRequests" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartWorkspaces

instance Core.NFData StartWorkspaces

instance Core.ToHeaders StartWorkspaces where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.StartWorkspaces" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartWorkspaces where
  toJSON StartWorkspaces' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "StartWorkspaceRequests"
                  Core..= startWorkspaceRequests
              )
          ]
      )

instance Core.ToPath StartWorkspaces where
  toPath = Core.const "/"

instance Core.ToQuery StartWorkspaces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartWorkspacesResponse' smart constructor.
data StartWorkspacesResponse = StartWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be started.
    failedRequests :: Core.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'startWorkspacesResponse_failedRequests' - Information about the WorkSpaces that could not be started.
--
-- 'httpStatus', 'startWorkspacesResponse_httpStatus' - The response's http status code.
newStartWorkspacesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartWorkspacesResponse
newStartWorkspacesResponse pHttpStatus_ =
  StartWorkspacesResponse'
    { failedRequests =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpaces that could not be started.
startWorkspacesResponse_failedRequests :: Lens.Lens' StartWorkspacesResponse (Core.Maybe [FailedWorkspaceChangeRequest])
startWorkspacesResponse_failedRequests = Lens.lens (\StartWorkspacesResponse' {failedRequests} -> failedRequests) (\s@StartWorkspacesResponse' {} a -> s {failedRequests = a} :: StartWorkspacesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
startWorkspacesResponse_httpStatus :: Lens.Lens' StartWorkspacesResponse Core.Int
startWorkspacesResponse_httpStatus = Lens.lens (\StartWorkspacesResponse' {httpStatus} -> httpStatus) (\s@StartWorkspacesResponse' {} a -> s {httpStatus = a} :: StartWorkspacesResponse)

instance Core.NFData StartWorkspacesResponse
