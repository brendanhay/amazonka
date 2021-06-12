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
-- Module      : Network.AWS.WorkSpaces.RebootWorkspaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots the specified WorkSpaces.
--
-- You cannot reboot a WorkSpace unless its state is @AVAILABLE@ or
-- @UNHEALTHY@.
--
-- This operation is asynchronous and returns before the WorkSpaces have
-- rebooted.
module Network.AWS.WorkSpaces.RebootWorkspaces
  ( -- * Creating a Request
    RebootWorkspaces (..),
    newRebootWorkspaces,

    -- * Request Lenses
    rebootWorkspaces_rebootWorkspaceRequests,

    -- * Destructuring the Response
    RebootWorkspacesResponse (..),
    newRebootWorkspacesResponse,

    -- * Response Lenses
    rebootWorkspacesResponse_failedRequests,
    rebootWorkspacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newRebootWorkspaces' smart constructor.
data RebootWorkspaces = RebootWorkspaces'
  { -- | The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
    rebootWorkspaceRequests :: Core.NonEmpty RebootRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebootWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rebootWorkspaceRequests', 'rebootWorkspaces_rebootWorkspaceRequests' - The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
newRebootWorkspaces ::
  -- | 'rebootWorkspaceRequests'
  Core.NonEmpty RebootRequest ->
  RebootWorkspaces
newRebootWorkspaces pRebootWorkspaceRequests_ =
  RebootWorkspaces'
    { rebootWorkspaceRequests =
        Lens._Coerce Lens.# pRebootWorkspaceRequests_
    }

-- | The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
rebootWorkspaces_rebootWorkspaceRequests :: Lens.Lens' RebootWorkspaces (Core.NonEmpty RebootRequest)
rebootWorkspaces_rebootWorkspaceRequests = Lens.lens (\RebootWorkspaces' {rebootWorkspaceRequests} -> rebootWorkspaceRequests) (\s@RebootWorkspaces' {} a -> s {rebootWorkspaceRequests = a} :: RebootWorkspaces) Core.. Lens._Coerce

instance Core.AWSRequest RebootWorkspaces where
  type
    AWSResponse RebootWorkspaces =
      RebootWorkspacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootWorkspacesResponse'
            Core.<$> (x Core..?> "FailedRequests" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RebootWorkspaces

instance Core.NFData RebootWorkspaces

instance Core.ToHeaders RebootWorkspaces where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.RebootWorkspaces" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RebootWorkspaces where
  toJSON RebootWorkspaces' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "RebootWorkspaceRequests"
                  Core..= rebootWorkspaceRequests
              )
          ]
      )

instance Core.ToPath RebootWorkspaces where
  toPath = Core.const "/"

instance Core.ToQuery RebootWorkspaces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRebootWorkspacesResponse' smart constructor.
data RebootWorkspacesResponse = RebootWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be rebooted.
    failedRequests :: Core.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebootWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'rebootWorkspacesResponse_failedRequests' - Information about the WorkSpaces that could not be rebooted.
--
-- 'httpStatus', 'rebootWorkspacesResponse_httpStatus' - The response's http status code.
newRebootWorkspacesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RebootWorkspacesResponse
newRebootWorkspacesResponse pHttpStatus_ =
  RebootWorkspacesResponse'
    { failedRequests =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpaces that could not be rebooted.
rebootWorkspacesResponse_failedRequests :: Lens.Lens' RebootWorkspacesResponse (Core.Maybe [FailedWorkspaceChangeRequest])
rebootWorkspacesResponse_failedRequests = Lens.lens (\RebootWorkspacesResponse' {failedRequests} -> failedRequests) (\s@RebootWorkspacesResponse' {} a -> s {failedRequests = a} :: RebootWorkspacesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
rebootWorkspacesResponse_httpStatus :: Lens.Lens' RebootWorkspacesResponse Core.Int
rebootWorkspacesResponse_httpStatus = Lens.lens (\RebootWorkspacesResponse' {httpStatus} -> httpStatus) (\s@RebootWorkspacesResponse' {} a -> s {httpStatus = a} :: RebootWorkspacesResponse)

instance Core.NFData RebootWorkspacesResponse
