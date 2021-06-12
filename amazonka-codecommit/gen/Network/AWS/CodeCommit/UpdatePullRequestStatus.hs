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
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a pull request.
module Network.AWS.CodeCommit.UpdatePullRequestStatus
  ( -- * Creating a Request
    UpdatePullRequestStatus (..),
    newUpdatePullRequestStatus,

    -- * Request Lenses
    updatePullRequestStatus_pullRequestId,
    updatePullRequestStatus_pullRequestStatus,

    -- * Destructuring the Response
    UpdatePullRequestStatusResponse (..),
    newUpdatePullRequestStatusResponse,

    -- * Response Lenses
    updatePullRequestStatusResponse_httpStatus,
    updatePullRequestStatusResponse_pullRequest,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePullRequestStatus' smart constructor.
data UpdatePullRequestStatus = UpdatePullRequestStatus'
  { -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Core.Text,
    -- | The status of the pull request. The only valid operations are to update
    -- the status from @OPEN@ to @OPEN@, @OPEN@ to @CLOSED@ or from @CLOSED@ to
    -- @CLOSED@.
    pullRequestStatus :: PullRequestStatusEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePullRequestStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'updatePullRequestStatus_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
--
-- 'pullRequestStatus', 'updatePullRequestStatus_pullRequestStatus' - The status of the pull request. The only valid operations are to update
-- the status from @OPEN@ to @OPEN@, @OPEN@ to @CLOSED@ or from @CLOSED@ to
-- @CLOSED@.
newUpdatePullRequestStatus ::
  -- | 'pullRequestId'
  Core.Text ->
  -- | 'pullRequestStatus'
  PullRequestStatusEnum ->
  UpdatePullRequestStatus
newUpdatePullRequestStatus
  pPullRequestId_
  pPullRequestStatus_ =
    UpdatePullRequestStatus'
      { pullRequestId =
          pPullRequestId_,
        pullRequestStatus = pPullRequestStatus_
      }

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
updatePullRequestStatus_pullRequestId :: Lens.Lens' UpdatePullRequestStatus Core.Text
updatePullRequestStatus_pullRequestId = Lens.lens (\UpdatePullRequestStatus' {pullRequestId} -> pullRequestId) (\s@UpdatePullRequestStatus' {} a -> s {pullRequestId = a} :: UpdatePullRequestStatus)

-- | The status of the pull request. The only valid operations are to update
-- the status from @OPEN@ to @OPEN@, @OPEN@ to @CLOSED@ or from @CLOSED@ to
-- @CLOSED@.
updatePullRequestStatus_pullRequestStatus :: Lens.Lens' UpdatePullRequestStatus PullRequestStatusEnum
updatePullRequestStatus_pullRequestStatus = Lens.lens (\UpdatePullRequestStatus' {pullRequestStatus} -> pullRequestStatus) (\s@UpdatePullRequestStatus' {} a -> s {pullRequestStatus = a} :: UpdatePullRequestStatus)

instance Core.AWSRequest UpdatePullRequestStatus where
  type
    AWSResponse UpdatePullRequestStatus =
      UpdatePullRequestStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestStatusResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "pullRequest")
      )

instance Core.Hashable UpdatePullRequestStatus

instance Core.NFData UpdatePullRequestStatus

instance Core.ToHeaders UpdatePullRequestStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.UpdatePullRequestStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdatePullRequestStatus where
  toJSON UpdatePullRequestStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just
              ("pullRequestStatus" Core..= pullRequestStatus)
          ]
      )

instance Core.ToPath UpdatePullRequestStatus where
  toPath = Core.const "/"

instance Core.ToQuery UpdatePullRequestStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePullRequestStatusResponse' smart constructor.
data UpdatePullRequestStatusResponse = UpdatePullRequestStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Information about the pull request.
    pullRequest :: PullRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePullRequestStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePullRequestStatusResponse_httpStatus' - The response's http status code.
--
-- 'pullRequest', 'updatePullRequestStatusResponse_pullRequest' - Information about the pull request.
newUpdatePullRequestStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'pullRequest'
  PullRequest ->
  UpdatePullRequestStatusResponse
newUpdatePullRequestStatusResponse
  pHttpStatus_
  pPullRequest_ =
    UpdatePullRequestStatusResponse'
      { httpStatus =
          pHttpStatus_,
        pullRequest = pPullRequest_
      }

-- | The response's http status code.
updatePullRequestStatusResponse_httpStatus :: Lens.Lens' UpdatePullRequestStatusResponse Core.Int
updatePullRequestStatusResponse_httpStatus = Lens.lens (\UpdatePullRequestStatusResponse' {httpStatus} -> httpStatus) (\s@UpdatePullRequestStatusResponse' {} a -> s {httpStatus = a} :: UpdatePullRequestStatusResponse)

-- | Information about the pull request.
updatePullRequestStatusResponse_pullRequest :: Lens.Lens' UpdatePullRequestStatusResponse PullRequest
updatePullRequestStatusResponse_pullRequest = Lens.lens (\UpdatePullRequestStatusResponse' {pullRequest} -> pullRequest) (\s@UpdatePullRequestStatusResponse' {} a -> s {pullRequest = a} :: UpdatePullRequestStatusResponse)

instance Core.NFData UpdatePullRequestStatusResponse
