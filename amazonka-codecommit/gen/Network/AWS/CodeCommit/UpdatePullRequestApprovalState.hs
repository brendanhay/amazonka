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
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestApprovalState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state of a user\'s approval on a pull request. The user is
-- derived from the signed-in account when the request is made.
module Network.AWS.CodeCommit.UpdatePullRequestApprovalState
  ( -- * Creating a Request
    UpdatePullRequestApprovalState (..),
    newUpdatePullRequestApprovalState,

    -- * Request Lenses
    updatePullRequestApprovalState_pullRequestId,
    updatePullRequestApprovalState_revisionId,
    updatePullRequestApprovalState_approvalState,

    -- * Destructuring the Response
    UpdatePullRequestApprovalStateResponse (..),
    newUpdatePullRequestApprovalStateResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePullRequestApprovalState' smart constructor.
data UpdatePullRequestApprovalState = UpdatePullRequestApprovalState'
  { -- | The system-generated ID of the pull request.
    pullRequestId :: Core.Text,
    -- | The system-generated ID of the revision.
    revisionId :: Core.Text,
    -- | The approval state to associate with the user on the pull request.
    approvalState :: ApprovalState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePullRequestApprovalState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'updatePullRequestApprovalState_pullRequestId' - The system-generated ID of the pull request.
--
-- 'revisionId', 'updatePullRequestApprovalState_revisionId' - The system-generated ID of the revision.
--
-- 'approvalState', 'updatePullRequestApprovalState_approvalState' - The approval state to associate with the user on the pull request.
newUpdatePullRequestApprovalState ::
  -- | 'pullRequestId'
  Core.Text ->
  -- | 'revisionId'
  Core.Text ->
  -- | 'approvalState'
  ApprovalState ->
  UpdatePullRequestApprovalState
newUpdatePullRequestApprovalState
  pPullRequestId_
  pRevisionId_
  pApprovalState_ =
    UpdatePullRequestApprovalState'
      { pullRequestId =
          pPullRequestId_,
        revisionId = pRevisionId_,
        approvalState = pApprovalState_
      }

-- | The system-generated ID of the pull request.
updatePullRequestApprovalState_pullRequestId :: Lens.Lens' UpdatePullRequestApprovalState Core.Text
updatePullRequestApprovalState_pullRequestId = Lens.lens (\UpdatePullRequestApprovalState' {pullRequestId} -> pullRequestId) (\s@UpdatePullRequestApprovalState' {} a -> s {pullRequestId = a} :: UpdatePullRequestApprovalState)

-- | The system-generated ID of the revision.
updatePullRequestApprovalState_revisionId :: Lens.Lens' UpdatePullRequestApprovalState Core.Text
updatePullRequestApprovalState_revisionId = Lens.lens (\UpdatePullRequestApprovalState' {revisionId} -> revisionId) (\s@UpdatePullRequestApprovalState' {} a -> s {revisionId = a} :: UpdatePullRequestApprovalState)

-- | The approval state to associate with the user on the pull request.
updatePullRequestApprovalState_approvalState :: Lens.Lens' UpdatePullRequestApprovalState ApprovalState
updatePullRequestApprovalState_approvalState = Lens.lens (\UpdatePullRequestApprovalState' {approvalState} -> approvalState) (\s@UpdatePullRequestApprovalState' {} a -> s {approvalState = a} :: UpdatePullRequestApprovalState)

instance
  Core.AWSRequest
    UpdatePullRequestApprovalState
  where
  type
    AWSResponse UpdatePullRequestApprovalState =
      UpdatePullRequestApprovalStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdatePullRequestApprovalStateResponse'

instance Core.Hashable UpdatePullRequestApprovalState

instance Core.NFData UpdatePullRequestApprovalState

instance
  Core.ToHeaders
    UpdatePullRequestApprovalState
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.UpdatePullRequestApprovalState" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdatePullRequestApprovalState where
  toJSON UpdatePullRequestApprovalState' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("revisionId" Core..= revisionId),
            Core.Just ("approvalState" Core..= approvalState)
          ]
      )

instance Core.ToPath UpdatePullRequestApprovalState where
  toPath = Core.const "/"

instance Core.ToQuery UpdatePullRequestApprovalState where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePullRequestApprovalStateResponse' smart constructor.
data UpdatePullRequestApprovalStateResponse = UpdatePullRequestApprovalStateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePullRequestApprovalStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdatePullRequestApprovalStateResponse ::
  UpdatePullRequestApprovalStateResponse
newUpdatePullRequestApprovalStateResponse =
  UpdatePullRequestApprovalStateResponse'

instance
  Core.NFData
    UpdatePullRequestApprovalStateResponse
