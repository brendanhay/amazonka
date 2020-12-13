{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestApprovalState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state of a user's approval on a pull request. The user is derived from the signed-in account when the request is made.
module Network.AWS.CodeCommit.UpdatePullRequestApprovalState
  ( -- * Creating a request
    UpdatePullRequestApprovalState (..),
    mkUpdatePullRequestApprovalState,

    -- ** Request lenses
    uprasApprovalState,
    uprasPullRequestId,
    uprasRevisionId,

    -- * Destructuring the response
    UpdatePullRequestApprovalStateResponse (..),
    mkUpdatePullRequestApprovalStateResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePullRequestApprovalState' smart constructor.
data UpdatePullRequestApprovalState = UpdatePullRequestApprovalState'
  { -- | The approval state to associate with the user on the pull request.
    approvalState :: ApprovalState,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Lude.Text,
    -- | The system-generated ID of the revision.
    revisionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestApprovalState' with the minimum fields required to make a request.
--
-- * 'approvalState' - The approval state to associate with the user on the pull request.
-- * 'pullRequestId' - The system-generated ID of the pull request.
-- * 'revisionId' - The system-generated ID of the revision.
mkUpdatePullRequestApprovalState ::
  -- | 'approvalState'
  ApprovalState ->
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'revisionId'
  Lude.Text ->
  UpdatePullRequestApprovalState
mkUpdatePullRequestApprovalState
  pApprovalState_
  pPullRequestId_
  pRevisionId_ =
    UpdatePullRequestApprovalState'
      { approvalState = pApprovalState_,
        pullRequestId = pPullRequestId_,
        revisionId = pRevisionId_
      }

-- | The approval state to associate with the user on the pull request.
--
-- /Note:/ Consider using 'approvalState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprasApprovalState :: Lens.Lens' UpdatePullRequestApprovalState ApprovalState
uprasApprovalState = Lens.lens (approvalState :: UpdatePullRequestApprovalState -> ApprovalState) (\s a -> s {approvalState = a} :: UpdatePullRequestApprovalState)
{-# DEPRECATED uprasApprovalState "Use generic-lens or generic-optics with 'approvalState' instead." #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprasPullRequestId :: Lens.Lens' UpdatePullRequestApprovalState Lude.Text
uprasPullRequestId = Lens.lens (pullRequestId :: UpdatePullRequestApprovalState -> Lude.Text) (\s a -> s {pullRequestId = a} :: UpdatePullRequestApprovalState)
{-# DEPRECATED uprasPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The system-generated ID of the revision.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprasRevisionId :: Lens.Lens' UpdatePullRequestApprovalState Lude.Text
uprasRevisionId = Lens.lens (revisionId :: UpdatePullRequestApprovalState -> Lude.Text) (\s a -> s {revisionId = a} :: UpdatePullRequestApprovalState)
{-# DEPRECATED uprasRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest UpdatePullRequestApprovalState where
  type
    Rs UpdatePullRequestApprovalState =
      UpdatePullRequestApprovalStateResponse
  request = Req.postJSON codeCommitService
  response = Res.receiveNull UpdatePullRequestApprovalStateResponse'

instance Lude.ToHeaders UpdatePullRequestApprovalState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.UpdatePullRequestApprovalState" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePullRequestApprovalState where
  toJSON UpdatePullRequestApprovalState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("approvalState" Lude..= approvalState),
            Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("revisionId" Lude..= revisionId)
          ]
      )

instance Lude.ToPath UpdatePullRequestApprovalState where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePullRequestApprovalState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePullRequestApprovalStateResponse' smart constructor.
data UpdatePullRequestApprovalStateResponse = UpdatePullRequestApprovalStateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestApprovalStateResponse' with the minimum fields required to make a request.
mkUpdatePullRequestApprovalStateResponse ::
  UpdatePullRequestApprovalStateResponse
mkUpdatePullRequestApprovalStateResponse =
  UpdatePullRequestApprovalStateResponse'
