{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdatePullRequestApprovalState (..)
    , mkUpdatePullRequestApprovalState
    -- ** Request lenses
    , uprasPullRequestId
    , uprasRevisionId
    , uprasApprovalState

    -- * Destructuring the response
    , UpdatePullRequestApprovalStateResponse (..)
    , mkUpdatePullRequestApprovalStateResponse
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePullRequestApprovalState' smart constructor.
data UpdatePullRequestApprovalState = UpdatePullRequestApprovalState'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request.
  , revisionId :: Types.RevisionId
    -- ^ The system-generated ID of the revision.
  , approvalState :: Types.ApprovalState
    -- ^ The approval state to associate with the user on the pull request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePullRequestApprovalState' value with any optional fields omitted.
mkUpdatePullRequestApprovalState
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.RevisionId -- ^ 'revisionId'
    -> Types.ApprovalState -- ^ 'approvalState'
    -> UpdatePullRequestApprovalState
mkUpdatePullRequestApprovalState pullRequestId revisionId
  approvalState
  = UpdatePullRequestApprovalState'{pullRequestId, revisionId,
                                    approvalState}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprasPullRequestId :: Lens.Lens' UpdatePullRequestApprovalState Types.PullRequestId
uprasPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE uprasPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The system-generated ID of the revision.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprasRevisionId :: Lens.Lens' UpdatePullRequestApprovalState Types.RevisionId
uprasRevisionId = Lens.field @"revisionId"
{-# INLINEABLE uprasRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

-- | The approval state to associate with the user on the pull request.
--
-- /Note:/ Consider using 'approvalState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprasApprovalState :: Lens.Lens' UpdatePullRequestApprovalState Types.ApprovalState
uprasApprovalState = Lens.field @"approvalState"
{-# INLINEABLE uprasApprovalState #-}
{-# DEPRECATED approvalState "Use generic-lens or generic-optics with 'approvalState' instead"  #-}

instance Core.ToQuery UpdatePullRequestApprovalState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePullRequestApprovalState where
        toHeaders UpdatePullRequestApprovalState{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.UpdatePullRequestApprovalState")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdatePullRequestApprovalState where
        toJSON UpdatePullRequestApprovalState{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("revisionId" Core..= revisionId),
                  Core.Just ("approvalState" Core..= approvalState)])

instance Core.AWSRequest UpdatePullRequestApprovalState where
        type Rs UpdatePullRequestApprovalState =
             UpdatePullRequestApprovalStateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull UpdatePullRequestApprovalStateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdatePullRequestApprovalStateResponse' smart constructor.
data UpdatePullRequestApprovalStateResponse = UpdatePullRequestApprovalStateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePullRequestApprovalStateResponse' value with any optional fields omitted.
mkUpdatePullRequestApprovalStateResponse
    :: UpdatePullRequestApprovalStateResponse
mkUpdatePullRequestApprovalStateResponse
  = UpdatePullRequestApprovalStateResponse'
