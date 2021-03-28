{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergePullRequestByFastForward
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the fast-forward merge strategy. If the merge is successful, it closes the pull request.
module Network.AWS.CodeCommit.MergePullRequestByFastForward
    (
    -- * Creating a request
      MergePullRequestByFastForward (..)
    , mkMergePullRequestByFastForward
    -- ** Request lenses
    , mprbffPullRequestId
    , mprbffRepositoryName
    , mprbffSourceCommitId

    -- * Destructuring the response
    , MergePullRequestByFastForwardResponse (..)
    , mkMergePullRequestByFastForwardResponse
    -- ** Response lenses
    , mprbffrrsPullRequest
    , mprbffrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMergePullRequestByFastForward' smart constructor.
data MergePullRequestByFastForward = MergePullRequestByFastForward'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
  , repositoryName :: Types.RepositoryName
    -- ^ The name of the repository where the pull request was created.
  , sourceCommitId :: Core.Maybe Types.ObjectId
    -- ^ The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergePullRequestByFastForward' value with any optional fields omitted.
mkMergePullRequestByFastForward
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.RepositoryName -- ^ 'repositoryName'
    -> MergePullRequestByFastForward
mkMergePullRequestByFastForward pullRequestId repositoryName
  = MergePullRequestByFastForward'{pullRequestId, repositoryName,
                                   sourceCommitId = Core.Nothing}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffPullRequestId :: Lens.Lens' MergePullRequestByFastForward Types.PullRequestId
mprbffPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE mprbffPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffRepositoryName :: Lens.Lens' MergePullRequestByFastForward Types.RepositoryName
mprbffRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE mprbffRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffSourceCommitId :: Lens.Lens' MergePullRequestByFastForward (Core.Maybe Types.ObjectId)
mprbffSourceCommitId = Lens.field @"sourceCommitId"
{-# INLINEABLE mprbffSourceCommitId #-}
{-# DEPRECATED sourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead"  #-}

instance Core.ToQuery MergePullRequestByFastForward where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders MergePullRequestByFastForward where
        toHeaders MergePullRequestByFastForward{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.MergePullRequestByFastForward")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON MergePullRequestByFastForward where
        toJSON MergePullRequestByFastForward{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("repositoryName" Core..= repositoryName),
                  ("sourceCommitId" Core..=) Core.<$> sourceCommitId])

instance Core.AWSRequest MergePullRequestByFastForward where
        type Rs MergePullRequestByFastForward =
             MergePullRequestByFastForwardResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 MergePullRequestByFastForwardResponse' Core.<$>
                   (x Core..:? "pullRequest") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkMergePullRequestByFastForwardResponse' smart constructor.
data MergePullRequestByFastForwardResponse = MergePullRequestByFastForwardResponse'
  { pullRequest :: Core.Maybe Types.PullRequest
    -- ^ Information about the specified pull request, including the merge.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MergePullRequestByFastForwardResponse' value with any optional fields omitted.
mkMergePullRequestByFastForwardResponse
    :: Core.Int -- ^ 'responseStatus'
    -> MergePullRequestByFastForwardResponse
mkMergePullRequestByFastForwardResponse responseStatus
  = MergePullRequestByFastForwardResponse'{pullRequest =
                                             Core.Nothing,
                                           responseStatus}

-- | Information about the specified pull request, including the merge.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffrrsPullRequest :: Lens.Lens' MergePullRequestByFastForwardResponse (Core.Maybe Types.PullRequest)
mprbffrrsPullRequest = Lens.field @"pullRequest"
{-# INLINEABLE mprbffrrsPullRequest #-}
{-# DEPRECATED pullRequest "Use generic-lens or generic-optics with 'pullRequest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffrrsResponseStatus :: Lens.Lens' MergePullRequestByFastForwardResponse Core.Int
mprbffrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mprbffrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
