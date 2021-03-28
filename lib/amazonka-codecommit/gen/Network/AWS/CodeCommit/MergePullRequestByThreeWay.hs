{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergePullRequestByThreeWay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the three-way merge strategy. If the merge is successful, it closes the pull request.
module Network.AWS.CodeCommit.MergePullRequestByThreeWay
    (
    -- * Creating a request
      MergePullRequestByThreeWay (..)
    , mkMergePullRequestByThreeWay
    -- ** Request lenses
    , mprbtwPullRequestId
    , mprbtwRepositoryName
    , mprbtwAuthorName
    , mprbtwCommitMessage
    , mprbtwConflictDetailLevel
    , mprbtwConflictResolution
    , mprbtwConflictResolutionStrategy
    , mprbtwEmail
    , mprbtwKeepEmptyFolders
    , mprbtwSourceCommitId

    -- * Destructuring the response
    , MergePullRequestByThreeWayResponse (..)
    , mkMergePullRequestByThreeWayResponse
    -- ** Response lenses
    , mprbtwrrsPullRequest
    , mprbtwrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMergePullRequestByThreeWay' smart constructor.
data MergePullRequestByThreeWay = MergePullRequestByThreeWay'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
  , repositoryName :: Types.RepositoryName
    -- ^ The name of the repository where the pull request was created.
  , authorName :: Core.Maybe Types.Name
    -- ^ The name of the author who created the commit. This information is used as both the author and committer for the commit.
  , commitMessage :: Core.Maybe Types.CommitMessage
    -- ^ The commit message to include in the commit information for the merge.
  , conflictDetailLevel :: Core.Maybe Types.ConflictDetailLevelTypeEnum
    -- ^ The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
  , conflictResolution :: Core.Maybe Types.ConflictResolution
    -- ^ If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
  , conflictResolutionStrategy :: Core.Maybe Types.ConflictResolutionStrategyTypeEnum
    -- ^ Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
  , email :: Core.Maybe Types.Email
    -- ^ The email address of the person merging the branches. This information is used in the commit information for the merge.
  , keepEmptyFolders :: Core.Maybe Core.Bool
    -- ^ If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
  , sourceCommitId :: Core.Maybe Types.ObjectId
    -- ^ The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergePullRequestByThreeWay' value with any optional fields omitted.
mkMergePullRequestByThreeWay
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.RepositoryName -- ^ 'repositoryName'
    -> MergePullRequestByThreeWay
mkMergePullRequestByThreeWay pullRequestId repositoryName
  = MergePullRequestByThreeWay'{pullRequestId, repositoryName,
                                authorName = Core.Nothing, commitMessage = Core.Nothing,
                                conflictDetailLevel = Core.Nothing,
                                conflictResolution = Core.Nothing,
                                conflictResolutionStrategy = Core.Nothing, email = Core.Nothing,
                                keepEmptyFolders = Core.Nothing, sourceCommitId = Core.Nothing}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwPullRequestId :: Lens.Lens' MergePullRequestByThreeWay Types.PullRequestId
mprbtwPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE mprbtwPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwRepositoryName :: Lens.Lens' MergePullRequestByThreeWay Types.RepositoryName
mprbtwRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE mprbtwRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwAuthorName :: Lens.Lens' MergePullRequestByThreeWay (Core.Maybe Types.Name)
mprbtwAuthorName = Lens.field @"authorName"
{-# INLINEABLE mprbtwAuthorName #-}
{-# DEPRECATED authorName "Use generic-lens or generic-optics with 'authorName' instead"  #-}

-- | The commit message to include in the commit information for the merge.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwCommitMessage :: Lens.Lens' MergePullRequestByThreeWay (Core.Maybe Types.CommitMessage)
mprbtwCommitMessage = Lens.field @"commitMessage"
{-# INLINEABLE mprbtwCommitMessage #-}
{-# DEPRECATED commitMessage "Use generic-lens or generic-optics with 'commitMessage' instead"  #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwConflictDetailLevel :: Lens.Lens' MergePullRequestByThreeWay (Core.Maybe Types.ConflictDetailLevelTypeEnum)
mprbtwConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# INLINEABLE mprbtwConflictDetailLevel #-}
{-# DEPRECATED conflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead"  #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwConflictResolution :: Lens.Lens' MergePullRequestByThreeWay (Core.Maybe Types.ConflictResolution)
mprbtwConflictResolution = Lens.field @"conflictResolution"
{-# INLINEABLE mprbtwConflictResolution #-}
{-# DEPRECATED conflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead"  #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwConflictResolutionStrategy :: Lens.Lens' MergePullRequestByThreeWay (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
mprbtwConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# INLINEABLE mprbtwConflictResolutionStrategy #-}
{-# DEPRECATED conflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead"  #-}

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwEmail :: Lens.Lens' MergePullRequestByThreeWay (Core.Maybe Types.Email)
mprbtwEmail = Lens.field @"email"
{-# INLINEABLE mprbtwEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwKeepEmptyFolders :: Lens.Lens' MergePullRequestByThreeWay (Core.Maybe Core.Bool)
mprbtwKeepEmptyFolders = Lens.field @"keepEmptyFolders"
{-# INLINEABLE mprbtwKeepEmptyFolders #-}
{-# DEPRECATED keepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead"  #-}

-- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwSourceCommitId :: Lens.Lens' MergePullRequestByThreeWay (Core.Maybe Types.ObjectId)
mprbtwSourceCommitId = Lens.field @"sourceCommitId"
{-# INLINEABLE mprbtwSourceCommitId #-}
{-# DEPRECATED sourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead"  #-}

instance Core.ToQuery MergePullRequestByThreeWay where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders MergePullRequestByThreeWay where
        toHeaders MergePullRequestByThreeWay{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.MergePullRequestByThreeWay")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON MergePullRequestByThreeWay where
        toJSON MergePullRequestByThreeWay{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("repositoryName" Core..= repositoryName),
                  ("authorName" Core..=) Core.<$> authorName,
                  ("commitMessage" Core..=) Core.<$> commitMessage,
                  ("conflictDetailLevel" Core..=) Core.<$> conflictDetailLevel,
                  ("conflictResolution" Core..=) Core.<$> conflictResolution,
                  ("conflictResolutionStrategy" Core..=) Core.<$>
                    conflictResolutionStrategy,
                  ("email" Core..=) Core.<$> email,
                  ("keepEmptyFolders" Core..=) Core.<$> keepEmptyFolders,
                  ("sourceCommitId" Core..=) Core.<$> sourceCommitId])

instance Core.AWSRequest MergePullRequestByThreeWay where
        type Rs MergePullRequestByThreeWay =
             MergePullRequestByThreeWayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 MergePullRequestByThreeWayResponse' Core.<$>
                   (x Core..:? "pullRequest") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkMergePullRequestByThreeWayResponse' smart constructor.
data MergePullRequestByThreeWayResponse = MergePullRequestByThreeWayResponse'
  { pullRequest :: Core.Maybe Types.PullRequest
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MergePullRequestByThreeWayResponse' value with any optional fields omitted.
mkMergePullRequestByThreeWayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> MergePullRequestByThreeWayResponse
mkMergePullRequestByThreeWayResponse responseStatus
  = MergePullRequestByThreeWayResponse'{pullRequest = Core.Nothing,
                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwrrsPullRequest :: Lens.Lens' MergePullRequestByThreeWayResponse (Core.Maybe Types.PullRequest)
mprbtwrrsPullRequest = Lens.field @"pullRequest"
{-# INLINEABLE mprbtwrrsPullRequest #-}
{-# DEPRECATED pullRequest "Use generic-lens or generic-optics with 'pullRequest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwrrsResponseStatus :: Lens.Lens' MergePullRequestByThreeWayResponse Core.Int
mprbtwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mprbtwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
