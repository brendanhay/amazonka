{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergeBranchesByThreeWay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two specified branches using the three-way merge strategy.
module Network.AWS.CodeCommit.MergeBranchesByThreeWay
    (
    -- * Creating a request
      MergeBranchesByThreeWay (..)
    , mkMergeBranchesByThreeWay
    -- ** Request lenses
    , mbbtwRepositoryName
    , mbbtwSourceCommitSpecifier
    , mbbtwDestinationCommitSpecifier
    , mbbtwAuthorName
    , mbbtwCommitMessage
    , mbbtwConflictDetailLevel
    , mbbtwConflictResolution
    , mbbtwConflictResolutionStrategy
    , mbbtwEmail
    , mbbtwKeepEmptyFolders
    , mbbtwTargetBranch

    -- * Destructuring the response
    , MergeBranchesByThreeWayResponse (..)
    , mkMergeBranchesByThreeWayResponse
    -- ** Response lenses
    , mbbtwrrsCommitId
    , mbbtwrrsTreeId
    , mbbtwrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMergeBranchesByThreeWay' smart constructor.
data MergeBranchesByThreeWay = MergeBranchesByThreeWay'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository where you want to merge two branches.
  , sourceCommitSpecifier :: Types.CommitName
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , destinationCommitSpecifier :: Types.CommitName
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , authorName :: Core.Maybe Types.Name
    -- ^ The name of the author who created the commit. This information is used as both the author and committer for the commit.
  , commitMessage :: Core.Maybe Types.Message
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
  , targetBranch :: Core.Maybe Types.BranchName
    -- ^ The branch where the merge is applied. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeBranchesByThreeWay' value with any optional fields omitted.
mkMergeBranchesByThreeWay
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.CommitName -- ^ 'sourceCommitSpecifier'
    -> Types.CommitName -- ^ 'destinationCommitSpecifier'
    -> MergeBranchesByThreeWay
mkMergeBranchesByThreeWay repositoryName sourceCommitSpecifier
  destinationCommitSpecifier
  = MergeBranchesByThreeWay'{repositoryName, sourceCommitSpecifier,
                             destinationCommitSpecifier, authorName = Core.Nothing,
                             commitMessage = Core.Nothing, conflictDetailLevel = Core.Nothing,
                             conflictResolution = Core.Nothing,
                             conflictResolutionStrategy = Core.Nothing, email = Core.Nothing,
                             keepEmptyFolders = Core.Nothing, targetBranch = Core.Nothing}

-- | The name of the repository where you want to merge two branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwRepositoryName :: Lens.Lens' MergeBranchesByThreeWay Types.RepositoryName
mbbtwRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE mbbtwRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwSourceCommitSpecifier :: Lens.Lens' MergeBranchesByThreeWay Types.CommitName
mbbtwSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# INLINEABLE mbbtwSourceCommitSpecifier #-}
{-# DEPRECATED sourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwDestinationCommitSpecifier :: Lens.Lens' MergeBranchesByThreeWay Types.CommitName
mbbtwDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# INLINEABLE mbbtwDestinationCommitSpecifier #-}
{-# DEPRECATED destinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead"  #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwAuthorName :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Types.Name)
mbbtwAuthorName = Lens.field @"authorName"
{-# INLINEABLE mbbtwAuthorName #-}
{-# DEPRECATED authorName "Use generic-lens or generic-optics with 'authorName' instead"  #-}

-- | The commit message to include in the commit information for the merge.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwCommitMessage :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Types.Message)
mbbtwCommitMessage = Lens.field @"commitMessage"
{-# INLINEABLE mbbtwCommitMessage #-}
{-# DEPRECATED commitMessage "Use generic-lens or generic-optics with 'commitMessage' instead"  #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwConflictDetailLevel :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Types.ConflictDetailLevelTypeEnum)
mbbtwConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# INLINEABLE mbbtwConflictDetailLevel #-}
{-# DEPRECATED conflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead"  #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwConflictResolution :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Types.ConflictResolution)
mbbtwConflictResolution = Lens.field @"conflictResolution"
{-# INLINEABLE mbbtwConflictResolution #-}
{-# DEPRECATED conflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead"  #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwConflictResolutionStrategy :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
mbbtwConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# INLINEABLE mbbtwConflictResolutionStrategy #-}
{-# DEPRECATED conflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead"  #-}

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwEmail :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Types.Email)
mbbtwEmail = Lens.field @"email"
{-# INLINEABLE mbbtwEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwKeepEmptyFolders :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Core.Bool)
mbbtwKeepEmptyFolders = Lens.field @"keepEmptyFolders"
{-# INLINEABLE mbbtwKeepEmptyFolders #-}
{-# DEPRECATED keepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead"  #-}

-- | The branch where the merge is applied. 
--
-- /Note:/ Consider using 'targetBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwTargetBranch :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Types.BranchName)
mbbtwTargetBranch = Lens.field @"targetBranch"
{-# INLINEABLE mbbtwTargetBranch #-}
{-# DEPRECATED targetBranch "Use generic-lens or generic-optics with 'targetBranch' instead"  #-}

instance Core.ToQuery MergeBranchesByThreeWay where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders MergeBranchesByThreeWay where
        toHeaders MergeBranchesByThreeWay{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.MergeBranchesByThreeWay")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON MergeBranchesByThreeWay where
        toJSON MergeBranchesByThreeWay{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("sourceCommitSpecifier" Core..= sourceCommitSpecifier),
                  Core.Just
                    ("destinationCommitSpecifier" Core..= destinationCommitSpecifier),
                  ("authorName" Core..=) Core.<$> authorName,
                  ("commitMessage" Core..=) Core.<$> commitMessage,
                  ("conflictDetailLevel" Core..=) Core.<$> conflictDetailLevel,
                  ("conflictResolution" Core..=) Core.<$> conflictResolution,
                  ("conflictResolutionStrategy" Core..=) Core.<$>
                    conflictResolutionStrategy,
                  ("email" Core..=) Core.<$> email,
                  ("keepEmptyFolders" Core..=) Core.<$> keepEmptyFolders,
                  ("targetBranch" Core..=) Core.<$> targetBranch])

instance Core.AWSRequest MergeBranchesByThreeWay where
        type Rs MergeBranchesByThreeWay = MergeBranchesByThreeWayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 MergeBranchesByThreeWayResponse' Core.<$>
                   (x Core..:? "commitId") Core.<*> x Core..:? "treeId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkMergeBranchesByThreeWayResponse' smart constructor.
data MergeBranchesByThreeWayResponse = MergeBranchesByThreeWayResponse'
  { commitId :: Core.Maybe Types.CommitId
    -- ^ The commit ID of the merge in the destination or target branch.
  , treeId :: Core.Maybe Types.TreeId
    -- ^ The tree ID of the merge in the destination or target branch.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeBranchesByThreeWayResponse' value with any optional fields omitted.
mkMergeBranchesByThreeWayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> MergeBranchesByThreeWayResponse
mkMergeBranchesByThreeWayResponse responseStatus
  = MergeBranchesByThreeWayResponse'{commitId = Core.Nothing,
                                     treeId = Core.Nothing, responseStatus}

-- | The commit ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwrrsCommitId :: Lens.Lens' MergeBranchesByThreeWayResponse (Core.Maybe Types.CommitId)
mbbtwrrsCommitId = Lens.field @"commitId"
{-# INLINEABLE mbbtwrrsCommitId #-}
{-# DEPRECATED commitId "Use generic-lens or generic-optics with 'commitId' instead"  #-}

-- | The tree ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwrrsTreeId :: Lens.Lens' MergeBranchesByThreeWayResponse (Core.Maybe Types.TreeId)
mbbtwrrsTreeId = Lens.field @"treeId"
{-# INLINEABLE mbbtwrrsTreeId #-}
{-# DEPRECATED treeId "Use generic-lens or generic-optics with 'treeId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwrrsResponseStatus :: Lens.Lens' MergeBranchesByThreeWayResponse Core.Int
mbbtwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mbbtwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
