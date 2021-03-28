{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergeBranchesBySquash
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two branches using the squash merge strategy.
module Network.AWS.CodeCommit.MergeBranchesBySquash
    (
    -- * Creating a request
      MergeBranchesBySquash (..)
    , mkMergeBranchesBySquash
    -- ** Request lenses
    , mbbsRepositoryName
    , mbbsSourceCommitSpecifier
    , mbbsDestinationCommitSpecifier
    , mbbsAuthorName
    , mbbsCommitMessage
    , mbbsConflictDetailLevel
    , mbbsConflictResolution
    , mbbsConflictResolutionStrategy
    , mbbsEmail
    , mbbsKeepEmptyFolders
    , mbbsTargetBranch

    -- * Destructuring the response
    , MergeBranchesBySquashResponse (..)
    , mkMergeBranchesBySquashResponse
    -- ** Response lenses
    , mbbsrrsCommitId
    , mbbsrrsTreeId
    , mbbsrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMergeBranchesBySquash' smart constructor.
data MergeBranchesBySquash = MergeBranchesBySquash'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository where you want to merge two branches.
  , sourceCommitSpecifier :: Types.SourceCommitSpecifier
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , destinationCommitSpecifier :: Types.DestinationCommitSpecifier
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , authorName :: Core.Maybe Types.Name
    -- ^ The name of the author who created the commit. This information is used as both the author and committer for the commit.
  , commitMessage :: Core.Maybe Types.CommitMessage
    -- ^ The commit message for the merge.
  , conflictDetailLevel :: Core.Maybe Types.ConflictDetailLevelTypeEnum
    -- ^ The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
  , conflictResolution :: Core.Maybe Types.ConflictResolution
    -- ^ If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
  , conflictResolutionStrategy :: Core.Maybe Types.ConflictResolutionStrategyTypeEnum
    -- ^ Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
  , email :: Core.Maybe Types.Email
    -- ^ The email address of the person merging the branches. This information is used in the commit information for the merge.
  , keepEmptyFolders :: Core.Maybe Core.Bool
    -- ^ If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
  , targetBranch :: Core.Maybe Types.BranchName
    -- ^ The branch where the merge is applied. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeBranchesBySquash' value with any optional fields omitted.
mkMergeBranchesBySquash
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.SourceCommitSpecifier -- ^ 'sourceCommitSpecifier'
    -> Types.DestinationCommitSpecifier -- ^ 'destinationCommitSpecifier'
    -> MergeBranchesBySquash
mkMergeBranchesBySquash repositoryName sourceCommitSpecifier
  destinationCommitSpecifier
  = MergeBranchesBySquash'{repositoryName, sourceCommitSpecifier,
                           destinationCommitSpecifier, authorName = Core.Nothing,
                           commitMessage = Core.Nothing, conflictDetailLevel = Core.Nothing,
                           conflictResolution = Core.Nothing,
                           conflictResolutionStrategy = Core.Nothing, email = Core.Nothing,
                           keepEmptyFolders = Core.Nothing, targetBranch = Core.Nothing}

-- | The name of the repository where you want to merge two branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsRepositoryName :: Lens.Lens' MergeBranchesBySquash Types.RepositoryName
mbbsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE mbbsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsSourceCommitSpecifier :: Lens.Lens' MergeBranchesBySquash Types.SourceCommitSpecifier
mbbsSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# INLINEABLE mbbsSourceCommitSpecifier #-}
{-# DEPRECATED sourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsDestinationCommitSpecifier :: Lens.Lens' MergeBranchesBySquash Types.DestinationCommitSpecifier
mbbsDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# INLINEABLE mbbsDestinationCommitSpecifier #-}
{-# DEPRECATED destinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead"  #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsAuthorName :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.Name)
mbbsAuthorName = Lens.field @"authorName"
{-# INLINEABLE mbbsAuthorName #-}
{-# DEPRECATED authorName "Use generic-lens or generic-optics with 'authorName' instead"  #-}

-- | The commit message for the merge.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsCommitMessage :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.CommitMessage)
mbbsCommitMessage = Lens.field @"commitMessage"
{-# INLINEABLE mbbsCommitMessage #-}
{-# DEPRECATED commitMessage "Use generic-lens or generic-optics with 'commitMessage' instead"  #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictDetailLevel :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.ConflictDetailLevelTypeEnum)
mbbsConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# INLINEABLE mbbsConflictDetailLevel #-}
{-# DEPRECATED conflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead"  #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictResolution :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.ConflictResolution)
mbbsConflictResolution = Lens.field @"conflictResolution"
{-# INLINEABLE mbbsConflictResolution #-}
{-# DEPRECATED conflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead"  #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictResolutionStrategy :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
mbbsConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# INLINEABLE mbbsConflictResolutionStrategy #-}
{-# DEPRECATED conflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead"  #-}

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsEmail :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.Email)
mbbsEmail = Lens.field @"email"
{-# INLINEABLE mbbsEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsKeepEmptyFolders :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Core.Bool)
mbbsKeepEmptyFolders = Lens.field @"keepEmptyFolders"
{-# INLINEABLE mbbsKeepEmptyFolders #-}
{-# DEPRECATED keepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead"  #-}

-- | The branch where the merge is applied. 
--
-- /Note:/ Consider using 'targetBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsTargetBranch :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.BranchName)
mbbsTargetBranch = Lens.field @"targetBranch"
{-# INLINEABLE mbbsTargetBranch #-}
{-# DEPRECATED targetBranch "Use generic-lens or generic-optics with 'targetBranch' instead"  #-}

instance Core.ToQuery MergeBranchesBySquash where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders MergeBranchesBySquash where
        toHeaders MergeBranchesBySquash{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.MergeBranchesBySquash")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON MergeBranchesBySquash where
        toJSON MergeBranchesBySquash{..}
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

instance Core.AWSRequest MergeBranchesBySquash where
        type Rs MergeBranchesBySquash = MergeBranchesBySquashResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 MergeBranchesBySquashResponse' Core.<$>
                   (x Core..:? "commitId") Core.<*> x Core..:? "treeId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkMergeBranchesBySquashResponse' smart constructor.
data MergeBranchesBySquashResponse = MergeBranchesBySquashResponse'
  { commitId :: Core.Maybe Types.ObjectId
    -- ^ The commit ID of the merge in the destination or target branch.
  , treeId :: Core.Maybe Types.ObjectId
    -- ^ The tree ID of the merge in the destination or target branch.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeBranchesBySquashResponse' value with any optional fields omitted.
mkMergeBranchesBySquashResponse
    :: Core.Int -- ^ 'responseStatus'
    -> MergeBranchesBySquashResponse
mkMergeBranchesBySquashResponse responseStatus
  = MergeBranchesBySquashResponse'{commitId = Core.Nothing,
                                   treeId = Core.Nothing, responseStatus}

-- | The commit ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrrsCommitId :: Lens.Lens' MergeBranchesBySquashResponse (Core.Maybe Types.ObjectId)
mbbsrrsCommitId = Lens.field @"commitId"
{-# INLINEABLE mbbsrrsCommitId #-}
{-# DEPRECATED commitId "Use generic-lens or generic-optics with 'commitId' instead"  #-}

-- | The tree ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrrsTreeId :: Lens.Lens' MergeBranchesBySquashResponse (Core.Maybe Types.ObjectId)
mbbsrrsTreeId = Lens.field @"treeId"
{-# INLINEABLE mbbsrrsTreeId #-}
{-# DEPRECATED treeId "Use generic-lens or generic-optics with 'treeId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrrsResponseStatus :: Lens.Lens' MergeBranchesBySquashResponse Core.Int
mbbsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mbbsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
