{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    MergeBranchesBySquash (..),
    mkMergeBranchesBySquash,

    -- ** Request lenses
    mbbsRepositoryName,
    mbbsSourceCommitSpecifier,
    mbbsDestinationCommitSpecifier,
    mbbsAuthorName,
    mbbsCommitMessage,
    mbbsConflictDetailLevel,
    mbbsConflictResolution,
    mbbsConflictResolutionStrategy,
    mbbsEmail,
    mbbsKeepEmptyFolders,
    mbbsTargetBranch,

    -- * Destructuring the response
    MergeBranchesBySquashResponse (..),
    mkMergeBranchesBySquashResponse,

    -- ** Response lenses
    mbbsrrsCommitId,
    mbbsrrsTreeId,
    mbbsrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMergeBranchesBySquash' smart constructor.
data MergeBranchesBySquash = MergeBranchesBySquash'
  { -- | The name of the repository where you want to merge two branches.
    repositoryName :: Types.RepositoryName,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Types.SourceCommitSpecifier,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Types.DestinationCommitSpecifier,
    -- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
    authorName :: Core.Maybe Types.Name,
    -- | The commit message for the merge.
    commitMessage :: Core.Maybe Types.CommitMessage,
    -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Core.Maybe Types.ConflictDetailLevelTypeEnum,
    -- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
    conflictResolution :: Core.Maybe Types.ConflictResolution,
    -- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
    conflictResolutionStrategy :: Core.Maybe Types.ConflictResolutionStrategyTypeEnum,
    -- | The email address of the person merging the branches. This information is used in the commit information for the merge.
    email :: Core.Maybe Types.Email,
    -- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
    keepEmptyFolders :: Core.Maybe Core.Bool,
    -- | The branch where the merge is applied.
    targetBranch :: Core.Maybe Types.BranchName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeBranchesBySquash' value with any optional fields omitted.
mkMergeBranchesBySquash ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'sourceCommitSpecifier'
  Types.SourceCommitSpecifier ->
  -- | 'destinationCommitSpecifier'
  Types.DestinationCommitSpecifier ->
  MergeBranchesBySquash
mkMergeBranchesBySquash
  repositoryName
  sourceCommitSpecifier
  destinationCommitSpecifier =
    MergeBranchesBySquash'
      { repositoryName,
        sourceCommitSpecifier,
        destinationCommitSpecifier,
        authorName = Core.Nothing,
        commitMessage = Core.Nothing,
        conflictDetailLevel = Core.Nothing,
        conflictResolution = Core.Nothing,
        conflictResolutionStrategy = Core.Nothing,
        email = Core.Nothing,
        keepEmptyFolders = Core.Nothing,
        targetBranch = Core.Nothing
      }

-- | The name of the repository where you want to merge two branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsRepositoryName :: Lens.Lens' MergeBranchesBySquash Types.RepositoryName
mbbsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED mbbsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsSourceCommitSpecifier :: Lens.Lens' MergeBranchesBySquash Types.SourceCommitSpecifier
mbbsSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# DEPRECATED mbbsSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsDestinationCommitSpecifier :: Lens.Lens' MergeBranchesBySquash Types.DestinationCommitSpecifier
mbbsDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# DEPRECATED mbbsDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsAuthorName :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.Name)
mbbsAuthorName = Lens.field @"authorName"
{-# DEPRECATED mbbsAuthorName "Use generic-lens or generic-optics with 'authorName' instead." #-}

-- | The commit message for the merge.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsCommitMessage :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.CommitMessage)
mbbsCommitMessage = Lens.field @"commitMessage"
{-# DEPRECATED mbbsCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictDetailLevel :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.ConflictDetailLevelTypeEnum)
mbbsConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# DEPRECATED mbbsConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictResolution :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.ConflictResolution)
mbbsConflictResolution = Lens.field @"conflictResolution"
{-# DEPRECATED mbbsConflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictResolutionStrategy :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
mbbsConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# DEPRECATED mbbsConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsEmail :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.Email)
mbbsEmail = Lens.field @"email"
{-# DEPRECATED mbbsEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsKeepEmptyFolders :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Core.Bool)
mbbsKeepEmptyFolders = Lens.field @"keepEmptyFolders"
{-# DEPRECATED mbbsKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

-- | The branch where the merge is applied.
--
-- /Note:/ Consider using 'targetBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsTargetBranch :: Lens.Lens' MergeBranchesBySquash (Core.Maybe Types.BranchName)
mbbsTargetBranch = Lens.field @"targetBranch"
{-# DEPRECATED mbbsTargetBranch "Use generic-lens or generic-optics with 'targetBranch' instead." #-}

instance Core.FromJSON MergeBranchesBySquash where
  toJSON MergeBranchesBySquash {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("sourceCommitSpecifier" Core..= sourceCommitSpecifier),
            Core.Just
              ("destinationCommitSpecifier" Core..= destinationCommitSpecifier),
            ("authorName" Core..=) Core.<$> authorName,
            ("commitMessage" Core..=) Core.<$> commitMessage,
            ("conflictDetailLevel" Core..=) Core.<$> conflictDetailLevel,
            ("conflictResolution" Core..=) Core.<$> conflictResolution,
            ("conflictResolutionStrategy" Core..=)
              Core.<$> conflictResolutionStrategy,
            ("email" Core..=) Core.<$> email,
            ("keepEmptyFolders" Core..=) Core.<$> keepEmptyFolders,
            ("targetBranch" Core..=) Core.<$> targetBranch
          ]
      )

instance Core.AWSRequest MergeBranchesBySquash where
  type Rs MergeBranchesBySquash = MergeBranchesBySquashResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.MergeBranchesBySquash")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          MergeBranchesBySquashResponse'
            Core.<$> (x Core..:? "commitId")
            Core.<*> (x Core..:? "treeId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkMergeBranchesBySquashResponse' smart constructor.
data MergeBranchesBySquashResponse = MergeBranchesBySquashResponse'
  { -- | The commit ID of the merge in the destination or target branch.
    commitId :: Core.Maybe Types.ObjectId,
    -- | The tree ID of the merge in the destination or target branch.
    treeId :: Core.Maybe Types.ObjectId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeBranchesBySquashResponse' value with any optional fields omitted.
mkMergeBranchesBySquashResponse ::
  -- | 'responseStatus'
  Core.Int ->
  MergeBranchesBySquashResponse
mkMergeBranchesBySquashResponse responseStatus =
  MergeBranchesBySquashResponse'
    { commitId = Core.Nothing,
      treeId = Core.Nothing,
      responseStatus
    }

-- | The commit ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrrsCommitId :: Lens.Lens' MergeBranchesBySquashResponse (Core.Maybe Types.ObjectId)
mbbsrrsCommitId = Lens.field @"commitId"
{-# DEPRECATED mbbsrrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The tree ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrrsTreeId :: Lens.Lens' MergeBranchesBySquashResponse (Core.Maybe Types.ObjectId)
mbbsrrsTreeId = Lens.field @"treeId"
{-# DEPRECATED mbbsrrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrrsResponseStatus :: Lens.Lens' MergeBranchesBySquashResponse Core.Int
mbbsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mbbsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
