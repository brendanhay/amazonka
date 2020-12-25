{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetMergeCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified merge commit.
module Network.AWS.CodeCommit.GetMergeCommit
  ( -- * Creating a request
    GetMergeCommit (..),
    mkGetMergeCommit,

    -- ** Request lenses
    gmcRepositoryName,
    gmcSourceCommitSpecifier,
    gmcDestinationCommitSpecifier,
    gmcConflictDetailLevel,
    gmcConflictResolutionStrategy,

    -- * Destructuring the response
    GetMergeCommitResponse (..),
    mkGetMergeCommitResponse,

    -- ** Response lenses
    gmcrrsBaseCommitId,
    gmcrrsDestinationCommitId,
    gmcrrsMergedCommitId,
    gmcrrsSourceCommitId,
    gmcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMergeCommit' smart constructor.
data GetMergeCommit = GetMergeCommit'
  { -- | The name of the repository that contains the merge commit about which you want to get information.
    repositoryName :: Types.RepositoryName,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Types.CommitName,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Types.CommitName,
    -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Core.Maybe Types.ConflictDetailLevelTypeEnum,
    -- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
    conflictResolutionStrategy :: Core.Maybe Types.ConflictResolutionStrategyTypeEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMergeCommit' value with any optional fields omitted.
mkGetMergeCommit ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'sourceCommitSpecifier'
  Types.CommitName ->
  -- | 'destinationCommitSpecifier'
  Types.CommitName ->
  GetMergeCommit
mkGetMergeCommit
  repositoryName
  sourceCommitSpecifier
  destinationCommitSpecifier =
    GetMergeCommit'
      { repositoryName,
        sourceCommitSpecifier,
        destinationCommitSpecifier,
        conflictDetailLevel = Core.Nothing,
        conflictResolutionStrategy = Core.Nothing
      }

-- | The name of the repository that contains the merge commit about which you want to get information.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcRepositoryName :: Lens.Lens' GetMergeCommit Types.RepositoryName
gmcRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED gmcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcSourceCommitSpecifier :: Lens.Lens' GetMergeCommit Types.CommitName
gmcSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# DEPRECATED gmcSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcDestinationCommitSpecifier :: Lens.Lens' GetMergeCommit Types.CommitName
gmcDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# DEPRECATED gmcDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcConflictDetailLevel :: Lens.Lens' GetMergeCommit (Core.Maybe Types.ConflictDetailLevelTypeEnum)
gmcConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# DEPRECATED gmcConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcConflictResolutionStrategy :: Lens.Lens' GetMergeCommit (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
gmcConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# DEPRECATED gmcConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

instance Core.FromJSON GetMergeCommit where
  toJSON GetMergeCommit {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("sourceCommitSpecifier" Core..= sourceCommitSpecifier),
            Core.Just
              ("destinationCommitSpecifier" Core..= destinationCommitSpecifier),
            ("conflictDetailLevel" Core..=) Core.<$> conflictDetailLevel,
            ("conflictResolutionStrategy" Core..=)
              Core.<$> conflictResolutionStrategy
          ]
      )

instance Core.AWSRequest GetMergeCommit where
  type Rs GetMergeCommit = GetMergeCommitResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetMergeCommit")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMergeCommitResponse'
            Core.<$> (x Core..:? "baseCommitId")
            Core.<*> (x Core..:? "destinationCommitId")
            Core.<*> (x Core..:? "mergedCommitId")
            Core.<*> (x Core..:? "sourceCommitId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMergeCommitResponse' smart constructor.
data GetMergeCommitResponse = GetMergeCommitResponse'
  { -- | The commit ID of the merge base.
    baseCommitId :: Core.Maybe Types.BaseCommitId,
    -- | The commit ID of the destination commit specifier that was used in the merge evaluation.
    destinationCommitId :: Core.Maybe Types.DestinationCommitId,
    -- | The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
    mergedCommitId :: Core.Maybe Types.MergedCommitId,
    -- | The commit ID of the source commit specifier that was used in the merge evaluation.
    sourceCommitId :: Core.Maybe Types.SourceCommitId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMergeCommitResponse' value with any optional fields omitted.
mkGetMergeCommitResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMergeCommitResponse
mkGetMergeCommitResponse responseStatus =
  GetMergeCommitResponse'
    { baseCommitId = Core.Nothing,
      destinationCommitId = Core.Nothing,
      mergedCommitId = Core.Nothing,
      sourceCommitId = Core.Nothing,
      responseStatus
    }

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsBaseCommitId :: Lens.Lens' GetMergeCommitResponse (Core.Maybe Types.BaseCommitId)
gmcrrsBaseCommitId = Lens.field @"baseCommitId"
{-# DEPRECATED gmcrrsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsDestinationCommitId :: Lens.Lens' GetMergeCommitResponse (Core.Maybe Types.DestinationCommitId)
gmcrrsDestinationCommitId = Lens.field @"destinationCommitId"
{-# DEPRECATED gmcrrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
--
-- /Note:/ Consider using 'mergedCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsMergedCommitId :: Lens.Lens' GetMergeCommitResponse (Core.Maybe Types.MergedCommitId)
gmcrrsMergedCommitId = Lens.field @"mergedCommitId"
{-# DEPRECATED gmcrrsMergedCommitId "Use generic-lens or generic-optics with 'mergedCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsSourceCommitId :: Lens.Lens' GetMergeCommitResponse (Core.Maybe Types.SourceCommitId)
gmcrrsSourceCommitId = Lens.field @"sourceCommitId"
{-# DEPRECATED gmcrrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsResponseStatus :: Lens.Lens' GetMergeCommitResponse Core.Int
gmcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
