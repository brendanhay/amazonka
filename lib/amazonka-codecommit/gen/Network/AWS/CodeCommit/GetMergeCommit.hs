{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetMergeCommit (..)
    , mkGetMergeCommit
    -- ** Request lenses
    , gmcRepositoryName
    , gmcSourceCommitSpecifier
    , gmcDestinationCommitSpecifier
    , gmcConflictDetailLevel
    , gmcConflictResolutionStrategy

    -- * Destructuring the response
    , GetMergeCommitResponse (..)
    , mkGetMergeCommitResponse
    -- ** Response lenses
    , gmcrrsBaseCommitId
    , gmcrrsDestinationCommitId
    , gmcrrsMergedCommitId
    , gmcrrsSourceCommitId
    , gmcrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMergeCommit' smart constructor.
data GetMergeCommit = GetMergeCommit'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository that contains the merge commit about which you want to get information.
  , sourceCommitSpecifier :: Types.CommitName
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , destinationCommitSpecifier :: Types.CommitName
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , conflictDetailLevel :: Core.Maybe Types.ConflictDetailLevelTypeEnum
    -- ^ The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
  , conflictResolutionStrategy :: Core.Maybe Types.ConflictResolutionStrategyTypeEnum
    -- ^ Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMergeCommit' value with any optional fields omitted.
mkGetMergeCommit
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.CommitName -- ^ 'sourceCommitSpecifier'
    -> Types.CommitName -- ^ 'destinationCommitSpecifier'
    -> GetMergeCommit
mkGetMergeCommit repositoryName sourceCommitSpecifier
  destinationCommitSpecifier
  = GetMergeCommit'{repositoryName, sourceCommitSpecifier,
                    destinationCommitSpecifier, conflictDetailLevel = Core.Nothing,
                    conflictResolutionStrategy = Core.Nothing}

-- | The name of the repository that contains the merge commit about which you want to get information.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcRepositoryName :: Lens.Lens' GetMergeCommit Types.RepositoryName
gmcRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE gmcRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcSourceCommitSpecifier :: Lens.Lens' GetMergeCommit Types.CommitName
gmcSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# INLINEABLE gmcSourceCommitSpecifier #-}
{-# DEPRECATED sourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcDestinationCommitSpecifier :: Lens.Lens' GetMergeCommit Types.CommitName
gmcDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# INLINEABLE gmcDestinationCommitSpecifier #-}
{-# DEPRECATED destinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead"  #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcConflictDetailLevel :: Lens.Lens' GetMergeCommit (Core.Maybe Types.ConflictDetailLevelTypeEnum)
gmcConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# INLINEABLE gmcConflictDetailLevel #-}
{-# DEPRECATED conflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead"  #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcConflictResolutionStrategy :: Lens.Lens' GetMergeCommit (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
gmcConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# INLINEABLE gmcConflictResolutionStrategy #-}
{-# DEPRECATED conflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead"  #-}

instance Core.ToQuery GetMergeCommit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMergeCommit where
        toHeaders GetMergeCommit{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetMergeCommit")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMergeCommit where
        toJSON GetMergeCommit{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("sourceCommitSpecifier" Core..= sourceCommitSpecifier),
                  Core.Just
                    ("destinationCommitSpecifier" Core..= destinationCommitSpecifier),
                  ("conflictDetailLevel" Core..=) Core.<$> conflictDetailLevel,
                  ("conflictResolutionStrategy" Core..=) Core.<$>
                    conflictResolutionStrategy])

instance Core.AWSRequest GetMergeCommit where
        type Rs GetMergeCommit = GetMergeCommitResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMergeCommitResponse' Core.<$>
                   (x Core..:? "baseCommitId") Core.<*>
                     x Core..:? "destinationCommitId"
                     Core.<*> x Core..:? "mergedCommitId"
                     Core.<*> x Core..:? "sourceCommitId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMergeCommitResponse' smart constructor.
data GetMergeCommitResponse = GetMergeCommitResponse'
  { baseCommitId :: Core.Maybe Types.BaseCommitId
    -- ^ The commit ID of the merge base.
  , destinationCommitId :: Core.Maybe Types.DestinationCommitId
    -- ^ The commit ID of the destination commit specifier that was used in the merge evaluation.
  , mergedCommitId :: Core.Maybe Types.MergedCommitId
    -- ^ The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
  , sourceCommitId :: Core.Maybe Types.SourceCommitId
    -- ^ The commit ID of the source commit specifier that was used in the merge evaluation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMergeCommitResponse' value with any optional fields omitted.
mkGetMergeCommitResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMergeCommitResponse
mkGetMergeCommitResponse responseStatus
  = GetMergeCommitResponse'{baseCommitId = Core.Nothing,
                            destinationCommitId = Core.Nothing, mergedCommitId = Core.Nothing,
                            sourceCommitId = Core.Nothing, responseStatus}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsBaseCommitId :: Lens.Lens' GetMergeCommitResponse (Core.Maybe Types.BaseCommitId)
gmcrrsBaseCommitId = Lens.field @"baseCommitId"
{-# INLINEABLE gmcrrsBaseCommitId #-}
{-# DEPRECATED baseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead"  #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsDestinationCommitId :: Lens.Lens' GetMergeCommitResponse (Core.Maybe Types.DestinationCommitId)
gmcrrsDestinationCommitId = Lens.field @"destinationCommitId"
{-# INLINEABLE gmcrrsDestinationCommitId #-}
{-# DEPRECATED destinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead"  #-}

-- | The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
--
-- /Note:/ Consider using 'mergedCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsMergedCommitId :: Lens.Lens' GetMergeCommitResponse (Core.Maybe Types.MergedCommitId)
gmcrrsMergedCommitId = Lens.field @"mergedCommitId"
{-# INLINEABLE gmcrrsMergedCommitId #-}
{-# DEPRECATED mergedCommitId "Use generic-lens or generic-optics with 'mergedCommitId' instead"  #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsSourceCommitId :: Lens.Lens' GetMergeCommitResponse (Core.Maybe Types.SourceCommitId)
gmcrrsSourceCommitId = Lens.field @"sourceCommitId"
{-# INLINEABLE gmcrrsSourceCommitId #-}
{-# DEPRECATED sourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrrsResponseStatus :: Lens.Lens' GetMergeCommitResponse Core.Int
gmcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
