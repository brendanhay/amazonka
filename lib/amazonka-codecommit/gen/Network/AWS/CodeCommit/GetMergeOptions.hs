{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetMergeOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the merge options available for merging two specified branches. For details about why a merge option is not available, use GetMergeConflicts or DescribeMergeConflicts.
module Network.AWS.CodeCommit.GetMergeOptions
    (
    -- * Creating a request
      GetMergeOptions (..)
    , mkGetMergeOptions
    -- ** Request lenses
    , gmoRepositoryName
    , gmoSourceCommitSpecifier
    , gmoDestinationCommitSpecifier
    , gmoConflictDetailLevel
    , gmoConflictResolutionStrategy

    -- * Destructuring the response
    , GetMergeOptionsResponse (..)
    , mkGetMergeOptionsResponse
    -- ** Response lenses
    , gmorrsMergeOptions
    , gmorrsSourceCommitId
    , gmorrsDestinationCommitId
    , gmorrsBaseCommitId
    , gmorrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMergeOptions' smart constructor.
data GetMergeOptions = GetMergeOptions'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository that contains the commits about which you want to get merge options.
  , sourceCommitSpecifier :: Types.SourceCommitSpecifier
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , destinationCommitSpecifier :: Types.DestinationCommitSpecifier
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , conflictDetailLevel :: Core.Maybe Types.ConflictDetailLevelTypeEnum
    -- ^ The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
  , conflictResolutionStrategy :: Core.Maybe Types.ConflictResolutionStrategyTypeEnum
    -- ^ Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMergeOptions' value with any optional fields omitted.
mkGetMergeOptions
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.SourceCommitSpecifier -- ^ 'sourceCommitSpecifier'
    -> Types.DestinationCommitSpecifier -- ^ 'destinationCommitSpecifier'
    -> GetMergeOptions
mkGetMergeOptions repositoryName sourceCommitSpecifier
  destinationCommitSpecifier
  = GetMergeOptions'{repositoryName, sourceCommitSpecifier,
                     destinationCommitSpecifier, conflictDetailLevel = Core.Nothing,
                     conflictResolutionStrategy = Core.Nothing}

-- | The name of the repository that contains the commits about which you want to get merge options.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoRepositoryName :: Lens.Lens' GetMergeOptions Types.RepositoryName
gmoRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE gmoRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoSourceCommitSpecifier :: Lens.Lens' GetMergeOptions Types.SourceCommitSpecifier
gmoSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# INLINEABLE gmoSourceCommitSpecifier #-}
{-# DEPRECATED sourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoDestinationCommitSpecifier :: Lens.Lens' GetMergeOptions Types.DestinationCommitSpecifier
gmoDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# INLINEABLE gmoDestinationCommitSpecifier #-}
{-# DEPRECATED destinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead"  #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoConflictDetailLevel :: Lens.Lens' GetMergeOptions (Core.Maybe Types.ConflictDetailLevelTypeEnum)
gmoConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# INLINEABLE gmoConflictDetailLevel #-}
{-# DEPRECATED conflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead"  #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoConflictResolutionStrategy :: Lens.Lens' GetMergeOptions (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
gmoConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# INLINEABLE gmoConflictResolutionStrategy #-}
{-# DEPRECATED conflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead"  #-}

instance Core.ToQuery GetMergeOptions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMergeOptions where
        toHeaders GetMergeOptions{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetMergeOptions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMergeOptions where
        toJSON GetMergeOptions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("sourceCommitSpecifier" Core..= sourceCommitSpecifier),
                  Core.Just
                    ("destinationCommitSpecifier" Core..= destinationCommitSpecifier),
                  ("conflictDetailLevel" Core..=) Core.<$> conflictDetailLevel,
                  ("conflictResolutionStrategy" Core..=) Core.<$>
                    conflictResolutionStrategy])

instance Core.AWSRequest GetMergeOptions where
        type Rs GetMergeOptions = GetMergeOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMergeOptionsResponse' Core.<$>
                   (x Core..:? "mergeOptions" Core..!= Core.mempty) Core.<*>
                     x Core..: "sourceCommitId"
                     Core.<*> x Core..: "destinationCommitId"
                     Core.<*> x Core..: "baseCommitId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMergeOptionsResponse' smart constructor.
data GetMergeOptionsResponse = GetMergeOptionsResponse'
  { mergeOptions :: [Types.MergeOptionTypeEnum]
    -- ^ The merge option or strategy used to merge the code.
  , sourceCommitId :: Types.ObjectId
    -- ^ The commit ID of the source commit specifier that was used in the merge evaluation.
  , destinationCommitId :: Types.ObjectId
    -- ^ The commit ID of the destination commit specifier that was used in the merge evaluation.
  , baseCommitId :: Types.ObjectId
    -- ^ The commit ID of the merge base.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMergeOptionsResponse' value with any optional fields omitted.
mkGetMergeOptionsResponse
    :: Types.ObjectId -- ^ 'sourceCommitId'
    -> Types.ObjectId -- ^ 'destinationCommitId'
    -> Types.ObjectId -- ^ 'baseCommitId'
    -> Core.Int -- ^ 'responseStatus'
    -> GetMergeOptionsResponse
mkGetMergeOptionsResponse sourceCommitId destinationCommitId
  baseCommitId responseStatus
  = GetMergeOptionsResponse'{mergeOptions = Core.mempty,
                             sourceCommitId, destinationCommitId, baseCommitId, responseStatus}

-- | The merge option or strategy used to merge the code.
--
-- /Note:/ Consider using 'mergeOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorrsMergeOptions :: Lens.Lens' GetMergeOptionsResponse [Types.MergeOptionTypeEnum]
gmorrsMergeOptions = Lens.field @"mergeOptions"
{-# INLINEABLE gmorrsMergeOptions #-}
{-# DEPRECATED mergeOptions "Use generic-lens or generic-optics with 'mergeOptions' instead"  #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorrsSourceCommitId :: Lens.Lens' GetMergeOptionsResponse Types.ObjectId
gmorrsSourceCommitId = Lens.field @"sourceCommitId"
{-# INLINEABLE gmorrsSourceCommitId #-}
{-# DEPRECATED sourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead"  #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorrsDestinationCommitId :: Lens.Lens' GetMergeOptionsResponse Types.ObjectId
gmorrsDestinationCommitId = Lens.field @"destinationCommitId"
{-# INLINEABLE gmorrsDestinationCommitId #-}
{-# DEPRECATED destinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead"  #-}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorrsBaseCommitId :: Lens.Lens' GetMergeOptionsResponse Types.ObjectId
gmorrsBaseCommitId = Lens.field @"baseCommitId"
{-# INLINEABLE gmorrsBaseCommitId #-}
{-# DEPRECATED baseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorrsResponseStatus :: Lens.Lens' GetMergeOptionsResponse Core.Int
gmorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
