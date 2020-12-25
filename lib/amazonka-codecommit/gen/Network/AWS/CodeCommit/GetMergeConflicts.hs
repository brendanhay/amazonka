{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetMergeConflicts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about merge conflicts between the before and after commit IDs for a pull request in a repository.
module Network.AWS.CodeCommit.GetMergeConflicts
  ( -- * Creating a request
    GetMergeConflicts (..),
    mkGetMergeConflicts,

    -- ** Request lenses
    gmcsRepositoryName,
    gmcsDestinationCommitSpecifier,
    gmcsSourceCommitSpecifier,
    gmcsMergeOption,
    gmcsConflictDetailLevel,
    gmcsConflictResolutionStrategy,
    gmcsMaxConflictFiles,
    gmcsNextToken,

    -- * Destructuring the response
    GetMergeConflictsResponse (..),
    mkGetMergeConflictsResponse,

    -- ** Response lenses
    gmcrfrsMergeable,
    gmcrfrsDestinationCommitId,
    gmcrfrsSourceCommitId,
    gmcrfrsConflictMetadataList,
    gmcrfrsBaseCommitId,
    gmcrfrsNextToken,
    gmcrfrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMergeConflicts' smart constructor.
data GetMergeConflicts = GetMergeConflicts'
  { -- | The name of the repository where the pull request was created.
    repositoryName :: Types.RepositoryName,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Types.DestinationCommitSpecifier,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Types.SourceCommitSpecifier,
    -- | The merge option or strategy you want to use to merge the code.
    mergeOption :: Types.MergeOptionTypeEnum,
    -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Core.Maybe Types.ConflictDetailLevelTypeEnum,
    -- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
    conflictResolutionStrategy :: Core.Maybe Types.ConflictResolutionStrategyTypeEnum,
    -- | The maximum number of files to include in the output.
    maxConflictFiles :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMergeConflicts' value with any optional fields omitted.
mkGetMergeConflicts ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'destinationCommitSpecifier'
  Types.DestinationCommitSpecifier ->
  -- | 'sourceCommitSpecifier'
  Types.SourceCommitSpecifier ->
  -- | 'mergeOption'
  Types.MergeOptionTypeEnum ->
  GetMergeConflicts
mkGetMergeConflicts
  repositoryName
  destinationCommitSpecifier
  sourceCommitSpecifier
  mergeOption =
    GetMergeConflicts'
      { repositoryName,
        destinationCommitSpecifier,
        sourceCommitSpecifier,
        mergeOption,
        conflictDetailLevel = Core.Nothing,
        conflictResolutionStrategy = Core.Nothing,
        maxConflictFiles = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsRepositoryName :: Lens.Lens' GetMergeConflicts Types.RepositoryName
gmcsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED gmcsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsDestinationCommitSpecifier :: Lens.Lens' GetMergeConflicts Types.DestinationCommitSpecifier
gmcsDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# DEPRECATED gmcsDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsSourceCommitSpecifier :: Lens.Lens' GetMergeConflicts Types.SourceCommitSpecifier
gmcsSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# DEPRECATED gmcsSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsMergeOption :: Lens.Lens' GetMergeConflicts Types.MergeOptionTypeEnum
gmcsMergeOption = Lens.field @"mergeOption"
{-# DEPRECATED gmcsMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsConflictDetailLevel :: Lens.Lens' GetMergeConflicts (Core.Maybe Types.ConflictDetailLevelTypeEnum)
gmcsConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# DEPRECATED gmcsConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsConflictResolutionStrategy :: Lens.Lens' GetMergeConflicts (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
gmcsConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# DEPRECATED gmcsConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The maximum number of files to include in the output.
--
-- /Note:/ Consider using 'maxConflictFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsMaxConflictFiles :: Lens.Lens' GetMergeConflicts (Core.Maybe Core.Int)
gmcsMaxConflictFiles = Lens.field @"maxConflictFiles"
{-# DEPRECATED gmcsMaxConflictFiles "Use generic-lens or generic-optics with 'maxConflictFiles' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsNextToken :: Lens.Lens' GetMergeConflicts (Core.Maybe Types.NextToken)
gmcsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gmcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetMergeConflicts where
  toJSON GetMergeConflicts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ("destinationCommitSpecifier" Core..= destinationCommitSpecifier),
            Core.Just ("sourceCommitSpecifier" Core..= sourceCommitSpecifier),
            Core.Just ("mergeOption" Core..= mergeOption),
            ("conflictDetailLevel" Core..=) Core.<$> conflictDetailLevel,
            ("conflictResolutionStrategy" Core..=)
              Core.<$> conflictResolutionStrategy,
            ("maxConflictFiles" Core..=) Core.<$> maxConflictFiles,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetMergeConflicts where
  type Rs GetMergeConflicts = GetMergeConflictsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetMergeConflicts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMergeConflictsResponse'
            Core.<$> (x Core..: "mergeable")
            Core.<*> (x Core..: "destinationCommitId")
            Core.<*> (x Core..: "sourceCommitId")
            Core.<*> (x Core..:? "conflictMetadataList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "baseCommitId")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMergeConflictsResponse' smart constructor.
data GetMergeConflictsResponse = GetMergeConflictsResponse'
  { -- | A Boolean value that indicates whether the code is mergeable by the specified merge option.
    mergeable :: Core.Bool,
    -- | The commit ID of the destination commit specifier that was used in the merge evaluation.
    destinationCommitId :: Types.DestinationCommitId,
    -- | The commit ID of the source commit specifier that was used in the merge evaluation.
    sourceCommitId :: Types.SourceCommitId,
    -- | A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
    conflictMetadataList :: [Types.ConflictMetadata],
    -- | The commit ID of the merge base.
    baseCommitId :: Core.Maybe Types.BaseCommitId,
    -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMergeConflictsResponse' value with any optional fields omitted.
mkGetMergeConflictsResponse ::
  -- | 'mergeable'
  Core.Bool ->
  -- | 'destinationCommitId'
  Types.DestinationCommitId ->
  -- | 'sourceCommitId'
  Types.SourceCommitId ->
  -- | 'responseStatus'
  Core.Int ->
  GetMergeConflictsResponse
mkGetMergeConflictsResponse
  mergeable
  destinationCommitId
  sourceCommitId
  responseStatus =
    GetMergeConflictsResponse'
      { mergeable,
        destinationCommitId,
        sourceCommitId,
        conflictMetadataList = Core.mempty,
        baseCommitId = Core.Nothing,
        nextToken = Core.Nothing,
        responseStatus
      }

-- | A Boolean value that indicates whether the code is mergeable by the specified merge option.
--
-- /Note:/ Consider using 'mergeable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrfrsMergeable :: Lens.Lens' GetMergeConflictsResponse Core.Bool
gmcrfrsMergeable = Lens.field @"mergeable"
{-# DEPRECATED gmcrfrsMergeable "Use generic-lens or generic-optics with 'mergeable' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrfrsDestinationCommitId :: Lens.Lens' GetMergeConflictsResponse Types.DestinationCommitId
gmcrfrsDestinationCommitId = Lens.field @"destinationCommitId"
{-# DEPRECATED gmcrfrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrfrsSourceCommitId :: Lens.Lens' GetMergeConflictsResponse Types.SourceCommitId
gmcrfrsSourceCommitId = Lens.field @"sourceCommitId"
{-# DEPRECATED gmcrfrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
--
-- /Note:/ Consider using 'conflictMetadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrfrsConflictMetadataList :: Lens.Lens' GetMergeConflictsResponse [Types.ConflictMetadata]
gmcrfrsConflictMetadataList = Lens.field @"conflictMetadataList"
{-# DEPRECATED gmcrfrsConflictMetadataList "Use generic-lens or generic-optics with 'conflictMetadataList' instead." #-}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrfrsBaseCommitId :: Lens.Lens' GetMergeConflictsResponse (Core.Maybe Types.BaseCommitId)
gmcrfrsBaseCommitId = Lens.field @"baseCommitId"
{-# DEPRECATED gmcrfrsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrfrsNextToken :: Lens.Lens' GetMergeConflictsResponse (Core.Maybe Types.NextToken)
gmcrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gmcrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrfrsResponseStatus :: Lens.Lens' GetMergeConflictsResponse Core.Int
gmcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
