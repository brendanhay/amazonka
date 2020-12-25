{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DescribeMergeConflicts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more merge conflicts in the attempted merge of two commit specifiers using the squash or three-way merge strategy. If the merge option for the attempted merge is specified as FAST_FORWARD_MERGE, an exception is thrown.
module Network.AWS.CodeCommit.DescribeMergeConflicts
  ( -- * Creating a request
    DescribeMergeConflicts (..),
    mkDescribeMergeConflicts,

    -- ** Request lenses
    dmcRepositoryName,
    dmcDestinationCommitSpecifier,
    dmcSourceCommitSpecifier,
    dmcMergeOption,
    dmcFilePath,
    dmcConflictDetailLevel,
    dmcConflictResolutionStrategy,
    dmcMaxMergeHunks,
    dmcNextToken,

    -- * Destructuring the response
    DescribeMergeConflictsResponse (..),
    mkDescribeMergeConflictsResponse,

    -- ** Response lenses
    dmcrrsConflictMetadata,
    dmcrrsMergeHunks,
    dmcrrsDestinationCommitId,
    dmcrrsSourceCommitId,
    dmcrrsBaseCommitId,
    dmcrrsNextToken,
    dmcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeMergeConflicts' smart constructor.
data DescribeMergeConflicts = DescribeMergeConflicts'
  { -- | The name of the repository where you want to get information about a merge conflict.
    repositoryName :: Types.RepositoryName,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Types.CommitName,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Types.CommitName,
    -- | The merge option or strategy you want to use to merge the code.
    mergeOption :: Types.MergeOptionTypeEnum,
    -- | The path of the target files used to describe the conflicts.
    filePath :: Types.Path,
    -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Core.Maybe Types.ConflictDetailLevelTypeEnum,
    -- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
    conflictResolutionStrategy :: Core.Maybe Types.ConflictResolutionStrategyTypeEnum,
    -- | The maximum number of merge hunks to include in the output.
    maxMergeHunks :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMergeConflicts' value with any optional fields omitted.
mkDescribeMergeConflicts ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'destinationCommitSpecifier'
  Types.CommitName ->
  -- | 'sourceCommitSpecifier'
  Types.CommitName ->
  -- | 'mergeOption'
  Types.MergeOptionTypeEnum ->
  -- | 'filePath'
  Types.Path ->
  DescribeMergeConflicts
mkDescribeMergeConflicts
  repositoryName
  destinationCommitSpecifier
  sourceCommitSpecifier
  mergeOption
  filePath =
    DescribeMergeConflicts'
      { repositoryName,
        destinationCommitSpecifier,
        sourceCommitSpecifier,
        mergeOption,
        filePath,
        conflictDetailLevel = Core.Nothing,
        conflictResolutionStrategy = Core.Nothing,
        maxMergeHunks = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The name of the repository where you want to get information about a merge conflict.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcRepositoryName :: Lens.Lens' DescribeMergeConflicts Types.RepositoryName
dmcRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED dmcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcDestinationCommitSpecifier :: Lens.Lens' DescribeMergeConflicts Types.CommitName
dmcDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# DEPRECATED dmcDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcSourceCommitSpecifier :: Lens.Lens' DescribeMergeConflicts Types.CommitName
dmcSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# DEPRECATED dmcSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcMergeOption :: Lens.Lens' DescribeMergeConflicts Types.MergeOptionTypeEnum
dmcMergeOption = Lens.field @"mergeOption"
{-# DEPRECATED dmcMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

-- | The path of the target files used to describe the conflicts.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcFilePath :: Lens.Lens' DescribeMergeConflicts Types.Path
dmcFilePath = Lens.field @"filePath"
{-# DEPRECATED dmcFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcConflictDetailLevel :: Lens.Lens' DescribeMergeConflicts (Core.Maybe Types.ConflictDetailLevelTypeEnum)
dmcConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# DEPRECATED dmcConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcConflictResolutionStrategy :: Lens.Lens' DescribeMergeConflicts (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
dmcConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# DEPRECATED dmcConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The maximum number of merge hunks to include in the output.
--
-- /Note:/ Consider using 'maxMergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcMaxMergeHunks :: Lens.Lens' DescribeMergeConflicts (Core.Maybe Core.Int)
dmcMaxMergeHunks = Lens.field @"maxMergeHunks"
{-# DEPRECATED dmcMaxMergeHunks "Use generic-lens or generic-optics with 'maxMergeHunks' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcNextToken :: Lens.Lens' DescribeMergeConflicts (Core.Maybe Types.NextToken)
dmcNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeMergeConflicts where
  toJSON DescribeMergeConflicts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ("destinationCommitSpecifier" Core..= destinationCommitSpecifier),
            Core.Just ("sourceCommitSpecifier" Core..= sourceCommitSpecifier),
            Core.Just ("mergeOption" Core..= mergeOption),
            Core.Just ("filePath" Core..= filePath),
            ("conflictDetailLevel" Core..=) Core.<$> conflictDetailLevel,
            ("conflictResolutionStrategy" Core..=)
              Core.<$> conflictResolutionStrategy,
            ("maxMergeHunks" Core..=) Core.<$> maxMergeHunks,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeMergeConflicts where
  type Rs DescribeMergeConflicts = DescribeMergeConflictsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.DescribeMergeConflicts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMergeConflictsResponse'
            Core.<$> (x Core..: "conflictMetadata")
            Core.<*> (x Core..:? "mergeHunks" Core..!= Core.mempty)
            Core.<*> (x Core..: "destinationCommitId")
            Core.<*> (x Core..: "sourceCommitId")
            Core.<*> (x Core..:? "baseCommitId")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeMergeConflictsResponse' smart constructor.
data DescribeMergeConflictsResponse = DescribeMergeConflictsResponse'
  { -- | Contains metadata about the conflicts found in the merge.
    conflictMetadata :: Types.ConflictMetadata,
    -- | A list of merge hunks of the differences between the files or lines.
    mergeHunks :: [Types.MergeHunk],
    -- | The commit ID of the destination commit specifier that was used in the merge evaluation.
    destinationCommitId :: Types.DestinationCommitId,
    -- | The commit ID of the source commit specifier that was used in the merge evaluation.
    sourceCommitId :: Types.SourceCommitId,
    -- | The commit ID of the merge base.
    baseCommitId :: Core.Maybe Types.BaseCommitId,
    -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMergeConflictsResponse' value with any optional fields omitted.
mkDescribeMergeConflictsResponse ::
  -- | 'conflictMetadata'
  Types.ConflictMetadata ->
  -- | 'destinationCommitId'
  Types.DestinationCommitId ->
  -- | 'sourceCommitId'
  Types.SourceCommitId ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeMergeConflictsResponse
mkDescribeMergeConflictsResponse
  conflictMetadata
  destinationCommitId
  sourceCommitId
  responseStatus =
    DescribeMergeConflictsResponse'
      { conflictMetadata,
        mergeHunks = Core.mempty,
        destinationCommitId,
        sourceCommitId,
        baseCommitId = Core.Nothing,
        nextToken = Core.Nothing,
        responseStatus
      }

-- | Contains metadata about the conflicts found in the merge.
--
-- /Note:/ Consider using 'conflictMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsConflictMetadata :: Lens.Lens' DescribeMergeConflictsResponse Types.ConflictMetadata
dmcrrsConflictMetadata = Lens.field @"conflictMetadata"
{-# DEPRECATED dmcrrsConflictMetadata "Use generic-lens or generic-optics with 'conflictMetadata' instead." #-}

-- | A list of merge hunks of the differences between the files or lines.
--
-- /Note:/ Consider using 'mergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsMergeHunks :: Lens.Lens' DescribeMergeConflictsResponse [Types.MergeHunk]
dmcrrsMergeHunks = Lens.field @"mergeHunks"
{-# DEPRECATED dmcrrsMergeHunks "Use generic-lens or generic-optics with 'mergeHunks' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsDestinationCommitId :: Lens.Lens' DescribeMergeConflictsResponse Types.DestinationCommitId
dmcrrsDestinationCommitId = Lens.field @"destinationCommitId"
{-# DEPRECATED dmcrrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsSourceCommitId :: Lens.Lens' DescribeMergeConflictsResponse Types.SourceCommitId
dmcrrsSourceCommitId = Lens.field @"sourceCommitId"
{-# DEPRECATED dmcrrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsBaseCommitId :: Lens.Lens' DescribeMergeConflictsResponse (Core.Maybe Types.BaseCommitId)
dmcrrsBaseCommitId = Lens.field @"baseCommitId"
{-# DEPRECATED dmcrrsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsNextToken :: Lens.Lens' DescribeMergeConflictsResponse (Core.Maybe Types.NextToken)
dmcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsResponseStatus :: Lens.Lens' DescribeMergeConflictsResponse Core.Int
dmcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
