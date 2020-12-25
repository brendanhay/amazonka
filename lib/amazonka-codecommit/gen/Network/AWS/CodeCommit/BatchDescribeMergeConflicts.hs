{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchDescribeMergeConflicts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more merge conflicts in the attempted merge of two commit specifiers using the squash or three-way merge strategy.
module Network.AWS.CodeCommit.BatchDescribeMergeConflicts
  ( -- * Creating a request
    BatchDescribeMergeConflicts (..),
    mkBatchDescribeMergeConflicts,

    -- ** Request lenses
    bdmcRepositoryName,
    bdmcDestinationCommitSpecifier,
    bdmcSourceCommitSpecifier,
    bdmcMergeOption,
    bdmcConflictDetailLevel,
    bdmcConflictResolutionStrategy,
    bdmcFilePaths,
    bdmcMaxConflictFiles,
    bdmcMaxMergeHunks,
    bdmcNextToken,

    -- * Destructuring the response
    BatchDescribeMergeConflictsResponse (..),
    mkBatchDescribeMergeConflictsResponse,

    -- ** Response lenses
    bdmcrrsConflicts,
    bdmcrrsDestinationCommitId,
    bdmcrrsSourceCommitId,
    bdmcrrsBaseCommitId,
    bdmcrrsErrors,
    bdmcrrsNextToken,
    bdmcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDescribeMergeConflicts' smart constructor.
data BatchDescribeMergeConflicts = BatchDescribeMergeConflicts'
  { -- | The name of the repository that contains the merge conflicts you want to review.
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
    -- | The path of the target files used to describe the conflicts. If not specified, the default is all conflict files.
    filePaths :: Core.Maybe [Types.Path],
    -- | The maximum number of files to include in the output.
    maxConflictFiles :: Core.Maybe Core.Int,
    -- | The maximum number of merge hunks to include in the output.
    maxMergeHunks :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDescribeMergeConflicts' value with any optional fields omitted.
mkBatchDescribeMergeConflicts ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'destinationCommitSpecifier'
  Types.DestinationCommitSpecifier ->
  -- | 'sourceCommitSpecifier'
  Types.SourceCommitSpecifier ->
  -- | 'mergeOption'
  Types.MergeOptionTypeEnum ->
  BatchDescribeMergeConflicts
mkBatchDescribeMergeConflicts
  repositoryName
  destinationCommitSpecifier
  sourceCommitSpecifier
  mergeOption =
    BatchDescribeMergeConflicts'
      { repositoryName,
        destinationCommitSpecifier,
        sourceCommitSpecifier,
        mergeOption,
        conflictDetailLevel = Core.Nothing,
        conflictResolutionStrategy = Core.Nothing,
        filePaths = Core.Nothing,
        maxConflictFiles = Core.Nothing,
        maxMergeHunks = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The name of the repository that contains the merge conflicts you want to review.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcRepositoryName :: Lens.Lens' BatchDescribeMergeConflicts Types.RepositoryName
bdmcRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED bdmcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcDestinationCommitSpecifier :: Lens.Lens' BatchDescribeMergeConflicts Types.DestinationCommitSpecifier
bdmcDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# DEPRECATED bdmcDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcSourceCommitSpecifier :: Lens.Lens' BatchDescribeMergeConflicts Types.SourceCommitSpecifier
bdmcSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# DEPRECATED bdmcSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcMergeOption :: Lens.Lens' BatchDescribeMergeConflicts Types.MergeOptionTypeEnum
bdmcMergeOption = Lens.field @"mergeOption"
{-# DEPRECATED bdmcMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcConflictDetailLevel :: Lens.Lens' BatchDescribeMergeConflicts (Core.Maybe Types.ConflictDetailLevelTypeEnum)
bdmcConflictDetailLevel = Lens.field @"conflictDetailLevel"
{-# DEPRECATED bdmcConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcConflictResolutionStrategy :: Lens.Lens' BatchDescribeMergeConflicts (Core.Maybe Types.ConflictResolutionStrategyTypeEnum)
bdmcConflictResolutionStrategy = Lens.field @"conflictResolutionStrategy"
{-# DEPRECATED bdmcConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The path of the target files used to describe the conflicts. If not specified, the default is all conflict files.
--
-- /Note:/ Consider using 'filePaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcFilePaths :: Lens.Lens' BatchDescribeMergeConflicts (Core.Maybe [Types.Path])
bdmcFilePaths = Lens.field @"filePaths"
{-# DEPRECATED bdmcFilePaths "Use generic-lens or generic-optics with 'filePaths' instead." #-}

-- | The maximum number of files to include in the output.
--
-- /Note:/ Consider using 'maxConflictFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcMaxConflictFiles :: Lens.Lens' BatchDescribeMergeConflicts (Core.Maybe Core.Int)
bdmcMaxConflictFiles = Lens.field @"maxConflictFiles"
{-# DEPRECATED bdmcMaxConflictFiles "Use generic-lens or generic-optics with 'maxConflictFiles' instead." #-}

-- | The maximum number of merge hunks to include in the output.
--
-- /Note:/ Consider using 'maxMergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcMaxMergeHunks :: Lens.Lens' BatchDescribeMergeConflicts (Core.Maybe Core.Int)
bdmcMaxMergeHunks = Lens.field @"maxMergeHunks"
{-# DEPRECATED bdmcMaxMergeHunks "Use generic-lens or generic-optics with 'maxMergeHunks' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcNextToken :: Lens.Lens' BatchDescribeMergeConflicts (Core.Maybe Types.NextToken)
bdmcNextToken = Lens.field @"nextToken"
{-# DEPRECATED bdmcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON BatchDescribeMergeConflicts where
  toJSON BatchDescribeMergeConflicts {..} =
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
            ("filePaths" Core..=) Core.<$> filePaths,
            ("maxConflictFiles" Core..=) Core.<$> maxConflictFiles,
            ("maxMergeHunks" Core..=) Core.<$> maxMergeHunks,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest BatchDescribeMergeConflicts where
  type
    Rs BatchDescribeMergeConflicts =
      BatchDescribeMergeConflictsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.BatchDescribeMergeConflicts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDescribeMergeConflictsResponse'
            Core.<$> (x Core..:? "conflicts" Core..!= Core.mempty)
            Core.<*> (x Core..: "destinationCommitId")
            Core.<*> (x Core..: "sourceCommitId")
            Core.<*> (x Core..:? "baseCommitId")
            Core.<*> (x Core..:? "errors")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDescribeMergeConflictsResponse' smart constructor.
data BatchDescribeMergeConflictsResponse = BatchDescribeMergeConflictsResponse'
  { -- | A list of conflicts for each file, including the conflict metadata and the hunks of the differences between the files.
    conflicts :: [Types.Conflict],
    -- | The commit ID of the destination commit specifier that was used in the merge evaluation.
    destinationCommitId :: Types.ObjectId,
    -- | The commit ID of the source commit specifier that was used in the merge evaluation.
    sourceCommitId :: Types.ObjectId,
    -- | The commit ID of the merge base.
    baseCommitId :: Core.Maybe Types.ObjectId,
    -- | A list of any errors returned while describing the merge conflicts for each file.
    errors :: Core.Maybe [Types.BatchDescribeMergeConflictsError],
    -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDescribeMergeConflictsResponse' value with any optional fields omitted.
mkBatchDescribeMergeConflictsResponse ::
  -- | 'destinationCommitId'
  Types.ObjectId ->
  -- | 'sourceCommitId'
  Types.ObjectId ->
  -- | 'responseStatus'
  Core.Int ->
  BatchDescribeMergeConflictsResponse
mkBatchDescribeMergeConflictsResponse
  destinationCommitId
  sourceCommitId
  responseStatus =
    BatchDescribeMergeConflictsResponse'
      { conflicts = Core.mempty,
        destinationCommitId,
        sourceCommitId,
        baseCommitId = Core.Nothing,
        errors = Core.Nothing,
        nextToken = Core.Nothing,
        responseStatus
      }

-- | A list of conflicts for each file, including the conflict metadata and the hunks of the differences between the files.
--
-- /Note:/ Consider using 'conflicts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrrsConflicts :: Lens.Lens' BatchDescribeMergeConflictsResponse [Types.Conflict]
bdmcrrsConflicts = Lens.field @"conflicts"
{-# DEPRECATED bdmcrrsConflicts "Use generic-lens or generic-optics with 'conflicts' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrrsDestinationCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse Types.ObjectId
bdmcrrsDestinationCommitId = Lens.field @"destinationCommitId"
{-# DEPRECATED bdmcrrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrrsSourceCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse Types.ObjectId
bdmcrrsSourceCommitId = Lens.field @"sourceCommitId"
{-# DEPRECATED bdmcrrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrrsBaseCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse (Core.Maybe Types.ObjectId)
bdmcrrsBaseCommitId = Lens.field @"baseCommitId"
{-# DEPRECATED bdmcrrsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | A list of any errors returned while describing the merge conflicts for each file.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrrsErrors :: Lens.Lens' BatchDescribeMergeConflictsResponse (Core.Maybe [Types.BatchDescribeMergeConflictsError])
bdmcrrsErrors = Lens.field @"errors"
{-# DEPRECATED bdmcrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrrsNextToken :: Lens.Lens' BatchDescribeMergeConflictsResponse (Core.Maybe Types.NextToken)
bdmcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED bdmcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrrsResponseStatus :: Lens.Lens' BatchDescribeMergeConflictsResponse Core.Int
bdmcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bdmcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
