{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    bdmcFilePaths,
    bdmcConflictDetailLevel,
    bdmcNextToken,
    bdmcMaxConflictFiles,
    bdmcMaxMergeHunks,
    bdmcConflictResolutionStrategy,
    bdmcRepositoryName,
    bdmcDestinationCommitSpecifier,
    bdmcSourceCommitSpecifier,
    bdmcMergeOption,

    -- * Destructuring the response
    BatchDescribeMergeConflictsResponse (..),
    mkBatchDescribeMergeConflictsResponse,

    -- ** Response lenses
    bdmcrsBaseCommitId,
    bdmcrsNextToken,
    bdmcrsErrors,
    bdmcrsResponseStatus,
    bdmcrsConflicts,
    bdmcrsDestinationCommitId,
    bdmcrsSourceCommitId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDescribeMergeConflicts' smart constructor.
data BatchDescribeMergeConflicts = BatchDescribeMergeConflicts'
  { filePaths ::
      Lude.Maybe [Lude.Text],
    conflictDetailLevel ::
      Lude.Maybe
        ConflictDetailLevelTypeEnum,
    nextToken :: Lude.Maybe Lude.Text,
    maxConflictFiles ::
      Lude.Maybe Lude.Int,
    maxMergeHunks ::
      Lude.Maybe Lude.Int,
    conflictResolutionStrategy ::
      Lude.Maybe
        ConflictResolutionStrategyTypeEnum,
    repositoryName :: Lude.Text,
    destinationCommitSpecifier ::
      Lude.Text,
    sourceCommitSpecifier :: Lude.Text,
    mergeOption :: MergeOptionTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDescribeMergeConflicts' with the minimum fields required to make a request.
--
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'filePaths' - The path of the target files used to describe the conflicts. If not specified, the default is all conflict files.
-- * 'maxConflictFiles' - The maximum number of files to include in the output.
-- * 'maxMergeHunks' - The maximum number of merge hunks to include in the output.
-- * 'mergeOption' - The merge option or strategy you want to use to merge the code.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'repositoryName' - The name of the repository that contains the merge conflicts you want to review.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkBatchDescribeMergeConflicts ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  BatchDescribeMergeConflicts
mkBatchDescribeMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_ =
    BatchDescribeMergeConflicts'
      { filePaths = Lude.Nothing,
        conflictDetailLevel = Lude.Nothing,
        nextToken = Lude.Nothing,
        maxConflictFiles = Lude.Nothing,
        maxMergeHunks = Lude.Nothing,
        conflictResolutionStrategy = Lude.Nothing,
        repositoryName = pRepositoryName_,
        destinationCommitSpecifier = pDestinationCommitSpecifier_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        mergeOption = pMergeOption_
      }

-- | The path of the target files used to describe the conflicts. If not specified, the default is all conflict files.
--
-- /Note:/ Consider using 'filePaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcFilePaths :: Lens.Lens' BatchDescribeMergeConflicts (Lude.Maybe [Lude.Text])
bdmcFilePaths = Lens.lens (filePaths :: BatchDescribeMergeConflicts -> Lude.Maybe [Lude.Text]) (\s a -> s {filePaths = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcFilePaths "Use generic-lens or generic-optics with 'filePaths' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcConflictDetailLevel :: Lens.Lens' BatchDescribeMergeConflicts (Lude.Maybe ConflictDetailLevelTypeEnum)
bdmcConflictDetailLevel = Lens.lens (conflictDetailLevel :: BatchDescribeMergeConflicts -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcNextToken :: Lens.Lens' BatchDescribeMergeConflicts (Lude.Maybe Lude.Text)
bdmcNextToken = Lens.lens (nextToken :: BatchDescribeMergeConflicts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of files to include in the output.
--
-- /Note:/ Consider using 'maxConflictFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcMaxConflictFiles :: Lens.Lens' BatchDescribeMergeConflicts (Lude.Maybe Lude.Int)
bdmcMaxConflictFiles = Lens.lens (maxConflictFiles :: BatchDescribeMergeConflicts -> Lude.Maybe Lude.Int) (\s a -> s {maxConflictFiles = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcMaxConflictFiles "Use generic-lens or generic-optics with 'maxConflictFiles' instead." #-}

-- | The maximum number of merge hunks to include in the output.
--
-- /Note:/ Consider using 'maxMergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcMaxMergeHunks :: Lens.Lens' BatchDescribeMergeConflicts (Lude.Maybe Lude.Int)
bdmcMaxMergeHunks = Lens.lens (maxMergeHunks :: BatchDescribeMergeConflicts -> Lude.Maybe Lude.Int) (\s a -> s {maxMergeHunks = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcMaxMergeHunks "Use generic-lens or generic-optics with 'maxMergeHunks' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcConflictResolutionStrategy :: Lens.Lens' BatchDescribeMergeConflicts (Lude.Maybe ConflictResolutionStrategyTypeEnum)
bdmcConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: BatchDescribeMergeConflicts -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The name of the repository that contains the merge conflicts you want to review.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcRepositoryName :: Lens.Lens' BatchDescribeMergeConflicts Lude.Text
bdmcRepositoryName = Lens.lens (repositoryName :: BatchDescribeMergeConflicts -> Lude.Text) (\s a -> s {repositoryName = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcDestinationCommitSpecifier :: Lens.Lens' BatchDescribeMergeConflicts Lude.Text
bdmcDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: BatchDescribeMergeConflicts -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcSourceCommitSpecifier :: Lens.Lens' BatchDescribeMergeConflicts Lude.Text
bdmcSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: BatchDescribeMergeConflicts -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcMergeOption :: Lens.Lens' BatchDescribeMergeConflicts MergeOptionTypeEnum
bdmcMergeOption = Lens.lens (mergeOption :: BatchDescribeMergeConflicts -> MergeOptionTypeEnum) (\s a -> s {mergeOption = a} :: BatchDescribeMergeConflicts)
{-# DEPRECATED bdmcMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

instance Lude.AWSRequest BatchDescribeMergeConflicts where
  type
    Rs BatchDescribeMergeConflicts =
      BatchDescribeMergeConflictsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDescribeMergeConflictsResponse'
            Lude.<$> (x Lude..?> "baseCommitId")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "conflicts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "destinationCommitId")
            Lude.<*> (x Lude..:> "sourceCommitId")
      )

instance Lude.ToHeaders BatchDescribeMergeConflicts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.BatchDescribeMergeConflicts" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDescribeMergeConflicts where
  toJSON BatchDescribeMergeConflicts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("filePaths" Lude..=) Lude.<$> filePaths,
            ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxConflictFiles" Lude..=) Lude.<$> maxConflictFiles,
            ("maxMergeHunks" Lude..=) Lude.<$> maxMergeHunks,
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            Lude.Just ("mergeOption" Lude..= mergeOption)
          ]
      )

instance Lude.ToPath BatchDescribeMergeConflicts where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDescribeMergeConflicts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDescribeMergeConflictsResponse' smart constructor.
data BatchDescribeMergeConflictsResponse = BatchDescribeMergeConflictsResponse'
  { baseCommitId ::
      Lude.Maybe
        Lude.Text,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    errors ::
      Lude.Maybe
        [BatchDescribeMergeConflictsError],
    responseStatus ::
      Lude.Int,
    conflicts ::
      [Conflict],
    destinationCommitId ::
      Lude.Text,
    sourceCommitId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDescribeMergeConflictsResponse' with the minimum fields required to make a request.
--
-- * 'baseCommitId' - The commit ID of the merge base.
-- * 'conflicts' - A list of conflicts for each file, including the conflict metadata and the hunks of the differences between the files.
-- * 'destinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
-- * 'errors' - A list of any errors returned while describing the merge conflicts for each file.
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'responseStatus' - The response status code.
-- * 'sourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
mkBatchDescribeMergeConflictsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'destinationCommitId'
  Lude.Text ->
  -- | 'sourceCommitId'
  Lude.Text ->
  BatchDescribeMergeConflictsResponse
mkBatchDescribeMergeConflictsResponse
  pResponseStatus_
  pDestinationCommitId_
  pSourceCommitId_ =
    BatchDescribeMergeConflictsResponse'
      { baseCommitId = Lude.Nothing,
        nextToken = Lude.Nothing,
        errors = Lude.Nothing,
        responseStatus = pResponseStatus_,
        conflicts = Lude.mempty,
        destinationCommitId = pDestinationCommitId_,
        sourceCommitId = pSourceCommitId_
      }

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrsBaseCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse (Lude.Maybe Lude.Text)
bdmcrsBaseCommitId = Lens.lens (baseCommitId :: BatchDescribeMergeConflictsResponse -> Lude.Maybe Lude.Text) (\s a -> s {baseCommitId = a} :: BatchDescribeMergeConflictsResponse)
{-# DEPRECATED bdmcrsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrsNextToken :: Lens.Lens' BatchDescribeMergeConflictsResponse (Lude.Maybe Lude.Text)
bdmcrsNextToken = Lens.lens (nextToken :: BatchDescribeMergeConflictsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchDescribeMergeConflictsResponse)
{-# DEPRECATED bdmcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of any errors returned while describing the merge conflicts for each file.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrsErrors :: Lens.Lens' BatchDescribeMergeConflictsResponse (Lude.Maybe [BatchDescribeMergeConflictsError])
bdmcrsErrors = Lens.lens (errors :: BatchDescribeMergeConflictsResponse -> Lude.Maybe [BatchDescribeMergeConflictsError]) (\s a -> s {errors = a} :: BatchDescribeMergeConflictsResponse)
{-# DEPRECATED bdmcrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrsResponseStatus :: Lens.Lens' BatchDescribeMergeConflictsResponse Lude.Int
bdmcrsResponseStatus = Lens.lens (responseStatus :: BatchDescribeMergeConflictsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDescribeMergeConflictsResponse)
{-# DEPRECATED bdmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of conflicts for each file, including the conflict metadata and the hunks of the differences between the files.
--
-- /Note:/ Consider using 'conflicts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrsConflicts :: Lens.Lens' BatchDescribeMergeConflictsResponse [Conflict]
bdmcrsConflicts = Lens.lens (conflicts :: BatchDescribeMergeConflictsResponse -> [Conflict]) (\s a -> s {conflicts = a} :: BatchDescribeMergeConflictsResponse)
{-# DEPRECATED bdmcrsConflicts "Use generic-lens or generic-optics with 'conflicts' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrsDestinationCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse Lude.Text
bdmcrsDestinationCommitId = Lens.lens (destinationCommitId :: BatchDescribeMergeConflictsResponse -> Lude.Text) (\s a -> s {destinationCommitId = a} :: BatchDescribeMergeConflictsResponse)
{-# DEPRECATED bdmcrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmcrsSourceCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse Lude.Text
bdmcrsSourceCommitId = Lens.lens (sourceCommitId :: BatchDescribeMergeConflictsResponse -> Lude.Text) (\s a -> s {sourceCommitId = a} :: BatchDescribeMergeConflictsResponse)
{-# DEPRECATED bdmcrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}
