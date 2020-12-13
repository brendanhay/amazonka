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
    dmcFilePath,
    dmcMergeOption,
    dmcConflictDetailLevel,
    dmcNextToken,
    dmcMaxMergeHunks,
    dmcRepositoryName,
    dmcSourceCommitSpecifier,
    dmcConflictResolutionStrategy,
    dmcDestinationCommitSpecifier,

    -- * Destructuring the response
    DescribeMergeConflictsResponse (..),
    mkDescribeMergeConflictsResponse,

    -- ** Response lenses
    dmcrsDestinationCommitId,
    dmcrsBaseCommitId,
    dmcrsNextToken,
    dmcrsMergeHunks,
    dmcrsConflictMetadata,
    dmcrsSourceCommitId,
    dmcrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMergeConflicts' smart constructor.
data DescribeMergeConflicts = DescribeMergeConflicts'
  { -- | The path of the target files used to describe the conflicts.
    filePath :: Lude.Text,
    -- | The merge option or strategy you want to use to merge the code.
    mergeOption :: MergeOptionTypeEnum,
    -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Lude.Maybe ConflictDetailLevelTypeEnum,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of merge hunks to include in the output.
    maxMergeHunks :: Lude.Maybe Lude.Int,
    -- | The name of the repository where you want to get information about a merge conflict.
    repositoryName :: Lude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Lude.Text,
    -- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
    conflictResolutionStrategy :: Lude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMergeConflicts' with the minimum fields required to make a request.
--
-- * 'filePath' - The path of the target files used to describe the conflicts.
-- * 'mergeOption' - The merge option or strategy you want to use to merge the code.
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'maxMergeHunks' - The maximum number of merge hunks to include in the output.
-- * 'repositoryName' - The name of the repository where you want to get information about a merge conflict.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkDescribeMergeConflicts ::
  -- | 'filePath'
  Lude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  DescribeMergeConflicts
mkDescribeMergeConflicts
  pFilePath_
  pMergeOption_
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    DescribeMergeConflicts'
      { filePath = pFilePath_,
        mergeOption = pMergeOption_,
        conflictDetailLevel = Lude.Nothing,
        nextToken = Lude.Nothing,
        maxMergeHunks = Lude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        conflictResolutionStrategy = Lude.Nothing,
        destinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The path of the target files used to describe the conflicts.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcFilePath :: Lens.Lens' DescribeMergeConflicts Lude.Text
dmcFilePath = Lens.lens (filePath :: DescribeMergeConflicts -> Lude.Text) (\s a -> s {filePath = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcMergeOption :: Lens.Lens' DescribeMergeConflicts MergeOptionTypeEnum
dmcMergeOption = Lens.lens (mergeOption :: DescribeMergeConflicts -> MergeOptionTypeEnum) (\s a -> s {mergeOption = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcConflictDetailLevel :: Lens.Lens' DescribeMergeConflicts (Lude.Maybe ConflictDetailLevelTypeEnum)
dmcConflictDetailLevel = Lens.lens (conflictDetailLevel :: DescribeMergeConflicts -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcNextToken :: Lens.Lens' DescribeMergeConflicts (Lude.Maybe Lude.Text)
dmcNextToken = Lens.lens (nextToken :: DescribeMergeConflicts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of merge hunks to include in the output.
--
-- /Note:/ Consider using 'maxMergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcMaxMergeHunks :: Lens.Lens' DescribeMergeConflicts (Lude.Maybe Lude.Int)
dmcMaxMergeHunks = Lens.lens (maxMergeHunks :: DescribeMergeConflicts -> Lude.Maybe Lude.Int) (\s a -> s {maxMergeHunks = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcMaxMergeHunks "Use generic-lens or generic-optics with 'maxMergeHunks' instead." #-}

-- | The name of the repository where you want to get information about a merge conflict.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcRepositoryName :: Lens.Lens' DescribeMergeConflicts Lude.Text
dmcRepositoryName = Lens.lens (repositoryName :: DescribeMergeConflicts -> Lude.Text) (\s a -> s {repositoryName = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcSourceCommitSpecifier :: Lens.Lens' DescribeMergeConflicts Lude.Text
dmcSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: DescribeMergeConflicts -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcConflictResolutionStrategy :: Lens.Lens' DescribeMergeConflicts (Lude.Maybe ConflictResolutionStrategyTypeEnum)
dmcConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: DescribeMergeConflicts -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcDestinationCommitSpecifier :: Lens.Lens' DescribeMergeConflicts Lude.Text
dmcDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: DescribeMergeConflicts -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

instance Lude.AWSRequest DescribeMergeConflicts where
  type Rs DescribeMergeConflicts = DescribeMergeConflictsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMergeConflictsResponse'
            Lude.<$> (x Lude..:> "destinationCommitId")
            Lude.<*> (x Lude..?> "baseCommitId")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "mergeHunks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "conflictMetadata")
            Lude.<*> (x Lude..:> "sourceCommitId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMergeConflicts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.DescribeMergeConflicts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMergeConflicts where
  toJSON DescribeMergeConflicts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("filePath" Lude..= filePath),
            Lude.Just ("mergeOption" Lude..= mergeOption),
            ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxMergeHunks" Lude..=) Lude.<$> maxMergeHunks,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier)
          ]
      )

instance Lude.ToPath DescribeMergeConflicts where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMergeConflicts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMergeConflictsResponse' smart constructor.
data DescribeMergeConflictsResponse = DescribeMergeConflictsResponse'
  { -- | The commit ID of the destination commit specifier that was used in the merge evaluation.
    destinationCommitId :: Lude.Text,
    -- | The commit ID of the merge base.
    baseCommitId :: Lude.Maybe Lude.Text,
    -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of merge hunks of the differences between the files or lines.
    mergeHunks :: [MergeHunk],
    -- | Contains metadata about the conflicts found in the merge.
    conflictMetadata :: ConflictMetadata,
    -- | The commit ID of the source commit specifier that was used in the merge evaluation.
    sourceCommitId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMergeConflictsResponse' with the minimum fields required to make a request.
--
-- * 'destinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
-- * 'baseCommitId' - The commit ID of the merge base.
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'mergeHunks' - A list of merge hunks of the differences between the files or lines.
-- * 'conflictMetadata' - Contains metadata about the conflicts found in the merge.
-- * 'sourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
-- * 'responseStatus' - The response status code.
mkDescribeMergeConflictsResponse ::
  -- | 'destinationCommitId'
  Lude.Text ->
  -- | 'conflictMetadata'
  ConflictMetadata ->
  -- | 'sourceCommitId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMergeConflictsResponse
mkDescribeMergeConflictsResponse
  pDestinationCommitId_
  pConflictMetadata_
  pSourceCommitId_
  pResponseStatus_ =
    DescribeMergeConflictsResponse'
      { destinationCommitId =
          pDestinationCommitId_,
        baseCommitId = Lude.Nothing,
        nextToken = Lude.Nothing,
        mergeHunks = Lude.mempty,
        conflictMetadata = pConflictMetadata_,
        sourceCommitId = pSourceCommitId_,
        responseStatus = pResponseStatus_
      }

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsDestinationCommitId :: Lens.Lens' DescribeMergeConflictsResponse Lude.Text
dmcrsDestinationCommitId = Lens.lens (destinationCommitId :: DescribeMergeConflictsResponse -> Lude.Text) (\s a -> s {destinationCommitId = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsBaseCommitId :: Lens.Lens' DescribeMergeConflictsResponse (Lude.Maybe Lude.Text)
dmcrsBaseCommitId = Lens.lens (baseCommitId :: DescribeMergeConflictsResponse -> Lude.Maybe Lude.Text) (\s a -> s {baseCommitId = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsNextToken :: Lens.Lens' DescribeMergeConflictsResponse (Lude.Maybe Lude.Text)
dmcrsNextToken = Lens.lens (nextToken :: DescribeMergeConflictsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of merge hunks of the differences between the files or lines.
--
-- /Note:/ Consider using 'mergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsMergeHunks :: Lens.Lens' DescribeMergeConflictsResponse [MergeHunk]
dmcrsMergeHunks = Lens.lens (mergeHunks :: DescribeMergeConflictsResponse -> [MergeHunk]) (\s a -> s {mergeHunks = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsMergeHunks "Use generic-lens or generic-optics with 'mergeHunks' instead." #-}

-- | Contains metadata about the conflicts found in the merge.
--
-- /Note:/ Consider using 'conflictMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsConflictMetadata :: Lens.Lens' DescribeMergeConflictsResponse ConflictMetadata
dmcrsConflictMetadata = Lens.lens (conflictMetadata :: DescribeMergeConflictsResponse -> ConflictMetadata) (\s a -> s {conflictMetadata = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsConflictMetadata "Use generic-lens or generic-optics with 'conflictMetadata' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsSourceCommitId :: Lens.Lens' DescribeMergeConflictsResponse Lude.Text
dmcrsSourceCommitId = Lens.lens (sourceCommitId :: DescribeMergeConflictsResponse -> Lude.Text) (\s a -> s {sourceCommitId = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsResponseStatus :: Lens.Lens' DescribeMergeConflictsResponse Lude.Int
dmcrsResponseStatus = Lens.lens (responseStatus :: DescribeMergeConflictsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
