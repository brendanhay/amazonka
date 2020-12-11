{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dmcConflictDetailLevel,
    dmcNextToken,
    dmcMaxMergeHunks,
    dmcConflictResolutionStrategy,
    dmcRepositoryName,
    dmcDestinationCommitSpecifier,
    dmcSourceCommitSpecifier,
    dmcMergeOption,
    dmcFilePath,

    -- * Destructuring the response
    DescribeMergeConflictsResponse (..),
    mkDescribeMergeConflictsResponse,

    -- ** Response lenses
    dmcrsBaseCommitId,
    dmcrsNextToken,
    dmcrsResponseStatus,
    dmcrsConflictMetadata,
    dmcrsMergeHunks,
    dmcrsDestinationCommitId,
    dmcrsSourceCommitId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMergeConflicts' smart constructor.
data DescribeMergeConflicts = DescribeMergeConflicts'
  { conflictDetailLevel ::
      Lude.Maybe ConflictDetailLevelTypeEnum,
    nextToken :: Lude.Maybe Lude.Text,
    maxMergeHunks :: Lude.Maybe Lude.Int,
    conflictResolutionStrategy ::
      Lude.Maybe ConflictResolutionStrategyTypeEnum,
    repositoryName :: Lude.Text,
    destinationCommitSpecifier :: Lude.Text,
    sourceCommitSpecifier :: Lude.Text,
    mergeOption :: MergeOptionTypeEnum,
    filePath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMergeConflicts' with the minimum fields required to make a request.
--
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'filePath' - The path of the target files used to describe the conflicts.
-- * 'maxMergeHunks' - The maximum number of merge hunks to include in the output.
-- * 'mergeOption' - The merge option or strategy you want to use to merge the code.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'repositoryName' - The name of the repository where you want to get information about a merge conflict.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkDescribeMergeConflicts ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  -- | 'filePath'
  Lude.Text ->
  DescribeMergeConflicts
mkDescribeMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_
  pFilePath_ =
    DescribeMergeConflicts'
      { conflictDetailLevel = Lude.Nothing,
        nextToken = Lude.Nothing,
        maxMergeHunks = Lude.Nothing,
        conflictResolutionStrategy = Lude.Nothing,
        repositoryName = pRepositoryName_,
        destinationCommitSpecifier = pDestinationCommitSpecifier_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        mergeOption = pMergeOption_,
        filePath = pFilePath_
      }

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

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcConflictResolutionStrategy :: Lens.Lens' DescribeMergeConflicts (Lude.Maybe ConflictResolutionStrategyTypeEnum)
dmcConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: DescribeMergeConflicts -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The name of the repository where you want to get information about a merge conflict.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcRepositoryName :: Lens.Lens' DescribeMergeConflicts Lude.Text
dmcRepositoryName = Lens.lens (repositoryName :: DescribeMergeConflicts -> Lude.Text) (\s a -> s {repositoryName = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcDestinationCommitSpecifier :: Lens.Lens' DescribeMergeConflicts Lude.Text
dmcDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: DescribeMergeConflicts -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcSourceCommitSpecifier :: Lens.Lens' DescribeMergeConflicts Lude.Text
dmcSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: DescribeMergeConflicts -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcMergeOption :: Lens.Lens' DescribeMergeConflicts MergeOptionTypeEnum
dmcMergeOption = Lens.lens (mergeOption :: DescribeMergeConflicts -> MergeOptionTypeEnum) (\s a -> s {mergeOption = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

-- | The path of the target files used to describe the conflicts.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcFilePath :: Lens.Lens' DescribeMergeConflicts Lude.Text
dmcFilePath = Lens.lens (filePath :: DescribeMergeConflicts -> Lude.Text) (\s a -> s {filePath = a} :: DescribeMergeConflicts)
{-# DEPRECATED dmcFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

instance Lude.AWSRequest DescribeMergeConflicts where
  type Rs DescribeMergeConflicts = DescribeMergeConflictsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMergeConflictsResponse'
            Lude.<$> (x Lude..?> "baseCommitId")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "conflictMetadata")
            Lude.<*> (x Lude..?> "mergeHunks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "destinationCommitId")
            Lude.<*> (x Lude..:> "sourceCommitId")
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
          [ ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxMergeHunks" Lude..=) Lude.<$> maxMergeHunks,
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            Lude.Just ("mergeOption" Lude..= mergeOption),
            Lude.Just ("filePath" Lude..= filePath)
          ]
      )

instance Lude.ToPath DescribeMergeConflicts where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMergeConflicts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMergeConflictsResponse' smart constructor.
data DescribeMergeConflictsResponse = DescribeMergeConflictsResponse'
  { baseCommitId ::
      Lude.Maybe Lude.Text,
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    conflictMetadata ::
      ConflictMetadata,
    mergeHunks :: [MergeHunk],
    destinationCommitId ::
      Lude.Text,
    sourceCommitId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMergeConflictsResponse' with the minimum fields required to make a request.
--
-- * 'baseCommitId' - The commit ID of the merge base.
-- * 'conflictMetadata' - Contains metadata about the conflicts found in the merge.
-- * 'destinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
-- * 'mergeHunks' - A list of merge hunks of the differences between the files or lines.
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'responseStatus' - The response status code.
-- * 'sourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
mkDescribeMergeConflictsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'conflictMetadata'
  ConflictMetadata ->
  -- | 'destinationCommitId'
  Lude.Text ->
  -- | 'sourceCommitId'
  Lude.Text ->
  DescribeMergeConflictsResponse
mkDescribeMergeConflictsResponse
  pResponseStatus_
  pConflictMetadata_
  pDestinationCommitId_
  pSourceCommitId_ =
    DescribeMergeConflictsResponse'
      { baseCommitId = Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_,
        conflictMetadata = pConflictMetadata_,
        mergeHunks = Lude.mempty,
        destinationCommitId = pDestinationCommitId_,
        sourceCommitId = pSourceCommitId_
      }

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

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsResponseStatus :: Lens.Lens' DescribeMergeConflictsResponse Lude.Int
dmcrsResponseStatus = Lens.lens (responseStatus :: DescribeMergeConflictsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Contains metadata about the conflicts found in the merge.
--
-- /Note:/ Consider using 'conflictMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsConflictMetadata :: Lens.Lens' DescribeMergeConflictsResponse ConflictMetadata
dmcrsConflictMetadata = Lens.lens (conflictMetadata :: DescribeMergeConflictsResponse -> ConflictMetadata) (\s a -> s {conflictMetadata = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsConflictMetadata "Use generic-lens or generic-optics with 'conflictMetadata' instead." #-}

-- | A list of merge hunks of the differences between the files or lines.
--
-- /Note:/ Consider using 'mergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsMergeHunks :: Lens.Lens' DescribeMergeConflictsResponse [MergeHunk]
dmcrsMergeHunks = Lens.lens (mergeHunks :: DescribeMergeConflictsResponse -> [MergeHunk]) (\s a -> s {mergeHunks = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsMergeHunks "Use generic-lens or generic-optics with 'mergeHunks' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsDestinationCommitId :: Lens.Lens' DescribeMergeConflictsResponse Lude.Text
dmcrsDestinationCommitId = Lens.lens (destinationCommitId :: DescribeMergeConflictsResponse -> Lude.Text) (\s a -> s {destinationCommitId = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsSourceCommitId :: Lens.Lens' DescribeMergeConflictsResponse Lude.Text
dmcrsSourceCommitId = Lens.lens (sourceCommitId :: DescribeMergeConflictsResponse -> Lude.Text) (\s a -> s {sourceCommitId = a} :: DescribeMergeConflictsResponse)
{-# DEPRECATED dmcrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}
