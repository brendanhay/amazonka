{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gmcsConflictDetailLevel,
    gmcsNextToken,
    gmcsMaxConflictFiles,
    gmcsConflictResolutionStrategy,
    gmcsRepositoryName,
    gmcsDestinationCommitSpecifier,
    gmcsSourceCommitSpecifier,
    gmcsMergeOption,

    -- * Destructuring the response
    GetMergeConflictsResponse (..),
    mkGetMergeConflictsResponse,

    -- ** Response lenses
    gmcsrsBaseCommitId,
    gmcsrsNextToken,
    gmcsrsResponseStatus,
    gmcsrsMergeable,
    gmcsrsDestinationCommitId,
    gmcsrsSourceCommitId,
    gmcsrsConflictMetadataList,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMergeConflicts' smart constructor.
data GetMergeConflicts = GetMergeConflicts'
  { conflictDetailLevel ::
      Lude.Maybe ConflictDetailLevelTypeEnum,
    nextToken :: Lude.Maybe Lude.Text,
    maxConflictFiles :: Lude.Maybe Lude.Int,
    conflictResolutionStrategy ::
      Lude.Maybe ConflictResolutionStrategyTypeEnum,
    repositoryName :: Lude.Text,
    destinationCommitSpecifier :: Lude.Text,
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

-- | Creates a value of 'GetMergeConflicts' with the minimum fields required to make a request.
--
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'maxConflictFiles' - The maximum number of files to include in the output.
-- * 'mergeOption' - The merge option or strategy you want to use to merge the code.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'repositoryName' - The name of the repository where the pull request was created.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkGetMergeConflicts ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  GetMergeConflicts
mkGetMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_ =
    GetMergeConflicts'
      { conflictDetailLevel = Lude.Nothing,
        nextToken = Lude.Nothing,
        maxConflictFiles = Lude.Nothing,
        conflictResolutionStrategy = Lude.Nothing,
        repositoryName = pRepositoryName_,
        destinationCommitSpecifier = pDestinationCommitSpecifier_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        mergeOption = pMergeOption_
      }

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsConflictDetailLevel :: Lens.Lens' GetMergeConflicts (Lude.Maybe ConflictDetailLevelTypeEnum)
gmcsConflictDetailLevel = Lens.lens (conflictDetailLevel :: GetMergeConflicts -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: GetMergeConflicts)
{-# DEPRECATED gmcsConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsNextToken :: Lens.Lens' GetMergeConflicts (Lude.Maybe Lude.Text)
gmcsNextToken = Lens.lens (nextToken :: GetMergeConflicts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMergeConflicts)
{-# DEPRECATED gmcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of files to include in the output.
--
-- /Note:/ Consider using 'maxConflictFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsMaxConflictFiles :: Lens.Lens' GetMergeConflicts (Lude.Maybe Lude.Int)
gmcsMaxConflictFiles = Lens.lens (maxConflictFiles :: GetMergeConflicts -> Lude.Maybe Lude.Int) (\s a -> s {maxConflictFiles = a} :: GetMergeConflicts)
{-# DEPRECATED gmcsMaxConflictFiles "Use generic-lens or generic-optics with 'maxConflictFiles' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsConflictResolutionStrategy :: Lens.Lens' GetMergeConflicts (Lude.Maybe ConflictResolutionStrategyTypeEnum)
gmcsConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: GetMergeConflicts -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: GetMergeConflicts)
{-# DEPRECATED gmcsConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsRepositoryName :: Lens.Lens' GetMergeConflicts Lude.Text
gmcsRepositoryName = Lens.lens (repositoryName :: GetMergeConflicts -> Lude.Text) (\s a -> s {repositoryName = a} :: GetMergeConflicts)
{-# DEPRECATED gmcsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsDestinationCommitSpecifier :: Lens.Lens' GetMergeConflicts Lude.Text
gmcsDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: GetMergeConflicts -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: GetMergeConflicts)
{-# DEPRECATED gmcsDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsSourceCommitSpecifier :: Lens.Lens' GetMergeConflicts Lude.Text
gmcsSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: GetMergeConflicts -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: GetMergeConflicts)
{-# DEPRECATED gmcsSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsMergeOption :: Lens.Lens' GetMergeConflicts MergeOptionTypeEnum
gmcsMergeOption = Lens.lens (mergeOption :: GetMergeConflicts -> MergeOptionTypeEnum) (\s a -> s {mergeOption = a} :: GetMergeConflicts)
{-# DEPRECATED gmcsMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

instance Lude.AWSRequest GetMergeConflicts where
  type Rs GetMergeConflicts = GetMergeConflictsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMergeConflictsResponse'
            Lude.<$> (x Lude..?> "baseCommitId")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "mergeable")
            Lude.<*> (x Lude..:> "destinationCommitId")
            Lude.<*> (x Lude..:> "sourceCommitId")
            Lude.<*> (x Lude..?> "conflictMetadataList" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetMergeConflicts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetMergeConflicts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMergeConflicts where
  toJSON GetMergeConflicts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxConflictFiles" Lude..=) Lude.<$> maxConflictFiles,
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            Lude.Just ("mergeOption" Lude..= mergeOption)
          ]
      )

instance Lude.ToPath GetMergeConflicts where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMergeConflicts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMergeConflictsResponse' smart constructor.
data GetMergeConflictsResponse = GetMergeConflictsResponse'
  { baseCommitId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    mergeable :: Lude.Bool,
    destinationCommitId :: Lude.Text,
    sourceCommitId :: Lude.Text,
    conflictMetadataList ::
      [ConflictMetadata]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMergeConflictsResponse' with the minimum fields required to make a request.
--
-- * 'baseCommitId' - The commit ID of the merge base.
-- * 'conflictMetadataList' - A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
-- * 'destinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
-- * 'mergeable' - A Boolean value that indicates whether the code is mergeable by the specified merge option.
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'responseStatus' - The response status code.
-- * 'sourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
mkGetMergeConflictsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'mergeable'
  Lude.Bool ->
  -- | 'destinationCommitId'
  Lude.Text ->
  -- | 'sourceCommitId'
  Lude.Text ->
  GetMergeConflictsResponse
mkGetMergeConflictsResponse
  pResponseStatus_
  pMergeable_
  pDestinationCommitId_
  pSourceCommitId_ =
    GetMergeConflictsResponse'
      { baseCommitId = Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_,
        mergeable = pMergeable_,
        destinationCommitId = pDestinationCommitId_,
        sourceCommitId = pSourceCommitId_,
        conflictMetadataList = Lude.mempty
      }

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsrsBaseCommitId :: Lens.Lens' GetMergeConflictsResponse (Lude.Maybe Lude.Text)
gmcsrsBaseCommitId = Lens.lens (baseCommitId :: GetMergeConflictsResponse -> Lude.Maybe Lude.Text) (\s a -> s {baseCommitId = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcsrsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsrsNextToken :: Lens.Lens' GetMergeConflictsResponse (Lude.Maybe Lude.Text)
gmcsrsNextToken = Lens.lens (nextToken :: GetMergeConflictsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsrsResponseStatus :: Lens.Lens' GetMergeConflictsResponse Lude.Int
gmcsrsResponseStatus = Lens.lens (responseStatus :: GetMergeConflictsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A Boolean value that indicates whether the code is mergeable by the specified merge option.
--
-- /Note:/ Consider using 'mergeable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsrsMergeable :: Lens.Lens' GetMergeConflictsResponse Lude.Bool
gmcsrsMergeable = Lens.lens (mergeable :: GetMergeConflictsResponse -> Lude.Bool) (\s a -> s {mergeable = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcsrsMergeable "Use generic-lens or generic-optics with 'mergeable' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsrsDestinationCommitId :: Lens.Lens' GetMergeConflictsResponse Lude.Text
gmcsrsDestinationCommitId = Lens.lens (destinationCommitId :: GetMergeConflictsResponse -> Lude.Text) (\s a -> s {destinationCommitId = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcsrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsrsSourceCommitId :: Lens.Lens' GetMergeConflictsResponse Lude.Text
gmcsrsSourceCommitId = Lens.lens (sourceCommitId :: GetMergeConflictsResponse -> Lude.Text) (\s a -> s {sourceCommitId = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcsrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
--
-- /Note:/ Consider using 'conflictMetadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcsrsConflictMetadataList :: Lens.Lens' GetMergeConflictsResponse [ConflictMetadata]
gmcsrsConflictMetadataList = Lens.lens (conflictMetadataList :: GetMergeConflictsResponse -> [ConflictMetadata]) (\s a -> s {conflictMetadataList = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcsrsConflictMetadataList "Use generic-lens or generic-optics with 'conflictMetadataList' instead." #-}
