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
    gmcMergeOption,
    gmcConflictDetailLevel,
    gmcNextToken,
    gmcMaxConflictFiles,
    gmcRepositoryName,
    gmcSourceCommitSpecifier,
    gmcConflictResolutionStrategy,
    gmcDestinationCommitSpecifier,

    -- * Destructuring the response
    GetMergeConflictsResponse (..),
    mkGetMergeConflictsResponse,

    -- ** Response lenses
    gmcrsMergeable,
    gmcrsDestinationCommitId,
    gmcrsBaseCommitId,
    gmcrsNextToken,
    gmcrsSourceCommitId,
    gmcrsConflictMetadataList,
    gmcrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMergeConflicts' smart constructor.
data GetMergeConflicts = GetMergeConflicts'
  { -- | The merge option or strategy you want to use to merge the code.
    mergeOption :: MergeOptionTypeEnum,
    -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Lude.Maybe ConflictDetailLevelTypeEnum,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of files to include in the output.
    maxConflictFiles :: Lude.Maybe Lude.Int,
    -- | The name of the repository where the pull request was created.
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

-- | Creates a value of 'GetMergeConflicts' with the minimum fields required to make a request.
--
-- * 'mergeOption' - The merge option or strategy you want to use to merge the code.
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'maxConflictFiles' - The maximum number of files to include in the output.
-- * 'repositoryName' - The name of the repository where the pull request was created.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkGetMergeConflicts ::
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  GetMergeConflicts
mkGetMergeConflicts
  pMergeOption_
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    GetMergeConflicts'
      { mergeOption = pMergeOption_,
        conflictDetailLevel = Lude.Nothing,
        nextToken = Lude.Nothing,
        maxConflictFiles = Lude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        conflictResolutionStrategy = Lude.Nothing,
        destinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcMergeOption :: Lens.Lens' GetMergeConflicts MergeOptionTypeEnum
gmcMergeOption = Lens.lens (mergeOption :: GetMergeConflicts -> MergeOptionTypeEnum) (\s a -> s {mergeOption = a} :: GetMergeConflicts)
{-# DEPRECATED gmcMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcConflictDetailLevel :: Lens.Lens' GetMergeConflicts (Lude.Maybe ConflictDetailLevelTypeEnum)
gmcConflictDetailLevel = Lens.lens (conflictDetailLevel :: GetMergeConflicts -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: GetMergeConflicts)
{-# DEPRECATED gmcConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcNextToken :: Lens.Lens' GetMergeConflicts (Lude.Maybe Lude.Text)
gmcNextToken = Lens.lens (nextToken :: GetMergeConflicts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMergeConflicts)
{-# DEPRECATED gmcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of files to include in the output.
--
-- /Note:/ Consider using 'maxConflictFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcMaxConflictFiles :: Lens.Lens' GetMergeConflicts (Lude.Maybe Lude.Int)
gmcMaxConflictFiles = Lens.lens (maxConflictFiles :: GetMergeConflicts -> Lude.Maybe Lude.Int) (\s a -> s {maxConflictFiles = a} :: GetMergeConflicts)
{-# DEPRECATED gmcMaxConflictFiles "Use generic-lens or generic-optics with 'maxConflictFiles' instead." #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcRepositoryName :: Lens.Lens' GetMergeConflicts Lude.Text
gmcRepositoryName = Lens.lens (repositoryName :: GetMergeConflicts -> Lude.Text) (\s a -> s {repositoryName = a} :: GetMergeConflicts)
{-# DEPRECATED gmcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcSourceCommitSpecifier :: Lens.Lens' GetMergeConflicts Lude.Text
gmcSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: GetMergeConflicts -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: GetMergeConflicts)
{-# DEPRECATED gmcSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcConflictResolutionStrategy :: Lens.Lens' GetMergeConflicts (Lude.Maybe ConflictResolutionStrategyTypeEnum)
gmcConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: GetMergeConflicts -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: GetMergeConflicts)
{-# DEPRECATED gmcConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcDestinationCommitSpecifier :: Lens.Lens' GetMergeConflicts Lude.Text
gmcDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: GetMergeConflicts -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: GetMergeConflicts)
{-# DEPRECATED gmcDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

instance Lude.AWSRequest GetMergeConflicts where
  type Rs GetMergeConflicts = GetMergeConflictsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMergeConflictsResponse'
            Lude.<$> (x Lude..:> "mergeable")
            Lude.<*> (x Lude..:> "destinationCommitId")
            Lude.<*> (x Lude..?> "baseCommitId")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..:> "sourceCommitId")
            Lude.<*> (x Lude..?> "conflictMetadataList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
          [ Lude.Just ("mergeOption" Lude..= mergeOption),
            ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxConflictFiles" Lude..=) Lude.<$> maxConflictFiles,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier)
          ]
      )

instance Lude.ToPath GetMergeConflicts where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMergeConflicts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMergeConflictsResponse' smart constructor.
data GetMergeConflictsResponse = GetMergeConflictsResponse'
  { -- | A Boolean value that indicates whether the code is mergeable by the specified merge option.
    mergeable :: Lude.Bool,
    -- | The commit ID of the destination commit specifier that was used in the merge evaluation.
    destinationCommitId :: Lude.Text,
    -- | The commit ID of the merge base.
    baseCommitId :: Lude.Maybe Lude.Text,
    -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The commit ID of the source commit specifier that was used in the merge evaluation.
    sourceCommitId :: Lude.Text,
    -- | A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
    conflictMetadataList :: [ConflictMetadata],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMergeConflictsResponse' with the minimum fields required to make a request.
--
-- * 'mergeable' - A Boolean value that indicates whether the code is mergeable by the specified merge option.
-- * 'destinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
-- * 'baseCommitId' - The commit ID of the merge base.
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'sourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
-- * 'conflictMetadataList' - A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
-- * 'responseStatus' - The response status code.
mkGetMergeConflictsResponse ::
  -- | 'mergeable'
  Lude.Bool ->
  -- | 'destinationCommitId'
  Lude.Text ->
  -- | 'sourceCommitId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetMergeConflictsResponse
mkGetMergeConflictsResponse
  pMergeable_
  pDestinationCommitId_
  pSourceCommitId_
  pResponseStatus_ =
    GetMergeConflictsResponse'
      { mergeable = pMergeable_,
        destinationCommitId = pDestinationCommitId_,
        baseCommitId = Lude.Nothing,
        nextToken = Lude.Nothing,
        sourceCommitId = pSourceCommitId_,
        conflictMetadataList = Lude.mempty,
        responseStatus = pResponseStatus_
      }

-- | A Boolean value that indicates whether the code is mergeable by the specified merge option.
--
-- /Note:/ Consider using 'mergeable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrsMergeable :: Lens.Lens' GetMergeConflictsResponse Lude.Bool
gmcrsMergeable = Lens.lens (mergeable :: GetMergeConflictsResponse -> Lude.Bool) (\s a -> s {mergeable = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcrsMergeable "Use generic-lens or generic-optics with 'mergeable' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrsDestinationCommitId :: Lens.Lens' GetMergeConflictsResponse Lude.Text
gmcrsDestinationCommitId = Lens.lens (destinationCommitId :: GetMergeConflictsResponse -> Lude.Text) (\s a -> s {destinationCommitId = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcrsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrsBaseCommitId :: Lens.Lens' GetMergeConflictsResponse (Lude.Maybe Lude.Text)
gmcrsBaseCommitId = Lens.lens (baseCommitId :: GetMergeConflictsResponse -> Lude.Maybe Lude.Text) (\s a -> s {baseCommitId = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcrsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrsNextToken :: Lens.Lens' GetMergeConflictsResponse (Lude.Maybe Lude.Text)
gmcrsNextToken = Lens.lens (nextToken :: GetMergeConflictsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrsSourceCommitId :: Lens.Lens' GetMergeConflictsResponse Lude.Text
gmcrsSourceCommitId = Lens.lens (sourceCommitId :: GetMergeConflictsResponse -> Lude.Text) (\s a -> s {sourceCommitId = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcrsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | A list of metadata for any conflicting files. If the specified merge strategy is FAST_FORWARD_MERGE, this list is always empty.
--
-- /Note:/ Consider using 'conflictMetadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrsConflictMetadataList :: Lens.Lens' GetMergeConflictsResponse [ConflictMetadata]
gmcrsConflictMetadataList = Lens.lens (conflictMetadataList :: GetMergeConflictsResponse -> [ConflictMetadata]) (\s a -> s {conflictMetadataList = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcrsConflictMetadataList "Use generic-lens or generic-optics with 'conflictMetadataList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcrsResponseStatus :: Lens.Lens' GetMergeConflictsResponse Lude.Int
gmcrsResponseStatus = Lens.lens (responseStatus :: GetMergeConflictsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMergeConflictsResponse)
{-# DEPRECATED gmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
