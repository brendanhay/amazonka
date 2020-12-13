{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetMergeOptions (..),
    mkGetMergeOptions,

    -- ** Request lenses
    gmoConflictDetailLevel,
    gmoRepositoryName,
    gmoSourceCommitSpecifier,
    gmoConflictResolutionStrategy,
    gmoDestinationCommitSpecifier,

    -- * Destructuring the response
    GetMergeOptionsResponse (..),
    mkGetMergeOptionsResponse,

    -- ** Response lenses
    gmorsDestinationCommitId,
    gmorsMergeOptions,
    gmorsBaseCommitId,
    gmorsSourceCommitId,
    gmorsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMergeOptions' smart constructor.
data GetMergeOptions = GetMergeOptions'
  { -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Lude.Maybe ConflictDetailLevelTypeEnum,
    -- | The name of the repository that contains the commits about which you want to get merge options.
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

-- | Creates a value of 'GetMergeOptions' with the minimum fields required to make a request.
--
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'repositoryName' - The name of the repository that contains the commits about which you want to get merge options.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkGetMergeOptions ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  GetMergeOptions
mkGetMergeOptions
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    GetMergeOptions'
      { conflictDetailLevel = Lude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        conflictResolutionStrategy = Lude.Nothing,
        destinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoConflictDetailLevel :: Lens.Lens' GetMergeOptions (Lude.Maybe ConflictDetailLevelTypeEnum)
gmoConflictDetailLevel = Lens.lens (conflictDetailLevel :: GetMergeOptions -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: GetMergeOptions)
{-# DEPRECATED gmoConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | The name of the repository that contains the commits about which you want to get merge options.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoRepositoryName :: Lens.Lens' GetMergeOptions Lude.Text
gmoRepositoryName = Lens.lens (repositoryName :: GetMergeOptions -> Lude.Text) (\s a -> s {repositoryName = a} :: GetMergeOptions)
{-# DEPRECATED gmoRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoSourceCommitSpecifier :: Lens.Lens' GetMergeOptions Lude.Text
gmoSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: GetMergeOptions -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: GetMergeOptions)
{-# DEPRECATED gmoSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoConflictResolutionStrategy :: Lens.Lens' GetMergeOptions (Lude.Maybe ConflictResolutionStrategyTypeEnum)
gmoConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: GetMergeOptions -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: GetMergeOptions)
{-# DEPRECATED gmoConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmoDestinationCommitSpecifier :: Lens.Lens' GetMergeOptions Lude.Text
gmoDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: GetMergeOptions -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: GetMergeOptions)
{-# DEPRECATED gmoDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

instance Lude.AWSRequest GetMergeOptions where
  type Rs GetMergeOptions = GetMergeOptionsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMergeOptionsResponse'
            Lude.<$> (x Lude..:> "destinationCommitId")
            Lude.<*> (x Lude..?> "mergeOptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "baseCommitId")
            Lude.<*> (x Lude..:> "sourceCommitId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMergeOptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetMergeOptions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMergeOptions where
  toJSON GetMergeOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier)
          ]
      )

instance Lude.ToPath GetMergeOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMergeOptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMergeOptionsResponse' smart constructor.
data GetMergeOptionsResponse = GetMergeOptionsResponse'
  { -- | The commit ID of the destination commit specifier that was used in the merge evaluation.
    destinationCommitId :: Lude.Text,
    -- | The merge option or strategy used to merge the code.
    mergeOptions :: [MergeOptionTypeEnum],
    -- | The commit ID of the merge base.
    baseCommitId :: Lude.Text,
    -- | The commit ID of the source commit specifier that was used in the merge evaluation.
    sourceCommitId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMergeOptionsResponse' with the minimum fields required to make a request.
--
-- * 'destinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
-- * 'mergeOptions' - The merge option or strategy used to merge the code.
-- * 'baseCommitId' - The commit ID of the merge base.
-- * 'sourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
-- * 'responseStatus' - The response status code.
mkGetMergeOptionsResponse ::
  -- | 'destinationCommitId'
  Lude.Text ->
  -- | 'baseCommitId'
  Lude.Text ->
  -- | 'sourceCommitId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetMergeOptionsResponse
mkGetMergeOptionsResponse
  pDestinationCommitId_
  pBaseCommitId_
  pSourceCommitId_
  pResponseStatus_ =
    GetMergeOptionsResponse'
      { destinationCommitId =
          pDestinationCommitId_,
        mergeOptions = Lude.mempty,
        baseCommitId = pBaseCommitId_,
        sourceCommitId = pSourceCommitId_,
        responseStatus = pResponseStatus_
      }

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorsDestinationCommitId :: Lens.Lens' GetMergeOptionsResponse Lude.Text
gmorsDestinationCommitId = Lens.lens (destinationCommitId :: GetMergeOptionsResponse -> Lude.Text) (\s a -> s {destinationCommitId = a} :: GetMergeOptionsResponse)
{-# DEPRECATED gmorsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The merge option or strategy used to merge the code.
--
-- /Note:/ Consider using 'mergeOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorsMergeOptions :: Lens.Lens' GetMergeOptionsResponse [MergeOptionTypeEnum]
gmorsMergeOptions = Lens.lens (mergeOptions :: GetMergeOptionsResponse -> [MergeOptionTypeEnum]) (\s a -> s {mergeOptions = a} :: GetMergeOptionsResponse)
{-# DEPRECATED gmorsMergeOptions "Use generic-lens or generic-optics with 'mergeOptions' instead." #-}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorsBaseCommitId :: Lens.Lens' GetMergeOptionsResponse Lude.Text
gmorsBaseCommitId = Lens.lens (baseCommitId :: GetMergeOptionsResponse -> Lude.Text) (\s a -> s {baseCommitId = a} :: GetMergeOptionsResponse)
{-# DEPRECATED gmorsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorsSourceCommitId :: Lens.Lens' GetMergeOptionsResponse Lude.Text
gmorsSourceCommitId = Lens.lens (sourceCommitId :: GetMergeOptionsResponse -> Lude.Text) (\s a -> s {sourceCommitId = a} :: GetMergeOptionsResponse)
{-# DEPRECATED gmorsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmorsResponseStatus :: Lens.Lens' GetMergeOptionsResponse Lude.Int
gmorsResponseStatus = Lens.lens (responseStatus :: GetMergeOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMergeOptionsResponse)
{-# DEPRECATED gmorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
