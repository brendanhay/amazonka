{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetMergeCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified merge commit.
module Network.AWS.CodeCommit.GetMergeCommit
  ( -- * Creating a request
    GetMergeCommit (..),
    mkGetMergeCommit,

    -- ** Request lenses
    gmcfConflictDetailLevel,
    gmcfRepositoryName,
    gmcfSourceCommitSpecifier,
    gmcfConflictResolutionStrategy,
    gmcfDestinationCommitSpecifier,

    -- * Destructuring the response
    GetMergeCommitResponse (..),
    mkGetMergeCommitResponse,

    -- ** Response lenses
    grsMergedCommitId,
    grsDestinationCommitId,
    grsBaseCommitId,
    grsSourceCommitId,
    grsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMergeCommit' smart constructor.
data GetMergeCommit = GetMergeCommit'
  { -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Lude.Maybe ConflictDetailLevelTypeEnum,
    -- | The name of the repository that contains the merge commit about which you want to get information.
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

-- | Creates a value of 'GetMergeCommit' with the minimum fields required to make a request.
--
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'repositoryName' - The name of the repository that contains the merge commit about which you want to get information.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkGetMergeCommit ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  GetMergeCommit
mkGetMergeCommit
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    GetMergeCommit'
      { conflictDetailLevel = Lude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        conflictResolutionStrategy = Lude.Nothing,
        destinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcfConflictDetailLevel :: Lens.Lens' GetMergeCommit (Lude.Maybe ConflictDetailLevelTypeEnum)
gmcfConflictDetailLevel = Lens.lens (conflictDetailLevel :: GetMergeCommit -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: GetMergeCommit)
{-# DEPRECATED gmcfConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | The name of the repository that contains the merge commit about which you want to get information.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcfRepositoryName :: Lens.Lens' GetMergeCommit Lude.Text
gmcfRepositoryName = Lens.lens (repositoryName :: GetMergeCommit -> Lude.Text) (\s a -> s {repositoryName = a} :: GetMergeCommit)
{-# DEPRECATED gmcfRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcfSourceCommitSpecifier :: Lens.Lens' GetMergeCommit Lude.Text
gmcfSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: GetMergeCommit -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: GetMergeCommit)
{-# DEPRECATED gmcfSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcfConflictResolutionStrategy :: Lens.Lens' GetMergeCommit (Lude.Maybe ConflictResolutionStrategyTypeEnum)
gmcfConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: GetMergeCommit -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: GetMergeCommit)
{-# DEPRECATED gmcfConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcfDestinationCommitSpecifier :: Lens.Lens' GetMergeCommit Lude.Text
gmcfDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: GetMergeCommit -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: GetMergeCommit)
{-# DEPRECATED gmcfDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

instance Lude.AWSRequest GetMergeCommit where
  type Rs GetMergeCommit = GetMergeCommitResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMergeCommitResponse'
            Lude.<$> (x Lude..?> "mergedCommitId")
            Lude.<*> (x Lude..?> "destinationCommitId")
            Lude.<*> (x Lude..?> "baseCommitId")
            Lude.<*> (x Lude..?> "sourceCommitId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMergeCommit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetMergeCommit" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMergeCommit where
  toJSON GetMergeCommit' {..} =
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

instance Lude.ToPath GetMergeCommit where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMergeCommit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMergeCommitResponse' smart constructor.
data GetMergeCommitResponse = GetMergeCommitResponse'
  { -- | The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
    mergedCommitId :: Lude.Maybe Lude.Text,
    -- | The commit ID of the destination commit specifier that was used in the merge evaluation.
    destinationCommitId :: Lude.Maybe Lude.Text,
    -- | The commit ID of the merge base.
    baseCommitId :: Lude.Maybe Lude.Text,
    -- | The commit ID of the source commit specifier that was used in the merge evaluation.
    sourceCommitId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMergeCommitResponse' with the minimum fields required to make a request.
--
-- * 'mergedCommitId' - The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
-- * 'destinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
-- * 'baseCommitId' - The commit ID of the merge base.
-- * 'sourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
-- * 'responseStatus' - The response status code.
mkGetMergeCommitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMergeCommitResponse
mkGetMergeCommitResponse pResponseStatus_ =
  GetMergeCommitResponse'
    { mergedCommitId = Lude.Nothing,
      destinationCommitId = Lude.Nothing,
      baseCommitId = Lude.Nothing,
      sourceCommitId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The commit ID for the merge commit created when the source branch was merged into the destination branch. If the fast-forward merge strategy was used, there is no merge commit.
--
-- /Note:/ Consider using 'mergedCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsMergedCommitId :: Lens.Lens' GetMergeCommitResponse (Lude.Maybe Lude.Text)
grsMergedCommitId = Lens.lens (mergedCommitId :: GetMergeCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {mergedCommitId = a} :: GetMergeCommitResponse)
{-# DEPRECATED grsMergedCommitId "Use generic-lens or generic-optics with 'mergedCommitId' instead." #-}

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsDestinationCommitId :: Lens.Lens' GetMergeCommitResponse (Lude.Maybe Lude.Text)
grsDestinationCommitId = Lens.lens (destinationCommitId :: GetMergeCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {destinationCommitId = a} :: GetMergeCommitResponse)
{-# DEPRECATED grsDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the merge base.
--
-- /Note:/ Consider using 'baseCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsBaseCommitId :: Lens.Lens' GetMergeCommitResponse (Lude.Maybe Lude.Text)
grsBaseCommitId = Lens.lens (baseCommitId :: GetMergeCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {baseCommitId = a} :: GetMergeCommitResponse)
{-# DEPRECATED grsBaseCommitId "Use generic-lens or generic-optics with 'baseCommitId' instead." #-}

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsSourceCommitId :: Lens.Lens' GetMergeCommitResponse (Lude.Maybe Lude.Text)
grsSourceCommitId = Lens.lens (sourceCommitId :: GetMergeCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {sourceCommitId = a} :: GetMergeCommitResponse)
{-# DEPRECATED grsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetMergeCommitResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetMergeCommitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMergeCommitResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
