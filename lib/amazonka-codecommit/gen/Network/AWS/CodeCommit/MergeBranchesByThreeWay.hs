{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergeBranchesByThreeWay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two specified branches using the three-way merge strategy.
module Network.AWS.CodeCommit.MergeBranchesByThreeWay
  ( -- * Creating a request
    MergeBranchesByThreeWay (..),
    mkMergeBranchesByThreeWay,

    -- ** Request lenses
    mbbtwEmail,
    mbbtwAuthorName,
    mbbtwTargetBranch,
    mbbtwConflictDetailLevel,
    mbbtwCommitMessage,
    mbbtwConflictResolution,
    mbbtwConflictResolutionStrategy,
    mbbtwKeepEmptyFolders,
    mbbtwRepositoryName,
    mbbtwSourceCommitSpecifier,
    mbbtwDestinationCommitSpecifier,

    -- * Destructuring the response
    MergeBranchesByThreeWayResponse (..),
    mkMergeBranchesByThreeWayResponse,

    -- ** Response lenses
    mbbtwrsCommitId,
    mbbtwrsTreeId,
    mbbtwrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMergeBranchesByThreeWay' smart constructor.
data MergeBranchesByThreeWay = MergeBranchesByThreeWay'
  { email ::
      Lude.Maybe Lude.Text,
    authorName :: Lude.Maybe Lude.Text,
    targetBranch :: Lude.Maybe Lude.Text,
    conflictDetailLevel ::
      Lude.Maybe ConflictDetailLevelTypeEnum,
    commitMessage :: Lude.Maybe Lude.Text,
    conflictResolution ::
      Lude.Maybe ConflictResolution,
    conflictResolutionStrategy ::
      Lude.Maybe
        ConflictResolutionStrategyTypeEnum,
    keepEmptyFolders :: Lude.Maybe Lude.Bool,
    repositoryName :: Lude.Text,
    sourceCommitSpecifier :: Lude.Text,
    destinationCommitSpecifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeBranchesByThreeWay' with the minimum fields required to make a request.
--
-- * 'authorName' - The name of the author who created the commit. This information is used as both the author and committer for the commit.
-- * 'commitMessage' - The commit message to include in the commit information for the merge.
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'email' - The email address of the person merging the branches. This information is used in the commit information for the merge.
-- * 'keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
-- * 'repositoryName' - The name of the repository where you want to merge two branches.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'targetBranch' - The branch where the merge is applied.
mkMergeBranchesByThreeWay ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  MergeBranchesByThreeWay
mkMergeBranchesByThreeWay
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    MergeBranchesByThreeWay'
      { email = Lude.Nothing,
        authorName = Lude.Nothing,
        targetBranch = Lude.Nothing,
        conflictDetailLevel = Lude.Nothing,
        commitMessage = Lude.Nothing,
        conflictResolution = Lude.Nothing,
        conflictResolutionStrategy = Lude.Nothing,
        keepEmptyFolders = Lude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        destinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwEmail :: Lens.Lens' MergeBranchesByThreeWay (Lude.Maybe Lude.Text)
mbbtwEmail = Lens.lens (email :: MergeBranchesByThreeWay -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwAuthorName :: Lens.Lens' MergeBranchesByThreeWay (Lude.Maybe Lude.Text)
mbbtwAuthorName = Lens.lens (authorName :: MergeBranchesByThreeWay -> Lude.Maybe Lude.Text) (\s a -> s {authorName = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwAuthorName "Use generic-lens or generic-optics with 'authorName' instead." #-}

-- | The branch where the merge is applied.
--
-- /Note:/ Consider using 'targetBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwTargetBranch :: Lens.Lens' MergeBranchesByThreeWay (Lude.Maybe Lude.Text)
mbbtwTargetBranch = Lens.lens (targetBranch :: MergeBranchesByThreeWay -> Lude.Maybe Lude.Text) (\s a -> s {targetBranch = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwTargetBranch "Use generic-lens or generic-optics with 'targetBranch' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwConflictDetailLevel :: Lens.Lens' MergeBranchesByThreeWay (Lude.Maybe ConflictDetailLevelTypeEnum)
mbbtwConflictDetailLevel = Lens.lens (conflictDetailLevel :: MergeBranchesByThreeWay -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | The commit message to include in the commit information for the merge.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwCommitMessage :: Lens.Lens' MergeBranchesByThreeWay (Lude.Maybe Lude.Text)
mbbtwCommitMessage = Lens.lens (commitMessage :: MergeBranchesByThreeWay -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwConflictResolution :: Lens.Lens' MergeBranchesByThreeWay (Lude.Maybe ConflictResolution)
mbbtwConflictResolution = Lens.lens (conflictResolution :: MergeBranchesByThreeWay -> Lude.Maybe ConflictResolution) (\s a -> s {conflictResolution = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwConflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwConflictResolutionStrategy :: Lens.Lens' MergeBranchesByThreeWay (Lude.Maybe ConflictResolutionStrategyTypeEnum)
mbbtwConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: MergeBranchesByThreeWay -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwKeepEmptyFolders :: Lens.Lens' MergeBranchesByThreeWay (Lude.Maybe Lude.Bool)
mbbtwKeepEmptyFolders = Lens.lens (keepEmptyFolders :: MergeBranchesByThreeWay -> Lude.Maybe Lude.Bool) (\s a -> s {keepEmptyFolders = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

-- | The name of the repository where you want to merge two branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwRepositoryName :: Lens.Lens' MergeBranchesByThreeWay Lude.Text
mbbtwRepositoryName = Lens.lens (repositoryName :: MergeBranchesByThreeWay -> Lude.Text) (\s a -> s {repositoryName = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwSourceCommitSpecifier :: Lens.Lens' MergeBranchesByThreeWay Lude.Text
mbbtwSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: MergeBranchesByThreeWay -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwDestinationCommitSpecifier :: Lens.Lens' MergeBranchesByThreeWay Lude.Text
mbbtwDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: MergeBranchesByThreeWay -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: MergeBranchesByThreeWay)
{-# DEPRECATED mbbtwDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

instance Lude.AWSRequest MergeBranchesByThreeWay where
  type Rs MergeBranchesByThreeWay = MergeBranchesByThreeWayResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          MergeBranchesByThreeWayResponse'
            Lude.<$> (x Lude..?> "commitId")
            Lude.<*> (x Lude..?> "treeId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MergeBranchesByThreeWay where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.MergeBranchesByThreeWay" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MergeBranchesByThreeWay where
  toJSON MergeBranchesByThreeWay' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("email" Lude..=) Lude.<$> email,
            ("authorName" Lude..=) Lude.<$> authorName,
            ("targetBranch" Lude..=) Lude.<$> targetBranch,
            ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("commitMessage" Lude..=) Lude.<$> commitMessage,
            ("conflictResolution" Lude..=) Lude.<$> conflictResolution,
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            ("keepEmptyFolders" Lude..=) Lude.<$> keepEmptyFolders,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier)
          ]
      )

instance Lude.ToPath MergeBranchesByThreeWay where
  toPath = Lude.const "/"

instance Lude.ToQuery MergeBranchesByThreeWay where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMergeBranchesByThreeWayResponse' smart constructor.
data MergeBranchesByThreeWayResponse = MergeBranchesByThreeWayResponse'
  { commitId ::
      Lude.Maybe Lude.Text,
    treeId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeBranchesByThreeWayResponse' with the minimum fields required to make a request.
--
-- * 'commitId' - The commit ID of the merge in the destination or target branch.
-- * 'responseStatus' - The response status code.
-- * 'treeId' - The tree ID of the merge in the destination or target branch.
mkMergeBranchesByThreeWayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MergeBranchesByThreeWayResponse
mkMergeBranchesByThreeWayResponse pResponseStatus_ =
  MergeBranchesByThreeWayResponse'
    { commitId = Lude.Nothing,
      treeId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The commit ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwrsCommitId :: Lens.Lens' MergeBranchesByThreeWayResponse (Lude.Maybe Lude.Text)
mbbtwrsCommitId = Lens.lens (commitId :: MergeBranchesByThreeWayResponse -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: MergeBranchesByThreeWayResponse)
{-# DEPRECATED mbbtwrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The tree ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwrsTreeId :: Lens.Lens' MergeBranchesByThreeWayResponse (Lude.Maybe Lude.Text)
mbbtwrsTreeId = Lens.lens (treeId :: MergeBranchesByThreeWayResponse -> Lude.Maybe Lude.Text) (\s a -> s {treeId = a} :: MergeBranchesByThreeWayResponse)
{-# DEPRECATED mbbtwrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbtwrsResponseStatus :: Lens.Lens' MergeBranchesByThreeWayResponse Lude.Int
mbbtwrsResponseStatus = Lens.lens (responseStatus :: MergeBranchesByThreeWayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MergeBranchesByThreeWayResponse)
{-# DEPRECATED mbbtwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
