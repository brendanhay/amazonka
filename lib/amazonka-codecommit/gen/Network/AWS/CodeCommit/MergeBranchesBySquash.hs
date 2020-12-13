{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergeBranchesBySquash
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two branches using the squash merge strategy.
module Network.AWS.CodeCommit.MergeBranchesBySquash
  ( -- * Creating a request
    MergeBranchesBySquash (..),
    mkMergeBranchesBySquash,

    -- ** Request lenses
    mbbsEmail,
    mbbsAuthorName,
    mbbsTargetBranch,
    mbbsConflictDetailLevel,
    mbbsCommitMessage,
    mbbsRepositoryName,
    mbbsSourceCommitSpecifier,
    mbbsConflictResolution,
    mbbsConflictResolutionStrategy,
    mbbsKeepEmptyFolders,
    mbbsDestinationCommitSpecifier,

    -- * Destructuring the response
    MergeBranchesBySquashResponse (..),
    mkMergeBranchesBySquashResponse,

    -- ** Response lenses
    mbbsrsCommitId,
    mbbsrsTreeId,
    mbbsrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMergeBranchesBySquash' smart constructor.
data MergeBranchesBySquash = MergeBranchesBySquash'
  { -- | The email address of the person merging the branches. This information is used in the commit information for the merge.
    email :: Lude.Maybe Lude.Text,
    -- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
    authorName :: Lude.Maybe Lude.Text,
    -- | The branch where the merge is applied.
    targetBranch :: Lude.Maybe Lude.Text,
    -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Lude.Maybe ConflictDetailLevelTypeEnum,
    -- | The commit message for the merge.
    commitMessage :: Lude.Maybe Lude.Text,
    -- | The name of the repository where you want to merge two branches.
    repositoryName :: Lude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Lude.Text,
    -- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
    conflictResolution :: Lude.Maybe ConflictResolution,
    -- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
    conflictResolutionStrategy :: Lude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
    keepEmptyFolders :: Lude.Maybe Lude.Bool,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeBranchesBySquash' with the minimum fields required to make a request.
--
-- * 'email' - The email address of the person merging the branches. This information is used in the commit information for the merge.
-- * 'authorName' - The name of the author who created the commit. This information is used as both the author and committer for the commit.
-- * 'targetBranch' - The branch where the merge is applied.
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'commitMessage' - The commit message for the merge.
-- * 'repositoryName' - The name of the repository where you want to merge two branches.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkMergeBranchesBySquash ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  MergeBranchesBySquash
mkMergeBranchesBySquash
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    MergeBranchesBySquash'
      { email = Lude.Nothing,
        authorName = Lude.Nothing,
        targetBranch = Lude.Nothing,
        conflictDetailLevel = Lude.Nothing,
        commitMessage = Lude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        conflictResolution = Lude.Nothing,
        conflictResolutionStrategy = Lude.Nothing,
        keepEmptyFolders = Lude.Nothing,
        destinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsEmail :: Lens.Lens' MergeBranchesBySquash (Lude.Maybe Lude.Text)
mbbsEmail = Lens.lens (email :: MergeBranchesBySquash -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsAuthorName :: Lens.Lens' MergeBranchesBySquash (Lude.Maybe Lude.Text)
mbbsAuthorName = Lens.lens (authorName :: MergeBranchesBySquash -> Lude.Maybe Lude.Text) (\s a -> s {authorName = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsAuthorName "Use generic-lens or generic-optics with 'authorName' instead." #-}

-- | The branch where the merge is applied.
--
-- /Note:/ Consider using 'targetBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsTargetBranch :: Lens.Lens' MergeBranchesBySquash (Lude.Maybe Lude.Text)
mbbsTargetBranch = Lens.lens (targetBranch :: MergeBranchesBySquash -> Lude.Maybe Lude.Text) (\s a -> s {targetBranch = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsTargetBranch "Use generic-lens or generic-optics with 'targetBranch' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictDetailLevel :: Lens.Lens' MergeBranchesBySquash (Lude.Maybe ConflictDetailLevelTypeEnum)
mbbsConflictDetailLevel = Lens.lens (conflictDetailLevel :: MergeBranchesBySquash -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | The commit message for the merge.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsCommitMessage :: Lens.Lens' MergeBranchesBySquash (Lude.Maybe Lude.Text)
mbbsCommitMessage = Lens.lens (commitMessage :: MergeBranchesBySquash -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | The name of the repository where you want to merge two branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsRepositoryName :: Lens.Lens' MergeBranchesBySquash Lude.Text
mbbsRepositoryName = Lens.lens (repositoryName :: MergeBranchesBySquash -> Lude.Text) (\s a -> s {repositoryName = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsSourceCommitSpecifier :: Lens.Lens' MergeBranchesBySquash Lude.Text
mbbsSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: MergeBranchesBySquash -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictResolution :: Lens.Lens' MergeBranchesBySquash (Lude.Maybe ConflictResolution)
mbbsConflictResolution = Lens.lens (conflictResolution :: MergeBranchesBySquash -> Lude.Maybe ConflictResolution) (\s a -> s {conflictResolution = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsConflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsConflictResolutionStrategy :: Lens.Lens' MergeBranchesBySquash (Lude.Maybe ConflictResolutionStrategyTypeEnum)
mbbsConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: MergeBranchesBySquash -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsKeepEmptyFolders :: Lens.Lens' MergeBranchesBySquash (Lude.Maybe Lude.Bool)
mbbsKeepEmptyFolders = Lens.lens (keepEmptyFolders :: MergeBranchesBySquash -> Lude.Maybe Lude.Bool) (\s a -> s {keepEmptyFolders = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsDestinationCommitSpecifier :: Lens.Lens' MergeBranchesBySquash Lude.Text
mbbsDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: MergeBranchesBySquash -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: MergeBranchesBySquash)
{-# DEPRECATED mbbsDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

instance Lude.AWSRequest MergeBranchesBySquash where
  type Rs MergeBranchesBySquash = MergeBranchesBySquashResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          MergeBranchesBySquashResponse'
            Lude.<$> (x Lude..?> "commitId")
            Lude.<*> (x Lude..?> "treeId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MergeBranchesBySquash where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.MergeBranchesBySquash" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MergeBranchesBySquash where
  toJSON MergeBranchesBySquash' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("email" Lude..=) Lude.<$> email,
            ("authorName" Lude..=) Lude.<$> authorName,
            ("targetBranch" Lude..=) Lude.<$> targetBranch,
            ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("commitMessage" Lude..=) Lude.<$> commitMessage,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            ("conflictResolution" Lude..=) Lude.<$> conflictResolution,
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            ("keepEmptyFolders" Lude..=) Lude.<$> keepEmptyFolders,
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier)
          ]
      )

instance Lude.ToPath MergeBranchesBySquash where
  toPath = Lude.const "/"

instance Lude.ToQuery MergeBranchesBySquash where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMergeBranchesBySquashResponse' smart constructor.
data MergeBranchesBySquashResponse = MergeBranchesBySquashResponse'
  { -- | The commit ID of the merge in the destination or target branch.
    commitId :: Lude.Maybe Lude.Text,
    -- | The tree ID of the merge in the destination or target branch.
    treeId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeBranchesBySquashResponse' with the minimum fields required to make a request.
--
-- * 'commitId' - The commit ID of the merge in the destination or target branch.
-- * 'treeId' - The tree ID of the merge in the destination or target branch.
-- * 'responseStatus' - The response status code.
mkMergeBranchesBySquashResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MergeBranchesBySquashResponse
mkMergeBranchesBySquashResponse pResponseStatus_ =
  MergeBranchesBySquashResponse'
    { commitId = Lude.Nothing,
      treeId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The commit ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrsCommitId :: Lens.Lens' MergeBranchesBySquashResponse (Lude.Maybe Lude.Text)
mbbsrsCommitId = Lens.lens (commitId :: MergeBranchesBySquashResponse -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: MergeBranchesBySquashResponse)
{-# DEPRECATED mbbsrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The tree ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrsTreeId :: Lens.Lens' MergeBranchesBySquashResponse (Lude.Maybe Lude.Text)
mbbsrsTreeId = Lens.lens (treeId :: MergeBranchesBySquashResponse -> Lude.Maybe Lude.Text) (\s a -> s {treeId = a} :: MergeBranchesBySquashResponse)
{-# DEPRECATED mbbsrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbsrsResponseStatus :: Lens.Lens' MergeBranchesBySquashResponse Lude.Int
mbbsrsResponseStatus = Lens.lens (responseStatus :: MergeBranchesBySquashResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MergeBranchesBySquashResponse)
{-# DEPRECATED mbbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
