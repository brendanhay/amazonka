{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergePullRequestByThreeWay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the three-way merge strategy. If the merge is successful, it closes the pull request.
module Network.AWS.CodeCommit.MergePullRequestByThreeWay
  ( -- * Creating a request
    MergePullRequestByThreeWay (..),
    mkMergePullRequestByThreeWay,

    -- ** Request lenses
    mprbtwEmail,
    mprbtwAuthorName,
    mprbtwConflictDetailLevel,
    mprbtwCommitMessage,
    mprbtwConflictResolution,
    mprbtwConflictResolutionStrategy,
    mprbtwKeepEmptyFolders,
    mprbtwSourceCommitId,
    mprbtwPullRequestId,
    mprbtwRepositoryName,

    -- * Destructuring the response
    MergePullRequestByThreeWayResponse (..),
    mkMergePullRequestByThreeWayResponse,

    -- ** Response lenses
    mprbtwrsPullRequest,
    mprbtwrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMergePullRequestByThreeWay' smart constructor.
data MergePullRequestByThreeWay = MergePullRequestByThreeWay'
  { email ::
      Lude.Maybe Lude.Text,
    authorName :: Lude.Maybe Lude.Text,
    conflictDetailLevel ::
      Lude.Maybe
        ConflictDetailLevelTypeEnum,
    commitMessage :: Lude.Maybe Lude.Text,
    conflictResolution ::
      Lude.Maybe ConflictResolution,
    conflictResolutionStrategy ::
      Lude.Maybe
        ConflictResolutionStrategyTypeEnum,
    keepEmptyFolders ::
      Lude.Maybe Lude.Bool,
    sourceCommitId ::
      Lude.Maybe Lude.Text,
    pullRequestId :: Lude.Text,
    repositoryName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergePullRequestByThreeWay' with the minimum fields required to make a request.
--
-- * 'authorName' - The name of the author who created the commit. This information is used as both the author and committer for the commit.
-- * 'commitMessage' - The commit message to include in the commit information for the merge.
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'email' - The email address of the person merging the branches. This information is used in the commit information for the merge.
-- * 'keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
-- * 'repositoryName' - The name of the repository where the pull request was created.
-- * 'sourceCommitId' - The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
mkMergePullRequestByThreeWay ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  MergePullRequestByThreeWay
mkMergePullRequestByThreeWay pPullRequestId_ pRepositoryName_ =
  MergePullRequestByThreeWay'
    { email = Lude.Nothing,
      authorName = Lude.Nothing,
      conflictDetailLevel = Lude.Nothing,
      commitMessage = Lude.Nothing,
      conflictResolution = Lude.Nothing,
      conflictResolutionStrategy = Lude.Nothing,
      keepEmptyFolders = Lude.Nothing,
      sourceCommitId = Lude.Nothing,
      pullRequestId = pPullRequestId_,
      repositoryName = pRepositoryName_
    }

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwEmail :: Lens.Lens' MergePullRequestByThreeWay (Lude.Maybe Lude.Text)
mprbtwEmail = Lens.lens (email :: MergePullRequestByThreeWay -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwAuthorName :: Lens.Lens' MergePullRequestByThreeWay (Lude.Maybe Lude.Text)
mprbtwAuthorName = Lens.lens (authorName :: MergePullRequestByThreeWay -> Lude.Maybe Lude.Text) (\s a -> s {authorName = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwAuthorName "Use generic-lens or generic-optics with 'authorName' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwConflictDetailLevel :: Lens.Lens' MergePullRequestByThreeWay (Lude.Maybe ConflictDetailLevelTypeEnum)
mprbtwConflictDetailLevel = Lens.lens (conflictDetailLevel :: MergePullRequestByThreeWay -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | The commit message to include in the commit information for the merge.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwCommitMessage :: Lens.Lens' MergePullRequestByThreeWay (Lude.Maybe Lude.Text)
mprbtwCommitMessage = Lens.lens (commitMessage :: MergePullRequestByThreeWay -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwConflictResolution :: Lens.Lens' MergePullRequestByThreeWay (Lude.Maybe ConflictResolution)
mprbtwConflictResolution = Lens.lens (conflictResolution :: MergePullRequestByThreeWay -> Lude.Maybe ConflictResolution) (\s a -> s {conflictResolution = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwConflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwConflictResolutionStrategy :: Lens.Lens' MergePullRequestByThreeWay (Lude.Maybe ConflictResolutionStrategyTypeEnum)
mprbtwConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: MergePullRequestByThreeWay -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwKeepEmptyFolders :: Lens.Lens' MergePullRequestByThreeWay (Lude.Maybe Lude.Bool)
mprbtwKeepEmptyFolders = Lens.lens (keepEmptyFolders :: MergePullRequestByThreeWay -> Lude.Maybe Lude.Bool) (\s a -> s {keepEmptyFolders = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

-- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwSourceCommitId :: Lens.Lens' MergePullRequestByThreeWay (Lude.Maybe Lude.Text)
mprbtwSourceCommitId = Lens.lens (sourceCommitId :: MergePullRequestByThreeWay -> Lude.Maybe Lude.Text) (\s a -> s {sourceCommitId = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwPullRequestId :: Lens.Lens' MergePullRequestByThreeWay Lude.Text
mprbtwPullRequestId = Lens.lens (pullRequestId :: MergePullRequestByThreeWay -> Lude.Text) (\s a -> s {pullRequestId = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwRepositoryName :: Lens.Lens' MergePullRequestByThreeWay Lude.Text
mprbtwRepositoryName = Lens.lens (repositoryName :: MergePullRequestByThreeWay -> Lude.Text) (\s a -> s {repositoryName = a} :: MergePullRequestByThreeWay)
{-# DEPRECATED mprbtwRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest MergePullRequestByThreeWay where
  type
    Rs MergePullRequestByThreeWay =
      MergePullRequestByThreeWayResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          MergePullRequestByThreeWayResponse'
            Lude.<$> (x Lude..?> "pullRequest") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MergePullRequestByThreeWay where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.MergePullRequestByThreeWay" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MergePullRequestByThreeWay where
  toJSON MergePullRequestByThreeWay' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("email" Lude..=) Lude.<$> email,
            ("authorName" Lude..=) Lude.<$> authorName,
            ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("commitMessage" Lude..=) Lude.<$> commitMessage,
            ("conflictResolution" Lude..=) Lude.<$> conflictResolution,
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            ("keepEmptyFolders" Lude..=) Lude.<$> keepEmptyFolders,
            ("sourceCommitId" Lude..=) Lude.<$> sourceCommitId,
            Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath MergePullRequestByThreeWay where
  toPath = Lude.const "/"

instance Lude.ToQuery MergePullRequestByThreeWay where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMergePullRequestByThreeWayResponse' smart constructor.
data MergePullRequestByThreeWayResponse = MergePullRequestByThreeWayResponse'
  { pullRequest ::
      Lude.Maybe
        PullRequest,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergePullRequestByThreeWayResponse' with the minimum fields required to make a request.
--
-- * 'pullRequest' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkMergePullRequestByThreeWayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MergePullRequestByThreeWayResponse
mkMergePullRequestByThreeWayResponse pResponseStatus_ =
  MergePullRequestByThreeWayResponse'
    { pullRequest = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwrsPullRequest :: Lens.Lens' MergePullRequestByThreeWayResponse (Lude.Maybe PullRequest)
mprbtwrsPullRequest = Lens.lens (pullRequest :: MergePullRequestByThreeWayResponse -> Lude.Maybe PullRequest) (\s a -> s {pullRequest = a} :: MergePullRequestByThreeWayResponse)
{-# DEPRECATED mprbtwrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbtwrsResponseStatus :: Lens.Lens' MergePullRequestByThreeWayResponse Lude.Int
mprbtwrsResponseStatus = Lens.lens (responseStatus :: MergePullRequestByThreeWayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MergePullRequestByThreeWayResponse)
{-# DEPRECATED mprbtwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
