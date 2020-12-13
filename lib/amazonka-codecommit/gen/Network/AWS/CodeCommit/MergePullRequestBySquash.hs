{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergePullRequestBySquash
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the squash merge strategy. If the merge is successful, it closes the pull request.
module Network.AWS.CodeCommit.MergePullRequestBySquash
  ( -- * Creating a request
    MergePullRequestBySquash (..),
    mkMergePullRequestBySquash,

    -- ** Request lenses
    mprbsEmail,
    mprbsAuthorName,
    mprbsPullRequestId,
    mprbsConflictDetailLevel,
    mprbsCommitMessage,
    mprbsRepositoryName,
    mprbsConflictResolution,
    mprbsConflictResolutionStrategy,
    mprbsKeepEmptyFolders,
    mprbsSourceCommitId,

    -- * Destructuring the response
    MergePullRequestBySquashResponse (..),
    mkMergePullRequestBySquashResponse,

    -- ** Response lenses
    mprbsrsPullRequest,
    mprbsrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMergePullRequestBySquash' smart constructor.
data MergePullRequestBySquash = MergePullRequestBySquash'
  { -- | The email address of the person merging the branches. This information is used in the commit information for the merge.
    email :: Lude.Maybe Lude.Text,
    -- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
    authorName :: Lude.Maybe Lude.Text,
    -- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
    pullRequestId :: Lude.Text,
    -- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
    conflictDetailLevel :: Lude.Maybe ConflictDetailLevelTypeEnum,
    -- | The commit message to include in the commit information for the merge.
    commitMessage :: Lude.Maybe Lude.Text,
    -- | The name of the repository where the pull request was created.
    repositoryName :: Lude.Text,
    -- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
    conflictResolution :: Lude.Maybe ConflictResolution,
    -- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
    conflictResolutionStrategy :: Lude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
    keepEmptyFolders :: Lude.Maybe Lude.Bool,
    -- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
    sourceCommitId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergePullRequestBySquash' with the minimum fields required to make a request.
--
-- * 'email' - The email address of the person merging the branches. This information is used in the commit information for the merge.
-- * 'authorName' - The name of the author who created the commit. This information is used as both the author and committer for the commit.
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'commitMessage' - The commit message to include in the commit information for the merge.
-- * 'repositoryName' - The name of the repository where the pull request was created.
-- * 'conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
-- * 'sourceCommitId' - The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
mkMergePullRequestBySquash ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  MergePullRequestBySquash
mkMergePullRequestBySquash pPullRequestId_ pRepositoryName_ =
  MergePullRequestBySquash'
    { email = Lude.Nothing,
      authorName = Lude.Nothing,
      pullRequestId = pPullRequestId_,
      conflictDetailLevel = Lude.Nothing,
      commitMessage = Lude.Nothing,
      repositoryName = pRepositoryName_,
      conflictResolution = Lude.Nothing,
      conflictResolutionStrategy = Lude.Nothing,
      keepEmptyFolders = Lude.Nothing,
      sourceCommitId = Lude.Nothing
    }

-- | The email address of the person merging the branches. This information is used in the commit information for the merge.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsEmail :: Lens.Lens' MergePullRequestBySquash (Lude.Maybe Lude.Text)
mprbsEmail = Lens.lens (email :: MergePullRequestBySquash -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsAuthorName :: Lens.Lens' MergePullRequestBySquash (Lude.Maybe Lude.Text)
mprbsAuthorName = Lens.lens (authorName :: MergePullRequestBySquash -> Lude.Maybe Lude.Text) (\s a -> s {authorName = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsAuthorName "Use generic-lens or generic-optics with 'authorName' instead." #-}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsPullRequestId :: Lens.Lens' MergePullRequestBySquash Lude.Text
mprbsPullRequestId = Lens.lens (pullRequestId :: MergePullRequestBySquash -> Lude.Text) (\s a -> s {pullRequestId = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsConflictDetailLevel :: Lens.Lens' MergePullRequestBySquash (Lude.Maybe ConflictDetailLevelTypeEnum)
mprbsConflictDetailLevel = Lens.lens (conflictDetailLevel :: MergePullRequestBySquash -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | The commit message to include in the commit information for the merge.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsCommitMessage :: Lens.Lens' MergePullRequestBySquash (Lude.Maybe Lude.Text)
mprbsCommitMessage = Lens.lens (commitMessage :: MergePullRequestBySquash -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsRepositoryName :: Lens.Lens' MergePullRequestBySquash Lude.Text
mprbsRepositoryName = Lens.lens (repositoryName :: MergePullRequestBySquash -> Lude.Text) (\s a -> s {repositoryName = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsConflictResolution :: Lens.Lens' MergePullRequestBySquash (Lude.Maybe ConflictResolution)
mprbsConflictResolution = Lens.lens (conflictResolution :: MergePullRequestBySquash -> Lude.Maybe ConflictResolution) (\s a -> s {conflictResolution = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsConflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsConflictResolutionStrategy :: Lens.Lens' MergePullRequestBySquash (Lude.Maybe ConflictResolutionStrategyTypeEnum)
mprbsConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: MergePullRequestBySquash -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsKeepEmptyFolders :: Lens.Lens' MergePullRequestBySquash (Lude.Maybe Lude.Bool)
mprbsKeepEmptyFolders = Lens.lens (keepEmptyFolders :: MergePullRequestBySquash -> Lude.Maybe Lude.Bool) (\s a -> s {keepEmptyFolders = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

-- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsSourceCommitId :: Lens.Lens' MergePullRequestBySquash (Lude.Maybe Lude.Text)
mprbsSourceCommitId = Lens.lens (sourceCommitId :: MergePullRequestBySquash -> Lude.Maybe Lude.Text) (\s a -> s {sourceCommitId = a} :: MergePullRequestBySquash)
{-# DEPRECATED mprbsSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

instance Lude.AWSRequest MergePullRequestBySquash where
  type Rs MergePullRequestBySquash = MergePullRequestBySquashResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          MergePullRequestBySquashResponse'
            Lude.<$> (x Lude..?> "pullRequest") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MergePullRequestBySquash where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.MergePullRequestBySquash" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MergePullRequestBySquash where
  toJSON MergePullRequestBySquash' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("email" Lude..=) Lude.<$> email,
            ("authorName" Lude..=) Lude.<$> authorName,
            Lude.Just ("pullRequestId" Lude..= pullRequestId),
            ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("commitMessage" Lude..=) Lude.<$> commitMessage,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("conflictResolution" Lude..=) Lude.<$> conflictResolution,
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            ("keepEmptyFolders" Lude..=) Lude.<$> keepEmptyFolders,
            ("sourceCommitId" Lude..=) Lude.<$> sourceCommitId
          ]
      )

instance Lude.ToPath MergePullRequestBySquash where
  toPath = Lude.const "/"

instance Lude.ToQuery MergePullRequestBySquash where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMergePullRequestBySquashResponse' smart constructor.
data MergePullRequestBySquashResponse = MergePullRequestBySquashResponse'
  { pullRequest :: Lude.Maybe PullRequest,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergePullRequestBySquashResponse' with the minimum fields required to make a request.
--
-- * 'pullRequest' -
-- * 'responseStatus' - The response status code.
mkMergePullRequestBySquashResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MergePullRequestBySquashResponse
mkMergePullRequestBySquashResponse pResponseStatus_ =
  MergePullRequestBySquashResponse'
    { pullRequest = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsrsPullRequest :: Lens.Lens' MergePullRequestBySquashResponse (Lude.Maybe PullRequest)
mprbsrsPullRequest = Lens.lens (pullRequest :: MergePullRequestBySquashResponse -> Lude.Maybe PullRequest) (\s a -> s {pullRequest = a} :: MergePullRequestBySquashResponse)
{-# DEPRECATED mprbsrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbsrsResponseStatus :: Lens.Lens' MergePullRequestBySquashResponse Lude.Int
mprbsrsResponseStatus = Lens.lens (responseStatus :: MergePullRequestBySquashResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MergePullRequestBySquashResponse)
{-# DEPRECATED mprbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
