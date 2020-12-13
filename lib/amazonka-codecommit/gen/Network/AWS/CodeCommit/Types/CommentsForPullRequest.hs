{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.CommentsForPullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.CommentsForPullRequest
  ( CommentsForPullRequest (..),

    -- * Smart constructor
    mkCommentsForPullRequest,

    -- * Lenses
    cfprBeforeBlobId,
    cfprLocation,
    cfprAfterCommitId,
    cfprPullRequestId,
    cfprAfterBlobId,
    cfprBeforeCommitId,
    cfprRepositoryName,
    cfprComments,
  )
where

import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about comments on a pull request.
--
-- /See:/ 'mkCommentsForPullRequest' smart constructor.
data CommentsForPullRequest = CommentsForPullRequest'
  { -- | The full blob ID of the file on which you want to comment on the destination commit.
    beforeBlobId :: Lude.Maybe Lude.Text,
    -- | Location information about the comment on the pull request, including the file name, line number, and whether the version of the file where the comment was made is BEFORE (destination branch) or AFTER (source branch).
    location :: Lude.Maybe Location,
    -- | The full commit ID of the commit that was the tip of the source branch at the time the comment was made.
    afterCommitId :: Lude.Maybe Lude.Text,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Lude.Maybe Lude.Text,
    -- | The full blob ID of the file on which you want to comment on the source commit.
    afterBlobId :: Lude.Maybe Lude.Text,
    -- | The full commit ID of the commit that was the tip of the destination branch when the pull request was created. This commit is superceded by the after commit in the source branch when and if you merge the source branch into the destination branch.
    beforeCommitId :: Lude.Maybe Lude.Text,
    -- | The name of the repository that contains the pull request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | An array of comment objects. Each comment object contains information about a comment on the pull request.
    comments :: Lude.Maybe [Comment]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CommentsForPullRequest' with the minimum fields required to make a request.
--
-- * 'beforeBlobId' - The full blob ID of the file on which you want to comment on the destination commit.
-- * 'location' - Location information about the comment on the pull request, including the file name, line number, and whether the version of the file where the comment was made is BEFORE (destination branch) or AFTER (source branch).
-- * 'afterCommitId' - The full commit ID of the commit that was the tip of the source branch at the time the comment was made.
-- * 'pullRequestId' - The system-generated ID of the pull request.
-- * 'afterBlobId' - The full blob ID of the file on which you want to comment on the source commit.
-- * 'beforeCommitId' - The full commit ID of the commit that was the tip of the destination branch when the pull request was created. This commit is superceded by the after commit in the source branch when and if you merge the source branch into the destination branch.
-- * 'repositoryName' - The name of the repository that contains the pull request.
-- * 'comments' - An array of comment objects. Each comment object contains information about a comment on the pull request.
mkCommentsForPullRequest ::
  CommentsForPullRequest
mkCommentsForPullRequest =
  CommentsForPullRequest'
    { beforeBlobId = Lude.Nothing,
      location = Lude.Nothing,
      afterCommitId = Lude.Nothing,
      pullRequestId = Lude.Nothing,
      afterBlobId = Lude.Nothing,
      beforeCommitId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      comments = Lude.Nothing
    }

-- | The full blob ID of the file on which you want to comment on the destination commit.
--
-- /Note:/ Consider using 'beforeBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprBeforeBlobId :: Lens.Lens' CommentsForPullRequest (Lude.Maybe Lude.Text)
cfprBeforeBlobId = Lens.lens (beforeBlobId :: CommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {beforeBlobId = a} :: CommentsForPullRequest)
{-# DEPRECATED cfprBeforeBlobId "Use generic-lens or generic-optics with 'beforeBlobId' instead." #-}

-- | Location information about the comment on the pull request, including the file name, line number, and whether the version of the file where the comment was made is BEFORE (destination branch) or AFTER (source branch).
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprLocation :: Lens.Lens' CommentsForPullRequest (Lude.Maybe Location)
cfprLocation = Lens.lens (location :: CommentsForPullRequest -> Lude.Maybe Location) (\s a -> s {location = a} :: CommentsForPullRequest)
{-# DEPRECATED cfprLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The full commit ID of the commit that was the tip of the source branch at the time the comment was made.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprAfterCommitId :: Lens.Lens' CommentsForPullRequest (Lude.Maybe Lude.Text)
cfprAfterCommitId = Lens.lens (afterCommitId :: CommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {afterCommitId = a} :: CommentsForPullRequest)
{-# DEPRECATED cfprAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprPullRequestId :: Lens.Lens' CommentsForPullRequest (Lude.Maybe Lude.Text)
cfprPullRequestId = Lens.lens (pullRequestId :: CommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {pullRequestId = a} :: CommentsForPullRequest)
{-# DEPRECATED cfprPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The full blob ID of the file on which you want to comment on the source commit.
--
-- /Note:/ Consider using 'afterBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprAfterBlobId :: Lens.Lens' CommentsForPullRequest (Lude.Maybe Lude.Text)
cfprAfterBlobId = Lens.lens (afterBlobId :: CommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {afterBlobId = a} :: CommentsForPullRequest)
{-# DEPRECATED cfprAfterBlobId "Use generic-lens or generic-optics with 'afterBlobId' instead." #-}

-- | The full commit ID of the commit that was the tip of the destination branch when the pull request was created. This commit is superceded by the after commit in the source branch when and if you merge the source branch into the destination branch.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprBeforeCommitId :: Lens.Lens' CommentsForPullRequest (Lude.Maybe Lude.Text)
cfprBeforeCommitId = Lens.lens (beforeCommitId :: CommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitId = a} :: CommentsForPullRequest)
{-# DEPRECATED cfprBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The name of the repository that contains the pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprRepositoryName :: Lens.Lens' CommentsForPullRequest (Lude.Maybe Lude.Text)
cfprRepositoryName = Lens.lens (repositoryName :: CommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: CommentsForPullRequest)
{-# DEPRECATED cfprRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | An array of comment objects. Each comment object contains information about a comment on the pull request.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprComments :: Lens.Lens' CommentsForPullRequest (Lude.Maybe [Comment])
cfprComments = Lens.lens (comments :: CommentsForPullRequest -> Lude.Maybe [Comment]) (\s a -> s {comments = a} :: CommentsForPullRequest)
{-# DEPRECATED cfprComments "Use generic-lens or generic-optics with 'comments' instead." #-}

instance Lude.FromJSON CommentsForPullRequest where
  parseJSON =
    Lude.withObject
      "CommentsForPullRequest"
      ( \x ->
          CommentsForPullRequest'
            Lude.<$> (x Lude..:? "beforeBlobId")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "afterCommitId")
            Lude.<*> (x Lude..:? "pullRequestId")
            Lude.<*> (x Lude..:? "afterBlobId")
            Lude.<*> (x Lude..:? "beforeCommitId")
            Lude.<*> (x Lude..:? "repositoryName")
            Lude.<*> (x Lude..:? "comments" Lude..!= Lude.mempty)
      )
