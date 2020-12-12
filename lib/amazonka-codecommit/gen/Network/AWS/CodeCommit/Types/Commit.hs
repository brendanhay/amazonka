{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Commit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Commit
  ( Commit (..),

    -- * Smart constructor
    mkCommit,

    -- * Lenses
    cCommitId,
    cCommitter,
    cTreeId,
    cAdditionalData,
    cParents,
    cAuthor,
    cMessage,
  )
where

import Network.AWS.CodeCommit.Types.UserInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a specific commit.
--
-- /See:/ 'mkCommit' smart constructor.
data Commit = Commit'
  { commitId :: Lude.Maybe Lude.Text,
    committer :: Lude.Maybe UserInfo,
    treeId :: Lude.Maybe Lude.Text,
    additionalData :: Lude.Maybe Lude.Text,
    parents :: Lude.Maybe [Lude.Text],
    author :: Lude.Maybe UserInfo,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Commit' with the minimum fields required to make a request.
--
-- * 'additionalData' - Any other data associated with the specified commit.
-- * 'author' - Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
-- * 'commitId' - The full SHA ID of the specified commit.
-- * 'committer' - Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git.
--
-- For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
-- * 'message' - The commit message associated with the specified commit.
-- * 'parents' - A list of parent commits for the specified commit. Each parent commit ID is the full commit ID.
-- * 'treeId' - Tree information for the specified commit.
mkCommit ::
  Commit
mkCommit =
  Commit'
    { commitId = Lude.Nothing,
      committer = Lude.Nothing,
      treeId = Lude.Nothing,
      additionalData = Lude.Nothing,
      parents = Lude.Nothing,
      author = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The full SHA ID of the specified commit.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommitId :: Lens.Lens' Commit (Lude.Maybe Lude.Text)
cCommitId = Lens.lens (commitId :: Commit -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: Commit)
{-# DEPRECATED cCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git.
--
-- For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
--
-- /Note:/ Consider using 'committer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommitter :: Lens.Lens' Commit (Lude.Maybe UserInfo)
cCommitter = Lens.lens (committer :: Commit -> Lude.Maybe UserInfo) (\s a -> s {committer = a} :: Commit)
{-# DEPRECATED cCommitter "Use generic-lens or generic-optics with 'committer' instead." #-}

-- | Tree information for the specified commit.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTreeId :: Lens.Lens' Commit (Lude.Maybe Lude.Text)
cTreeId = Lens.lens (treeId :: Commit -> Lude.Maybe Lude.Text) (\s a -> s {treeId = a} :: Commit)
{-# DEPRECATED cTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | Any other data associated with the specified commit.
--
-- /Note:/ Consider using 'additionalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAdditionalData :: Lens.Lens' Commit (Lude.Maybe Lude.Text)
cAdditionalData = Lens.lens (additionalData :: Commit -> Lude.Maybe Lude.Text) (\s a -> s {additionalData = a} :: Commit)
{-# DEPRECATED cAdditionalData "Use generic-lens or generic-optics with 'additionalData' instead." #-}

-- | A list of parent commits for the specified commit. Each parent commit ID is the full commit ID.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParents :: Lens.Lens' Commit (Lude.Maybe [Lude.Text])
cParents = Lens.lens (parents :: Commit -> Lude.Maybe [Lude.Text]) (\s a -> s {parents = a} :: Commit)
{-# DEPRECATED cParents "Use generic-lens or generic-optics with 'parents' instead." #-}

-- | Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAuthor :: Lens.Lens' Commit (Lude.Maybe UserInfo)
cAuthor = Lens.lens (author :: Commit -> Lude.Maybe UserInfo) (\s a -> s {author = a} :: Commit)
{-# DEPRECATED cAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | The commit message associated with the specified commit.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMessage :: Lens.Lens' Commit (Lude.Maybe Lude.Text)
cMessage = Lens.lens (message :: Commit -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Commit)
{-# DEPRECATED cMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON Commit where
  parseJSON =
    Lude.withObject
      "Commit"
      ( \x ->
          Commit'
            Lude.<$> (x Lude..:? "commitId")
            Lude.<*> (x Lude..:? "committer")
            Lude.<*> (x Lude..:? "treeId")
            Lude.<*> (x Lude..:? "additionalData")
            Lude.<*> (x Lude..:? "parents" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "author")
            Lude.<*> (x Lude..:? "message")
      )
