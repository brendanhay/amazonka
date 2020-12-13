{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.CommentsForComparedCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.CommentsForComparedCommit
  ( CommentsForComparedCommit (..),

    -- * Smart constructor
    mkCommentsForComparedCommit,

    -- * Lenses
    cfccBeforeBlobId,
    cfccLocation,
    cfccAfterCommitId,
    cfccAfterBlobId,
    cfccBeforeCommitId,
    cfccRepositoryName,
    cfccComments,
  )
where

import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about comments on the comparison between two commits.
--
-- /See:/ 'mkCommentsForComparedCommit' smart constructor.
data CommentsForComparedCommit = CommentsForComparedCommit'
  { -- | The full blob ID of the commit used to establish the before of the comparison.
    beforeBlobId :: Lude.Maybe Lude.Text,
    -- | Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is BEFORE or AFTER.
    location :: Lude.Maybe Location,
    -- | The full commit ID of the commit used to establish the after of the comparison.
    afterCommitId :: Lude.Maybe Lude.Text,
    -- | The full blob ID of the commit used to establish the after of the comparison.
    afterBlobId :: Lude.Maybe Lude.Text,
    -- | The full commit ID of the commit used to establish the before of the comparison.
    beforeCommitId :: Lude.Maybe Lude.Text,
    -- | The name of the repository that contains the compared commits.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
    comments :: Lude.Maybe [Comment]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CommentsForComparedCommit' with the minimum fields required to make a request.
--
-- * 'beforeBlobId' - The full blob ID of the commit used to establish the before of the comparison.
-- * 'location' - Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is BEFORE or AFTER.
-- * 'afterCommitId' - The full commit ID of the commit used to establish the after of the comparison.
-- * 'afterBlobId' - The full blob ID of the commit used to establish the after of the comparison.
-- * 'beforeCommitId' - The full commit ID of the commit used to establish the before of the comparison.
-- * 'repositoryName' - The name of the repository that contains the compared commits.
-- * 'comments' - An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
mkCommentsForComparedCommit ::
  CommentsForComparedCommit
mkCommentsForComparedCommit =
  CommentsForComparedCommit'
    { beforeBlobId = Lude.Nothing,
      location = Lude.Nothing,
      afterCommitId = Lude.Nothing,
      afterBlobId = Lude.Nothing,
      beforeCommitId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      comments = Lude.Nothing
    }

-- | The full blob ID of the commit used to establish the before of the comparison.
--
-- /Note:/ Consider using 'beforeBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccBeforeBlobId :: Lens.Lens' CommentsForComparedCommit (Lude.Maybe Lude.Text)
cfccBeforeBlobId = Lens.lens (beforeBlobId :: CommentsForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {beforeBlobId = a} :: CommentsForComparedCommit)
{-# DEPRECATED cfccBeforeBlobId "Use generic-lens or generic-optics with 'beforeBlobId' instead." #-}

-- | Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is BEFORE or AFTER.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccLocation :: Lens.Lens' CommentsForComparedCommit (Lude.Maybe Location)
cfccLocation = Lens.lens (location :: CommentsForComparedCommit -> Lude.Maybe Location) (\s a -> s {location = a} :: CommentsForComparedCommit)
{-# DEPRECATED cfccLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The full commit ID of the commit used to establish the after of the comparison.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccAfterCommitId :: Lens.Lens' CommentsForComparedCommit (Lude.Maybe Lude.Text)
cfccAfterCommitId = Lens.lens (afterCommitId :: CommentsForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {afterCommitId = a} :: CommentsForComparedCommit)
{-# DEPRECATED cfccAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | The full blob ID of the commit used to establish the after of the comparison.
--
-- /Note:/ Consider using 'afterBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccAfterBlobId :: Lens.Lens' CommentsForComparedCommit (Lude.Maybe Lude.Text)
cfccAfterBlobId = Lens.lens (afterBlobId :: CommentsForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {afterBlobId = a} :: CommentsForComparedCommit)
{-# DEPRECATED cfccAfterBlobId "Use generic-lens or generic-optics with 'afterBlobId' instead." #-}

-- | The full commit ID of the commit used to establish the before of the comparison.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccBeforeCommitId :: Lens.Lens' CommentsForComparedCommit (Lude.Maybe Lude.Text)
cfccBeforeCommitId = Lens.lens (beforeCommitId :: CommentsForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitId = a} :: CommentsForComparedCommit)
{-# DEPRECATED cfccBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The name of the repository that contains the compared commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccRepositoryName :: Lens.Lens' CommentsForComparedCommit (Lude.Maybe Lude.Text)
cfccRepositoryName = Lens.lens (repositoryName :: CommentsForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: CommentsForComparedCommit)
{-# DEPRECATED cfccRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccComments :: Lens.Lens' CommentsForComparedCommit (Lude.Maybe [Comment])
cfccComments = Lens.lens (comments :: CommentsForComparedCommit -> Lude.Maybe [Comment]) (\s a -> s {comments = a} :: CommentsForComparedCommit)
{-# DEPRECATED cfccComments "Use generic-lens or generic-optics with 'comments' instead." #-}

instance Lude.FromJSON CommentsForComparedCommit where
  parseJSON =
    Lude.withObject
      "CommentsForComparedCommit"
      ( \x ->
          CommentsForComparedCommit'
            Lude.<$> (x Lude..:? "beforeBlobId")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "afterCommitId")
            Lude.<*> (x Lude..:? "afterBlobId")
            Lude.<*> (x Lude..:? "beforeCommitId")
            Lude.<*> (x Lude..:? "repositoryName")
            Lude.<*> (x Lude..:? "comments" Lude..!= Lude.mempty)
      )
