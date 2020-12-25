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
    cAdditionalData,
    cAuthor,
    cCommitId,
    cCommitter,
    cMessage,
    cParents,
    cTreeId,
  )
where

import qualified Network.AWS.CodeCommit.Types.AdditionalData as Types
import qualified Network.AWS.CodeCommit.Types.Message as Types
import qualified Network.AWS.CodeCommit.Types.ObjectId as Types
import qualified Network.AWS.CodeCommit.Types.UserInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a specific commit.
--
-- /See:/ 'mkCommit' smart constructor.
data Commit = Commit'
  { -- | Any other data associated with the specified commit.
    additionalData :: Core.Maybe Types.AdditionalData,
    -- | Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
    author :: Core.Maybe Types.UserInfo,
    -- | The full SHA ID of the specified commit.
    commitId :: Core.Maybe Types.ObjectId,
    -- | Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git.
    --
    -- For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
    committer :: Core.Maybe Types.UserInfo,
    -- | The commit message associated with the specified commit.
    message :: Core.Maybe Types.Message,
    -- | A list of parent commits for the specified commit. Each parent commit ID is the full commit ID.
    parents :: Core.Maybe [Types.ObjectId],
    -- | Tree information for the specified commit.
    treeId :: Core.Maybe Types.ObjectId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Commit' value with any optional fields omitted.
mkCommit ::
  Commit
mkCommit =
  Commit'
    { additionalData = Core.Nothing,
      author = Core.Nothing,
      commitId = Core.Nothing,
      committer = Core.Nothing,
      message = Core.Nothing,
      parents = Core.Nothing,
      treeId = Core.Nothing
    }

-- | Any other data associated with the specified commit.
--
-- /Note:/ Consider using 'additionalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAdditionalData :: Lens.Lens' Commit (Core.Maybe Types.AdditionalData)
cAdditionalData = Lens.field @"additionalData"
{-# DEPRECATED cAdditionalData "Use generic-lens or generic-optics with 'additionalData' instead." #-}

-- | Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAuthor :: Lens.Lens' Commit (Core.Maybe Types.UserInfo)
cAuthor = Lens.field @"author"
{-# DEPRECATED cAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | The full SHA ID of the specified commit.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommitId :: Lens.Lens' Commit (Core.Maybe Types.ObjectId)
cCommitId = Lens.field @"commitId"
{-# DEPRECATED cCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git.
--
-- For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
--
-- /Note:/ Consider using 'committer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommitter :: Lens.Lens' Commit (Core.Maybe Types.UserInfo)
cCommitter = Lens.field @"committer"
{-# DEPRECATED cCommitter "Use generic-lens or generic-optics with 'committer' instead." #-}

-- | The commit message associated with the specified commit.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMessage :: Lens.Lens' Commit (Core.Maybe Types.Message)
cMessage = Lens.field @"message"
{-# DEPRECATED cMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A list of parent commits for the specified commit. Each parent commit ID is the full commit ID.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParents :: Lens.Lens' Commit (Core.Maybe [Types.ObjectId])
cParents = Lens.field @"parents"
{-# DEPRECATED cParents "Use generic-lens or generic-optics with 'parents' instead." #-}

-- | Tree information for the specified commit.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTreeId :: Lens.Lens' Commit (Core.Maybe Types.ObjectId)
cTreeId = Lens.field @"treeId"
{-# DEPRECATED cTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

instance Core.FromJSON Commit where
  parseJSON =
    Core.withObject "Commit" Core.$
      \x ->
        Commit'
          Core.<$> (x Core..:? "additionalData")
          Core.<*> (x Core..:? "author")
          Core.<*> (x Core..:? "commitId")
          Core.<*> (x Core..:? "committer")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "parents")
          Core.<*> (x Core..:? "treeId")
