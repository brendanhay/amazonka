{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Commit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Commit where

import Network.AWS.CodeCommit.Types.UserInfo
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a specific commit.
--
-- /See:/ 'newCommit' smart constructor.
data Commit = Commit'
  { -- | A list of parent commits for the specified commit. Each parent commit ID
    -- is the full commit ID.
    parents :: Core.Maybe [Core.Text],
    -- | The full SHA ID of the specified commit.
    commitId :: Core.Maybe Core.Text,
    -- | Any other data associated with the specified commit.
    additionalData :: Core.Maybe Core.Text,
    -- | The commit message associated with the specified commit.
    message :: Core.Maybe Core.Text,
    -- | Tree information for the specified commit.
    treeId :: Core.Maybe Core.Text,
    -- | Information about the author of the specified commit. Information
    -- includes the date in timestamp format with GMT offset, the name of the
    -- author, and the email address for the author, as configured in Git.
    author :: Core.Maybe UserInfo,
    -- | Information about the person who committed the specified commit, also
    -- known as the committer. Information includes the date in timestamp
    -- format with GMT offset, the name of the committer, and the email address
    -- for the committer, as configured in Git.
    --
    -- For more information about the difference between an author and a
    -- committer in Git, see
    -- <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro
    -- Git by Scott Chacon and Ben Straub.
    committer :: Core.Maybe UserInfo
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Commit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parents', 'commit_parents' - A list of parent commits for the specified commit. Each parent commit ID
-- is the full commit ID.
--
-- 'commitId', 'commit_commitId' - The full SHA ID of the specified commit.
--
-- 'additionalData', 'commit_additionalData' - Any other data associated with the specified commit.
--
-- 'message', 'commit_message' - The commit message associated with the specified commit.
--
-- 'treeId', 'commit_treeId' - Tree information for the specified commit.
--
-- 'author', 'commit_author' - Information about the author of the specified commit. Information
-- includes the date in timestamp format with GMT offset, the name of the
-- author, and the email address for the author, as configured in Git.
--
-- 'committer', 'commit_committer' - Information about the person who committed the specified commit, also
-- known as the committer. Information includes the date in timestamp
-- format with GMT offset, the name of the committer, and the email address
-- for the committer, as configured in Git.
--
-- For more information about the difference between an author and a
-- committer in Git, see
-- <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro
-- Git by Scott Chacon and Ben Straub.
newCommit ::
  Commit
newCommit =
  Commit'
    { parents = Core.Nothing,
      commitId = Core.Nothing,
      additionalData = Core.Nothing,
      message = Core.Nothing,
      treeId = Core.Nothing,
      author = Core.Nothing,
      committer = Core.Nothing
    }

-- | A list of parent commits for the specified commit. Each parent commit ID
-- is the full commit ID.
commit_parents :: Lens.Lens' Commit (Core.Maybe [Core.Text])
commit_parents = Lens.lens (\Commit' {parents} -> parents) (\s@Commit' {} a -> s {parents = a} :: Commit) Core.. Lens.mapping Lens._Coerce

-- | The full SHA ID of the specified commit.
commit_commitId :: Lens.Lens' Commit (Core.Maybe Core.Text)
commit_commitId = Lens.lens (\Commit' {commitId} -> commitId) (\s@Commit' {} a -> s {commitId = a} :: Commit)

-- | Any other data associated with the specified commit.
commit_additionalData :: Lens.Lens' Commit (Core.Maybe Core.Text)
commit_additionalData = Lens.lens (\Commit' {additionalData} -> additionalData) (\s@Commit' {} a -> s {additionalData = a} :: Commit)

-- | The commit message associated with the specified commit.
commit_message :: Lens.Lens' Commit (Core.Maybe Core.Text)
commit_message = Lens.lens (\Commit' {message} -> message) (\s@Commit' {} a -> s {message = a} :: Commit)

-- | Tree information for the specified commit.
commit_treeId :: Lens.Lens' Commit (Core.Maybe Core.Text)
commit_treeId = Lens.lens (\Commit' {treeId} -> treeId) (\s@Commit' {} a -> s {treeId = a} :: Commit)

-- | Information about the author of the specified commit. Information
-- includes the date in timestamp format with GMT offset, the name of the
-- author, and the email address for the author, as configured in Git.
commit_author :: Lens.Lens' Commit (Core.Maybe UserInfo)
commit_author = Lens.lens (\Commit' {author} -> author) (\s@Commit' {} a -> s {author = a} :: Commit)

-- | Information about the person who committed the specified commit, also
-- known as the committer. Information includes the date in timestamp
-- format with GMT offset, the name of the committer, and the email address
-- for the committer, as configured in Git.
--
-- For more information about the difference between an author and a
-- committer in Git, see
-- <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro
-- Git by Scott Chacon and Ben Straub.
commit_committer :: Lens.Lens' Commit (Core.Maybe UserInfo)
commit_committer = Lens.lens (\Commit' {committer} -> committer) (\s@Commit' {} a -> s {committer = a} :: Commit)

instance Core.FromJSON Commit where
  parseJSON =
    Core.withObject
      "Commit"
      ( \x ->
          Commit'
            Core.<$> (x Core..:? "parents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "commitId")
            Core.<*> (x Core..:? "additionalData")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "treeId")
            Core.<*> (x Core..:? "author")
            Core.<*> (x Core..:? "committer")
      )

instance Core.Hashable Commit

instance Core.NFData Commit
