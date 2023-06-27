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
-- Module      : Amazonka.CodeCommit.Types.Commit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Commit where

import Amazonka.CodeCommit.Types.UserInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a specific commit.
--
-- /See:/ 'newCommit' smart constructor.
data Commit = Commit'
  { -- | Any other data associated with the specified commit.
    additionalData :: Prelude.Maybe Prelude.Text,
    -- | Information about the author of the specified commit. Information
    -- includes the date in timestamp format with GMT offset, the name of the
    -- author, and the email address for the author, as configured in Git.
    author :: Prelude.Maybe UserInfo,
    -- | The full SHA ID of the specified commit.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | Information about the person who committed the specified commit, also
    -- known as the committer. Information includes the date in timestamp
    -- format with GMT offset, the name of the committer, and the email address
    -- for the committer, as configured in Git.
    --
    -- For more information about the difference between an author and a
    -- committer in Git, see
    -- <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro
    -- Git by Scott Chacon and Ben Straub.
    committer :: Prelude.Maybe UserInfo,
    -- | The commit message associated with the specified commit.
    message :: Prelude.Maybe Prelude.Text,
    -- | A list of parent commits for the specified commit. Each parent commit ID
    -- is the full commit ID.
    parents :: Prelude.Maybe [Prelude.Text],
    -- | Tree information for the specified commit.
    treeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Commit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalData', 'commit_additionalData' - Any other data associated with the specified commit.
--
-- 'author', 'commit_author' - Information about the author of the specified commit. Information
-- includes the date in timestamp format with GMT offset, the name of the
-- author, and the email address for the author, as configured in Git.
--
-- 'commitId', 'commit_commitId' - The full SHA ID of the specified commit.
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
--
-- 'message', 'commit_message' - The commit message associated with the specified commit.
--
-- 'parents', 'commit_parents' - A list of parent commits for the specified commit. Each parent commit ID
-- is the full commit ID.
--
-- 'treeId', 'commit_treeId' - Tree information for the specified commit.
newCommit ::
  Commit
newCommit =
  Commit'
    { additionalData = Prelude.Nothing,
      author = Prelude.Nothing,
      commitId = Prelude.Nothing,
      committer = Prelude.Nothing,
      message = Prelude.Nothing,
      parents = Prelude.Nothing,
      treeId = Prelude.Nothing
    }

-- | Any other data associated with the specified commit.
commit_additionalData :: Lens.Lens' Commit (Prelude.Maybe Prelude.Text)
commit_additionalData = Lens.lens (\Commit' {additionalData} -> additionalData) (\s@Commit' {} a -> s {additionalData = a} :: Commit)

-- | Information about the author of the specified commit. Information
-- includes the date in timestamp format with GMT offset, the name of the
-- author, and the email address for the author, as configured in Git.
commit_author :: Lens.Lens' Commit (Prelude.Maybe UserInfo)
commit_author = Lens.lens (\Commit' {author} -> author) (\s@Commit' {} a -> s {author = a} :: Commit)

-- | The full SHA ID of the specified commit.
commit_commitId :: Lens.Lens' Commit (Prelude.Maybe Prelude.Text)
commit_commitId = Lens.lens (\Commit' {commitId} -> commitId) (\s@Commit' {} a -> s {commitId = a} :: Commit)

-- | Information about the person who committed the specified commit, also
-- known as the committer. Information includes the date in timestamp
-- format with GMT offset, the name of the committer, and the email address
-- for the committer, as configured in Git.
--
-- For more information about the difference between an author and a
-- committer in Git, see
-- <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro
-- Git by Scott Chacon and Ben Straub.
commit_committer :: Lens.Lens' Commit (Prelude.Maybe UserInfo)
commit_committer = Lens.lens (\Commit' {committer} -> committer) (\s@Commit' {} a -> s {committer = a} :: Commit)

-- | The commit message associated with the specified commit.
commit_message :: Lens.Lens' Commit (Prelude.Maybe Prelude.Text)
commit_message = Lens.lens (\Commit' {message} -> message) (\s@Commit' {} a -> s {message = a} :: Commit)

-- | A list of parent commits for the specified commit. Each parent commit ID
-- is the full commit ID.
commit_parents :: Lens.Lens' Commit (Prelude.Maybe [Prelude.Text])
commit_parents = Lens.lens (\Commit' {parents} -> parents) (\s@Commit' {} a -> s {parents = a} :: Commit) Prelude.. Lens.mapping Lens.coerced

-- | Tree information for the specified commit.
commit_treeId :: Lens.Lens' Commit (Prelude.Maybe Prelude.Text)
commit_treeId = Lens.lens (\Commit' {treeId} -> treeId) (\s@Commit' {} a -> s {treeId = a} :: Commit)

instance Data.FromJSON Commit where
  parseJSON =
    Data.withObject
      "Commit"
      ( \x ->
          Commit'
            Prelude.<$> (x Data..:? "additionalData")
            Prelude.<*> (x Data..:? "author")
            Prelude.<*> (x Data..:? "commitId")
            Prelude.<*> (x Data..:? "committer")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "parents" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "treeId")
      )

instance Prelude.Hashable Commit where
  hashWithSalt _salt Commit' {..} =
    _salt
      `Prelude.hashWithSalt` additionalData
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` commitId
      `Prelude.hashWithSalt` committer
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` parents
      `Prelude.hashWithSalt` treeId

instance Prelude.NFData Commit where
  rnf Commit' {..} =
    Prelude.rnf additionalData
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf committer
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf parents
      `Prelude.seq` Prelude.rnf treeId
