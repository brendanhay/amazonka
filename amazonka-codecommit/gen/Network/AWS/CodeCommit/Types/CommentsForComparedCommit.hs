{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.Types.CommentsForComparedCommit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.CommentsForComparedCommit where

import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about comments on the comparison between two
-- commits.
--
-- /See:/ 'newCommentsForComparedCommit' smart constructor.
data CommentsForComparedCommit = CommentsForComparedCommit'
  { -- | The full blob ID of the commit used to establish the before of the
    -- comparison.
    beforeBlobId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the compared commits.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit used to establish the before of the
    -- comparison.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | The full blob ID of the commit used to establish the after of the
    -- comparison.
    afterBlobId :: Prelude.Maybe Prelude.Text,
    -- | An array of comment objects. Each comment object contains information
    -- about a comment on the comparison between commits.
    comments :: Prelude.Maybe [Comment],
    -- | The full commit ID of the commit used to establish the after of the
    -- comparison.
    afterCommitId :: Prelude.Maybe Prelude.Text,
    -- | Location information about the comment on the comparison, including the
    -- file name, line number, and whether the version of the file where the
    -- comment was made is BEFORE or AFTER.
    location :: Prelude.Maybe Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CommentsForComparedCommit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beforeBlobId', 'commentsForComparedCommit_beforeBlobId' - The full blob ID of the commit used to establish the before of the
-- comparison.
--
-- 'repositoryName', 'commentsForComparedCommit_repositoryName' - The name of the repository that contains the compared commits.
--
-- 'beforeCommitId', 'commentsForComparedCommit_beforeCommitId' - The full commit ID of the commit used to establish the before of the
-- comparison.
--
-- 'afterBlobId', 'commentsForComparedCommit_afterBlobId' - The full blob ID of the commit used to establish the after of the
-- comparison.
--
-- 'comments', 'commentsForComparedCommit_comments' - An array of comment objects. Each comment object contains information
-- about a comment on the comparison between commits.
--
-- 'afterCommitId', 'commentsForComparedCommit_afterCommitId' - The full commit ID of the commit used to establish the after of the
-- comparison.
--
-- 'location', 'commentsForComparedCommit_location' - Location information about the comment on the comparison, including the
-- file name, line number, and whether the version of the file where the
-- comment was made is BEFORE or AFTER.
newCommentsForComparedCommit ::
  CommentsForComparedCommit
newCommentsForComparedCommit =
  CommentsForComparedCommit'
    { beforeBlobId =
        Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      beforeCommitId = Prelude.Nothing,
      afterBlobId = Prelude.Nothing,
      comments = Prelude.Nothing,
      afterCommitId = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The full blob ID of the commit used to establish the before of the
-- comparison.
commentsForComparedCommit_beforeBlobId :: Lens.Lens' CommentsForComparedCommit (Prelude.Maybe Prelude.Text)
commentsForComparedCommit_beforeBlobId = Lens.lens (\CommentsForComparedCommit' {beforeBlobId} -> beforeBlobId) (\s@CommentsForComparedCommit' {} a -> s {beforeBlobId = a} :: CommentsForComparedCommit)

-- | The name of the repository that contains the compared commits.
commentsForComparedCommit_repositoryName :: Lens.Lens' CommentsForComparedCommit (Prelude.Maybe Prelude.Text)
commentsForComparedCommit_repositoryName = Lens.lens (\CommentsForComparedCommit' {repositoryName} -> repositoryName) (\s@CommentsForComparedCommit' {} a -> s {repositoryName = a} :: CommentsForComparedCommit)

-- | The full commit ID of the commit used to establish the before of the
-- comparison.
commentsForComparedCommit_beforeCommitId :: Lens.Lens' CommentsForComparedCommit (Prelude.Maybe Prelude.Text)
commentsForComparedCommit_beforeCommitId = Lens.lens (\CommentsForComparedCommit' {beforeCommitId} -> beforeCommitId) (\s@CommentsForComparedCommit' {} a -> s {beforeCommitId = a} :: CommentsForComparedCommit)

-- | The full blob ID of the commit used to establish the after of the
-- comparison.
commentsForComparedCommit_afterBlobId :: Lens.Lens' CommentsForComparedCommit (Prelude.Maybe Prelude.Text)
commentsForComparedCommit_afterBlobId = Lens.lens (\CommentsForComparedCommit' {afterBlobId} -> afterBlobId) (\s@CommentsForComparedCommit' {} a -> s {afterBlobId = a} :: CommentsForComparedCommit)

-- | An array of comment objects. Each comment object contains information
-- about a comment on the comparison between commits.
commentsForComparedCommit_comments :: Lens.Lens' CommentsForComparedCommit (Prelude.Maybe [Comment])
commentsForComparedCommit_comments = Lens.lens (\CommentsForComparedCommit' {comments} -> comments) (\s@CommentsForComparedCommit' {} a -> s {comments = a} :: CommentsForComparedCommit) Prelude.. Lens.mapping Prelude._Coerce

-- | The full commit ID of the commit used to establish the after of the
-- comparison.
commentsForComparedCommit_afterCommitId :: Lens.Lens' CommentsForComparedCommit (Prelude.Maybe Prelude.Text)
commentsForComparedCommit_afterCommitId = Lens.lens (\CommentsForComparedCommit' {afterCommitId} -> afterCommitId) (\s@CommentsForComparedCommit' {} a -> s {afterCommitId = a} :: CommentsForComparedCommit)

-- | Location information about the comment on the comparison, including the
-- file name, line number, and whether the version of the file where the
-- comment was made is BEFORE or AFTER.
commentsForComparedCommit_location :: Lens.Lens' CommentsForComparedCommit (Prelude.Maybe Location)
commentsForComparedCommit_location = Lens.lens (\CommentsForComparedCommit' {location} -> location) (\s@CommentsForComparedCommit' {} a -> s {location = a} :: CommentsForComparedCommit)

instance Prelude.FromJSON CommentsForComparedCommit where
  parseJSON =
    Prelude.withObject
      "CommentsForComparedCommit"
      ( \x ->
          CommentsForComparedCommit'
            Prelude.<$> (x Prelude..:? "beforeBlobId")
            Prelude.<*> (x Prelude..:? "repositoryName")
            Prelude.<*> (x Prelude..:? "beforeCommitId")
            Prelude.<*> (x Prelude..:? "afterBlobId")
            Prelude.<*> (x Prelude..:? "comments" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "afterCommitId")
            Prelude.<*> (x Prelude..:? "location")
      )

instance Prelude.Hashable CommentsForComparedCommit

instance Prelude.NFData CommentsForComparedCommit
