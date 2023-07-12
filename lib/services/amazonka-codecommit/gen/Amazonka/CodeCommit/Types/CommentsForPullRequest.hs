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
-- Module      : Amazonka.CodeCommit.Types.CommentsForPullRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.CommentsForPullRequest where

import Amazonka.CodeCommit.Types.Comment
import Amazonka.CodeCommit.Types.Location
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about comments on a pull request.
--
-- /See:/ 'newCommentsForPullRequest' smart constructor.
data CommentsForPullRequest = CommentsForPullRequest'
  { -- | The full blob ID of the file on which you want to comment on the source
    -- commit.
    afterBlobId :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit that was the tip of the source branch
    -- at the time the comment was made.
    afterCommitId :: Prelude.Maybe Prelude.Text,
    -- | The full blob ID of the file on which you want to comment on the
    -- destination commit.
    beforeBlobId :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit that was the tip of the destination
    -- branch when the pull request was created. This commit is superceded by
    -- the after commit in the source branch when and if you merge the source
    -- branch into the destination branch.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | An array of comment objects. Each comment object contains information
    -- about a comment on the pull request.
    comments :: Prelude.Maybe [Comment],
    -- | Location information about the comment on the pull request, including
    -- the file name, line number, and whether the version of the file where
    -- the comment was made is BEFORE (destination branch) or AFTER (source
    -- branch).
    location :: Prelude.Maybe Location,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the pull request.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommentsForPullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'afterBlobId', 'commentsForPullRequest_afterBlobId' - The full blob ID of the file on which you want to comment on the source
-- commit.
--
-- 'afterCommitId', 'commentsForPullRequest_afterCommitId' - The full commit ID of the commit that was the tip of the source branch
-- at the time the comment was made.
--
-- 'beforeBlobId', 'commentsForPullRequest_beforeBlobId' - The full blob ID of the file on which you want to comment on the
-- destination commit.
--
-- 'beforeCommitId', 'commentsForPullRequest_beforeCommitId' - The full commit ID of the commit that was the tip of the destination
-- branch when the pull request was created. This commit is superceded by
-- the after commit in the source branch when and if you merge the source
-- branch into the destination branch.
--
-- 'comments', 'commentsForPullRequest_comments' - An array of comment objects. Each comment object contains information
-- about a comment on the pull request.
--
-- 'location', 'commentsForPullRequest_location' - Location information about the comment on the pull request, including
-- the file name, line number, and whether the version of the file where
-- the comment was made is BEFORE (destination branch) or AFTER (source
-- branch).
--
-- 'pullRequestId', 'commentsForPullRequest_pullRequestId' - The system-generated ID of the pull request.
--
-- 'repositoryName', 'commentsForPullRequest_repositoryName' - The name of the repository that contains the pull request.
newCommentsForPullRequest ::
  CommentsForPullRequest
newCommentsForPullRequest =
  CommentsForPullRequest'
    { afterBlobId =
        Prelude.Nothing,
      afterCommitId = Prelude.Nothing,
      beforeBlobId = Prelude.Nothing,
      beforeCommitId = Prelude.Nothing,
      comments = Prelude.Nothing,
      location = Prelude.Nothing,
      pullRequestId = Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | The full blob ID of the file on which you want to comment on the source
-- commit.
commentsForPullRequest_afterBlobId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_afterBlobId = Lens.lens (\CommentsForPullRequest' {afterBlobId} -> afterBlobId) (\s@CommentsForPullRequest' {} a -> s {afterBlobId = a} :: CommentsForPullRequest)

-- | The full commit ID of the commit that was the tip of the source branch
-- at the time the comment was made.
commentsForPullRequest_afterCommitId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_afterCommitId = Lens.lens (\CommentsForPullRequest' {afterCommitId} -> afterCommitId) (\s@CommentsForPullRequest' {} a -> s {afterCommitId = a} :: CommentsForPullRequest)

-- | The full blob ID of the file on which you want to comment on the
-- destination commit.
commentsForPullRequest_beforeBlobId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_beforeBlobId = Lens.lens (\CommentsForPullRequest' {beforeBlobId} -> beforeBlobId) (\s@CommentsForPullRequest' {} a -> s {beforeBlobId = a} :: CommentsForPullRequest)

-- | The full commit ID of the commit that was the tip of the destination
-- branch when the pull request was created. This commit is superceded by
-- the after commit in the source branch when and if you merge the source
-- branch into the destination branch.
commentsForPullRequest_beforeCommitId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_beforeCommitId = Lens.lens (\CommentsForPullRequest' {beforeCommitId} -> beforeCommitId) (\s@CommentsForPullRequest' {} a -> s {beforeCommitId = a} :: CommentsForPullRequest)

-- | An array of comment objects. Each comment object contains information
-- about a comment on the pull request.
commentsForPullRequest_comments :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe [Comment])
commentsForPullRequest_comments = Lens.lens (\CommentsForPullRequest' {comments} -> comments) (\s@CommentsForPullRequest' {} a -> s {comments = a} :: CommentsForPullRequest) Prelude.. Lens.mapping Lens.coerced

-- | Location information about the comment on the pull request, including
-- the file name, line number, and whether the version of the file where
-- the comment was made is BEFORE (destination branch) or AFTER (source
-- branch).
commentsForPullRequest_location :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Location)
commentsForPullRequest_location = Lens.lens (\CommentsForPullRequest' {location} -> location) (\s@CommentsForPullRequest' {} a -> s {location = a} :: CommentsForPullRequest)

-- | The system-generated ID of the pull request.
commentsForPullRequest_pullRequestId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_pullRequestId = Lens.lens (\CommentsForPullRequest' {pullRequestId} -> pullRequestId) (\s@CommentsForPullRequest' {} a -> s {pullRequestId = a} :: CommentsForPullRequest)

-- | The name of the repository that contains the pull request.
commentsForPullRequest_repositoryName :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_repositoryName = Lens.lens (\CommentsForPullRequest' {repositoryName} -> repositoryName) (\s@CommentsForPullRequest' {} a -> s {repositoryName = a} :: CommentsForPullRequest)

instance Data.FromJSON CommentsForPullRequest where
  parseJSON =
    Data.withObject
      "CommentsForPullRequest"
      ( \x ->
          CommentsForPullRequest'
            Prelude.<$> (x Data..:? "afterBlobId")
            Prelude.<*> (x Data..:? "afterCommitId")
            Prelude.<*> (x Data..:? "beforeBlobId")
            Prelude.<*> (x Data..:? "beforeCommitId")
            Prelude.<*> (x Data..:? "comments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "pullRequestId")
            Prelude.<*> (x Data..:? "repositoryName")
      )

instance Prelude.Hashable CommentsForPullRequest where
  hashWithSalt _salt CommentsForPullRequest' {..} =
    _salt
      `Prelude.hashWithSalt` afterBlobId
      `Prelude.hashWithSalt` afterCommitId
      `Prelude.hashWithSalt` beforeBlobId
      `Prelude.hashWithSalt` beforeCommitId
      `Prelude.hashWithSalt` comments
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData CommentsForPullRequest where
  rnf CommentsForPullRequest' {..} =
    Prelude.rnf afterBlobId
      `Prelude.seq` Prelude.rnf afterCommitId
      `Prelude.seq` Prelude.rnf beforeBlobId
      `Prelude.seq` Prelude.rnf beforeCommitId
      `Prelude.seq` Prelude.rnf comments
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf repositoryName
