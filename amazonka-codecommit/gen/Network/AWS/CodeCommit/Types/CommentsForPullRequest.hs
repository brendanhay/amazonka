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
-- Module      : Network.AWS.CodeCommit.Types.CommentsForPullRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.CommentsForPullRequest where

import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about comments on a pull request.
--
-- /See:/ 'newCommentsForPullRequest' smart constructor.
data CommentsForPullRequest = CommentsForPullRequest'
  { -- | The full blob ID of the file on which you want to comment on the
    -- destination commit.
    beforeBlobId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the pull request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit that was the tip of the destination
    -- branch when the pull request was created. This commit is superceded by
    -- the after commit in the source branch when and if you merge the source
    -- branch into the destination branch.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | The full blob ID of the file on which you want to comment on the source
    -- commit.
    afterBlobId :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Prelude.Maybe Prelude.Text,
    -- | An array of comment objects. Each comment object contains information
    -- about a comment on the pull request.
    comments :: Prelude.Maybe [Comment],
    -- | The full commit ID of the commit that was the tip of the source branch
    -- at the time the comment was made.
    afterCommitId :: Prelude.Maybe Prelude.Text,
    -- | Location information about the comment on the pull request, including
    -- the file name, line number, and whether the version of the file where
    -- the comment was made is BEFORE (destination branch) or AFTER (source
    -- branch).
    location :: Prelude.Maybe Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CommentsForPullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beforeBlobId', 'commentsForPullRequest_beforeBlobId' - The full blob ID of the file on which you want to comment on the
-- destination commit.
--
-- 'repositoryName', 'commentsForPullRequest_repositoryName' - The name of the repository that contains the pull request.
--
-- 'beforeCommitId', 'commentsForPullRequest_beforeCommitId' - The full commit ID of the commit that was the tip of the destination
-- branch when the pull request was created. This commit is superceded by
-- the after commit in the source branch when and if you merge the source
-- branch into the destination branch.
--
-- 'afterBlobId', 'commentsForPullRequest_afterBlobId' - The full blob ID of the file on which you want to comment on the source
-- commit.
--
-- 'pullRequestId', 'commentsForPullRequest_pullRequestId' - The system-generated ID of the pull request.
--
-- 'comments', 'commentsForPullRequest_comments' - An array of comment objects. Each comment object contains information
-- about a comment on the pull request.
--
-- 'afterCommitId', 'commentsForPullRequest_afterCommitId' - The full commit ID of the commit that was the tip of the source branch
-- at the time the comment was made.
--
-- 'location', 'commentsForPullRequest_location' - Location information about the comment on the pull request, including
-- the file name, line number, and whether the version of the file where
-- the comment was made is BEFORE (destination branch) or AFTER (source
-- branch).
newCommentsForPullRequest ::
  CommentsForPullRequest
newCommentsForPullRequest =
  CommentsForPullRequest'
    { beforeBlobId =
        Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      beforeCommitId = Prelude.Nothing,
      afterBlobId = Prelude.Nothing,
      pullRequestId = Prelude.Nothing,
      comments = Prelude.Nothing,
      afterCommitId = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The full blob ID of the file on which you want to comment on the
-- destination commit.
commentsForPullRequest_beforeBlobId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_beforeBlobId = Lens.lens (\CommentsForPullRequest' {beforeBlobId} -> beforeBlobId) (\s@CommentsForPullRequest' {} a -> s {beforeBlobId = a} :: CommentsForPullRequest)

-- | The name of the repository that contains the pull request.
commentsForPullRequest_repositoryName :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_repositoryName = Lens.lens (\CommentsForPullRequest' {repositoryName} -> repositoryName) (\s@CommentsForPullRequest' {} a -> s {repositoryName = a} :: CommentsForPullRequest)

-- | The full commit ID of the commit that was the tip of the destination
-- branch when the pull request was created. This commit is superceded by
-- the after commit in the source branch when and if you merge the source
-- branch into the destination branch.
commentsForPullRequest_beforeCommitId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_beforeCommitId = Lens.lens (\CommentsForPullRequest' {beforeCommitId} -> beforeCommitId) (\s@CommentsForPullRequest' {} a -> s {beforeCommitId = a} :: CommentsForPullRequest)

-- | The full blob ID of the file on which you want to comment on the source
-- commit.
commentsForPullRequest_afterBlobId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_afterBlobId = Lens.lens (\CommentsForPullRequest' {afterBlobId} -> afterBlobId) (\s@CommentsForPullRequest' {} a -> s {afterBlobId = a} :: CommentsForPullRequest)

-- | The system-generated ID of the pull request.
commentsForPullRequest_pullRequestId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_pullRequestId = Lens.lens (\CommentsForPullRequest' {pullRequestId} -> pullRequestId) (\s@CommentsForPullRequest' {} a -> s {pullRequestId = a} :: CommentsForPullRequest)

-- | An array of comment objects. Each comment object contains information
-- about a comment on the pull request.
commentsForPullRequest_comments :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe [Comment])
commentsForPullRequest_comments = Lens.lens (\CommentsForPullRequest' {comments} -> comments) (\s@CommentsForPullRequest' {} a -> s {comments = a} :: CommentsForPullRequest) Prelude.. Lens.mapping Prelude._Coerce

-- | The full commit ID of the commit that was the tip of the source branch
-- at the time the comment was made.
commentsForPullRequest_afterCommitId :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Prelude.Text)
commentsForPullRequest_afterCommitId = Lens.lens (\CommentsForPullRequest' {afterCommitId} -> afterCommitId) (\s@CommentsForPullRequest' {} a -> s {afterCommitId = a} :: CommentsForPullRequest)

-- | Location information about the comment on the pull request, including
-- the file name, line number, and whether the version of the file where
-- the comment was made is BEFORE (destination branch) or AFTER (source
-- branch).
commentsForPullRequest_location :: Lens.Lens' CommentsForPullRequest (Prelude.Maybe Location)
commentsForPullRequest_location = Lens.lens (\CommentsForPullRequest' {location} -> location) (\s@CommentsForPullRequest' {} a -> s {location = a} :: CommentsForPullRequest)

instance Prelude.FromJSON CommentsForPullRequest where
  parseJSON =
    Prelude.withObject
      "CommentsForPullRequest"
      ( \x ->
          CommentsForPullRequest'
            Prelude.<$> (x Prelude..:? "beforeBlobId")
            Prelude.<*> (x Prelude..:? "repositoryName")
            Prelude.<*> (x Prelude..:? "beforeCommitId")
            Prelude.<*> (x Prelude..:? "afterBlobId")
            Prelude.<*> (x Prelude..:? "pullRequestId")
            Prelude.<*> (x Prelude..:? "comments" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "afterCommitId")
            Prelude.<*> (x Prelude..:? "location")
      )

instance Prelude.Hashable CommentsForPullRequest

instance Prelude.NFData CommentsForPullRequest
