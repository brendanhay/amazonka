{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergePullRequestBySquash
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified
-- destination branch for that pull request at the specified commit using
-- the squash merge strategy. If the merge is successful, it closes the
-- pull request.
module Network.AWS.CodeCommit.MergePullRequestBySquash
  ( -- * Creating a Request
    MergePullRequestBySquash (..),
    newMergePullRequestBySquash,

    -- * Request Lenses
    mergePullRequestBySquash_authorName,
    mergePullRequestBySquash_commitMessage,
    mergePullRequestBySquash_email,
    mergePullRequestBySquash_sourceCommitId,
    mergePullRequestBySquash_conflictDetailLevel,
    mergePullRequestBySquash_keepEmptyFolders,
    mergePullRequestBySquash_conflictResolutionStrategy,
    mergePullRequestBySquash_conflictResolution,
    mergePullRequestBySquash_pullRequestId,
    mergePullRequestBySquash_repositoryName,

    -- * Destructuring the Response
    MergePullRequestBySquashResponse (..),
    newMergePullRequestBySquashResponse,

    -- * Response Lenses
    mergePullRequestBySquashResponse_pullRequest,
    mergePullRequestBySquashResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newMergePullRequestBySquash' smart constructor.
data MergePullRequestBySquash = MergePullRequestBySquash'
  { -- | The name of the author who created the commit. This information is used
    -- as both the author and committer for the commit.
    authorName :: Prelude.Maybe Prelude.Text,
    -- | The commit message to include in the commit information for the merge.
    commitMessage :: Prelude.Maybe Prelude.Text,
    -- | The email address of the person merging the branches. This information
    -- is used in the commit information for the merge.
    email :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the original or updated commit in the pull request
    -- source branch. Pass this value if you want an exception thrown if the
    -- current commit ID of the tip of the source branch does not match this
    -- commit ID.
    sourceCommitId :: Prelude.Maybe Prelude.Text,
    -- | The level of conflict detail to use. If unspecified, the default
    -- FILE_LEVEL is used, which returns a not-mergeable result if the same
    -- file has differences in both branches. If LINE_LEVEL is specified, a
    -- conflict is considered not mergeable if the same file in both branches
    -- has differences on the same line.
    conflictDetailLevel :: Prelude.Maybe ConflictDetailLevelTypeEnum,
    -- | If the commit contains deletions, whether to keep a folder or folder
    -- structure if the changes leave the folders empty. If true, a .gitkeep
    -- file is created for empty folders. The default is false.
    keepEmptyFolders :: Prelude.Maybe Prelude.Bool,
    -- | Specifies which branch to use when resolving conflicts, or whether to
    -- attempt automatically merging two versions of a file. The default is
    -- NONE, which requires any conflicts to be resolved manually before the
    -- merge operation is successful.
    conflictResolutionStrategy :: Prelude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
    -- use when resolving conflicts during a merge.
    conflictResolution :: Prelude.Maybe ConflictResolution,
    -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text,
    -- | The name of the repository where the pull request was created.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergePullRequestBySquash' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorName', 'mergePullRequestBySquash_authorName' - The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
--
-- 'commitMessage', 'mergePullRequestBySquash_commitMessage' - The commit message to include in the commit information for the merge.
--
-- 'email', 'mergePullRequestBySquash_email' - The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
--
-- 'sourceCommitId', 'mergePullRequestBySquash_sourceCommitId' - The full commit ID of the original or updated commit in the pull request
-- source branch. Pass this value if you want an exception thrown if the
-- current commit ID of the tip of the source branch does not match this
-- commit ID.
--
-- 'conflictDetailLevel', 'mergePullRequestBySquash_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'keepEmptyFolders', 'mergePullRequestBySquash_keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a .gitkeep
-- file is created for empty folders. The default is false.
--
-- 'conflictResolutionStrategy', 'mergePullRequestBySquash_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'conflictResolution', 'mergePullRequestBySquash_conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
--
-- 'pullRequestId', 'mergePullRequestBySquash_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
--
-- 'repositoryName', 'mergePullRequestBySquash_repositoryName' - The name of the repository where the pull request was created.
newMergePullRequestBySquash ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  MergePullRequestBySquash
newMergePullRequestBySquash
  pPullRequestId_
  pRepositoryName_ =
    MergePullRequestBySquash'
      { authorName =
          Prelude.Nothing,
        commitMessage = Prelude.Nothing,
        email = Prelude.Nothing,
        sourceCommitId = Prelude.Nothing,
        conflictDetailLevel = Prelude.Nothing,
        keepEmptyFolders = Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        conflictResolution = Prelude.Nothing,
        pullRequestId = pPullRequestId_,
        repositoryName = pRepositoryName_
      }

-- | The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
mergePullRequestBySquash_authorName :: Lens.Lens' MergePullRequestBySquash (Prelude.Maybe Prelude.Text)
mergePullRequestBySquash_authorName = Lens.lens (\MergePullRequestBySquash' {authorName} -> authorName) (\s@MergePullRequestBySquash' {} a -> s {authorName = a} :: MergePullRequestBySquash)

-- | The commit message to include in the commit information for the merge.
mergePullRequestBySquash_commitMessage :: Lens.Lens' MergePullRequestBySquash (Prelude.Maybe Prelude.Text)
mergePullRequestBySquash_commitMessage = Lens.lens (\MergePullRequestBySquash' {commitMessage} -> commitMessage) (\s@MergePullRequestBySquash' {} a -> s {commitMessage = a} :: MergePullRequestBySquash)

-- | The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
mergePullRequestBySquash_email :: Lens.Lens' MergePullRequestBySquash (Prelude.Maybe Prelude.Text)
mergePullRequestBySquash_email = Lens.lens (\MergePullRequestBySquash' {email} -> email) (\s@MergePullRequestBySquash' {} a -> s {email = a} :: MergePullRequestBySquash)

-- | The full commit ID of the original or updated commit in the pull request
-- source branch. Pass this value if you want an exception thrown if the
-- current commit ID of the tip of the source branch does not match this
-- commit ID.
mergePullRequestBySquash_sourceCommitId :: Lens.Lens' MergePullRequestBySquash (Prelude.Maybe Prelude.Text)
mergePullRequestBySquash_sourceCommitId = Lens.lens (\MergePullRequestBySquash' {sourceCommitId} -> sourceCommitId) (\s@MergePullRequestBySquash' {} a -> s {sourceCommitId = a} :: MergePullRequestBySquash)

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
mergePullRequestBySquash_conflictDetailLevel :: Lens.Lens' MergePullRequestBySquash (Prelude.Maybe ConflictDetailLevelTypeEnum)
mergePullRequestBySquash_conflictDetailLevel = Lens.lens (\MergePullRequestBySquash' {conflictDetailLevel} -> conflictDetailLevel) (\s@MergePullRequestBySquash' {} a -> s {conflictDetailLevel = a} :: MergePullRequestBySquash)

-- | If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a .gitkeep
-- file is created for empty folders. The default is false.
mergePullRequestBySquash_keepEmptyFolders :: Lens.Lens' MergePullRequestBySquash (Prelude.Maybe Prelude.Bool)
mergePullRequestBySquash_keepEmptyFolders = Lens.lens (\MergePullRequestBySquash' {keepEmptyFolders} -> keepEmptyFolders) (\s@MergePullRequestBySquash' {} a -> s {keepEmptyFolders = a} :: MergePullRequestBySquash)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
mergePullRequestBySquash_conflictResolutionStrategy :: Lens.Lens' MergePullRequestBySquash (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
mergePullRequestBySquash_conflictResolutionStrategy = Lens.lens (\MergePullRequestBySquash' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@MergePullRequestBySquash' {} a -> s {conflictResolutionStrategy = a} :: MergePullRequestBySquash)

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
mergePullRequestBySquash_conflictResolution :: Lens.Lens' MergePullRequestBySquash (Prelude.Maybe ConflictResolution)
mergePullRequestBySquash_conflictResolution = Lens.lens (\MergePullRequestBySquash' {conflictResolution} -> conflictResolution) (\s@MergePullRequestBySquash' {} a -> s {conflictResolution = a} :: MergePullRequestBySquash)

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
mergePullRequestBySquash_pullRequestId :: Lens.Lens' MergePullRequestBySquash Prelude.Text
mergePullRequestBySquash_pullRequestId = Lens.lens (\MergePullRequestBySquash' {pullRequestId} -> pullRequestId) (\s@MergePullRequestBySquash' {} a -> s {pullRequestId = a} :: MergePullRequestBySquash)

-- | The name of the repository where the pull request was created.
mergePullRequestBySquash_repositoryName :: Lens.Lens' MergePullRequestBySquash Prelude.Text
mergePullRequestBySquash_repositoryName = Lens.lens (\MergePullRequestBySquash' {repositoryName} -> repositoryName) (\s@MergePullRequestBySquash' {} a -> s {repositoryName = a} :: MergePullRequestBySquash)

instance Core.AWSRequest MergePullRequestBySquash where
  type
    AWSResponse MergePullRequestBySquash =
      MergePullRequestBySquashResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          MergePullRequestBySquashResponse'
            Prelude.<$> (x Core..?> "pullRequest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MergePullRequestBySquash

instance Prelude.NFData MergePullRequestBySquash

instance Core.ToHeaders MergePullRequestBySquash where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.MergePullRequestBySquash" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON MergePullRequestBySquash where
  toJSON MergePullRequestBySquash' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("authorName" Core..=) Prelude.<$> authorName,
            ("commitMessage" Core..=) Prelude.<$> commitMessage,
            ("email" Core..=) Prelude.<$> email,
            ("sourceCommitId" Core..=)
              Prelude.<$> sourceCommitId,
            ("conflictDetailLevel" Core..=)
              Prelude.<$> conflictDetailLevel,
            ("keepEmptyFolders" Core..=)
              Prelude.<$> keepEmptyFolders,
            ("conflictResolutionStrategy" Core..=)
              Prelude.<$> conflictResolutionStrategy,
            ("conflictResolution" Core..=)
              Prelude.<$> conflictResolution,
            Prelude.Just ("pullRequestId" Core..= pullRequestId),
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath MergePullRequestBySquash where
  toPath = Prelude.const "/"

instance Core.ToQuery MergePullRequestBySquash where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMergePullRequestBySquashResponse' smart constructor.
data MergePullRequestBySquashResponse = MergePullRequestBySquashResponse'
  { pullRequest :: Prelude.Maybe PullRequest,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergePullRequestBySquashResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequest', 'mergePullRequestBySquashResponse_pullRequest' - Undocumented member.
--
-- 'httpStatus', 'mergePullRequestBySquashResponse_httpStatus' - The response's http status code.
newMergePullRequestBySquashResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MergePullRequestBySquashResponse
newMergePullRequestBySquashResponse pHttpStatus_ =
  MergePullRequestBySquashResponse'
    { pullRequest =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
mergePullRequestBySquashResponse_pullRequest :: Lens.Lens' MergePullRequestBySquashResponse (Prelude.Maybe PullRequest)
mergePullRequestBySquashResponse_pullRequest = Lens.lens (\MergePullRequestBySquashResponse' {pullRequest} -> pullRequest) (\s@MergePullRequestBySquashResponse' {} a -> s {pullRequest = a} :: MergePullRequestBySquashResponse)

-- | The response's http status code.
mergePullRequestBySquashResponse_httpStatus :: Lens.Lens' MergePullRequestBySquashResponse Prelude.Int
mergePullRequestBySquashResponse_httpStatus = Lens.lens (\MergePullRequestBySquashResponse' {httpStatus} -> httpStatus) (\s@MergePullRequestBySquashResponse' {} a -> s {httpStatus = a} :: MergePullRequestBySquashResponse)

instance
  Prelude.NFData
    MergePullRequestBySquashResponse
