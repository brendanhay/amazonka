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
-- Module      : Amazonka.CodeCommit.MergePullRequestByThreeWay
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified
-- destination branch for that pull request at the specified commit using
-- the three-way merge strategy. If the merge is successful, it closes the
-- pull request.
module Amazonka.CodeCommit.MergePullRequestByThreeWay
  ( -- * Creating a Request
    MergePullRequestByThreeWay (..),
    newMergePullRequestByThreeWay,

    -- * Request Lenses
    mergePullRequestByThreeWay_keepEmptyFolders,
    mergePullRequestByThreeWay_conflictResolution,
    mergePullRequestByThreeWay_email,
    mergePullRequestByThreeWay_sourceCommitId,
    mergePullRequestByThreeWay_authorName,
    mergePullRequestByThreeWay_commitMessage,
    mergePullRequestByThreeWay_conflictResolutionStrategy,
    mergePullRequestByThreeWay_conflictDetailLevel,
    mergePullRequestByThreeWay_pullRequestId,
    mergePullRequestByThreeWay_repositoryName,

    -- * Destructuring the Response
    MergePullRequestByThreeWayResponse (..),
    newMergePullRequestByThreeWayResponse,

    -- * Response Lenses
    mergePullRequestByThreeWayResponse_pullRequest,
    mergePullRequestByThreeWayResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMergePullRequestByThreeWay' smart constructor.
data MergePullRequestByThreeWay = MergePullRequestByThreeWay'
  { -- | If the commit contains deletions, whether to keep a folder or folder
    -- structure if the changes leave the folders empty. If true, a .gitkeep
    -- file is created for empty folders. The default is false.
    keepEmptyFolders :: Prelude.Maybe Prelude.Bool,
    -- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
    -- use when resolving conflicts during a merge.
    conflictResolution :: Prelude.Maybe ConflictResolution,
    -- | The email address of the person merging the branches. This information
    -- is used in the commit information for the merge.
    email :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the original or updated commit in the pull request
    -- source branch. Pass this value if you want an exception thrown if the
    -- current commit ID of the tip of the source branch does not match this
    -- commit ID.
    sourceCommitId :: Prelude.Maybe Prelude.Text,
    -- | The name of the author who created the commit. This information is used
    -- as both the author and committer for the commit.
    authorName :: Prelude.Maybe Prelude.Text,
    -- | The commit message to include in the commit information for the merge.
    commitMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies which branch to use when resolving conflicts, or whether to
    -- attempt automatically merging two versions of a file. The default is
    -- NONE, which requires any conflicts to be resolved manually before the
    -- merge operation is successful.
    conflictResolutionStrategy :: Prelude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | The level of conflict detail to use. If unspecified, the default
    -- FILE_LEVEL is used, which returns a not-mergeable result if the same
    -- file has differences in both branches. If LINE_LEVEL is specified, a
    -- conflict is considered not mergeable if the same file in both branches
    -- has differences on the same line.
    conflictDetailLevel :: Prelude.Maybe ConflictDetailLevelTypeEnum,
    -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text,
    -- | The name of the repository where the pull request was created.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergePullRequestByThreeWay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keepEmptyFolders', 'mergePullRequestByThreeWay_keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a .gitkeep
-- file is created for empty folders. The default is false.
--
-- 'conflictResolution', 'mergePullRequestByThreeWay_conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
--
-- 'email', 'mergePullRequestByThreeWay_email' - The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
--
-- 'sourceCommitId', 'mergePullRequestByThreeWay_sourceCommitId' - The full commit ID of the original or updated commit in the pull request
-- source branch. Pass this value if you want an exception thrown if the
-- current commit ID of the tip of the source branch does not match this
-- commit ID.
--
-- 'authorName', 'mergePullRequestByThreeWay_authorName' - The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
--
-- 'commitMessage', 'mergePullRequestByThreeWay_commitMessage' - The commit message to include in the commit information for the merge.
--
-- 'conflictResolutionStrategy', 'mergePullRequestByThreeWay_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'conflictDetailLevel', 'mergePullRequestByThreeWay_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'pullRequestId', 'mergePullRequestByThreeWay_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
--
-- 'repositoryName', 'mergePullRequestByThreeWay_repositoryName' - The name of the repository where the pull request was created.
newMergePullRequestByThreeWay ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  MergePullRequestByThreeWay
newMergePullRequestByThreeWay
  pPullRequestId_
  pRepositoryName_ =
    MergePullRequestByThreeWay'
      { keepEmptyFolders =
          Prelude.Nothing,
        conflictResolution = Prelude.Nothing,
        email = Prelude.Nothing,
        sourceCommitId = Prelude.Nothing,
        authorName = Prelude.Nothing,
        commitMessage = Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        conflictDetailLevel = Prelude.Nothing,
        pullRequestId = pPullRequestId_,
        repositoryName = pRepositoryName_
      }

-- | If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a .gitkeep
-- file is created for empty folders. The default is false.
mergePullRequestByThreeWay_keepEmptyFolders :: Lens.Lens' MergePullRequestByThreeWay (Prelude.Maybe Prelude.Bool)
mergePullRequestByThreeWay_keepEmptyFolders = Lens.lens (\MergePullRequestByThreeWay' {keepEmptyFolders} -> keepEmptyFolders) (\s@MergePullRequestByThreeWay' {} a -> s {keepEmptyFolders = a} :: MergePullRequestByThreeWay)

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
mergePullRequestByThreeWay_conflictResolution :: Lens.Lens' MergePullRequestByThreeWay (Prelude.Maybe ConflictResolution)
mergePullRequestByThreeWay_conflictResolution = Lens.lens (\MergePullRequestByThreeWay' {conflictResolution} -> conflictResolution) (\s@MergePullRequestByThreeWay' {} a -> s {conflictResolution = a} :: MergePullRequestByThreeWay)

-- | The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
mergePullRequestByThreeWay_email :: Lens.Lens' MergePullRequestByThreeWay (Prelude.Maybe Prelude.Text)
mergePullRequestByThreeWay_email = Lens.lens (\MergePullRequestByThreeWay' {email} -> email) (\s@MergePullRequestByThreeWay' {} a -> s {email = a} :: MergePullRequestByThreeWay)

-- | The full commit ID of the original or updated commit in the pull request
-- source branch. Pass this value if you want an exception thrown if the
-- current commit ID of the tip of the source branch does not match this
-- commit ID.
mergePullRequestByThreeWay_sourceCommitId :: Lens.Lens' MergePullRequestByThreeWay (Prelude.Maybe Prelude.Text)
mergePullRequestByThreeWay_sourceCommitId = Lens.lens (\MergePullRequestByThreeWay' {sourceCommitId} -> sourceCommitId) (\s@MergePullRequestByThreeWay' {} a -> s {sourceCommitId = a} :: MergePullRequestByThreeWay)

-- | The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
mergePullRequestByThreeWay_authorName :: Lens.Lens' MergePullRequestByThreeWay (Prelude.Maybe Prelude.Text)
mergePullRequestByThreeWay_authorName = Lens.lens (\MergePullRequestByThreeWay' {authorName} -> authorName) (\s@MergePullRequestByThreeWay' {} a -> s {authorName = a} :: MergePullRequestByThreeWay)

-- | The commit message to include in the commit information for the merge.
mergePullRequestByThreeWay_commitMessage :: Lens.Lens' MergePullRequestByThreeWay (Prelude.Maybe Prelude.Text)
mergePullRequestByThreeWay_commitMessage = Lens.lens (\MergePullRequestByThreeWay' {commitMessage} -> commitMessage) (\s@MergePullRequestByThreeWay' {} a -> s {commitMessage = a} :: MergePullRequestByThreeWay)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
mergePullRequestByThreeWay_conflictResolutionStrategy :: Lens.Lens' MergePullRequestByThreeWay (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
mergePullRequestByThreeWay_conflictResolutionStrategy = Lens.lens (\MergePullRequestByThreeWay' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@MergePullRequestByThreeWay' {} a -> s {conflictResolutionStrategy = a} :: MergePullRequestByThreeWay)

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
mergePullRequestByThreeWay_conflictDetailLevel :: Lens.Lens' MergePullRequestByThreeWay (Prelude.Maybe ConflictDetailLevelTypeEnum)
mergePullRequestByThreeWay_conflictDetailLevel = Lens.lens (\MergePullRequestByThreeWay' {conflictDetailLevel} -> conflictDetailLevel) (\s@MergePullRequestByThreeWay' {} a -> s {conflictDetailLevel = a} :: MergePullRequestByThreeWay)

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
mergePullRequestByThreeWay_pullRequestId :: Lens.Lens' MergePullRequestByThreeWay Prelude.Text
mergePullRequestByThreeWay_pullRequestId = Lens.lens (\MergePullRequestByThreeWay' {pullRequestId} -> pullRequestId) (\s@MergePullRequestByThreeWay' {} a -> s {pullRequestId = a} :: MergePullRequestByThreeWay)

-- | The name of the repository where the pull request was created.
mergePullRequestByThreeWay_repositoryName :: Lens.Lens' MergePullRequestByThreeWay Prelude.Text
mergePullRequestByThreeWay_repositoryName = Lens.lens (\MergePullRequestByThreeWay' {repositoryName} -> repositoryName) (\s@MergePullRequestByThreeWay' {} a -> s {repositoryName = a} :: MergePullRequestByThreeWay)

instance Core.AWSRequest MergePullRequestByThreeWay where
  type
    AWSResponse MergePullRequestByThreeWay =
      MergePullRequestByThreeWayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          MergePullRequestByThreeWayResponse'
            Prelude.<$> (x Core..?> "pullRequest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MergePullRequestByThreeWay where
  hashWithSalt _salt MergePullRequestByThreeWay' {..} =
    _salt `Prelude.hashWithSalt` keepEmptyFolders
      `Prelude.hashWithSalt` conflictResolution
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` sourceCommitId
      `Prelude.hashWithSalt` authorName
      `Prelude.hashWithSalt` commitMessage
      `Prelude.hashWithSalt` conflictResolutionStrategy
      `Prelude.hashWithSalt` conflictDetailLevel
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData MergePullRequestByThreeWay where
  rnf MergePullRequestByThreeWay' {..} =
    Prelude.rnf keepEmptyFolders
      `Prelude.seq` Prelude.rnf conflictResolution
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf sourceCommitId
      `Prelude.seq` Prelude.rnf authorName
      `Prelude.seq` Prelude.rnf commitMessage
      `Prelude.seq` Prelude.rnf conflictResolutionStrategy
      `Prelude.seq` Prelude.rnf conflictDetailLevel
      `Prelude.seq` Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf repositoryName

instance Core.ToHeaders MergePullRequestByThreeWay where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.MergePullRequestByThreeWay" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON MergePullRequestByThreeWay where
  toJSON MergePullRequestByThreeWay' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("keepEmptyFolders" Core..=)
              Prelude.<$> keepEmptyFolders,
            ("conflictResolution" Core..=)
              Prelude.<$> conflictResolution,
            ("email" Core..=) Prelude.<$> email,
            ("sourceCommitId" Core..=)
              Prelude.<$> sourceCommitId,
            ("authorName" Core..=) Prelude.<$> authorName,
            ("commitMessage" Core..=) Prelude.<$> commitMessage,
            ("conflictResolutionStrategy" Core..=)
              Prelude.<$> conflictResolutionStrategy,
            ("conflictDetailLevel" Core..=)
              Prelude.<$> conflictDetailLevel,
            Prelude.Just ("pullRequestId" Core..= pullRequestId),
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath MergePullRequestByThreeWay where
  toPath = Prelude.const "/"

instance Core.ToQuery MergePullRequestByThreeWay where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMergePullRequestByThreeWayResponse' smart constructor.
data MergePullRequestByThreeWayResponse = MergePullRequestByThreeWayResponse'
  { pullRequest :: Prelude.Maybe PullRequest,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergePullRequestByThreeWayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequest', 'mergePullRequestByThreeWayResponse_pullRequest' - Undocumented member.
--
-- 'httpStatus', 'mergePullRequestByThreeWayResponse_httpStatus' - The response's http status code.
newMergePullRequestByThreeWayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MergePullRequestByThreeWayResponse
newMergePullRequestByThreeWayResponse pHttpStatus_ =
  MergePullRequestByThreeWayResponse'
    { pullRequest =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
mergePullRequestByThreeWayResponse_pullRequest :: Lens.Lens' MergePullRequestByThreeWayResponse (Prelude.Maybe PullRequest)
mergePullRequestByThreeWayResponse_pullRequest = Lens.lens (\MergePullRequestByThreeWayResponse' {pullRequest} -> pullRequest) (\s@MergePullRequestByThreeWayResponse' {} a -> s {pullRequest = a} :: MergePullRequestByThreeWayResponse)

-- | The response's http status code.
mergePullRequestByThreeWayResponse_httpStatus :: Lens.Lens' MergePullRequestByThreeWayResponse Prelude.Int
mergePullRequestByThreeWayResponse_httpStatus = Lens.lens (\MergePullRequestByThreeWayResponse' {httpStatus} -> httpStatus) (\s@MergePullRequestByThreeWayResponse' {} a -> s {httpStatus = a} :: MergePullRequestByThreeWayResponse)

instance
  Prelude.NFData
    MergePullRequestByThreeWayResponse
  where
  rnf MergePullRequestByThreeWayResponse' {..} =
    Prelude.rnf pullRequest
      `Prelude.seq` Prelude.rnf httpStatus
