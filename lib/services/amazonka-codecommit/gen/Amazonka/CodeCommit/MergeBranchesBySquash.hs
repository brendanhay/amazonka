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
-- Module      : Amazonka.CodeCommit.MergeBranchesBySquash
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two branches using the squash merge strategy.
module Amazonka.CodeCommit.MergeBranchesBySquash
  ( -- * Creating a Request
    MergeBranchesBySquash (..),
    newMergeBranchesBySquash,

    -- * Request Lenses
    mergeBranchesBySquash_authorName,
    mergeBranchesBySquash_commitMessage,
    mergeBranchesBySquash_conflictDetailLevel,
    mergeBranchesBySquash_conflictResolution,
    mergeBranchesBySquash_conflictResolutionStrategy,
    mergeBranchesBySquash_email,
    mergeBranchesBySquash_keepEmptyFolders,
    mergeBranchesBySquash_targetBranch,
    mergeBranchesBySquash_repositoryName,
    mergeBranchesBySquash_sourceCommitSpecifier,
    mergeBranchesBySquash_destinationCommitSpecifier,

    -- * Destructuring the Response
    MergeBranchesBySquashResponse (..),
    newMergeBranchesBySquashResponse,

    -- * Response Lenses
    mergeBranchesBySquashResponse_commitId,
    mergeBranchesBySquashResponse_treeId,
    mergeBranchesBySquashResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMergeBranchesBySquash' smart constructor.
data MergeBranchesBySquash = MergeBranchesBySquash'
  { -- | The name of the author who created the commit. This information is used
    -- as both the author and committer for the commit.
    authorName :: Prelude.Maybe Prelude.Text,
    -- | The commit message for the merge.
    commitMessage :: Prelude.Maybe Prelude.Text,
    -- | The level of conflict detail to use. If unspecified, the default
    -- FILE_LEVEL is used, which returns a not-mergeable result if the same
    -- file has differences in both branches. If LINE_LEVEL is specified, a
    -- conflict is considered not mergeable if the same file in both branches
    -- has differences on the same line.
    conflictDetailLevel :: Prelude.Maybe ConflictDetailLevelTypeEnum,
    -- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
    -- use when resolving conflicts during a merge.
    conflictResolution :: Prelude.Maybe ConflictResolution,
    -- | Specifies which branch to use when resolving conflicts, or whether to
    -- attempt automatically merging two versions of a file. The default is
    -- NONE, which requires any conflicts to be resolved manually before the
    -- merge operation is successful.
    conflictResolutionStrategy :: Prelude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | The email address of the person merging the branches. This information
    -- is used in the commit information for the merge.
    email :: Prelude.Maybe Prelude.Text,
    -- | If the commit contains deletions, whether to keep a folder or folder
    -- structure if the changes leave the folders empty. If this is specified
    -- as true, a .gitkeep file is created for empty folders. The default is
    -- false.
    keepEmptyFolders :: Prelude.Maybe Prelude.Bool,
    -- | The branch where the merge is applied.
    targetBranch :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where you want to merge two branches.
    repositoryName :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergeBranchesBySquash' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorName', 'mergeBranchesBySquash_authorName' - The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
--
-- 'commitMessage', 'mergeBranchesBySquash_commitMessage' - The commit message for the merge.
--
-- 'conflictDetailLevel', 'mergeBranchesBySquash_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'conflictResolution', 'mergeBranchesBySquash_conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
--
-- 'conflictResolutionStrategy', 'mergeBranchesBySquash_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'email', 'mergeBranchesBySquash_email' - The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
--
-- 'keepEmptyFolders', 'mergeBranchesBySquash_keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If this is specified
-- as true, a .gitkeep file is created for empty folders. The default is
-- false.
--
-- 'targetBranch', 'mergeBranchesBySquash_targetBranch' - The branch where the merge is applied.
--
-- 'repositoryName', 'mergeBranchesBySquash_repositoryName' - The name of the repository where you want to merge two branches.
--
-- 'sourceCommitSpecifier', 'mergeBranchesBySquash_sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'destinationCommitSpecifier', 'mergeBranchesBySquash_destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
newMergeBranchesBySquash ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'sourceCommitSpecifier'
  Prelude.Text ->
  -- | 'destinationCommitSpecifier'
  Prelude.Text ->
  MergeBranchesBySquash
newMergeBranchesBySquash
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    MergeBranchesBySquash'
      { authorName =
          Prelude.Nothing,
        commitMessage = Prelude.Nothing,
        conflictDetailLevel = Prelude.Nothing,
        conflictResolution = Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        email = Prelude.Nothing,
        keepEmptyFolders = Prelude.Nothing,
        targetBranch = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_
      }

-- | The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
mergeBranchesBySquash_authorName :: Lens.Lens' MergeBranchesBySquash (Prelude.Maybe Prelude.Text)
mergeBranchesBySquash_authorName = Lens.lens (\MergeBranchesBySquash' {authorName} -> authorName) (\s@MergeBranchesBySquash' {} a -> s {authorName = a} :: MergeBranchesBySquash)

-- | The commit message for the merge.
mergeBranchesBySquash_commitMessage :: Lens.Lens' MergeBranchesBySquash (Prelude.Maybe Prelude.Text)
mergeBranchesBySquash_commitMessage = Lens.lens (\MergeBranchesBySquash' {commitMessage} -> commitMessage) (\s@MergeBranchesBySquash' {} a -> s {commitMessage = a} :: MergeBranchesBySquash)

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
mergeBranchesBySquash_conflictDetailLevel :: Lens.Lens' MergeBranchesBySquash (Prelude.Maybe ConflictDetailLevelTypeEnum)
mergeBranchesBySquash_conflictDetailLevel = Lens.lens (\MergeBranchesBySquash' {conflictDetailLevel} -> conflictDetailLevel) (\s@MergeBranchesBySquash' {} a -> s {conflictDetailLevel = a} :: MergeBranchesBySquash)

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
mergeBranchesBySquash_conflictResolution :: Lens.Lens' MergeBranchesBySquash (Prelude.Maybe ConflictResolution)
mergeBranchesBySquash_conflictResolution = Lens.lens (\MergeBranchesBySquash' {conflictResolution} -> conflictResolution) (\s@MergeBranchesBySquash' {} a -> s {conflictResolution = a} :: MergeBranchesBySquash)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
mergeBranchesBySquash_conflictResolutionStrategy :: Lens.Lens' MergeBranchesBySquash (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
mergeBranchesBySquash_conflictResolutionStrategy = Lens.lens (\MergeBranchesBySquash' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@MergeBranchesBySquash' {} a -> s {conflictResolutionStrategy = a} :: MergeBranchesBySquash)

-- | The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
mergeBranchesBySquash_email :: Lens.Lens' MergeBranchesBySquash (Prelude.Maybe Prelude.Text)
mergeBranchesBySquash_email = Lens.lens (\MergeBranchesBySquash' {email} -> email) (\s@MergeBranchesBySquash' {} a -> s {email = a} :: MergeBranchesBySquash)

-- | If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If this is specified
-- as true, a .gitkeep file is created for empty folders. The default is
-- false.
mergeBranchesBySquash_keepEmptyFolders :: Lens.Lens' MergeBranchesBySquash (Prelude.Maybe Prelude.Bool)
mergeBranchesBySquash_keepEmptyFolders = Lens.lens (\MergeBranchesBySquash' {keepEmptyFolders} -> keepEmptyFolders) (\s@MergeBranchesBySquash' {} a -> s {keepEmptyFolders = a} :: MergeBranchesBySquash)

-- | The branch where the merge is applied.
mergeBranchesBySquash_targetBranch :: Lens.Lens' MergeBranchesBySquash (Prelude.Maybe Prelude.Text)
mergeBranchesBySquash_targetBranch = Lens.lens (\MergeBranchesBySquash' {targetBranch} -> targetBranch) (\s@MergeBranchesBySquash' {} a -> s {targetBranch = a} :: MergeBranchesBySquash)

-- | The name of the repository where you want to merge two branches.
mergeBranchesBySquash_repositoryName :: Lens.Lens' MergeBranchesBySquash Prelude.Text
mergeBranchesBySquash_repositoryName = Lens.lens (\MergeBranchesBySquash' {repositoryName} -> repositoryName) (\s@MergeBranchesBySquash' {} a -> s {repositoryName = a} :: MergeBranchesBySquash)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
mergeBranchesBySquash_sourceCommitSpecifier :: Lens.Lens' MergeBranchesBySquash Prelude.Text
mergeBranchesBySquash_sourceCommitSpecifier = Lens.lens (\MergeBranchesBySquash' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@MergeBranchesBySquash' {} a -> s {sourceCommitSpecifier = a} :: MergeBranchesBySquash)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
mergeBranchesBySquash_destinationCommitSpecifier :: Lens.Lens' MergeBranchesBySquash Prelude.Text
mergeBranchesBySquash_destinationCommitSpecifier = Lens.lens (\MergeBranchesBySquash' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@MergeBranchesBySquash' {} a -> s {destinationCommitSpecifier = a} :: MergeBranchesBySquash)

instance Core.AWSRequest MergeBranchesBySquash where
  type
    AWSResponse MergeBranchesBySquash =
      MergeBranchesBySquashResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          MergeBranchesBySquashResponse'
            Prelude.<$> (x Data..?> "commitId")
            Prelude.<*> (x Data..?> "treeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MergeBranchesBySquash where
  hashWithSalt _salt MergeBranchesBySquash' {..} =
    _salt
      `Prelude.hashWithSalt` authorName
      `Prelude.hashWithSalt` commitMessage
      `Prelude.hashWithSalt` conflictDetailLevel
      `Prelude.hashWithSalt` conflictResolution
      `Prelude.hashWithSalt` conflictResolutionStrategy
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` keepEmptyFolders
      `Prelude.hashWithSalt` targetBranch
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` sourceCommitSpecifier
      `Prelude.hashWithSalt` destinationCommitSpecifier

instance Prelude.NFData MergeBranchesBySquash where
  rnf MergeBranchesBySquash' {..} =
    Prelude.rnf authorName
      `Prelude.seq` Prelude.rnf commitMessage
      `Prelude.seq` Prelude.rnf conflictDetailLevel
      `Prelude.seq` Prelude.rnf conflictResolution
      `Prelude.seq` Prelude.rnf conflictResolutionStrategy
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf keepEmptyFolders
      `Prelude.seq` Prelude.rnf targetBranch
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf sourceCommitSpecifier
      `Prelude.seq` Prelude.rnf destinationCommitSpecifier

instance Data.ToHeaders MergeBranchesBySquash where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.MergeBranchesBySquash" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON MergeBranchesBySquash where
  toJSON MergeBranchesBySquash' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorName" Data..=) Prelude.<$> authorName,
            ("commitMessage" Data..=) Prelude.<$> commitMessage,
            ("conflictDetailLevel" Data..=)
              Prelude.<$> conflictDetailLevel,
            ("conflictResolution" Data..=)
              Prelude.<$> conflictResolution,
            ("conflictResolutionStrategy" Data..=)
              Prelude.<$> conflictResolutionStrategy,
            ("email" Data..=) Prelude.<$> email,
            ("keepEmptyFolders" Data..=)
              Prelude.<$> keepEmptyFolders,
            ("targetBranch" Data..=) Prelude.<$> targetBranch,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ( "sourceCommitSpecifier"
                  Data..= sourceCommitSpecifier
              ),
            Prelude.Just
              ( "destinationCommitSpecifier"
                  Data..= destinationCommitSpecifier
              )
          ]
      )

instance Data.ToPath MergeBranchesBySquash where
  toPath = Prelude.const "/"

instance Data.ToQuery MergeBranchesBySquash where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMergeBranchesBySquashResponse' smart constructor.
data MergeBranchesBySquashResponse = MergeBranchesBySquashResponse'
  { -- | The commit ID of the merge in the destination or target branch.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | The tree ID of the merge in the destination or target branch.
    treeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergeBranchesBySquashResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'mergeBranchesBySquashResponse_commitId' - The commit ID of the merge in the destination or target branch.
--
-- 'treeId', 'mergeBranchesBySquashResponse_treeId' - The tree ID of the merge in the destination or target branch.
--
-- 'httpStatus', 'mergeBranchesBySquashResponse_httpStatus' - The response's http status code.
newMergeBranchesBySquashResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MergeBranchesBySquashResponse
newMergeBranchesBySquashResponse pHttpStatus_ =
  MergeBranchesBySquashResponse'
    { commitId =
        Prelude.Nothing,
      treeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The commit ID of the merge in the destination or target branch.
mergeBranchesBySquashResponse_commitId :: Lens.Lens' MergeBranchesBySquashResponse (Prelude.Maybe Prelude.Text)
mergeBranchesBySquashResponse_commitId = Lens.lens (\MergeBranchesBySquashResponse' {commitId} -> commitId) (\s@MergeBranchesBySquashResponse' {} a -> s {commitId = a} :: MergeBranchesBySquashResponse)

-- | The tree ID of the merge in the destination or target branch.
mergeBranchesBySquashResponse_treeId :: Lens.Lens' MergeBranchesBySquashResponse (Prelude.Maybe Prelude.Text)
mergeBranchesBySquashResponse_treeId = Lens.lens (\MergeBranchesBySquashResponse' {treeId} -> treeId) (\s@MergeBranchesBySquashResponse' {} a -> s {treeId = a} :: MergeBranchesBySquashResponse)

-- | The response's http status code.
mergeBranchesBySquashResponse_httpStatus :: Lens.Lens' MergeBranchesBySquashResponse Prelude.Int
mergeBranchesBySquashResponse_httpStatus = Lens.lens (\MergeBranchesBySquashResponse' {httpStatus} -> httpStatus) (\s@MergeBranchesBySquashResponse' {} a -> s {httpStatus = a} :: MergeBranchesBySquashResponse)

instance Prelude.NFData MergeBranchesBySquashResponse where
  rnf MergeBranchesBySquashResponse' {..} =
    Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf treeId
      `Prelude.seq` Prelude.rnf httpStatus
