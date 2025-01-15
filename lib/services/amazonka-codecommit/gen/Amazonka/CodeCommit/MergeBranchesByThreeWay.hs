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
-- Module      : Amazonka.CodeCommit.MergeBranchesByThreeWay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two specified branches using the three-way merge strategy.
module Amazonka.CodeCommit.MergeBranchesByThreeWay
  ( -- * Creating a Request
    MergeBranchesByThreeWay (..),
    newMergeBranchesByThreeWay,

    -- * Request Lenses
    mergeBranchesByThreeWay_authorName,
    mergeBranchesByThreeWay_commitMessage,
    mergeBranchesByThreeWay_conflictDetailLevel,
    mergeBranchesByThreeWay_conflictResolution,
    mergeBranchesByThreeWay_conflictResolutionStrategy,
    mergeBranchesByThreeWay_email,
    mergeBranchesByThreeWay_keepEmptyFolders,
    mergeBranchesByThreeWay_targetBranch,
    mergeBranchesByThreeWay_repositoryName,
    mergeBranchesByThreeWay_sourceCommitSpecifier,
    mergeBranchesByThreeWay_destinationCommitSpecifier,

    -- * Destructuring the Response
    MergeBranchesByThreeWayResponse (..),
    newMergeBranchesByThreeWayResponse,

    -- * Response Lenses
    mergeBranchesByThreeWayResponse_commitId,
    mergeBranchesByThreeWayResponse_treeId,
    mergeBranchesByThreeWayResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMergeBranchesByThreeWay' smart constructor.
data MergeBranchesByThreeWay = MergeBranchesByThreeWay'
  { -- | The name of the author who created the commit. This information is used
    -- as both the author and committer for the commit.
    authorName :: Prelude.Maybe Prelude.Text,
    -- | The commit message to include in the commit information for the merge.
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
    -- structure if the changes leave the folders empty. If true, a .gitkeep
    -- file is created for empty folders. The default is false.
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
-- Create a value of 'MergeBranchesByThreeWay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorName', 'mergeBranchesByThreeWay_authorName' - The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
--
-- 'commitMessage', 'mergeBranchesByThreeWay_commitMessage' - The commit message to include in the commit information for the merge.
--
-- 'conflictDetailLevel', 'mergeBranchesByThreeWay_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'conflictResolution', 'mergeBranchesByThreeWay_conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
--
-- 'conflictResolutionStrategy', 'mergeBranchesByThreeWay_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'email', 'mergeBranchesByThreeWay_email' - The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
--
-- 'keepEmptyFolders', 'mergeBranchesByThreeWay_keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a .gitkeep
-- file is created for empty folders. The default is false.
--
-- 'targetBranch', 'mergeBranchesByThreeWay_targetBranch' - The branch where the merge is applied.
--
-- 'repositoryName', 'mergeBranchesByThreeWay_repositoryName' - The name of the repository where you want to merge two branches.
--
-- 'sourceCommitSpecifier', 'mergeBranchesByThreeWay_sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'destinationCommitSpecifier', 'mergeBranchesByThreeWay_destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
newMergeBranchesByThreeWay ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'sourceCommitSpecifier'
  Prelude.Text ->
  -- | 'destinationCommitSpecifier'
  Prelude.Text ->
  MergeBranchesByThreeWay
newMergeBranchesByThreeWay
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    MergeBranchesByThreeWay'
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
mergeBranchesByThreeWay_authorName :: Lens.Lens' MergeBranchesByThreeWay (Prelude.Maybe Prelude.Text)
mergeBranchesByThreeWay_authorName = Lens.lens (\MergeBranchesByThreeWay' {authorName} -> authorName) (\s@MergeBranchesByThreeWay' {} a -> s {authorName = a} :: MergeBranchesByThreeWay)

-- | The commit message to include in the commit information for the merge.
mergeBranchesByThreeWay_commitMessage :: Lens.Lens' MergeBranchesByThreeWay (Prelude.Maybe Prelude.Text)
mergeBranchesByThreeWay_commitMessage = Lens.lens (\MergeBranchesByThreeWay' {commitMessage} -> commitMessage) (\s@MergeBranchesByThreeWay' {} a -> s {commitMessage = a} :: MergeBranchesByThreeWay)

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
mergeBranchesByThreeWay_conflictDetailLevel :: Lens.Lens' MergeBranchesByThreeWay (Prelude.Maybe ConflictDetailLevelTypeEnum)
mergeBranchesByThreeWay_conflictDetailLevel = Lens.lens (\MergeBranchesByThreeWay' {conflictDetailLevel} -> conflictDetailLevel) (\s@MergeBranchesByThreeWay' {} a -> s {conflictDetailLevel = a} :: MergeBranchesByThreeWay)

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
mergeBranchesByThreeWay_conflictResolution :: Lens.Lens' MergeBranchesByThreeWay (Prelude.Maybe ConflictResolution)
mergeBranchesByThreeWay_conflictResolution = Lens.lens (\MergeBranchesByThreeWay' {conflictResolution} -> conflictResolution) (\s@MergeBranchesByThreeWay' {} a -> s {conflictResolution = a} :: MergeBranchesByThreeWay)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
mergeBranchesByThreeWay_conflictResolutionStrategy :: Lens.Lens' MergeBranchesByThreeWay (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
mergeBranchesByThreeWay_conflictResolutionStrategy = Lens.lens (\MergeBranchesByThreeWay' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@MergeBranchesByThreeWay' {} a -> s {conflictResolutionStrategy = a} :: MergeBranchesByThreeWay)

-- | The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
mergeBranchesByThreeWay_email :: Lens.Lens' MergeBranchesByThreeWay (Prelude.Maybe Prelude.Text)
mergeBranchesByThreeWay_email = Lens.lens (\MergeBranchesByThreeWay' {email} -> email) (\s@MergeBranchesByThreeWay' {} a -> s {email = a} :: MergeBranchesByThreeWay)

-- | If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a .gitkeep
-- file is created for empty folders. The default is false.
mergeBranchesByThreeWay_keepEmptyFolders :: Lens.Lens' MergeBranchesByThreeWay (Prelude.Maybe Prelude.Bool)
mergeBranchesByThreeWay_keepEmptyFolders = Lens.lens (\MergeBranchesByThreeWay' {keepEmptyFolders} -> keepEmptyFolders) (\s@MergeBranchesByThreeWay' {} a -> s {keepEmptyFolders = a} :: MergeBranchesByThreeWay)

-- | The branch where the merge is applied.
mergeBranchesByThreeWay_targetBranch :: Lens.Lens' MergeBranchesByThreeWay (Prelude.Maybe Prelude.Text)
mergeBranchesByThreeWay_targetBranch = Lens.lens (\MergeBranchesByThreeWay' {targetBranch} -> targetBranch) (\s@MergeBranchesByThreeWay' {} a -> s {targetBranch = a} :: MergeBranchesByThreeWay)

-- | The name of the repository where you want to merge two branches.
mergeBranchesByThreeWay_repositoryName :: Lens.Lens' MergeBranchesByThreeWay Prelude.Text
mergeBranchesByThreeWay_repositoryName = Lens.lens (\MergeBranchesByThreeWay' {repositoryName} -> repositoryName) (\s@MergeBranchesByThreeWay' {} a -> s {repositoryName = a} :: MergeBranchesByThreeWay)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
mergeBranchesByThreeWay_sourceCommitSpecifier :: Lens.Lens' MergeBranchesByThreeWay Prelude.Text
mergeBranchesByThreeWay_sourceCommitSpecifier = Lens.lens (\MergeBranchesByThreeWay' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@MergeBranchesByThreeWay' {} a -> s {sourceCommitSpecifier = a} :: MergeBranchesByThreeWay)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
mergeBranchesByThreeWay_destinationCommitSpecifier :: Lens.Lens' MergeBranchesByThreeWay Prelude.Text
mergeBranchesByThreeWay_destinationCommitSpecifier = Lens.lens (\MergeBranchesByThreeWay' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@MergeBranchesByThreeWay' {} a -> s {destinationCommitSpecifier = a} :: MergeBranchesByThreeWay)

instance Core.AWSRequest MergeBranchesByThreeWay where
  type
    AWSResponse MergeBranchesByThreeWay =
      MergeBranchesByThreeWayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          MergeBranchesByThreeWayResponse'
            Prelude.<$> (x Data..?> "commitId")
            Prelude.<*> (x Data..?> "treeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MergeBranchesByThreeWay where
  hashWithSalt _salt MergeBranchesByThreeWay' {..} =
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

instance Prelude.NFData MergeBranchesByThreeWay where
  rnf MergeBranchesByThreeWay' {..} =
    Prelude.rnf authorName `Prelude.seq`
      Prelude.rnf commitMessage `Prelude.seq`
        Prelude.rnf conflictDetailLevel `Prelude.seq`
          Prelude.rnf conflictResolution `Prelude.seq`
            Prelude.rnf conflictResolutionStrategy `Prelude.seq`
              Prelude.rnf email `Prelude.seq`
                Prelude.rnf keepEmptyFolders `Prelude.seq`
                  Prelude.rnf targetBranch `Prelude.seq`
                    Prelude.rnf repositoryName `Prelude.seq`
                      Prelude.rnf sourceCommitSpecifier `Prelude.seq`
                        Prelude.rnf destinationCommitSpecifier

instance Data.ToHeaders MergeBranchesByThreeWay where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.MergeBranchesByThreeWay" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON MergeBranchesByThreeWay where
  toJSON MergeBranchesByThreeWay' {..} =
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

instance Data.ToPath MergeBranchesByThreeWay where
  toPath = Prelude.const "/"

instance Data.ToQuery MergeBranchesByThreeWay where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMergeBranchesByThreeWayResponse' smart constructor.
data MergeBranchesByThreeWayResponse = MergeBranchesByThreeWayResponse'
  { -- | The commit ID of the merge in the destination or target branch.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | The tree ID of the merge in the destination or target branch.
    treeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergeBranchesByThreeWayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'mergeBranchesByThreeWayResponse_commitId' - The commit ID of the merge in the destination or target branch.
--
-- 'treeId', 'mergeBranchesByThreeWayResponse_treeId' - The tree ID of the merge in the destination or target branch.
--
-- 'httpStatus', 'mergeBranchesByThreeWayResponse_httpStatus' - The response's http status code.
newMergeBranchesByThreeWayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MergeBranchesByThreeWayResponse
newMergeBranchesByThreeWayResponse pHttpStatus_ =
  MergeBranchesByThreeWayResponse'
    { commitId =
        Prelude.Nothing,
      treeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The commit ID of the merge in the destination or target branch.
mergeBranchesByThreeWayResponse_commitId :: Lens.Lens' MergeBranchesByThreeWayResponse (Prelude.Maybe Prelude.Text)
mergeBranchesByThreeWayResponse_commitId = Lens.lens (\MergeBranchesByThreeWayResponse' {commitId} -> commitId) (\s@MergeBranchesByThreeWayResponse' {} a -> s {commitId = a} :: MergeBranchesByThreeWayResponse)

-- | The tree ID of the merge in the destination or target branch.
mergeBranchesByThreeWayResponse_treeId :: Lens.Lens' MergeBranchesByThreeWayResponse (Prelude.Maybe Prelude.Text)
mergeBranchesByThreeWayResponse_treeId = Lens.lens (\MergeBranchesByThreeWayResponse' {treeId} -> treeId) (\s@MergeBranchesByThreeWayResponse' {} a -> s {treeId = a} :: MergeBranchesByThreeWayResponse)

-- | The response's http status code.
mergeBranchesByThreeWayResponse_httpStatus :: Lens.Lens' MergeBranchesByThreeWayResponse Prelude.Int
mergeBranchesByThreeWayResponse_httpStatus = Lens.lens (\MergeBranchesByThreeWayResponse' {httpStatus} -> httpStatus) (\s@MergeBranchesByThreeWayResponse' {} a -> s {httpStatus = a} :: MergeBranchesByThreeWayResponse)

instance
  Prelude.NFData
    MergeBranchesByThreeWayResponse
  where
  rnf MergeBranchesByThreeWayResponse' {..} =
    Prelude.rnf commitId `Prelude.seq`
      Prelude.rnf treeId `Prelude.seq`
        Prelude.rnf httpStatus
