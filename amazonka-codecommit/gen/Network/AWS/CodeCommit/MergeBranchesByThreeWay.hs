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
-- Module      : Network.AWS.CodeCommit.MergeBranchesByThreeWay
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two specified branches using the three-way merge strategy.
module Network.AWS.CodeCommit.MergeBranchesByThreeWay
  ( -- * Creating a Request
    MergeBranchesByThreeWay (..),
    newMergeBranchesByThreeWay,

    -- * Request Lenses
    mergeBranchesByThreeWay_commitMessage,
    mergeBranchesByThreeWay_authorName,
    mergeBranchesByThreeWay_email,
    mergeBranchesByThreeWay_conflictDetailLevel,
    mergeBranchesByThreeWay_conflictResolutionStrategy,
    mergeBranchesByThreeWay_keepEmptyFolders,
    mergeBranchesByThreeWay_conflictResolution,
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newMergeBranchesByThreeWay' smart constructor.
data MergeBranchesByThreeWay = MergeBranchesByThreeWay'
  { -- | The commit message to include in the commit information for the merge.
    commitMessage :: Core.Maybe Core.Text,
    -- | The name of the author who created the commit. This information is used
    -- as both the author and committer for the commit.
    authorName :: Core.Maybe Core.Text,
    -- | The email address of the person merging the branches. This information
    -- is used in the commit information for the merge.
    email :: Core.Maybe Core.Text,
    -- | The level of conflict detail to use. If unspecified, the default
    -- FILE_LEVEL is used, which returns a not-mergeable result if the same
    -- file has differences in both branches. If LINE_LEVEL is specified, a
    -- conflict is considered not mergeable if the same file in both branches
    -- has differences on the same line.
    conflictDetailLevel :: Core.Maybe ConflictDetailLevelTypeEnum,
    -- | Specifies which branch to use when resolving conflicts, or whether to
    -- attempt automatically merging two versions of a file. The default is
    -- NONE, which requires any conflicts to be resolved manually before the
    -- merge operation is successful.
    conflictResolutionStrategy :: Core.Maybe ConflictResolutionStrategyTypeEnum,
    -- | If the commit contains deletions, whether to keep a folder or folder
    -- structure if the changes leave the folders empty. If true, a .gitkeep
    -- file is created for empty folders. The default is false.
    keepEmptyFolders :: Core.Maybe Core.Bool,
    -- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
    -- use when resolving conflicts during a merge.
    conflictResolution :: Core.Maybe ConflictResolution,
    -- | The branch where the merge is applied.
    targetBranch :: Core.Maybe Core.Text,
    -- | The name of the repository where you want to merge two branches.
    repositoryName :: Core.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Core.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MergeBranchesByThreeWay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitMessage', 'mergeBranchesByThreeWay_commitMessage' - The commit message to include in the commit information for the merge.
--
-- 'authorName', 'mergeBranchesByThreeWay_authorName' - The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
--
-- 'email', 'mergeBranchesByThreeWay_email' - The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
--
-- 'conflictDetailLevel', 'mergeBranchesByThreeWay_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'conflictResolutionStrategy', 'mergeBranchesByThreeWay_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'keepEmptyFolders', 'mergeBranchesByThreeWay_keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a .gitkeep
-- file is created for empty folders. The default is false.
--
-- 'conflictResolution', 'mergeBranchesByThreeWay_conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
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
  Core.Text ->
  -- | 'sourceCommitSpecifier'
  Core.Text ->
  -- | 'destinationCommitSpecifier'
  Core.Text ->
  MergeBranchesByThreeWay
newMergeBranchesByThreeWay
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    MergeBranchesByThreeWay'
      { commitMessage =
          Core.Nothing,
        authorName = Core.Nothing,
        email = Core.Nothing,
        conflictDetailLevel = Core.Nothing,
        conflictResolutionStrategy = Core.Nothing,
        keepEmptyFolders = Core.Nothing,
        conflictResolution = Core.Nothing,
        targetBranch = Core.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_
      }

-- | The commit message to include in the commit information for the merge.
mergeBranchesByThreeWay_commitMessage :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Core.Text)
mergeBranchesByThreeWay_commitMessage = Lens.lens (\MergeBranchesByThreeWay' {commitMessage} -> commitMessage) (\s@MergeBranchesByThreeWay' {} a -> s {commitMessage = a} :: MergeBranchesByThreeWay)

-- | The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
mergeBranchesByThreeWay_authorName :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Core.Text)
mergeBranchesByThreeWay_authorName = Lens.lens (\MergeBranchesByThreeWay' {authorName} -> authorName) (\s@MergeBranchesByThreeWay' {} a -> s {authorName = a} :: MergeBranchesByThreeWay)

-- | The email address of the person merging the branches. This information
-- is used in the commit information for the merge.
mergeBranchesByThreeWay_email :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Core.Text)
mergeBranchesByThreeWay_email = Lens.lens (\MergeBranchesByThreeWay' {email} -> email) (\s@MergeBranchesByThreeWay' {} a -> s {email = a} :: MergeBranchesByThreeWay)

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
mergeBranchesByThreeWay_conflictDetailLevel :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe ConflictDetailLevelTypeEnum)
mergeBranchesByThreeWay_conflictDetailLevel = Lens.lens (\MergeBranchesByThreeWay' {conflictDetailLevel} -> conflictDetailLevel) (\s@MergeBranchesByThreeWay' {} a -> s {conflictDetailLevel = a} :: MergeBranchesByThreeWay)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
mergeBranchesByThreeWay_conflictResolutionStrategy :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe ConflictResolutionStrategyTypeEnum)
mergeBranchesByThreeWay_conflictResolutionStrategy = Lens.lens (\MergeBranchesByThreeWay' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@MergeBranchesByThreeWay' {} a -> s {conflictResolutionStrategy = a} :: MergeBranchesByThreeWay)

-- | If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a .gitkeep
-- file is created for empty folders. The default is false.
mergeBranchesByThreeWay_keepEmptyFolders :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Core.Bool)
mergeBranchesByThreeWay_keepEmptyFolders = Lens.lens (\MergeBranchesByThreeWay' {keepEmptyFolders} -> keepEmptyFolders) (\s@MergeBranchesByThreeWay' {} a -> s {keepEmptyFolders = a} :: MergeBranchesByThreeWay)

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
mergeBranchesByThreeWay_conflictResolution :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe ConflictResolution)
mergeBranchesByThreeWay_conflictResolution = Lens.lens (\MergeBranchesByThreeWay' {conflictResolution} -> conflictResolution) (\s@MergeBranchesByThreeWay' {} a -> s {conflictResolution = a} :: MergeBranchesByThreeWay)

-- | The branch where the merge is applied.
mergeBranchesByThreeWay_targetBranch :: Lens.Lens' MergeBranchesByThreeWay (Core.Maybe Core.Text)
mergeBranchesByThreeWay_targetBranch = Lens.lens (\MergeBranchesByThreeWay' {targetBranch} -> targetBranch) (\s@MergeBranchesByThreeWay' {} a -> s {targetBranch = a} :: MergeBranchesByThreeWay)

-- | The name of the repository where you want to merge two branches.
mergeBranchesByThreeWay_repositoryName :: Lens.Lens' MergeBranchesByThreeWay Core.Text
mergeBranchesByThreeWay_repositoryName = Lens.lens (\MergeBranchesByThreeWay' {repositoryName} -> repositoryName) (\s@MergeBranchesByThreeWay' {} a -> s {repositoryName = a} :: MergeBranchesByThreeWay)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
mergeBranchesByThreeWay_sourceCommitSpecifier :: Lens.Lens' MergeBranchesByThreeWay Core.Text
mergeBranchesByThreeWay_sourceCommitSpecifier = Lens.lens (\MergeBranchesByThreeWay' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@MergeBranchesByThreeWay' {} a -> s {sourceCommitSpecifier = a} :: MergeBranchesByThreeWay)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
mergeBranchesByThreeWay_destinationCommitSpecifier :: Lens.Lens' MergeBranchesByThreeWay Core.Text
mergeBranchesByThreeWay_destinationCommitSpecifier = Lens.lens (\MergeBranchesByThreeWay' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@MergeBranchesByThreeWay' {} a -> s {destinationCommitSpecifier = a} :: MergeBranchesByThreeWay)

instance Core.AWSRequest MergeBranchesByThreeWay where
  type
    AWSResponse MergeBranchesByThreeWay =
      MergeBranchesByThreeWayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          MergeBranchesByThreeWayResponse'
            Core.<$> (x Core..?> "commitId")
            Core.<*> (x Core..?> "treeId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable MergeBranchesByThreeWay

instance Core.NFData MergeBranchesByThreeWay

instance Core.ToHeaders MergeBranchesByThreeWay where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.MergeBranchesByThreeWay" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON MergeBranchesByThreeWay where
  toJSON MergeBranchesByThreeWay' {..} =
    Core.object
      ( Core.catMaybes
          [ ("commitMessage" Core..=) Core.<$> commitMessage,
            ("authorName" Core..=) Core.<$> authorName,
            ("email" Core..=) Core.<$> email,
            ("conflictDetailLevel" Core..=)
              Core.<$> conflictDetailLevel,
            ("conflictResolutionStrategy" Core..=)
              Core.<$> conflictResolutionStrategy,
            ("keepEmptyFolders" Core..=)
              Core.<$> keepEmptyFolders,
            ("conflictResolution" Core..=)
              Core.<$> conflictResolution,
            ("targetBranch" Core..=) Core.<$> targetBranch,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ( "sourceCommitSpecifier"
                  Core..= sourceCommitSpecifier
              ),
            Core.Just
              ( "destinationCommitSpecifier"
                  Core..= destinationCommitSpecifier
              )
          ]
      )

instance Core.ToPath MergeBranchesByThreeWay where
  toPath = Core.const "/"

instance Core.ToQuery MergeBranchesByThreeWay where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newMergeBranchesByThreeWayResponse' smart constructor.
data MergeBranchesByThreeWayResponse = MergeBranchesByThreeWayResponse'
  { -- | The commit ID of the merge in the destination or target branch.
    commitId :: Core.Maybe Core.Text,
    -- | The tree ID of the merge in the destination or target branch.
    treeId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  MergeBranchesByThreeWayResponse
newMergeBranchesByThreeWayResponse pHttpStatus_ =
  MergeBranchesByThreeWayResponse'
    { commitId =
        Core.Nothing,
      treeId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The commit ID of the merge in the destination or target branch.
mergeBranchesByThreeWayResponse_commitId :: Lens.Lens' MergeBranchesByThreeWayResponse (Core.Maybe Core.Text)
mergeBranchesByThreeWayResponse_commitId = Lens.lens (\MergeBranchesByThreeWayResponse' {commitId} -> commitId) (\s@MergeBranchesByThreeWayResponse' {} a -> s {commitId = a} :: MergeBranchesByThreeWayResponse)

-- | The tree ID of the merge in the destination or target branch.
mergeBranchesByThreeWayResponse_treeId :: Lens.Lens' MergeBranchesByThreeWayResponse (Core.Maybe Core.Text)
mergeBranchesByThreeWayResponse_treeId = Lens.lens (\MergeBranchesByThreeWayResponse' {treeId} -> treeId) (\s@MergeBranchesByThreeWayResponse' {} a -> s {treeId = a} :: MergeBranchesByThreeWayResponse)

-- | The response's http status code.
mergeBranchesByThreeWayResponse_httpStatus :: Lens.Lens' MergeBranchesByThreeWayResponse Core.Int
mergeBranchesByThreeWayResponse_httpStatus = Lens.lens (\MergeBranchesByThreeWayResponse' {httpStatus} -> httpStatus) (\s@MergeBranchesByThreeWayResponse' {} a -> s {httpStatus = a} :: MergeBranchesByThreeWayResponse)

instance Core.NFData MergeBranchesByThreeWayResponse
