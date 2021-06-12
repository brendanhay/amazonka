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
-- Module      : Network.AWS.CodeCommit.MergeBranchesByFastForward
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two branches using the fast-forward merge strategy.
module Network.AWS.CodeCommit.MergeBranchesByFastForward
  ( -- * Creating a Request
    MergeBranchesByFastForward (..),
    newMergeBranchesByFastForward,

    -- * Request Lenses
    mergeBranchesByFastForward_targetBranch,
    mergeBranchesByFastForward_repositoryName,
    mergeBranchesByFastForward_sourceCommitSpecifier,
    mergeBranchesByFastForward_destinationCommitSpecifier,

    -- * Destructuring the Response
    MergeBranchesByFastForwardResponse (..),
    newMergeBranchesByFastForwardResponse,

    -- * Response Lenses
    mergeBranchesByFastForwardResponse_commitId,
    mergeBranchesByFastForwardResponse_treeId,
    mergeBranchesByFastForwardResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newMergeBranchesByFastForward' smart constructor.
data MergeBranchesByFastForward = MergeBranchesByFastForward'
  { -- | The branch where the merge is applied.
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
-- Create a value of 'MergeBranchesByFastForward' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetBranch', 'mergeBranchesByFastForward_targetBranch' - The branch where the merge is applied.
--
-- 'repositoryName', 'mergeBranchesByFastForward_repositoryName' - The name of the repository where you want to merge two branches.
--
-- 'sourceCommitSpecifier', 'mergeBranchesByFastForward_sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'destinationCommitSpecifier', 'mergeBranchesByFastForward_destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
newMergeBranchesByFastForward ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'sourceCommitSpecifier'
  Core.Text ->
  -- | 'destinationCommitSpecifier'
  Core.Text ->
  MergeBranchesByFastForward
newMergeBranchesByFastForward
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    MergeBranchesByFastForward'
      { targetBranch =
          Core.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_
      }

-- | The branch where the merge is applied.
mergeBranchesByFastForward_targetBranch :: Lens.Lens' MergeBranchesByFastForward (Core.Maybe Core.Text)
mergeBranchesByFastForward_targetBranch = Lens.lens (\MergeBranchesByFastForward' {targetBranch} -> targetBranch) (\s@MergeBranchesByFastForward' {} a -> s {targetBranch = a} :: MergeBranchesByFastForward)

-- | The name of the repository where you want to merge two branches.
mergeBranchesByFastForward_repositoryName :: Lens.Lens' MergeBranchesByFastForward Core.Text
mergeBranchesByFastForward_repositoryName = Lens.lens (\MergeBranchesByFastForward' {repositoryName} -> repositoryName) (\s@MergeBranchesByFastForward' {} a -> s {repositoryName = a} :: MergeBranchesByFastForward)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
mergeBranchesByFastForward_sourceCommitSpecifier :: Lens.Lens' MergeBranchesByFastForward Core.Text
mergeBranchesByFastForward_sourceCommitSpecifier = Lens.lens (\MergeBranchesByFastForward' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@MergeBranchesByFastForward' {} a -> s {sourceCommitSpecifier = a} :: MergeBranchesByFastForward)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
mergeBranchesByFastForward_destinationCommitSpecifier :: Lens.Lens' MergeBranchesByFastForward Core.Text
mergeBranchesByFastForward_destinationCommitSpecifier = Lens.lens (\MergeBranchesByFastForward' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@MergeBranchesByFastForward' {} a -> s {destinationCommitSpecifier = a} :: MergeBranchesByFastForward)

instance Core.AWSRequest MergeBranchesByFastForward where
  type
    AWSResponse MergeBranchesByFastForward =
      MergeBranchesByFastForwardResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          MergeBranchesByFastForwardResponse'
            Core.<$> (x Core..?> "commitId")
            Core.<*> (x Core..?> "treeId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable MergeBranchesByFastForward

instance Core.NFData MergeBranchesByFastForward

instance Core.ToHeaders MergeBranchesByFastForward where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.MergeBranchesByFastForward" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON MergeBranchesByFastForward where
  toJSON MergeBranchesByFastForward' {..} =
    Core.object
      ( Core.catMaybes
          [ ("targetBranch" Core..=) Core.<$> targetBranch,
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

instance Core.ToPath MergeBranchesByFastForward where
  toPath = Core.const "/"

instance Core.ToQuery MergeBranchesByFastForward where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newMergeBranchesByFastForwardResponse' smart constructor.
data MergeBranchesByFastForwardResponse = MergeBranchesByFastForwardResponse'
  { -- | The commit ID of the merge in the destination or target branch.
    commitId :: Core.Maybe Core.Text,
    -- | The tree ID of the merge in the destination or target branch.
    treeId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MergeBranchesByFastForwardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'mergeBranchesByFastForwardResponse_commitId' - The commit ID of the merge in the destination or target branch.
--
-- 'treeId', 'mergeBranchesByFastForwardResponse_treeId' - The tree ID of the merge in the destination or target branch.
--
-- 'httpStatus', 'mergeBranchesByFastForwardResponse_httpStatus' - The response's http status code.
newMergeBranchesByFastForwardResponse ::
  -- | 'httpStatus'
  Core.Int ->
  MergeBranchesByFastForwardResponse
newMergeBranchesByFastForwardResponse pHttpStatus_ =
  MergeBranchesByFastForwardResponse'
    { commitId =
        Core.Nothing,
      treeId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The commit ID of the merge in the destination or target branch.
mergeBranchesByFastForwardResponse_commitId :: Lens.Lens' MergeBranchesByFastForwardResponse (Core.Maybe Core.Text)
mergeBranchesByFastForwardResponse_commitId = Lens.lens (\MergeBranchesByFastForwardResponse' {commitId} -> commitId) (\s@MergeBranchesByFastForwardResponse' {} a -> s {commitId = a} :: MergeBranchesByFastForwardResponse)

-- | The tree ID of the merge in the destination or target branch.
mergeBranchesByFastForwardResponse_treeId :: Lens.Lens' MergeBranchesByFastForwardResponse (Core.Maybe Core.Text)
mergeBranchesByFastForwardResponse_treeId = Lens.lens (\MergeBranchesByFastForwardResponse' {treeId} -> treeId) (\s@MergeBranchesByFastForwardResponse' {} a -> s {treeId = a} :: MergeBranchesByFastForwardResponse)

-- | The response's http status code.
mergeBranchesByFastForwardResponse_httpStatus :: Lens.Lens' MergeBranchesByFastForwardResponse Core.Int
mergeBranchesByFastForwardResponse_httpStatus = Lens.lens (\MergeBranchesByFastForwardResponse' {httpStatus} -> httpStatus) (\s@MergeBranchesByFastForwardResponse' {} a -> s {httpStatus = a} :: MergeBranchesByFastForwardResponse)

instance
  Core.NFData
    MergeBranchesByFastForwardResponse
