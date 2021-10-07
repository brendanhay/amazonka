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
-- Module      : Network.AWS.CodeCommit.DeleteBranch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a branch from a repository, unless that branch is the default
-- branch for the repository.
module Network.AWS.CodeCommit.DeleteBranch
  ( -- * Creating a Request
    DeleteBranch (..),
    newDeleteBranch,

    -- * Request Lenses
    deleteBranch_repositoryName,
    deleteBranch_branchName,

    -- * Destructuring the Response
    DeleteBranchResponse (..),
    newDeleteBranchResponse,

    -- * Response Lenses
    deleteBranchResponse_deletedBranch,
    deleteBranchResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a delete branch operation.
--
-- /See:/ 'newDeleteBranch' smart constructor.
data DeleteBranch = DeleteBranch'
  { -- | The name of the repository that contains the branch to be deleted.
    repositoryName :: Prelude.Text,
    -- | The name of the branch to delete.
    branchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'deleteBranch_repositoryName' - The name of the repository that contains the branch to be deleted.
--
-- 'branchName', 'deleteBranch_branchName' - The name of the branch to delete.
newDeleteBranch ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  DeleteBranch
newDeleteBranch pRepositoryName_ pBranchName_ =
  DeleteBranch'
    { repositoryName = pRepositoryName_,
      branchName = pBranchName_
    }

-- | The name of the repository that contains the branch to be deleted.
deleteBranch_repositoryName :: Lens.Lens' DeleteBranch Prelude.Text
deleteBranch_repositoryName = Lens.lens (\DeleteBranch' {repositoryName} -> repositoryName) (\s@DeleteBranch' {} a -> s {repositoryName = a} :: DeleteBranch)

-- | The name of the branch to delete.
deleteBranch_branchName :: Lens.Lens' DeleteBranch Prelude.Text
deleteBranch_branchName = Lens.lens (\DeleteBranch' {branchName} -> branchName) (\s@DeleteBranch' {} a -> s {branchName = a} :: DeleteBranch)

instance Core.AWSRequest DeleteBranch where
  type AWSResponse DeleteBranch = DeleteBranchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBranchResponse'
            Prelude.<$> (x Core..?> "deletedBranch")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBranch

instance Prelude.NFData DeleteBranch

instance Core.ToHeaders DeleteBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.DeleteBranch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteBranch where
  toJSON DeleteBranch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just ("branchName" Core..= branchName)
          ]
      )

instance Core.ToPath DeleteBranch where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteBranch where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a delete branch operation.
--
-- /See:/ 'newDeleteBranchResponse' smart constructor.
data DeleteBranchResponse = DeleteBranchResponse'
  { -- | Information about the branch deleted by the operation, including the
    -- branch name and the commit ID that was the tip of the branch.
    deletedBranch :: Prelude.Maybe BranchInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletedBranch', 'deleteBranchResponse_deletedBranch' - Information about the branch deleted by the operation, including the
-- branch name and the commit ID that was the tip of the branch.
--
-- 'httpStatus', 'deleteBranchResponse_httpStatus' - The response's http status code.
newDeleteBranchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBranchResponse
newDeleteBranchResponse pHttpStatus_ =
  DeleteBranchResponse'
    { deletedBranch =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the branch deleted by the operation, including the
-- branch name and the commit ID that was the tip of the branch.
deleteBranchResponse_deletedBranch :: Lens.Lens' DeleteBranchResponse (Prelude.Maybe BranchInfo)
deleteBranchResponse_deletedBranch = Lens.lens (\DeleteBranchResponse' {deletedBranch} -> deletedBranch) (\s@DeleteBranchResponse' {} a -> s {deletedBranch = a} :: DeleteBranchResponse)

-- | The response's http status code.
deleteBranchResponse_httpStatus :: Lens.Lens' DeleteBranchResponse Prelude.Int
deleteBranchResponse_httpStatus = Lens.lens (\DeleteBranchResponse' {httpStatus} -> httpStatus) (\s@DeleteBranchResponse' {} a -> s {httpStatus = a} :: DeleteBranchResponse)

instance Prelude.NFData DeleteBranchResponse
