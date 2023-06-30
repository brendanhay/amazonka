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
-- Module      : Amazonka.Amplify.DeleteBranch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a branch for an Amplify app.
module Amazonka.Amplify.DeleteBranch
  ( -- * Creating a Request
    DeleteBranch (..),
    newDeleteBranch,

    -- * Request Lenses
    deleteBranch_appId,
    deleteBranch_branchName,

    -- * Destructuring the Response
    DeleteBranchResponse (..),
    newDeleteBranchResponse,

    -- * Response Lenses
    deleteBranchResponse_httpStatus,
    deleteBranchResponse_branch,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the delete branch request.
--
-- /See:/ 'newDeleteBranch' smart constructor.
data DeleteBranch = DeleteBranch'
  { -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the branch.
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
-- 'appId', 'deleteBranch_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'deleteBranch_branchName' - The name for the branch.
newDeleteBranch ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  DeleteBranch
newDeleteBranch pAppId_ pBranchName_ =
  DeleteBranch'
    { appId = pAppId_,
      branchName = pBranchName_
    }

-- | The unique ID for an Amplify app.
deleteBranch_appId :: Lens.Lens' DeleteBranch Prelude.Text
deleteBranch_appId = Lens.lens (\DeleteBranch' {appId} -> appId) (\s@DeleteBranch' {} a -> s {appId = a} :: DeleteBranch)

-- | The name for the branch.
deleteBranch_branchName :: Lens.Lens' DeleteBranch Prelude.Text
deleteBranch_branchName = Lens.lens (\DeleteBranch' {branchName} -> branchName) (\s@DeleteBranch' {} a -> s {branchName = a} :: DeleteBranch)

instance Core.AWSRequest DeleteBranch where
  type AWSResponse DeleteBranch = DeleteBranchResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBranchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "branch")
      )

instance Prelude.Hashable DeleteBranch where
  hashWithSalt _salt DeleteBranch' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` branchName

instance Prelude.NFData DeleteBranch where
  rnf DeleteBranch' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf branchName

instance Data.ToHeaders DeleteBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBranch where
  toPath DeleteBranch' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/branches/",
        Data.toBS branchName
      ]

instance Data.ToQuery DeleteBranch where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the delete branch request.
--
-- /See:/ 'newDeleteBranchResponse' smart constructor.
data DeleteBranchResponse = DeleteBranchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The branch for an Amplify app, which maps to a third-party repository
    -- branch.
    branch :: Branch
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBranchResponse_httpStatus' - The response's http status code.
--
-- 'branch', 'deleteBranchResponse_branch' - The branch for an Amplify app, which maps to a third-party repository
-- branch.
newDeleteBranchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'branch'
  Branch ->
  DeleteBranchResponse
newDeleteBranchResponse pHttpStatus_ pBranch_ =
  DeleteBranchResponse'
    { httpStatus = pHttpStatus_,
      branch = pBranch_
    }

-- | The response's http status code.
deleteBranchResponse_httpStatus :: Lens.Lens' DeleteBranchResponse Prelude.Int
deleteBranchResponse_httpStatus = Lens.lens (\DeleteBranchResponse' {httpStatus} -> httpStatus) (\s@DeleteBranchResponse' {} a -> s {httpStatus = a} :: DeleteBranchResponse)

-- | The branch for an Amplify app, which maps to a third-party repository
-- branch.
deleteBranchResponse_branch :: Lens.Lens' DeleteBranchResponse Branch
deleteBranchResponse_branch = Lens.lens (\DeleteBranchResponse' {branch} -> branch) (\s@DeleteBranchResponse' {} a -> s {branch = a} :: DeleteBranchResponse)

instance Prelude.NFData DeleteBranchResponse where
  rnf DeleteBranchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf branch
