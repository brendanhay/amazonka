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
-- Module      : Amazonka.CodeCommit.GetBranch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a repository branch, including its name and
-- the last commit ID.
module Amazonka.CodeCommit.GetBranch
  ( -- * Creating a Request
    GetBranch (..),
    newGetBranch,

    -- * Request Lenses
    getBranch_branchName,
    getBranch_repositoryName,

    -- * Destructuring the Response
    GetBranchResponse (..),
    newGetBranchResponse,

    -- * Response Lenses
    getBranchResponse_branch,
    getBranchResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a get branch operation.
--
-- /See:/ 'newGetBranch' smart constructor.
data GetBranch = GetBranch'
  { -- | The name of the branch for which you want to retrieve information.
    branchName :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the branch for which you want
    -- to retrieve information.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchName', 'getBranch_branchName' - The name of the branch for which you want to retrieve information.
--
-- 'repositoryName', 'getBranch_repositoryName' - The name of the repository that contains the branch for which you want
-- to retrieve information.
newGetBranch ::
  GetBranch
newGetBranch =
  GetBranch'
    { branchName = Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | The name of the branch for which you want to retrieve information.
getBranch_branchName :: Lens.Lens' GetBranch (Prelude.Maybe Prelude.Text)
getBranch_branchName = Lens.lens (\GetBranch' {branchName} -> branchName) (\s@GetBranch' {} a -> s {branchName = a} :: GetBranch)

-- | The name of the repository that contains the branch for which you want
-- to retrieve information.
getBranch_repositoryName :: Lens.Lens' GetBranch (Prelude.Maybe Prelude.Text)
getBranch_repositoryName = Lens.lens (\GetBranch' {repositoryName} -> repositoryName) (\s@GetBranch' {} a -> s {repositoryName = a} :: GetBranch)

instance Core.AWSRequest GetBranch where
  type AWSResponse GetBranch = GetBranchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBranchResponse'
            Prelude.<$> (x Data..?> "branch")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBranch where
  hashWithSalt _salt GetBranch' {..} =
    _salt
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData GetBranch where
  rnf GetBranch' {..} =
    Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToHeaders GetBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.GetBranch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBranch where
  toJSON GetBranch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("branchName" Data..=) Prelude.<$> branchName,
            ("repositoryName" Data..=)
              Prelude.<$> repositoryName
          ]
      )

instance Data.ToPath GetBranch where
  toPath = Prelude.const "/"

instance Data.ToQuery GetBranch where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a get branch operation.
--
-- /See:/ 'newGetBranchResponse' smart constructor.
data GetBranchResponse = GetBranchResponse'
  { -- | The name of the branch.
    branch :: Prelude.Maybe BranchInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'getBranchResponse_branch' - The name of the branch.
--
-- 'httpStatus', 'getBranchResponse_httpStatus' - The response's http status code.
newGetBranchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBranchResponse
newGetBranchResponse pHttpStatus_ =
  GetBranchResponse'
    { branch = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the branch.
getBranchResponse_branch :: Lens.Lens' GetBranchResponse (Prelude.Maybe BranchInfo)
getBranchResponse_branch = Lens.lens (\GetBranchResponse' {branch} -> branch) (\s@GetBranchResponse' {} a -> s {branch = a} :: GetBranchResponse)

-- | The response's http status code.
getBranchResponse_httpStatus :: Lens.Lens' GetBranchResponse Prelude.Int
getBranchResponse_httpStatus = Lens.lens (\GetBranchResponse' {httpStatus} -> httpStatus) (\s@GetBranchResponse' {} a -> s {httpStatus = a} :: GetBranchResponse)

instance Prelude.NFData GetBranchResponse where
  rnf GetBranchResponse' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf httpStatus
