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
-- Module      : Network.AWS.CodeCommit.GetBranch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a repository branch, including its name and
-- the last commit ID.
module Network.AWS.CodeCommit.GetBranch
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get branch operation.
--
-- /See:/ 'newGetBranch' smart constructor.
data GetBranch = GetBranch'
  { -- | The name of the branch for which you want to retrieve information.
    branchName :: Core.Maybe Core.Text,
    -- | The name of the repository that contains the branch for which you want
    -- to retrieve information.
    repositoryName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { branchName = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | The name of the branch for which you want to retrieve information.
getBranch_branchName :: Lens.Lens' GetBranch (Core.Maybe Core.Text)
getBranch_branchName = Lens.lens (\GetBranch' {branchName} -> branchName) (\s@GetBranch' {} a -> s {branchName = a} :: GetBranch)

-- | The name of the repository that contains the branch for which you want
-- to retrieve information.
getBranch_repositoryName :: Lens.Lens' GetBranch (Core.Maybe Core.Text)
getBranch_repositoryName = Lens.lens (\GetBranch' {repositoryName} -> repositoryName) (\s@GetBranch' {} a -> s {repositoryName = a} :: GetBranch)

instance Core.AWSRequest GetBranch where
  type AWSResponse GetBranch = GetBranchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBranchResponse'
            Core.<$> (x Core..?> "branch")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBranch

instance Core.NFData GetBranch

instance Core.ToHeaders GetBranch where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("CodeCommit_20150413.GetBranch" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetBranch where
  toJSON GetBranch' {..} =
    Core.object
      ( Core.catMaybes
          [ ("branchName" Core..=) Core.<$> branchName,
            ("repositoryName" Core..=) Core.<$> repositoryName
          ]
      )

instance Core.ToPath GetBranch where
  toPath = Core.const "/"

instance Core.ToQuery GetBranch where
  toQuery = Core.const Core.mempty

-- | Represents the output of a get branch operation.
--
-- /See:/ 'newGetBranchResponse' smart constructor.
data GetBranchResponse = GetBranchResponse'
  { -- | The name of the branch.
    branch :: Core.Maybe BranchInfo,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetBranchResponse
newGetBranchResponse pHttpStatus_ =
  GetBranchResponse'
    { branch = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the branch.
getBranchResponse_branch :: Lens.Lens' GetBranchResponse (Core.Maybe BranchInfo)
getBranchResponse_branch = Lens.lens (\GetBranchResponse' {branch} -> branch) (\s@GetBranchResponse' {} a -> s {branch = a} :: GetBranchResponse)

-- | The response's http status code.
getBranchResponse_httpStatus :: Lens.Lens' GetBranchResponse Core.Int
getBranchResponse_httpStatus = Lens.lens (\GetBranchResponse' {httpStatus} -> httpStatus) (\s@GetBranchResponse' {} a -> s {httpStatus = a} :: GetBranchResponse)

instance Core.NFData GetBranchResponse
