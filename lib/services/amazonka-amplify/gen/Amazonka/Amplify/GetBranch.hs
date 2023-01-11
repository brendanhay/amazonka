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
-- Module      : Amazonka.Amplify.GetBranch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a branch for an Amplify app.
module Amazonka.Amplify.GetBranch
  ( -- * Creating a Request
    GetBranch (..),
    newGetBranch,

    -- * Request Lenses
    getBranch_appId,
    getBranch_branchName,

    -- * Destructuring the Response
    GetBranchResponse (..),
    newGetBranchResponse,

    -- * Response Lenses
    getBranchResponse_httpStatus,
    getBranchResponse_branch,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the get branch request.
--
-- /See:/ 'newGetBranch' smart constructor.
data GetBranch = GetBranch'
  { -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the branch.
    branchName :: Prelude.Text
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
-- 'appId', 'getBranch_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'getBranch_branchName' - The name for the branch.
newGetBranch ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  GetBranch
newGetBranch pAppId_ pBranchName_ =
  GetBranch'
    { appId = pAppId_,
      branchName = pBranchName_
    }

-- | The unique ID for an Amplify app.
getBranch_appId :: Lens.Lens' GetBranch Prelude.Text
getBranch_appId = Lens.lens (\GetBranch' {appId} -> appId) (\s@GetBranch' {} a -> s {appId = a} :: GetBranch)

-- | The name for the branch.
getBranch_branchName :: Lens.Lens' GetBranch Prelude.Text
getBranch_branchName = Lens.lens (\GetBranch' {branchName} -> branchName) (\s@GetBranch' {} a -> s {branchName = a} :: GetBranch)

instance Core.AWSRequest GetBranch where
  type AWSResponse GetBranch = GetBranchResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBranchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "branch")
      )

instance Prelude.Hashable GetBranch where
  hashWithSalt _salt GetBranch' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` branchName

instance Prelude.NFData GetBranch where
  rnf GetBranch' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf branchName

instance Data.ToHeaders GetBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBranch where
  toPath GetBranch' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/branches/",
        Data.toBS branchName
      ]

instance Data.ToQuery GetBranch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBranchResponse' smart constructor.
data GetBranchResponse = GetBranchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    branch :: Branch
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBranchResponse_httpStatus' - The response's http status code.
--
-- 'branch', 'getBranchResponse_branch' - Undocumented member.
newGetBranchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'branch'
  Branch ->
  GetBranchResponse
newGetBranchResponse pHttpStatus_ pBranch_ =
  GetBranchResponse'
    { httpStatus = pHttpStatus_,
      branch = pBranch_
    }

-- | The response's http status code.
getBranchResponse_httpStatus :: Lens.Lens' GetBranchResponse Prelude.Int
getBranchResponse_httpStatus = Lens.lens (\GetBranchResponse' {httpStatus} -> httpStatus) (\s@GetBranchResponse' {} a -> s {httpStatus = a} :: GetBranchResponse)

-- | Undocumented member.
getBranchResponse_branch :: Lens.Lens' GetBranchResponse Branch
getBranchResponse_branch = Lens.lens (\GetBranchResponse' {branch} -> branch) (\s@GetBranchResponse' {} a -> s {branch = a} :: GetBranchResponse)

instance Prelude.NFData GetBranchResponse where
  rnf GetBranchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf branch
