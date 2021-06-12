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
-- Module      : Network.AWS.CodeCommit.ListBranches
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more branches in a repository.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListBranches
  ( -- * Creating a Request
    ListBranches (..),
    newListBranches,

    -- * Request Lenses
    listBranches_nextToken,
    listBranches_repositoryName,

    -- * Destructuring the Response
    ListBranchesResponse (..),
    newListBranchesResponse,

    -- * Response Lenses
    listBranchesResponse_nextToken,
    listBranchesResponse_branches,
    listBranchesResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a list branches operation.
--
-- /See:/ 'newListBranches' smart constructor.
data ListBranches = ListBranches'
  { -- | An enumeration token that allows the operation to batch the results.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the repository that contains the branches.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBranches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBranches_nextToken' - An enumeration token that allows the operation to batch the results.
--
-- 'repositoryName', 'listBranches_repositoryName' - The name of the repository that contains the branches.
newListBranches ::
  -- | 'repositoryName'
  Core.Text ->
  ListBranches
newListBranches pRepositoryName_ =
  ListBranches'
    { nextToken = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | An enumeration token that allows the operation to batch the results.
listBranches_nextToken :: Lens.Lens' ListBranches (Core.Maybe Core.Text)
listBranches_nextToken = Lens.lens (\ListBranches' {nextToken} -> nextToken) (\s@ListBranches' {} a -> s {nextToken = a} :: ListBranches)

-- | The name of the repository that contains the branches.
listBranches_repositoryName :: Lens.Lens' ListBranches Core.Text
listBranches_repositoryName = Lens.lens (\ListBranches' {repositoryName} -> repositoryName) (\s@ListBranches' {} a -> s {repositoryName = a} :: ListBranches)

instance Core.AWSPager ListBranches where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBranchesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBranchesResponse_branches Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBranches_nextToken
          Lens..~ rs
          Lens.^? listBranchesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListBranches where
  type AWSResponse ListBranches = ListBranchesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBranchesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "branches" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBranches

instance Core.NFData ListBranches

instance Core.ToHeaders ListBranches where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.ListBranches" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBranches where
  toJSON ListBranches' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath ListBranches where
  toPath = Core.const "/"

instance Core.ToQuery ListBranches where
  toQuery = Core.const Core.mempty

-- | Represents the output of a list branches operation.
--
-- /See:/ 'newListBranchesResponse' smart constructor.
data ListBranchesResponse = ListBranchesResponse'
  { -- | An enumeration token that returns the batch of the results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of branch names.
    branches :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBranchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBranchesResponse_nextToken' - An enumeration token that returns the batch of the results.
--
-- 'branches', 'listBranchesResponse_branches' - The list of branch names.
--
-- 'httpStatus', 'listBranchesResponse_httpStatus' - The response's http status code.
newListBranchesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBranchesResponse
newListBranchesResponse pHttpStatus_ =
  ListBranchesResponse'
    { nextToken = Core.Nothing,
      branches = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An enumeration token that returns the batch of the results.
listBranchesResponse_nextToken :: Lens.Lens' ListBranchesResponse (Core.Maybe Core.Text)
listBranchesResponse_nextToken = Lens.lens (\ListBranchesResponse' {nextToken} -> nextToken) (\s@ListBranchesResponse' {} a -> s {nextToken = a} :: ListBranchesResponse)

-- | The list of branch names.
listBranchesResponse_branches :: Lens.Lens' ListBranchesResponse (Core.Maybe [Core.Text])
listBranchesResponse_branches = Lens.lens (\ListBranchesResponse' {branches} -> branches) (\s@ListBranchesResponse' {} a -> s {branches = a} :: ListBranchesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBranchesResponse_httpStatus :: Lens.Lens' ListBranchesResponse Core.Int
listBranchesResponse_httpStatus = Lens.lens (\ListBranchesResponse' {httpStatus} -> httpStatus) (\s@ListBranchesResponse' {} a -> s {httpStatus = a} :: ListBranchesResponse)

instance Core.NFData ListBranchesResponse
