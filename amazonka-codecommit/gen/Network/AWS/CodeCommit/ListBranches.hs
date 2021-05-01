{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a list branches operation.
--
-- /See:/ 'newListBranches' smart constructor.
data ListBranches = ListBranches'
  { -- | An enumeration token that allows the operation to batch the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the branches.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListBranches
newListBranches pRepositoryName_ =
  ListBranches'
    { nextToken = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | An enumeration token that allows the operation to batch the results.
listBranches_nextToken :: Lens.Lens' ListBranches (Prelude.Maybe Prelude.Text)
listBranches_nextToken = Lens.lens (\ListBranches' {nextToken} -> nextToken) (\s@ListBranches' {} a -> s {nextToken = a} :: ListBranches)

-- | The name of the repository that contains the branches.
listBranches_repositoryName :: Lens.Lens' ListBranches Prelude.Text
listBranches_repositoryName = Lens.lens (\ListBranches' {repositoryName} -> repositoryName) (\s@ListBranches' {} a -> s {repositoryName = a} :: ListBranches)

instance Pager.AWSPager ListBranches where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listBranchesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listBranchesResponse_branches Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listBranches_nextToken
          Lens..~ rs
          Lens.^? listBranchesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListBranches where
  type Rs ListBranches = ListBranchesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBranchesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "branches" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBranches

instance Prelude.NFData ListBranches

instance Prelude.ToHeaders ListBranches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.ListBranches" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListBranches where
  toJSON ListBranches' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName)
          ]
      )

instance Prelude.ToPath ListBranches where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListBranches where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a list branches operation.
--
-- /See:/ 'newListBranchesResponse' smart constructor.
data ListBranchesResponse = ListBranchesResponse'
  { -- | An enumeration token that returns the batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of branch names.
    branches :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListBranchesResponse
newListBranchesResponse pHttpStatus_ =
  ListBranchesResponse'
    { nextToken = Prelude.Nothing,
      branches = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An enumeration token that returns the batch of the results.
listBranchesResponse_nextToken :: Lens.Lens' ListBranchesResponse (Prelude.Maybe Prelude.Text)
listBranchesResponse_nextToken = Lens.lens (\ListBranchesResponse' {nextToken} -> nextToken) (\s@ListBranchesResponse' {} a -> s {nextToken = a} :: ListBranchesResponse)

-- | The list of branch names.
listBranchesResponse_branches :: Lens.Lens' ListBranchesResponse (Prelude.Maybe [Prelude.Text])
listBranchesResponse_branches = Lens.lens (\ListBranchesResponse' {branches} -> branches) (\s@ListBranchesResponse' {} a -> s {branches = a} :: ListBranchesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listBranchesResponse_httpStatus :: Lens.Lens' ListBranchesResponse Prelude.Int
listBranchesResponse_httpStatus = Lens.lens (\ListBranchesResponse' {httpStatus} -> httpStatus) (\s@ListBranchesResponse' {} a -> s {httpStatus = a} :: ListBranchesResponse)

instance Prelude.NFData ListBranchesResponse
