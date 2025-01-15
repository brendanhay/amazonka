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
-- Module      : Amazonka.SecurityHub.ListOrganizationAdminAccounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Security Hub administrator accounts. Can only be called by the
-- organization management account.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.ListOrganizationAdminAccounts
  ( -- * Creating a Request
    ListOrganizationAdminAccounts (..),
    newListOrganizationAdminAccounts,

    -- * Request Lenses
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccounts_nextToken,

    -- * Destructuring the Response
    ListOrganizationAdminAccountsResponse (..),
    newListOrganizationAdminAccountsResponse,

    -- * Response Lenses
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newListOrganizationAdminAccounts' smart constructor.
data ListOrganizationAdminAccounts = ListOrganizationAdminAccounts'
  { -- | The maximum number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that is required for pagination. On your first call to the
    -- @ListOrganizationAdminAccounts@ operation, set the value of this
    -- parameter to @NULL@. For subsequent calls to the operation, to continue
    -- listing data, set the value of this parameter to the value returned from
    -- the previous response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationAdminAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listOrganizationAdminAccounts_maxResults' - The maximum number of items to return in the response.
--
-- 'nextToken', 'listOrganizationAdminAccounts_nextToken' - The token that is required for pagination. On your first call to the
-- @ListOrganizationAdminAccounts@ operation, set the value of this
-- parameter to @NULL@. For subsequent calls to the operation, to continue
-- listing data, set the value of this parameter to the value returned from
-- the previous response.
newListOrganizationAdminAccounts ::
  ListOrganizationAdminAccounts
newListOrganizationAdminAccounts =
  ListOrganizationAdminAccounts'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to return in the response.
listOrganizationAdminAccounts_maxResults :: Lens.Lens' ListOrganizationAdminAccounts (Prelude.Maybe Prelude.Natural)
listOrganizationAdminAccounts_maxResults = Lens.lens (\ListOrganizationAdminAccounts' {maxResults} -> maxResults) (\s@ListOrganizationAdminAccounts' {} a -> s {maxResults = a} :: ListOrganizationAdminAccounts)

-- | The token that is required for pagination. On your first call to the
-- @ListOrganizationAdminAccounts@ operation, set the value of this
-- parameter to @NULL@. For subsequent calls to the operation, to continue
-- listing data, set the value of this parameter to the value returned from
-- the previous response.
listOrganizationAdminAccounts_nextToken :: Lens.Lens' ListOrganizationAdminAccounts (Prelude.Maybe Prelude.Text)
listOrganizationAdminAccounts_nextToken = Lens.lens (\ListOrganizationAdminAccounts' {nextToken} -> nextToken) (\s@ListOrganizationAdminAccounts' {} a -> s {nextToken = a} :: ListOrganizationAdminAccounts)

instance Core.AWSPager ListOrganizationAdminAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOrganizationAdminAccountsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOrganizationAdminAccountsResponse_adminAccounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listOrganizationAdminAccounts_nextToken
              Lens..~ rs
              Lens.^? listOrganizationAdminAccountsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListOrganizationAdminAccounts
  where
  type
    AWSResponse ListOrganizationAdminAccounts =
      ListOrganizationAdminAccountsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationAdminAccountsResponse'
            Prelude.<$> (x Data..?> "AdminAccounts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListOrganizationAdminAccounts
  where
  hashWithSalt _salt ListOrganizationAdminAccounts' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListOrganizationAdminAccounts where
  rnf ListOrganizationAdminAccounts' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListOrganizationAdminAccounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListOrganizationAdminAccounts where
  toPath = Prelude.const "/organization/admin"

instance Data.ToQuery ListOrganizationAdminAccounts where
  toQuery ListOrganizationAdminAccounts' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListOrganizationAdminAccountsResponse' smart constructor.
data ListOrganizationAdminAccountsResponse = ListOrganizationAdminAccountsResponse'
  { -- | The list of Security Hub administrator accounts.
    adminAccounts :: Prelude.Maybe [AdminAccount],
    -- | The pagination token to use to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationAdminAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccounts', 'listOrganizationAdminAccountsResponse_adminAccounts' - The list of Security Hub administrator accounts.
--
-- 'nextToken', 'listOrganizationAdminAccountsResponse_nextToken' - The pagination token to use to request the next page of results.
--
-- 'httpStatus', 'listOrganizationAdminAccountsResponse_httpStatus' - The response's http status code.
newListOrganizationAdminAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOrganizationAdminAccountsResponse
newListOrganizationAdminAccountsResponse pHttpStatus_ =
  ListOrganizationAdminAccountsResponse'
    { adminAccounts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of Security Hub administrator accounts.
listOrganizationAdminAccountsResponse_adminAccounts :: Lens.Lens' ListOrganizationAdminAccountsResponse (Prelude.Maybe [AdminAccount])
listOrganizationAdminAccountsResponse_adminAccounts = Lens.lens (\ListOrganizationAdminAccountsResponse' {adminAccounts} -> adminAccounts) (\s@ListOrganizationAdminAccountsResponse' {} a -> s {adminAccounts = a} :: ListOrganizationAdminAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to request the next page of results.
listOrganizationAdminAccountsResponse_nextToken :: Lens.Lens' ListOrganizationAdminAccountsResponse (Prelude.Maybe Prelude.Text)
listOrganizationAdminAccountsResponse_nextToken = Lens.lens (\ListOrganizationAdminAccountsResponse' {nextToken} -> nextToken) (\s@ListOrganizationAdminAccountsResponse' {} a -> s {nextToken = a} :: ListOrganizationAdminAccountsResponse)

-- | The response's http status code.
listOrganizationAdminAccountsResponse_httpStatus :: Lens.Lens' ListOrganizationAdminAccountsResponse Prelude.Int
listOrganizationAdminAccountsResponse_httpStatus = Lens.lens (\ListOrganizationAdminAccountsResponse' {httpStatus} -> httpStatus) (\s@ListOrganizationAdminAccountsResponse' {} a -> s {httpStatus = a} :: ListOrganizationAdminAccountsResponse)

instance
  Prelude.NFData
    ListOrganizationAdminAccountsResponse
  where
  rnf ListOrganizationAdminAccountsResponse' {..} =
    Prelude.rnf adminAccounts `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
