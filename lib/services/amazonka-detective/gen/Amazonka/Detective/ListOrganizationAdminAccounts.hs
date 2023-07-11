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
-- Module      : Amazonka.Detective.ListOrganizationAdminAccounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the Detective administrator account for an
-- organization. Can only be called by the organization management account.
module Amazonka.Detective.ListOrganizationAdminAccounts
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
    listOrganizationAdminAccountsResponse_administrators,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOrganizationAdminAccounts' smart constructor.
data ListOrganizationAdminAccounts = ListOrganizationAdminAccounts'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For requests to get the next page of results, the pagination token that
    -- was returned with the previous set of results. The initial request does
    -- not include a pagination token.
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
-- 'maxResults', 'listOrganizationAdminAccounts_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listOrganizationAdminAccounts_nextToken' - For requests to get the next page of results, the pagination token that
-- was returned with the previous set of results. The initial request does
-- not include a pagination token.
newListOrganizationAdminAccounts ::
  ListOrganizationAdminAccounts
newListOrganizationAdminAccounts =
  ListOrganizationAdminAccounts'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return.
listOrganizationAdminAccounts_maxResults :: Lens.Lens' ListOrganizationAdminAccounts (Prelude.Maybe Prelude.Natural)
listOrganizationAdminAccounts_maxResults = Lens.lens (\ListOrganizationAdminAccounts' {maxResults} -> maxResults) (\s@ListOrganizationAdminAccounts' {} a -> s {maxResults = a} :: ListOrganizationAdminAccounts)

-- | For requests to get the next page of results, the pagination token that
-- was returned with the previous set of results. The initial request does
-- not include a pagination token.
listOrganizationAdminAccounts_nextToken :: Lens.Lens' ListOrganizationAdminAccounts (Prelude.Maybe Prelude.Text)
listOrganizationAdminAccounts_nextToken = Lens.lens (\ListOrganizationAdminAccounts' {nextToken} -> nextToken) (\s@ListOrganizationAdminAccounts' {} a -> s {nextToken = a} :: ListOrganizationAdminAccounts)

instance
  Core.AWSRequest
    ListOrganizationAdminAccounts
  where
  type
    AWSResponse ListOrganizationAdminAccounts =
      ListOrganizationAdminAccountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationAdminAccountsResponse'
            Prelude.<$> (x Data..?> "Administrators" Core..!@ Prelude.mempty)
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
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

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

instance Data.ToJSON ListOrganizationAdminAccounts where
  toJSON ListOrganizationAdminAccounts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListOrganizationAdminAccounts where
  toPath = Prelude.const "/orgs/adminAccountslist"

instance Data.ToQuery ListOrganizationAdminAccounts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOrganizationAdminAccountsResponse' smart constructor.
data ListOrganizationAdminAccountsResponse = ListOrganizationAdminAccountsResponse'
  { -- | The list of Detective administrator accounts.
    administrators :: Prelude.Maybe [Administrator],
    -- | If there are more accounts remaining in the results, then this is the
    -- pagination token to use to request the next page of accounts.
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
-- 'administrators', 'listOrganizationAdminAccountsResponse_administrators' - The list of Detective administrator accounts.
--
-- 'nextToken', 'listOrganizationAdminAccountsResponse_nextToken' - If there are more accounts remaining in the results, then this is the
-- pagination token to use to request the next page of accounts.
--
-- 'httpStatus', 'listOrganizationAdminAccountsResponse_httpStatus' - The response's http status code.
newListOrganizationAdminAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOrganizationAdminAccountsResponse
newListOrganizationAdminAccountsResponse pHttpStatus_ =
  ListOrganizationAdminAccountsResponse'
    { administrators =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of Detective administrator accounts.
listOrganizationAdminAccountsResponse_administrators :: Lens.Lens' ListOrganizationAdminAccountsResponse (Prelude.Maybe [Administrator])
listOrganizationAdminAccountsResponse_administrators = Lens.lens (\ListOrganizationAdminAccountsResponse' {administrators} -> administrators) (\s@ListOrganizationAdminAccountsResponse' {} a -> s {administrators = a} :: ListOrganizationAdminAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more accounts remaining in the results, then this is the
-- pagination token to use to request the next page of accounts.
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
    Prelude.rnf administrators
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
