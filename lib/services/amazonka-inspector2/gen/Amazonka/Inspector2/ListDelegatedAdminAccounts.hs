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
-- Module      : Amazonka.Inspector2.ListDelegatedAdminAccounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the Amazon Inspector delegated administrator of
-- your organization.
--
-- This operation returns paginated results.
module Amazonka.Inspector2.ListDelegatedAdminAccounts
  ( -- * Creating a Request
    ListDelegatedAdminAccounts (..),
    newListDelegatedAdminAccounts,

    -- * Request Lenses
    listDelegatedAdminAccounts_maxResults,
    listDelegatedAdminAccounts_nextToken,

    -- * Destructuring the Response
    ListDelegatedAdminAccountsResponse (..),
    newListDelegatedAdminAccountsResponse,

    -- * Response Lenses
    listDelegatedAdminAccountsResponse_delegatedAdminAccounts,
    listDelegatedAdminAccountsResponse_nextToken,
    listDelegatedAdminAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDelegatedAdminAccounts' smart constructor.
data ListDelegatedAdminAccounts = ListDelegatedAdminAccounts'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDelegatedAdminAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDelegatedAdminAccounts_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listDelegatedAdminAccounts_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
newListDelegatedAdminAccounts ::
  ListDelegatedAdminAccounts
newListDelegatedAdminAccounts =
  ListDelegatedAdminAccounts'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in the response.
listDelegatedAdminAccounts_maxResults :: Lens.Lens' ListDelegatedAdminAccounts (Prelude.Maybe Prelude.Natural)
listDelegatedAdminAccounts_maxResults = Lens.lens (\ListDelegatedAdminAccounts' {maxResults} -> maxResults) (\s@ListDelegatedAdminAccounts' {} a -> s {maxResults = a} :: ListDelegatedAdminAccounts)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listDelegatedAdminAccounts_nextToken :: Lens.Lens' ListDelegatedAdminAccounts (Prelude.Maybe Prelude.Text)
listDelegatedAdminAccounts_nextToken = Lens.lens (\ListDelegatedAdminAccounts' {nextToken} -> nextToken) (\s@ListDelegatedAdminAccounts' {} a -> s {nextToken = a} :: ListDelegatedAdminAccounts)

instance Core.AWSPager ListDelegatedAdminAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDelegatedAdminAccountsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDelegatedAdminAccountsResponse_delegatedAdminAccounts
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDelegatedAdminAccounts_nextToken
          Lens..~ rs
          Lens.^? listDelegatedAdminAccountsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDelegatedAdminAccounts where
  type
    AWSResponse ListDelegatedAdminAccounts =
      ListDelegatedAdminAccountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDelegatedAdminAccountsResponse'
            Prelude.<$> ( x Data..?> "delegatedAdminAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDelegatedAdminAccounts where
  hashWithSalt _salt ListDelegatedAdminAccounts' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDelegatedAdminAccounts where
  rnf ListDelegatedAdminAccounts' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDelegatedAdminAccounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDelegatedAdminAccounts where
  toJSON ListDelegatedAdminAccounts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDelegatedAdminAccounts where
  toPath = Prelude.const "/delegatedadminaccounts/list"

instance Data.ToQuery ListDelegatedAdminAccounts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDelegatedAdminAccountsResponse' smart constructor.
data ListDelegatedAdminAccountsResponse = ListDelegatedAdminAccountsResponse'
  { -- | Details of the Amazon Inspector delegated administrator of your
    -- organization.
    delegatedAdminAccounts :: Prelude.Maybe [DelegatedAdminAccount],
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDelegatedAdminAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegatedAdminAccounts', 'listDelegatedAdminAccountsResponse_delegatedAdminAccounts' - Details of the Amazon Inspector delegated administrator of your
-- organization.
--
-- 'nextToken', 'listDelegatedAdminAccountsResponse_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'httpStatus', 'listDelegatedAdminAccountsResponse_httpStatus' - The response's http status code.
newListDelegatedAdminAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDelegatedAdminAccountsResponse
newListDelegatedAdminAccountsResponse pHttpStatus_ =
  ListDelegatedAdminAccountsResponse'
    { delegatedAdminAccounts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of the Amazon Inspector delegated administrator of your
-- organization.
listDelegatedAdminAccountsResponse_delegatedAdminAccounts :: Lens.Lens' ListDelegatedAdminAccountsResponse (Prelude.Maybe [DelegatedAdminAccount])
listDelegatedAdminAccountsResponse_delegatedAdminAccounts = Lens.lens (\ListDelegatedAdminAccountsResponse' {delegatedAdminAccounts} -> delegatedAdminAccounts) (\s@ListDelegatedAdminAccountsResponse' {} a -> s {delegatedAdminAccounts = a} :: ListDelegatedAdminAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listDelegatedAdminAccountsResponse_nextToken :: Lens.Lens' ListDelegatedAdminAccountsResponse (Prelude.Maybe Prelude.Text)
listDelegatedAdminAccountsResponse_nextToken = Lens.lens (\ListDelegatedAdminAccountsResponse' {nextToken} -> nextToken) (\s@ListDelegatedAdminAccountsResponse' {} a -> s {nextToken = a} :: ListDelegatedAdminAccountsResponse)

-- | The response's http status code.
listDelegatedAdminAccountsResponse_httpStatus :: Lens.Lens' ListDelegatedAdminAccountsResponse Prelude.Int
listDelegatedAdminAccountsResponse_httpStatus = Lens.lens (\ListDelegatedAdminAccountsResponse' {httpStatus} -> httpStatus) (\s@ListDelegatedAdminAccountsResponse' {} a -> s {httpStatus = a} :: ListDelegatedAdminAccountsResponse)

instance
  Prelude.NFData
    ListDelegatedAdminAccountsResponse
  where
  rnf ListDelegatedAdminAccountsResponse' {..} =
    Prelude.rnf delegatedAdminAccounts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
