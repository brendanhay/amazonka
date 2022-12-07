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
-- Module      : Amazonka.DrS.ListStagingAccounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of staging accounts for existing extended source
-- servers.
--
-- This operation returns paginated results.
module Amazonka.DrS.ListStagingAccounts
  ( -- * Creating a Request
    ListStagingAccounts (..),
    newListStagingAccounts,

    -- * Request Lenses
    listStagingAccounts_nextToken,
    listStagingAccounts_maxResults,

    -- * Destructuring the Response
    ListStagingAccountsResponse (..),
    newListStagingAccountsResponse,

    -- * Response Lenses
    listStagingAccountsResponse_nextToken,
    listStagingAccountsResponse_accounts,
    listStagingAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStagingAccounts' smart constructor.
data ListStagingAccounts = ListStagingAccounts'
  { -- | The token of the next staging Account to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of staging Accounts to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStagingAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStagingAccounts_nextToken' - The token of the next staging Account to retrieve.
--
-- 'maxResults', 'listStagingAccounts_maxResults' - The maximum number of staging Accounts to retrieve.
newListStagingAccounts ::
  ListStagingAccounts
newListStagingAccounts =
  ListStagingAccounts'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token of the next staging Account to retrieve.
listStagingAccounts_nextToken :: Lens.Lens' ListStagingAccounts (Prelude.Maybe Prelude.Text)
listStagingAccounts_nextToken = Lens.lens (\ListStagingAccounts' {nextToken} -> nextToken) (\s@ListStagingAccounts' {} a -> s {nextToken = a} :: ListStagingAccounts)

-- | The maximum number of staging Accounts to retrieve.
listStagingAccounts_maxResults :: Lens.Lens' ListStagingAccounts (Prelude.Maybe Prelude.Natural)
listStagingAccounts_maxResults = Lens.lens (\ListStagingAccounts' {maxResults} -> maxResults) (\s@ListStagingAccounts' {} a -> s {maxResults = a} :: ListStagingAccounts)

instance Core.AWSPager ListStagingAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStagingAccountsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStagingAccountsResponse_accounts
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStagingAccounts_nextToken
          Lens..~ rs
          Lens.^? listStagingAccountsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStagingAccounts where
  type
    AWSResponse ListStagingAccounts =
      ListStagingAccountsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStagingAccountsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "accounts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStagingAccounts where
  hashWithSalt _salt ListStagingAccounts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListStagingAccounts where
  rnf ListStagingAccounts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListStagingAccounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListStagingAccounts where
  toPath = Prelude.const "/ListStagingAccounts"

instance Data.ToQuery ListStagingAccounts where
  toQuery ListStagingAccounts' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListStagingAccountsResponse' smart constructor.
data ListStagingAccountsResponse = ListStagingAccountsResponse'
  { -- | The token of the next staging Account to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of staging AWS Accounts.
    accounts :: Prelude.Maybe [Account],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStagingAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStagingAccountsResponse_nextToken' - The token of the next staging Account to retrieve.
--
-- 'accounts', 'listStagingAccountsResponse_accounts' - An array of staging AWS Accounts.
--
-- 'httpStatus', 'listStagingAccountsResponse_httpStatus' - The response's http status code.
newListStagingAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStagingAccountsResponse
newListStagingAccountsResponse pHttpStatus_ =
  ListStagingAccountsResponse'
    { nextToken =
        Prelude.Nothing,
      accounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token of the next staging Account to retrieve.
listStagingAccountsResponse_nextToken :: Lens.Lens' ListStagingAccountsResponse (Prelude.Maybe Prelude.Text)
listStagingAccountsResponse_nextToken = Lens.lens (\ListStagingAccountsResponse' {nextToken} -> nextToken) (\s@ListStagingAccountsResponse' {} a -> s {nextToken = a} :: ListStagingAccountsResponse)

-- | An array of staging AWS Accounts.
listStagingAccountsResponse_accounts :: Lens.Lens' ListStagingAccountsResponse (Prelude.Maybe [Account])
listStagingAccountsResponse_accounts = Lens.lens (\ListStagingAccountsResponse' {accounts} -> accounts) (\s@ListStagingAccountsResponse' {} a -> s {accounts = a} :: ListStagingAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStagingAccountsResponse_httpStatus :: Lens.Lens' ListStagingAccountsResponse Prelude.Int
listStagingAccountsResponse_httpStatus = Lens.lens (\ListStagingAccountsResponse' {httpStatus} -> httpStatus) (\s@ListStagingAccountsResponse' {} a -> s {httpStatus = a} :: ListStagingAccountsResponse)

instance Prelude.NFData ListStagingAccountsResponse where
  rnf ListStagingAccountsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf httpStatus
