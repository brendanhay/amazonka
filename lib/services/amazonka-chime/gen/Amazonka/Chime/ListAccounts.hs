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
-- Module      : Amazonka.Chime.ListAccounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Chime accounts under the administrator\'s AWS account.
-- You can filter accounts by account name prefix. To find out which Amazon
-- Chime account a user belongs to, you can filter by the user\'s email
-- address, which returns one account result.
--
-- This operation returns paginated results.
module Amazonka.Chime.ListAccounts
  ( -- * Creating a Request
    ListAccounts (..),
    newListAccounts,

    -- * Request Lenses
    listAccounts_name,
    listAccounts_nextToken,
    listAccounts_userEmail,
    listAccounts_maxResults,

    -- * Destructuring the Response
    ListAccountsResponse (..),
    newListAccountsResponse,

    -- * Response Lenses
    listAccountsResponse_nextToken,
    listAccountsResponse_accounts,
    listAccountsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccounts' smart constructor.
data ListAccounts = ListAccounts'
  { -- | Amazon Chime account name prefix with which to filter results.
    name :: Prelude.Maybe Prelude.Text,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | User email address with which to filter results.
    userEmail :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The maximum number of results to return in a single call. Defaults to
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'listAccounts_name' - Amazon Chime account name prefix with which to filter results.
--
-- 'nextToken', 'listAccounts_nextToken' - The token to use to retrieve the next page of results.
--
-- 'userEmail', 'listAccounts_userEmail' - User email address with which to filter results.
--
-- 'maxResults', 'listAccounts_maxResults' - The maximum number of results to return in a single call. Defaults to
-- 100.
newListAccounts ::
  ListAccounts
newListAccounts =
  ListAccounts'
    { name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      userEmail = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Amazon Chime account name prefix with which to filter results.
listAccounts_name :: Lens.Lens' ListAccounts (Prelude.Maybe Prelude.Text)
listAccounts_name = Lens.lens (\ListAccounts' {name} -> name) (\s@ListAccounts' {} a -> s {name = a} :: ListAccounts)

-- | The token to use to retrieve the next page of results.
listAccounts_nextToken :: Lens.Lens' ListAccounts (Prelude.Maybe Prelude.Text)
listAccounts_nextToken = Lens.lens (\ListAccounts' {nextToken} -> nextToken) (\s@ListAccounts' {} a -> s {nextToken = a} :: ListAccounts)

-- | User email address with which to filter results.
listAccounts_userEmail :: Lens.Lens' ListAccounts (Prelude.Maybe Prelude.Text)
listAccounts_userEmail = Lens.lens (\ListAccounts' {userEmail} -> userEmail) (\s@ListAccounts' {} a -> s {userEmail = a} :: ListAccounts) Prelude.. Lens.mapping Data._Sensitive

-- | The maximum number of results to return in a single call. Defaults to
-- 100.
listAccounts_maxResults :: Lens.Lens' ListAccounts (Prelude.Maybe Prelude.Natural)
listAccounts_maxResults = Lens.lens (\ListAccounts' {maxResults} -> maxResults) (\s@ListAccounts' {} a -> s {maxResults = a} :: ListAccounts)

instance Core.AWSPager ListAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountsResponse_accounts Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccounts_nextToken
          Lens..~ rs
          Lens.^? listAccountsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAccounts where
  type AWSResponse ListAccounts = ListAccountsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Accounts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccounts where
  hashWithSalt _salt ListAccounts' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` userEmail
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAccounts where
  rnf ListAccounts' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userEmail
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListAccounts where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAccounts where
  toPath = Prelude.const "/accounts"

instance Data.ToQuery ListAccounts where
  toQuery ListAccounts' {..} =
    Prelude.mconcat
      [ "name" Data.=: name,
        "next-token" Data.=: nextToken,
        "user-email" Data.=: userEmail,
        "max-results" Data.=: maxResults
      ]

-- | /See:/ 'newListAccountsResponse' smart constructor.
data ListAccountsResponse = ListAccountsResponse'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of Amazon Chime accounts and account details.
    accounts :: Prelude.Maybe [Account],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'accounts', 'listAccountsResponse_accounts' - List of Amazon Chime accounts and account details.
--
-- 'httpStatus', 'listAccountsResponse_httpStatus' - The response's http status code.
newListAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountsResponse
newListAccountsResponse pHttpStatus_ =
  ListAccountsResponse'
    { nextToken = Prelude.Nothing,
      accounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results.
listAccountsResponse_nextToken :: Lens.Lens' ListAccountsResponse (Prelude.Maybe Prelude.Text)
listAccountsResponse_nextToken = Lens.lens (\ListAccountsResponse' {nextToken} -> nextToken) (\s@ListAccountsResponse' {} a -> s {nextToken = a} :: ListAccountsResponse)

-- | List of Amazon Chime accounts and account details.
listAccountsResponse_accounts :: Lens.Lens' ListAccountsResponse (Prelude.Maybe [Account])
listAccountsResponse_accounts = Lens.lens (\ListAccountsResponse' {accounts} -> accounts) (\s@ListAccountsResponse' {} a -> s {accounts = a} :: ListAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAccountsResponse_httpStatus :: Lens.Lens' ListAccountsResponse Prelude.Int
listAccountsResponse_httpStatus = Lens.lens (\ListAccountsResponse' {httpStatus} -> httpStatus) (\s@ListAccountsResponse' {} a -> s {httpStatus = a} :: ListAccountsResponse)

instance Prelude.NFData ListAccountsResponse where
  rnf ListAccountsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf httpStatus
