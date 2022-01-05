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
-- Module      : Amazonka.SSO.ListAccounts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all AWS accounts assigned to the user. These AWS accounts are
-- assigned by the administrator of the account. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/useraccess.html#assignusers Assign User Access>
-- in the /AWS SSO User Guide/. This operation returns a paginated
-- response.
--
-- This operation returns paginated results.
module Amazonka.SSO.ListAccounts
  ( -- * Creating a Request
    ListAccounts (..),
    newListAccounts,

    -- * Request Lenses
    listAccounts_nextToken,
    listAccounts_maxResults,
    listAccounts_accessToken,

    -- * Destructuring the Response
    ListAccountsResponse (..),
    newListAccountsResponse,

    -- * Response Lenses
    listAccountsResponse_accountList,
    listAccountsResponse_nextToken,
    listAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSO.Types

-- | /See:/ 'newListAccounts' smart constructor.
data ListAccounts = ListAccounts'
  { -- | (Optional) When requesting subsequent pages, this is the page token from
    -- the previous response output.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This is the number of items clients can request per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token issued by the @CreateToken@ API call. For more information,
    -- see
    -- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
    -- in the /AWS SSO OIDC API Reference Guide/.
    accessToken :: Core.Sensitive Prelude.Text
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
-- 'nextToken', 'listAccounts_nextToken' - (Optional) When requesting subsequent pages, this is the page token from
-- the previous response output.
--
-- 'maxResults', 'listAccounts_maxResults' - This is the number of items clients can request per page.
--
-- 'accessToken', 'listAccounts_accessToken' - The token issued by the @CreateToken@ API call. For more information,
-- see
-- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
-- in the /AWS SSO OIDC API Reference Guide/.
newListAccounts ::
  -- | 'accessToken'
  Prelude.Text ->
  ListAccounts
newListAccounts pAccessToken_ =
  ListAccounts'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      accessToken = Core._Sensitive Lens.# pAccessToken_
    }

-- | (Optional) When requesting subsequent pages, this is the page token from
-- the previous response output.
listAccounts_nextToken :: Lens.Lens' ListAccounts (Prelude.Maybe Prelude.Text)
listAccounts_nextToken = Lens.lens (\ListAccounts' {nextToken} -> nextToken) (\s@ListAccounts' {} a -> s {nextToken = a} :: ListAccounts)

-- | This is the number of items clients can request per page.
listAccounts_maxResults :: Lens.Lens' ListAccounts (Prelude.Maybe Prelude.Natural)
listAccounts_maxResults = Lens.lens (\ListAccounts' {maxResults} -> maxResults) (\s@ListAccounts' {} a -> s {maxResults = a} :: ListAccounts)

-- | The token issued by the @CreateToken@ API call. For more information,
-- see
-- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
-- in the /AWS SSO OIDC API Reference Guide/.
listAccounts_accessToken :: Lens.Lens' ListAccounts Prelude.Text
listAccounts_accessToken = Lens.lens (\ListAccounts' {accessToken} -> accessToken) (\s@ListAccounts' {} a -> s {accessToken = a} :: ListAccounts) Prelude.. Core._Sensitive

instance Core.AWSPager ListAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountsResponse_accountList
              Prelude.. Lens._Just
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountsResponse'
            Prelude.<$> (x Core..?> "accountList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccounts where
  hashWithSalt _salt ListAccounts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` accessToken

instance Prelude.NFData ListAccounts where
  rnf ListAccounts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf accessToken

instance Core.ToHeaders ListAccounts where
  toHeaders ListAccounts' {..} =
    Prelude.mconcat
      [ "x-amz-sso_bearer_token" Core.=# accessToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath ListAccounts where
  toPath = Prelude.const "/assignment/accounts"

instance Core.ToQuery ListAccounts where
  toQuery ListAccounts' {..} =
    Prelude.mconcat
      [ "next_token" Core.=: nextToken,
        "max_result" Core.=: maxResults
      ]

-- | /See:/ 'newListAccountsResponse' smart constructor.
data ListAccountsResponse = ListAccountsResponse'
  { -- | A paginated response with the list of account information and the next
    -- token if more results are available.
    accountList :: Prelude.Maybe [AccountInfo],
    -- | The page token client that is used to retrieve the list of accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'accountList', 'listAccountsResponse_accountList' - A paginated response with the list of account information and the next
-- token if more results are available.
--
-- 'nextToken', 'listAccountsResponse_nextToken' - The page token client that is used to retrieve the list of accounts.
--
-- 'httpStatus', 'listAccountsResponse_httpStatus' - The response's http status code.
newListAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountsResponse
newListAccountsResponse pHttpStatus_ =
  ListAccountsResponse'
    { accountList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A paginated response with the list of account information and the next
-- token if more results are available.
listAccountsResponse_accountList :: Lens.Lens' ListAccountsResponse (Prelude.Maybe [AccountInfo])
listAccountsResponse_accountList = Lens.lens (\ListAccountsResponse' {accountList} -> accountList) (\s@ListAccountsResponse' {} a -> s {accountList = a} :: ListAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The page token client that is used to retrieve the list of accounts.
listAccountsResponse_nextToken :: Lens.Lens' ListAccountsResponse (Prelude.Maybe Prelude.Text)
listAccountsResponse_nextToken = Lens.lens (\ListAccountsResponse' {nextToken} -> nextToken) (\s@ListAccountsResponse' {} a -> s {nextToken = a} :: ListAccountsResponse)

-- | The response's http status code.
listAccountsResponse_httpStatus :: Lens.Lens' ListAccountsResponse Prelude.Int
listAccountsResponse_httpStatus = Lens.lens (\ListAccountsResponse' {httpStatus} -> httpStatus) (\s@ListAccountsResponse' {} a -> s {httpStatus = a} :: ListAccountsResponse)

instance Prelude.NFData ListAccountsResponse where
  rnf ListAccountsResponse' {..} =
    Prelude.rnf accountList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
