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
-- Module      : Amazonka.SSO.ListAccountRoles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all roles that are assigned to the user for a given AWS account.
--
-- This operation returns paginated results.
module Amazonka.SSO.ListAccountRoles
  ( -- * Creating a Request
    ListAccountRoles (..),
    newListAccountRoles,

    -- * Request Lenses
    listAccountRoles_nextToken,
    listAccountRoles_maxResults,
    listAccountRoles_accessToken,
    listAccountRoles_accountId,

    -- * Destructuring the Response
    ListAccountRolesResponse (..),
    newListAccountRolesResponse,

    -- * Response Lenses
    listAccountRolesResponse_nextToken,
    listAccountRolesResponse_roleList,
    listAccountRolesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSO.Types

-- | /See:/ 'newListAccountRoles' smart constructor.
data ListAccountRoles = ListAccountRoles'
  { -- | The page token from the previous response output when you request
    -- subsequent pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of items that clients can request per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token issued by the @CreateToken@ API call. For more information,
    -- see
    -- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
    -- in the /IAM Identity Center OIDC API Reference Guide/.
    accessToken :: Data.Sensitive Prelude.Text,
    -- | The identifier for the AWS account that is assigned to the user.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountRoles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountRoles_nextToken' - The page token from the previous response output when you request
-- subsequent pages.
--
-- 'maxResults', 'listAccountRoles_maxResults' - The number of items that clients can request per page.
--
-- 'accessToken', 'listAccountRoles_accessToken' - The token issued by the @CreateToken@ API call. For more information,
-- see
-- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
-- in the /IAM Identity Center OIDC API Reference Guide/.
--
-- 'accountId', 'listAccountRoles_accountId' - The identifier for the AWS account that is assigned to the user.
newListAccountRoles ::
  -- | 'accessToken'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  ListAccountRoles
newListAccountRoles pAccessToken_ pAccountId_ =
  ListAccountRoles'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      accessToken = Data._Sensitive Lens.# pAccessToken_,
      accountId = pAccountId_
    }

-- | The page token from the previous response output when you request
-- subsequent pages.
listAccountRoles_nextToken :: Lens.Lens' ListAccountRoles (Prelude.Maybe Prelude.Text)
listAccountRoles_nextToken = Lens.lens (\ListAccountRoles' {nextToken} -> nextToken) (\s@ListAccountRoles' {} a -> s {nextToken = a} :: ListAccountRoles)

-- | The number of items that clients can request per page.
listAccountRoles_maxResults :: Lens.Lens' ListAccountRoles (Prelude.Maybe Prelude.Natural)
listAccountRoles_maxResults = Lens.lens (\ListAccountRoles' {maxResults} -> maxResults) (\s@ListAccountRoles' {} a -> s {maxResults = a} :: ListAccountRoles)

-- | The token issued by the @CreateToken@ API call. For more information,
-- see
-- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
-- in the /IAM Identity Center OIDC API Reference Guide/.
listAccountRoles_accessToken :: Lens.Lens' ListAccountRoles Prelude.Text
listAccountRoles_accessToken = Lens.lens (\ListAccountRoles' {accessToken} -> accessToken) (\s@ListAccountRoles' {} a -> s {accessToken = a} :: ListAccountRoles) Prelude.. Data._Sensitive

-- | The identifier for the AWS account that is assigned to the user.
listAccountRoles_accountId :: Lens.Lens' ListAccountRoles Prelude.Text
listAccountRoles_accountId = Lens.lens (\ListAccountRoles' {accountId} -> accountId) (\s@ListAccountRoles' {} a -> s {accountId = a} :: ListAccountRoles)

instance Core.AWSPager ListAccountRoles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountRolesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountRolesResponse_roleList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccountRoles_nextToken
          Lens..~ rs
          Lens.^? listAccountRolesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAccountRoles where
  type
    AWSResponse ListAccountRoles =
      ListAccountRolesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountRolesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "roleList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccountRoles where
  hashWithSalt _salt ListAccountRoles' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData ListAccountRoles where
  rnf ListAccountRoles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToHeaders ListAccountRoles where
  toHeaders ListAccountRoles' {..} =
    Prelude.mconcat
      [ "x-amz-sso_bearer_token" Data.=# accessToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath ListAccountRoles where
  toPath = Prelude.const "/assignment/roles"

instance Data.ToQuery ListAccountRoles where
  toQuery ListAccountRoles' {..} =
    Prelude.mconcat
      [ "next_token" Data.=: nextToken,
        "max_result" Data.=: maxResults,
        "account_id" Data.=: accountId
      ]

-- | /See:/ 'newListAccountRolesResponse' smart constructor.
data ListAccountRolesResponse = ListAccountRolesResponse'
  { -- | The page token client that is used to retrieve the list of accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A paginated response with the list of roles and the next token if more
    -- results are available.
    roleList :: Prelude.Maybe [RoleInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountRolesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountRolesResponse_nextToken' - The page token client that is used to retrieve the list of accounts.
--
-- 'roleList', 'listAccountRolesResponse_roleList' - A paginated response with the list of roles and the next token if more
-- results are available.
--
-- 'httpStatus', 'listAccountRolesResponse_httpStatus' - The response's http status code.
newListAccountRolesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountRolesResponse
newListAccountRolesResponse pHttpStatus_ =
  ListAccountRolesResponse'
    { nextToken =
        Prelude.Nothing,
      roleList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token client that is used to retrieve the list of accounts.
listAccountRolesResponse_nextToken :: Lens.Lens' ListAccountRolesResponse (Prelude.Maybe Prelude.Text)
listAccountRolesResponse_nextToken = Lens.lens (\ListAccountRolesResponse' {nextToken} -> nextToken) (\s@ListAccountRolesResponse' {} a -> s {nextToken = a} :: ListAccountRolesResponse)

-- | A paginated response with the list of roles and the next token if more
-- results are available.
listAccountRolesResponse_roleList :: Lens.Lens' ListAccountRolesResponse (Prelude.Maybe [RoleInfo])
listAccountRolesResponse_roleList = Lens.lens (\ListAccountRolesResponse' {roleList} -> roleList) (\s@ListAccountRolesResponse' {} a -> s {roleList = a} :: ListAccountRolesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAccountRolesResponse_httpStatus :: Lens.Lens' ListAccountRolesResponse Prelude.Int
listAccountRolesResponse_httpStatus = Lens.lens (\ListAccountRolesResponse' {httpStatus} -> httpStatus) (\s@ListAccountRolesResponse' {} a -> s {httpStatus = a} :: ListAccountRolesResponse)

instance Prelude.NFData ListAccountRolesResponse where
  rnf ListAccountRolesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf roleList
      `Prelude.seq` Prelude.rnf httpStatus
