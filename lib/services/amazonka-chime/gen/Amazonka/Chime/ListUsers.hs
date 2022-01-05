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
-- Module      : Amazonka.Chime.ListUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users that belong to the specified Amazon Chime account. You
-- can specify an email address to list only the user that the email
-- address belongs to.
--
-- This operation returns paginated results.
module Amazonka.Chime.ListUsers
  ( -- * Creating a Request
    ListUsers (..),
    newListUsers,

    -- * Request Lenses
    listUsers_nextToken,
    listUsers_userEmail,
    listUsers_maxResults,
    listUsers_userType,
    listUsers_accountId,

    -- * Destructuring the Response
    ListUsersResponse (..),
    newListUsersResponse,

    -- * Response Lenses
    listUsersResponse_users,
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional. The user email address used to filter results. Maximum 1.
    userEmail :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of results to return in a single call. Defaults to
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The user type.
    userType :: Prelude.Maybe UserType,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsers_nextToken' - The token to use to retrieve the next page of results.
--
-- 'userEmail', 'listUsers_userEmail' - Optional. The user email address used to filter results. Maximum 1.
--
-- 'maxResults', 'listUsers_maxResults' - The maximum number of results to return in a single call. Defaults to
-- 100.
--
-- 'userType', 'listUsers_userType' - The user type.
--
-- 'accountId', 'listUsers_accountId' - The Amazon Chime account ID.
newListUsers ::
  -- | 'accountId'
  Prelude.Text ->
  ListUsers
newListUsers pAccountId_ =
  ListUsers'
    { nextToken = Prelude.Nothing,
      userEmail = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      userType = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | The token to use to retrieve the next page of results.
listUsers_nextToken :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Text)
listUsers_nextToken = Lens.lens (\ListUsers' {nextToken} -> nextToken) (\s@ListUsers' {} a -> s {nextToken = a} :: ListUsers)

-- | Optional. The user email address used to filter results. Maximum 1.
listUsers_userEmail :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Text)
listUsers_userEmail = Lens.lens (\ListUsers' {userEmail} -> userEmail) (\s@ListUsers' {} a -> s {userEmail = a} :: ListUsers) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of results to return in a single call. Defaults to
-- 100.
listUsers_maxResults :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Natural)
listUsers_maxResults = Lens.lens (\ListUsers' {maxResults} -> maxResults) (\s@ListUsers' {} a -> s {maxResults = a} :: ListUsers)

-- | The user type.
listUsers_userType :: Lens.Lens' ListUsers (Prelude.Maybe UserType)
listUsers_userType = Lens.lens (\ListUsers' {userType} -> userType) (\s@ListUsers' {} a -> s {userType = a} :: ListUsers)

-- | The Amazon Chime account ID.
listUsers_accountId :: Lens.Lens' ListUsers Prelude.Text
listUsers_accountId = Lens.lens (\ListUsers' {accountId} -> accountId) (\s@ListUsers' {} a -> s {accountId = a} :: ListUsers)

instance Core.AWSPager ListUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_users Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listUsers_nextToken
          Lens..~ rs
          Lens.^? listUsersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListUsers where
  type AWSResponse ListUsers = ListUsersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Prelude.<$> (x Core..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUsers where
  hashWithSalt _salt ListUsers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` userEmail
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` userType
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData ListUsers where
  rnf ListUsers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userEmail
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf userType
      `Prelude.seq` Prelude.rnf accountId

instance Core.ToHeaders ListUsers where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListUsers where
  toPath ListUsers' {..} =
    Prelude.mconcat
      ["/accounts/", Core.toBS accountId, "/users"]

instance Core.ToQuery ListUsers where
  toQuery ListUsers' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "user-email" Core.=: userEmail,
        "max-results" Core.=: maxResults,
        "user-type" Core.=: userType
      ]

-- | /See:/ 'newListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | List of users and user details.
    users :: Prelude.Maybe [User],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'users', 'listUsersResponse_users' - List of users and user details.
--
-- 'nextToken', 'listUsersResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'httpStatus', 'listUsersResponse_httpStatus' - The response's http status code.
newListUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUsersResponse
newListUsersResponse pHttpStatus_ =
  ListUsersResponse'
    { users = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of users and user details.
listUsersResponse_users :: Lens.Lens' ListUsersResponse (Prelude.Maybe [User])
listUsersResponse_users = Lens.lens (\ListUsersResponse' {users} -> users) (\s@ListUsersResponse' {} a -> s {users = a} :: ListUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results.
listUsersResponse_nextToken :: Lens.Lens' ListUsersResponse (Prelude.Maybe Prelude.Text)
listUsersResponse_nextToken = Lens.lens (\ListUsersResponse' {nextToken} -> nextToken) (\s@ListUsersResponse' {} a -> s {nextToken = a} :: ListUsersResponse)

-- | The response's http status code.
listUsersResponse_httpStatus :: Lens.Lens' ListUsersResponse Prelude.Int
listUsersResponse_httpStatus = Lens.lens (\ListUsersResponse' {httpStatus} -> httpStatus) (\s@ListUsersResponse' {} a -> s {httpStatus = a} :: ListUsersResponse)

instance Prelude.NFData ListUsersResponse where
  rnf ListUsersResponse' {..} =
    Prelude.rnf users
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
