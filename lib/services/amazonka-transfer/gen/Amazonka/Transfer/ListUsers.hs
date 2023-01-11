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
-- Module      : Amazonka.Transfer.ListUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users for a file transfer protocol-enabled server that you
-- specify by passing the @ServerId@ parameter.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListUsers
  ( -- * Creating a Request
    ListUsers (..),
    newListUsers,

    -- * Request Lenses
    listUsers_maxResults,
    listUsers_nextToken,
    listUsers_serverId,

    -- * Destructuring the Response
    ListUsersResponse (..),
    newListUsersResponse,

    -- * Response Lenses
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,
    listUsersResponse_serverId,
    listUsersResponse_users,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | Specifies the number of users to return as a response to the @ListUsers@
    -- request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you can get additional results from the @ListUsers@ call, a
    -- @NextToken@ parameter is returned in the output. You can then pass in a
    -- subsequent command to the @NextToken@ parameter to continue listing
    -- additional users.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A system-assigned unique identifier for a server that has users assigned
    -- to it.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listUsers_maxResults' - Specifies the number of users to return as a response to the @ListUsers@
-- request.
--
-- 'nextToken', 'listUsers_nextToken' - When you can get additional results from the @ListUsers@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional users.
--
-- 'serverId', 'listUsers_serverId' - A system-assigned unique identifier for a server that has users assigned
-- to it.
newListUsers ::
  -- | 'serverId'
  Prelude.Text ->
  ListUsers
newListUsers pServerId_ =
  ListUsers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serverId = pServerId_
    }

-- | Specifies the number of users to return as a response to the @ListUsers@
-- request.
listUsers_maxResults :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Natural)
listUsers_maxResults = Lens.lens (\ListUsers' {maxResults} -> maxResults) (\s@ListUsers' {} a -> s {maxResults = a} :: ListUsers)

-- | When you can get additional results from the @ListUsers@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional users.
listUsers_nextToken :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Text)
listUsers_nextToken = Lens.lens (\ListUsers' {nextToken} -> nextToken) (\s@ListUsers' {} a -> s {nextToken = a} :: ListUsers)

-- | A system-assigned unique identifier for a server that has users assigned
-- to it.
listUsers_serverId :: Lens.Lens' ListUsers Prelude.Text
listUsers_serverId = Lens.lens (\ListUsers' {serverId} -> serverId) (\s@ListUsers' {} a -> s {serverId = a} :: ListUsers)

instance Core.AWSPager ListUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. listUsersResponse_users) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listUsers_nextToken
          Lens..~ rs
          Lens.^? listUsersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListUsers where
  type AWSResponse ListUsers = ListUsersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
            Prelude.<*> (x Data..?> "Users" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListUsers where
  hashWithSalt _salt ListUsers' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData ListUsers where
  rnf ListUsers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serverId

instance Data.ToHeaders ListUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TransferService.ListUsers" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListUsers where
  toJSON ListUsers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServerId" Data..= serverId)
          ]
      )

instance Data.ToPath ListUsers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | When you can get additional results from the @ListUsers@ call, a
    -- @NextToken@ parameter is returned in the output. You can then pass in a
    -- subsequent command to the @NextToken@ parameter to continue listing
    -- additional users.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A system-assigned unique identifier for a server that the users are
    -- assigned to.
    serverId :: Prelude.Text,
    -- | Returns the user accounts and their properties for the @ServerId@ value
    -- that you specify.
    users :: [ListedUser]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsersResponse_nextToken' - When you can get additional results from the @ListUsers@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional users.
--
-- 'httpStatus', 'listUsersResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'listUsersResponse_serverId' - A system-assigned unique identifier for a server that the users are
-- assigned to.
--
-- 'users', 'listUsersResponse_users' - Returns the user accounts and their properties for the @ServerId@ value
-- that you specify.
newListUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  ListUsersResponse
newListUsersResponse pHttpStatus_ pServerId_ =
  ListUsersResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      serverId = pServerId_,
      users = Prelude.mempty
    }

-- | When you can get additional results from the @ListUsers@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional users.
listUsersResponse_nextToken :: Lens.Lens' ListUsersResponse (Prelude.Maybe Prelude.Text)
listUsersResponse_nextToken = Lens.lens (\ListUsersResponse' {nextToken} -> nextToken) (\s@ListUsersResponse' {} a -> s {nextToken = a} :: ListUsersResponse)

-- | The response's http status code.
listUsersResponse_httpStatus :: Lens.Lens' ListUsersResponse Prelude.Int
listUsersResponse_httpStatus = Lens.lens (\ListUsersResponse' {httpStatus} -> httpStatus) (\s@ListUsersResponse' {} a -> s {httpStatus = a} :: ListUsersResponse)

-- | A system-assigned unique identifier for a server that the users are
-- assigned to.
listUsersResponse_serverId :: Lens.Lens' ListUsersResponse Prelude.Text
listUsersResponse_serverId = Lens.lens (\ListUsersResponse' {serverId} -> serverId) (\s@ListUsersResponse' {} a -> s {serverId = a} :: ListUsersResponse)

-- | Returns the user accounts and their properties for the @ServerId@ value
-- that you specify.
listUsersResponse_users :: Lens.Lens' ListUsersResponse [ListedUser]
listUsersResponse_users = Lens.lens (\ListUsersResponse' {users} -> users) (\s@ListUsersResponse' {} a -> s {users = a} :: ListUsersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListUsersResponse where
  rnf ListUsersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf users
