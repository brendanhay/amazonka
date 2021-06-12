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
-- Module      : Network.AWS.MQ.ListUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all ActiveMQ users.
module Network.AWS.MQ.ListUsers
  ( -- * Creating a Request
    ListUsers (..),
    newListUsers,

    -- * Request Lenses
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_brokerId,

    -- * Destructuring the Response
    ListUsersResponse (..),
    newListUsersResponse,

    -- * Response Lenses
    listUsersResponse_nextToken,
    listUsersResponse_brokerId,
    listUsersResponse_maxResults,
    listUsersResponse_users,
    listUsersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of ActiveMQ users that can be returned per page (20
    -- by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsers_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'maxResults', 'listUsers_maxResults' - The maximum number of ActiveMQ users that can be returned per page (20
-- by default). This value must be an integer from 5 to 100.
--
-- 'brokerId', 'listUsers_brokerId' - The unique ID that Amazon MQ generates for the broker.
newListUsers ::
  -- | 'brokerId'
  Core.Text ->
  ListUsers
newListUsers pBrokerId_ =
  ListUsers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      brokerId = pBrokerId_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listUsers_nextToken :: Lens.Lens' ListUsers (Core.Maybe Core.Text)
listUsers_nextToken = Lens.lens (\ListUsers' {nextToken} -> nextToken) (\s@ListUsers' {} a -> s {nextToken = a} :: ListUsers)

-- | The maximum number of ActiveMQ users that can be returned per page (20
-- by default). This value must be an integer from 5 to 100.
listUsers_maxResults :: Lens.Lens' ListUsers (Core.Maybe Core.Natural)
listUsers_maxResults = Lens.lens (\ListUsers' {maxResults} -> maxResults) (\s@ListUsers' {} a -> s {maxResults = a} :: ListUsers)

-- | The unique ID that Amazon MQ generates for the broker.
listUsers_brokerId :: Lens.Lens' ListUsers Core.Text
listUsers_brokerId = Lens.lens (\ListUsers' {brokerId} -> brokerId) (\s@ListUsers' {} a -> s {brokerId = a} :: ListUsers)

instance Core.AWSRequest ListUsers where
  type AWSResponse ListUsers = ListUsersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "brokerId")
            Core.<*> (x Core..?> "maxResults")
            Core.<*> (x Core..?> "users" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUsers

instance Core.NFData ListUsers

instance Core.ToHeaders ListUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListUsers where
  toPath ListUsers' {..} =
    Core.mconcat
      ["/v1/brokers/", Core.toBS brokerId, "/users"]

instance Core.ToQuery ListUsers where
  toQuery ListUsers' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | Required. The maximum number of ActiveMQ users that can be returned per
    -- page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Required. The list of all ActiveMQ usernames for the specified broker.
    users :: Core.Maybe [UserSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsersResponse_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'brokerId', 'listUsersResponse_brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- 'maxResults', 'listUsersResponse_maxResults' - Required. The maximum number of ActiveMQ users that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
--
-- 'users', 'listUsersResponse_users' - Required. The list of all ActiveMQ usernames for the specified broker.
--
-- 'httpStatus', 'listUsersResponse_httpStatus' - The response's http status code.
newListUsersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUsersResponse
newListUsersResponse pHttpStatus_ =
  ListUsersResponse'
    { nextToken = Core.Nothing,
      brokerId = Core.Nothing,
      maxResults = Core.Nothing,
      users = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listUsersResponse_nextToken :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Text)
listUsersResponse_nextToken = Lens.lens (\ListUsersResponse' {nextToken} -> nextToken) (\s@ListUsersResponse' {} a -> s {nextToken = a} :: ListUsersResponse)

-- | Required. The unique ID that Amazon MQ generates for the broker.
listUsersResponse_brokerId :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Text)
listUsersResponse_brokerId = Lens.lens (\ListUsersResponse' {brokerId} -> brokerId) (\s@ListUsersResponse' {} a -> s {brokerId = a} :: ListUsersResponse)

-- | Required. The maximum number of ActiveMQ users that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
listUsersResponse_maxResults :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Natural)
listUsersResponse_maxResults = Lens.lens (\ListUsersResponse' {maxResults} -> maxResults) (\s@ListUsersResponse' {} a -> s {maxResults = a} :: ListUsersResponse)

-- | Required. The list of all ActiveMQ usernames for the specified broker.
listUsersResponse_users :: Lens.Lens' ListUsersResponse (Core.Maybe [UserSummary])
listUsersResponse_users = Lens.lens (\ListUsersResponse' {users} -> users) (\s@ListUsersResponse' {} a -> s {users = a} :: ListUsersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUsersResponse_httpStatus :: Lens.Lens' ListUsersResponse Core.Int
listUsersResponse_httpStatus = Lens.lens (\ListUsersResponse' {httpStatus} -> httpStatus) (\s@ListUsersResponse' {} a -> s {httpStatus = a} :: ListUsersResponse)

instance Core.NFData ListUsersResponse
