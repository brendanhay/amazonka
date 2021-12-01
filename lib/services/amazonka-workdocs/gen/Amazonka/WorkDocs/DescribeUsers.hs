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
-- Module      : Amazonka.WorkDocs.DescribeUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified users. You can describe all users or filter the
-- results (for example, by status or organization).
--
-- By default, Amazon WorkDocs returns the first 24 active or pending
-- users. If there are more results, the response includes a marker that
-- you can use to request the next set of results.
--
-- This operation returns paginated results.
module Amazonka.WorkDocs.DescribeUsers
  ( -- * Creating a Request
    DescribeUsers (..),
    newDescribeUsers,

    -- * Request Lenses
    describeUsers_include,
    describeUsers_userIds,
    describeUsers_authenticationToken,
    describeUsers_sort,
    describeUsers_marker,
    describeUsers_query,
    describeUsers_limit,
    describeUsers_order,
    describeUsers_organizationId,
    describeUsers_fields,

    -- * Destructuring the Response
    DescribeUsersResponse (..),
    newDescribeUsersResponse,

    -- * Response Lenses
    describeUsersResponse_users,
    describeUsersResponse_totalNumberOfUsers,
    describeUsersResponse_marker,
    describeUsersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | The state of the users. Specify \"ALL\" to include inactive users.
    include :: Prelude.Maybe UserFilterType,
    -- | The IDs of the users.
    userIds :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The sorting criteria.
    sort :: Prelude.Maybe UserSortType,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | A query to filter users by user name.
    query :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The order for the results.
    order :: Prelude.Maybe OrderType,
    -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | A comma-separated list of values. Specify \"STORAGE_METADATA\" to
    -- include the user storage quota and utilization information.
    fields :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeUsers_include' - The state of the users. Specify \"ALL\" to include inactive users.
--
-- 'userIds', 'describeUsers_userIds' - The IDs of the users.
--
-- 'authenticationToken', 'describeUsers_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'sort', 'describeUsers_sort' - The sorting criteria.
--
-- 'marker', 'describeUsers_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'query', 'describeUsers_query' - A query to filter users by user name.
--
-- 'limit', 'describeUsers_limit' - The maximum number of items to return.
--
-- 'order', 'describeUsers_order' - The order for the results.
--
-- 'organizationId', 'describeUsers_organizationId' - The ID of the organization.
--
-- 'fields', 'describeUsers_fields' - A comma-separated list of values. Specify \"STORAGE_METADATA\" to
-- include the user storage quota and utilization information.
newDescribeUsers ::
  DescribeUsers
newDescribeUsers =
  DescribeUsers'
    { include = Prelude.Nothing,
      userIds = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      sort = Prelude.Nothing,
      marker = Prelude.Nothing,
      query = Prelude.Nothing,
      limit = Prelude.Nothing,
      order = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      fields = Prelude.Nothing
    }

-- | The state of the users. Specify \"ALL\" to include inactive users.
describeUsers_include :: Lens.Lens' DescribeUsers (Prelude.Maybe UserFilterType)
describeUsers_include = Lens.lens (\DescribeUsers' {include} -> include) (\s@DescribeUsers' {} a -> s {include = a} :: DescribeUsers)

-- | The IDs of the users.
describeUsers_userIds :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_userIds = Lens.lens (\DescribeUsers' {userIds} -> userIds) (\s@DescribeUsers' {} a -> s {userIds = a} :: DescribeUsers)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeUsers_authenticationToken :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_authenticationToken = Lens.lens (\DescribeUsers' {authenticationToken} -> authenticationToken) (\s@DescribeUsers' {} a -> s {authenticationToken = a} :: DescribeUsers) Prelude.. Lens.mapping Core._Sensitive

-- | The sorting criteria.
describeUsers_sort :: Lens.Lens' DescribeUsers (Prelude.Maybe UserSortType)
describeUsers_sort = Lens.lens (\DescribeUsers' {sort} -> sort) (\s@DescribeUsers' {} a -> s {sort = a} :: DescribeUsers)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeUsers_marker :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_marker = Lens.lens (\DescribeUsers' {marker} -> marker) (\s@DescribeUsers' {} a -> s {marker = a} :: DescribeUsers)

-- | A query to filter users by user name.
describeUsers_query :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_query = Lens.lens (\DescribeUsers' {query} -> query) (\s@DescribeUsers' {} a -> s {query = a} :: DescribeUsers) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of items to return.
describeUsers_limit :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Natural)
describeUsers_limit = Lens.lens (\DescribeUsers' {limit} -> limit) (\s@DescribeUsers' {} a -> s {limit = a} :: DescribeUsers)

-- | The order for the results.
describeUsers_order :: Lens.Lens' DescribeUsers (Prelude.Maybe OrderType)
describeUsers_order = Lens.lens (\DescribeUsers' {order} -> order) (\s@DescribeUsers' {} a -> s {order = a} :: DescribeUsers)

-- | The ID of the organization.
describeUsers_organizationId :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_organizationId = Lens.lens (\DescribeUsers' {organizationId} -> organizationId) (\s@DescribeUsers' {} a -> s {organizationId = a} :: DescribeUsers)

-- | A comma-separated list of values. Specify \"STORAGE_METADATA\" to
-- include the user storage quota and utilization information.
describeUsers_fields :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_fields = Lens.lens (\DescribeUsers' {fields} -> fields) (\s@DescribeUsers' {} a -> s {fields = a} :: DescribeUsers)

instance Core.AWSPager DescribeUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_users Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeUsers_marker
          Lens..~ rs
          Lens.^? describeUsersResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest DescribeUsers where
  type
    AWSResponse DescribeUsers =
      DescribeUsersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsersResponse'
            Prelude.<$> (x Core..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "TotalNumberOfUsers")
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUsers where
  hashWithSalt salt' DescribeUsers' {..} =
    salt' `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` include

instance Prelude.NFData DescribeUsers where
  rnf DescribeUsers' {..} =
    Prelude.rnf include
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf order
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf query
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf userIds

instance Core.ToHeaders DescribeUsers where
  toHeaders DescribeUsers' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DescribeUsers where
  toPath = Prelude.const "/api/v1/users"

instance Core.ToQuery DescribeUsers where
  toQuery DescribeUsers' {..} =
    Prelude.mconcat
      [ "include" Core.=: include,
        "userIds" Core.=: userIds,
        "sort" Core.=: sort,
        "marker" Core.=: marker,
        "query" Core.=: query,
        "limit" Core.=: limit,
        "order" Core.=: order,
        "organizationId" Core.=: organizationId,
        "fields" Core.=: fields
      ]

-- | /See:/ 'newDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | The users.
    users :: Prelude.Maybe [User],
    -- | The total number of users included in the results.
    totalNumberOfUsers :: Prelude.Maybe Prelude.Integer,
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'users', 'describeUsersResponse_users' - The users.
--
-- 'totalNumberOfUsers', 'describeUsersResponse_totalNumberOfUsers' - The total number of users included in the results.
--
-- 'marker', 'describeUsersResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeUsersResponse_httpStatus' - The response's http status code.
newDescribeUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUsersResponse
newDescribeUsersResponse pHttpStatus_ =
  DescribeUsersResponse'
    { users = Prelude.Nothing,
      totalNumberOfUsers = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The users.
describeUsersResponse_users :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe [User])
describeUsersResponse_users = Lens.lens (\DescribeUsersResponse' {users} -> users) (\s@DescribeUsersResponse' {} a -> s {users = a} :: DescribeUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of users included in the results.
describeUsersResponse_totalNumberOfUsers :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Integer)
describeUsersResponse_totalNumberOfUsers = Lens.lens (\DescribeUsersResponse' {totalNumberOfUsers} -> totalNumberOfUsers) (\s@DescribeUsersResponse' {} a -> s {totalNumberOfUsers = a} :: DescribeUsersResponse)

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeUsersResponse_marker :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Text)
describeUsersResponse_marker = Lens.lens (\DescribeUsersResponse' {marker} -> marker) (\s@DescribeUsersResponse' {} a -> s {marker = a} :: DescribeUsersResponse)

-- | The response's http status code.
describeUsersResponse_httpStatus :: Lens.Lens' DescribeUsersResponse Prelude.Int
describeUsersResponse_httpStatus = Lens.lens (\DescribeUsersResponse' {httpStatus} -> httpStatus) (\s@DescribeUsersResponse' {} a -> s {httpStatus = a} :: DescribeUsersResponse)

instance Prelude.NFData DescribeUsersResponse where
  rnf DescribeUsersResponse' {..} =
    Prelude.rnf users
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf totalNumberOfUsers
