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
-- Module      : Network.AWS.WorkDocs.DescribeUsers
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
module Network.AWS.WorkDocs.DescribeUsers
  ( -- * Creating a Request
    DescribeUsers (..),
    newDescribeUsers,

    -- * Request Lenses
    describeUsers_organizationId,
    describeUsers_query,
    describeUsers_userIds,
    describeUsers_include,
    describeUsers_fields,
    describeUsers_order,
    describeUsers_authenticationToken,
    describeUsers_limit,
    describeUsers_sort,
    describeUsers_marker,

    -- * Destructuring the Response
    DescribeUsersResponse (..),
    newDescribeUsersResponse,

    -- * Response Lenses
    describeUsersResponse_totalNumberOfUsers,
    describeUsersResponse_users,
    describeUsersResponse_marker,
    describeUsersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | A query to filter users by user name.
    query :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The IDs of the users.
    userIds :: Prelude.Maybe Prelude.Text,
    -- | The state of the users. Specify \"ALL\" to include inactive users.
    include :: Prelude.Maybe UserFilterType,
    -- | A comma-separated list of values. Specify \"STORAGE_METADATA\" to
    -- include the user storage quota and utilization information.
    fields :: Prelude.Maybe Prelude.Text,
    -- | The order for the results.
    order :: Prelude.Maybe OrderType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The sorting criteria.
    sort :: Prelude.Maybe UserSortType,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text
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
-- 'organizationId', 'describeUsers_organizationId' - The ID of the organization.
--
-- 'query', 'describeUsers_query' - A query to filter users by user name.
--
-- 'userIds', 'describeUsers_userIds' - The IDs of the users.
--
-- 'include', 'describeUsers_include' - The state of the users. Specify \"ALL\" to include inactive users.
--
-- 'fields', 'describeUsers_fields' - A comma-separated list of values. Specify \"STORAGE_METADATA\" to
-- include the user storage quota and utilization information.
--
-- 'order', 'describeUsers_order' - The order for the results.
--
-- 'authenticationToken', 'describeUsers_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'limit', 'describeUsers_limit' - The maximum number of items to return.
--
-- 'sort', 'describeUsers_sort' - The sorting criteria.
--
-- 'marker', 'describeUsers_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
newDescribeUsers ::
  DescribeUsers
newDescribeUsers =
  DescribeUsers'
    { organizationId = Prelude.Nothing,
      query = Prelude.Nothing,
      userIds = Prelude.Nothing,
      include = Prelude.Nothing,
      fields = Prelude.Nothing,
      order = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      sort = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The ID of the organization.
describeUsers_organizationId :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_organizationId = Lens.lens (\DescribeUsers' {organizationId} -> organizationId) (\s@DescribeUsers' {} a -> s {organizationId = a} :: DescribeUsers)

-- | A query to filter users by user name.
describeUsers_query :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_query = Lens.lens (\DescribeUsers' {query} -> query) (\s@DescribeUsers' {} a -> s {query = a} :: DescribeUsers) Prelude.. Lens.mapping Core._Sensitive

-- | The IDs of the users.
describeUsers_userIds :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_userIds = Lens.lens (\DescribeUsers' {userIds} -> userIds) (\s@DescribeUsers' {} a -> s {userIds = a} :: DescribeUsers)

-- | The state of the users. Specify \"ALL\" to include inactive users.
describeUsers_include :: Lens.Lens' DescribeUsers (Prelude.Maybe UserFilterType)
describeUsers_include = Lens.lens (\DescribeUsers' {include} -> include) (\s@DescribeUsers' {} a -> s {include = a} :: DescribeUsers)

-- | A comma-separated list of values. Specify \"STORAGE_METADATA\" to
-- include the user storage quota and utilization information.
describeUsers_fields :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_fields = Lens.lens (\DescribeUsers' {fields} -> fields) (\s@DescribeUsers' {} a -> s {fields = a} :: DescribeUsers)

-- | The order for the results.
describeUsers_order :: Lens.Lens' DescribeUsers (Prelude.Maybe OrderType)
describeUsers_order = Lens.lens (\DescribeUsers' {order} -> order) (\s@DescribeUsers' {} a -> s {order = a} :: DescribeUsers)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeUsers_authenticationToken :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_authenticationToken = Lens.lens (\DescribeUsers' {authenticationToken} -> authenticationToken) (\s@DescribeUsers' {} a -> s {authenticationToken = a} :: DescribeUsers) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of items to return.
describeUsers_limit :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Natural)
describeUsers_limit = Lens.lens (\DescribeUsers' {limit} -> limit) (\s@DescribeUsers' {} a -> s {limit = a} :: DescribeUsers)

-- | The sorting criteria.
describeUsers_sort :: Lens.Lens' DescribeUsers (Prelude.Maybe UserSortType)
describeUsers_sort = Lens.lens (\DescribeUsers' {sort} -> sort) (\s@DescribeUsers' {} a -> s {sort = a} :: DescribeUsers)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeUsers_marker :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_marker = Lens.lens (\DescribeUsers' {marker} -> marker) (\s@DescribeUsers' {} a -> s {marker = a} :: DescribeUsers)

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
            Prelude.<$> (x Core..?> "TotalNumberOfUsers")
            Prelude.<*> (x Core..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUsers

instance Prelude.NFData DescribeUsers

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
      [ "organizationId" Core.=: organizationId,
        "query" Core.=: query,
        "userIds" Core.=: userIds,
        "include" Core.=: include,
        "fields" Core.=: fields,
        "order" Core.=: order,
        "limit" Core.=: limit,
        "sort" Core.=: sort,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | The total number of users included in the results.
    totalNumberOfUsers :: Prelude.Maybe Prelude.Integer,
    -- | The users.
    users :: Prelude.Maybe [User],
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
-- 'totalNumberOfUsers', 'describeUsersResponse_totalNumberOfUsers' - The total number of users included in the results.
--
-- 'users', 'describeUsersResponse_users' - The users.
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
    { totalNumberOfUsers =
        Prelude.Nothing,
      users = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of users included in the results.
describeUsersResponse_totalNumberOfUsers :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Integer)
describeUsersResponse_totalNumberOfUsers = Lens.lens (\DescribeUsersResponse' {totalNumberOfUsers} -> totalNumberOfUsers) (\s@DescribeUsersResponse' {} a -> s {totalNumberOfUsers = a} :: DescribeUsersResponse)

-- | The users.
describeUsersResponse_users :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe [User])
describeUsersResponse_users = Lens.lens (\DescribeUsersResponse' {users} -> users) (\s@DescribeUsersResponse' {} a -> s {users = a} :: DescribeUsersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeUsersResponse_marker :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Text)
describeUsersResponse_marker = Lens.lens (\DescribeUsersResponse' {marker} -> marker) (\s@DescribeUsersResponse' {} a -> s {marker = a} :: DescribeUsersResponse)

-- | The response's http status code.
describeUsersResponse_httpStatus :: Lens.Lens' DescribeUsersResponse Prelude.Int
describeUsersResponse_httpStatus = Lens.lens (\DescribeUsersResponse' {httpStatus} -> httpStatus) (\s@DescribeUsersResponse' {} a -> s {httpStatus = a} :: DescribeUsersResponse)

instance Prelude.NFData DescribeUsersResponse
