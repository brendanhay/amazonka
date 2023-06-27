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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    describeUsers_authenticationToken,
    describeUsers_fields,
    describeUsers_include,
    describeUsers_limit,
    describeUsers_marker,
    describeUsers_order,
    describeUsers_organizationId,
    describeUsers_query,
    describeUsers_sort,
    describeUsers_userIds,

    -- * Destructuring the Response
    DescribeUsersResponse (..),
    newDescribeUsersResponse,

    -- * Response Lenses
    describeUsersResponse_marker,
    describeUsersResponse_totalNumberOfUsers,
    describeUsersResponse_users,
    describeUsersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | Amazon WorkDocs authentication token. Not required when using Amazon Web
    -- Services administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A comma-separated list of values. Specify \"STORAGE_METADATA\" to
    -- include the user storage quota and utilization information.
    fields :: Prelude.Maybe Prelude.Text,
    -- | The state of the users. Specify \"ALL\" to include inactive users.
    include :: Prelude.Maybe UserFilterType,
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The order for the results.
    order :: Prelude.Maybe OrderType,
    -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | A query to filter users by user name. Remember the following about the
    -- @Userids@ and @Query@ parameters:
    --
    -- -   If you don\'t use either parameter, the API returns a paginated list
    --     of all users on the site.
    --
    -- -   If you use both parameters, the API ignores the @Query@ parameter.
    --
    -- -   The @Userid@ parameter only returns user names that match a
    --     corresponding user ID.
    --
    -- -   The @Query@ parameter runs a \"prefix\" search for users by the
    --     @GivenName@, @SurName@, or @UserName@ fields included in a
    --     <https://docs.aws.amazon.com/workdocs/latest/APIReference/API_CreateUser.html CreateUser>
    --     API call. For example, querying on @Ma@ returns Márcia Oliveira,
    --     María García, and Mateo Jackson. If you use multiple characters, the
    --     API only returns data that matches all characters. For example,
    --     querying on @Ma J@ only returns Mateo Jackson.
    query :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The sorting criteria.
    sort :: Prelude.Maybe UserSortType,
    -- | The IDs of the users.
    userIds :: Prelude.Maybe Prelude.Text
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
-- 'authenticationToken', 'describeUsers_authenticationToken' - Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
--
-- 'fields', 'describeUsers_fields' - A comma-separated list of values. Specify \"STORAGE_METADATA\" to
-- include the user storage quota and utilization information.
--
-- 'include', 'describeUsers_include' - The state of the users. Specify \"ALL\" to include inactive users.
--
-- 'limit', 'describeUsers_limit' - The maximum number of items to return.
--
-- 'marker', 'describeUsers_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'order', 'describeUsers_order' - The order for the results.
--
-- 'organizationId', 'describeUsers_organizationId' - The ID of the organization.
--
-- 'query', 'describeUsers_query' - A query to filter users by user name. Remember the following about the
-- @Userids@ and @Query@ parameters:
--
-- -   If you don\'t use either parameter, the API returns a paginated list
--     of all users on the site.
--
-- -   If you use both parameters, the API ignores the @Query@ parameter.
--
-- -   The @Userid@ parameter only returns user names that match a
--     corresponding user ID.
--
-- -   The @Query@ parameter runs a \"prefix\" search for users by the
--     @GivenName@, @SurName@, or @UserName@ fields included in a
--     <https://docs.aws.amazon.com/workdocs/latest/APIReference/API_CreateUser.html CreateUser>
--     API call. For example, querying on @Ma@ returns Márcia Oliveira,
--     María García, and Mateo Jackson. If you use multiple characters, the
--     API only returns data that matches all characters. For example,
--     querying on @Ma J@ only returns Mateo Jackson.
--
-- 'sort', 'describeUsers_sort' - The sorting criteria.
--
-- 'userIds', 'describeUsers_userIds' - The IDs of the users.
newDescribeUsers ::
  DescribeUsers
newDescribeUsers =
  DescribeUsers'
    { authenticationToken =
        Prelude.Nothing,
      fields = Prelude.Nothing,
      include = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      order = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      query = Prelude.Nothing,
      sort = Prelude.Nothing,
      userIds = Prelude.Nothing
    }

-- | Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
describeUsers_authenticationToken :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_authenticationToken = Lens.lens (\DescribeUsers' {authenticationToken} -> authenticationToken) (\s@DescribeUsers' {} a -> s {authenticationToken = a} :: DescribeUsers) Prelude.. Lens.mapping Data._Sensitive

-- | A comma-separated list of values. Specify \"STORAGE_METADATA\" to
-- include the user storage quota and utilization information.
describeUsers_fields :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_fields = Lens.lens (\DescribeUsers' {fields} -> fields) (\s@DescribeUsers' {} a -> s {fields = a} :: DescribeUsers)

-- | The state of the users. Specify \"ALL\" to include inactive users.
describeUsers_include :: Lens.Lens' DescribeUsers (Prelude.Maybe UserFilterType)
describeUsers_include = Lens.lens (\DescribeUsers' {include} -> include) (\s@DescribeUsers' {} a -> s {include = a} :: DescribeUsers)

-- | The maximum number of items to return.
describeUsers_limit :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Natural)
describeUsers_limit = Lens.lens (\DescribeUsers' {limit} -> limit) (\s@DescribeUsers' {} a -> s {limit = a} :: DescribeUsers)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeUsers_marker :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_marker = Lens.lens (\DescribeUsers' {marker} -> marker) (\s@DescribeUsers' {} a -> s {marker = a} :: DescribeUsers)

-- | The order for the results.
describeUsers_order :: Lens.Lens' DescribeUsers (Prelude.Maybe OrderType)
describeUsers_order = Lens.lens (\DescribeUsers' {order} -> order) (\s@DescribeUsers' {} a -> s {order = a} :: DescribeUsers)

-- | The ID of the organization.
describeUsers_organizationId :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_organizationId = Lens.lens (\DescribeUsers' {organizationId} -> organizationId) (\s@DescribeUsers' {} a -> s {organizationId = a} :: DescribeUsers)

-- | A query to filter users by user name. Remember the following about the
-- @Userids@ and @Query@ parameters:
--
-- -   If you don\'t use either parameter, the API returns a paginated list
--     of all users on the site.
--
-- -   If you use both parameters, the API ignores the @Query@ parameter.
--
-- -   The @Userid@ parameter only returns user names that match a
--     corresponding user ID.
--
-- -   The @Query@ parameter runs a \"prefix\" search for users by the
--     @GivenName@, @SurName@, or @UserName@ fields included in a
--     <https://docs.aws.amazon.com/workdocs/latest/APIReference/API_CreateUser.html CreateUser>
--     API call. For example, querying on @Ma@ returns Márcia Oliveira,
--     María García, and Mateo Jackson. If you use multiple characters, the
--     API only returns data that matches all characters. For example,
--     querying on @Ma J@ only returns Mateo Jackson.
describeUsers_query :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_query = Lens.lens (\DescribeUsers' {query} -> query) (\s@DescribeUsers' {} a -> s {query = a} :: DescribeUsers) Prelude.. Lens.mapping Data._Sensitive

-- | The sorting criteria.
describeUsers_sort :: Lens.Lens' DescribeUsers (Prelude.Maybe UserSortType)
describeUsers_sort = Lens.lens (\DescribeUsers' {sort} -> sort) (\s@DescribeUsers' {} a -> s {sort = a} :: DescribeUsers)

-- | The IDs of the users.
describeUsers_userIds :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_userIds = Lens.lens (\DescribeUsers' {userIds} -> userIds) (\s@DescribeUsers' {} a -> s {userIds = a} :: DescribeUsers)

instance Core.AWSPager DescribeUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_users
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeUsers_marker
          Lens..~ rs
          Lens.^? describeUsersResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeUsers where
  type
    AWSResponse DescribeUsers =
      DescribeUsersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsersResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "TotalNumberOfUsers")
            Prelude.<*> (x Data..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUsers where
  hashWithSalt _salt DescribeUsers' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` include
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` userIds

instance Prelude.NFData DescribeUsers where
  rnf DescribeUsers' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf include
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf order
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf query
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf userIds

instance Data.ToHeaders DescribeUsers where
  toHeaders DescribeUsers' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DescribeUsers where
  toPath = Prelude.const "/api/v1/users"

instance Data.ToQuery DescribeUsers where
  toQuery DescribeUsers' {..} =
    Prelude.mconcat
      [ "fields" Data.=: fields,
        "include" Data.=: include,
        "limit" Data.=: limit,
        "marker" Data.=: marker,
        "order" Data.=: order,
        "organizationId" Data.=: organizationId,
        "query" Data.=: query,
        "sort" Data.=: sort,
        "userIds" Data.=: userIds
      ]

-- | /See:/ 'newDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The total number of users included in the results.
    totalNumberOfUsers :: Prelude.Maybe Prelude.Integer,
    -- | The users.
    users :: Prelude.Maybe [User],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeUsersResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'totalNumberOfUsers', 'describeUsersResponse_totalNumberOfUsers' - The total number of users included in the results.
--
-- 'users', 'describeUsersResponse_users' - The users.
--
-- 'httpStatus', 'describeUsersResponse_httpStatus' - The response's http status code.
newDescribeUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUsersResponse
newDescribeUsersResponse pHttpStatus_ =
  DescribeUsersResponse'
    { marker = Prelude.Nothing,
      totalNumberOfUsers = Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeUsersResponse_marker :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Text)
describeUsersResponse_marker = Lens.lens (\DescribeUsersResponse' {marker} -> marker) (\s@DescribeUsersResponse' {} a -> s {marker = a} :: DescribeUsersResponse)

-- | The total number of users included in the results.
describeUsersResponse_totalNumberOfUsers :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Integer)
describeUsersResponse_totalNumberOfUsers = Lens.lens (\DescribeUsersResponse' {totalNumberOfUsers} -> totalNumberOfUsers) (\s@DescribeUsersResponse' {} a -> s {totalNumberOfUsers = a} :: DescribeUsersResponse)

-- | The users.
describeUsersResponse_users :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe [User])
describeUsersResponse_users = Lens.lens (\DescribeUsersResponse' {users} -> users) (\s@DescribeUsersResponse' {} a -> s {users = a} :: DescribeUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeUsersResponse_httpStatus :: Lens.Lens' DescribeUsersResponse Prelude.Int
describeUsersResponse_httpStatus = Lens.lens (\DescribeUsersResponse' {httpStatus} -> httpStatus) (\s@DescribeUsersResponse' {} a -> s {httpStatus = a} :: DescribeUsersResponse)

instance Prelude.NFData DescribeUsersResponse where
  rnf DescribeUsersResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf totalNumberOfUsers
      `Prelude.seq` Prelude.rnf users
      `Prelude.seq` Prelude.rnf httpStatus
