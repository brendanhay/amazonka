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
-- Module      : Network.AWS.AlexaBusiness.SearchUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches users and lists the ones that meet a set of filter and sort
-- criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchUsers
  ( -- * Creating a Request
    SearchUsers (..),
    newSearchUsers,

    -- * Request Lenses
    searchUsers_nextToken,
    searchUsers_sortCriteria,
    searchUsers_maxResults,
    searchUsers_filters,

    -- * Destructuring the Response
    SearchUsersResponse (..),
    newSearchUsersResponse,

    -- * Response Lenses
    searchUsersResponse_nextToken,
    searchUsersResponse_totalCount,
    searchUsersResponse_users,
    searchUsersResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchUsers' smart constructor.
data SearchUsers = SearchUsers'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@. Required.
    nextToken :: Core.Maybe Core.Text,
    -- | The sort order to use in listing the filtered set of users. Required.
    -- Supported sort keys are UserId, FirstName, LastName, Email, and
    -- EnrollmentStatus.
    sortCriteria :: Core.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    -- Required.
    maxResults :: Core.Maybe Core.Natural,
    -- | The filters to use for listing a specific set of users. Required.
    -- Supported filter keys are UserId, FirstName, LastName, Email, and
    -- EnrollmentStatus.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchUsers_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@. Required.
--
-- 'sortCriteria', 'searchUsers_sortCriteria' - The sort order to use in listing the filtered set of users. Required.
-- Supported sort keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
--
-- 'maxResults', 'searchUsers_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
-- Required.
--
-- 'filters', 'searchUsers_filters' - The filters to use for listing a specific set of users. Required.
-- Supported filter keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
newSearchUsers ::
  SearchUsers
newSearchUsers =
  SearchUsers'
    { nextToken = Core.Nothing,
      sortCriteria = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@. Required.
searchUsers_nextToken :: Lens.Lens' SearchUsers (Core.Maybe Core.Text)
searchUsers_nextToken = Lens.lens (\SearchUsers' {nextToken} -> nextToken) (\s@SearchUsers' {} a -> s {nextToken = a} :: SearchUsers)

-- | The sort order to use in listing the filtered set of users. Required.
-- Supported sort keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
searchUsers_sortCriteria :: Lens.Lens' SearchUsers (Core.Maybe [Sort])
searchUsers_sortCriteria = Lens.lens (\SearchUsers' {sortCriteria} -> sortCriteria) (\s@SearchUsers' {} a -> s {sortCriteria = a} :: SearchUsers) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
-- Required.
searchUsers_maxResults :: Lens.Lens' SearchUsers (Core.Maybe Core.Natural)
searchUsers_maxResults = Lens.lens (\SearchUsers' {maxResults} -> maxResults) (\s@SearchUsers' {} a -> s {maxResults = a} :: SearchUsers)

-- | The filters to use for listing a specific set of users. Required.
-- Supported filter keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
searchUsers_filters :: Lens.Lens' SearchUsers (Core.Maybe [Filter])
searchUsers_filters = Lens.lens (\SearchUsers' {filters} -> filters) (\s@SearchUsers' {} a -> s {filters = a} :: SearchUsers) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager SearchUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchUsersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? searchUsersResponse_users Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& searchUsers_nextToken
          Lens..~ rs
          Lens.^? searchUsersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest SearchUsers where
  type AWSResponse SearchUsers = SearchUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchUsersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "TotalCount")
            Core.<*> (x Core..?> "Users" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchUsers

instance Core.NFData SearchUsers

instance Core.ToHeaders SearchUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.SearchUsers" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchUsers where
  toJSON SearchUsers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath SearchUsers where
  toPath = Core.const "/"

instance Core.ToQuery SearchUsers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchUsersResponse' smart constructor.
data SearchUsersResponse = SearchUsersResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of users returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The users that meet the specified set of filter criteria, in sort order.
    users :: Core.Maybe [UserData],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchUsersResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'totalCount', 'searchUsersResponse_totalCount' - The total number of users returned.
--
-- 'users', 'searchUsersResponse_users' - The users that meet the specified set of filter criteria, in sort order.
--
-- 'httpStatus', 'searchUsersResponse_httpStatus' - The response's http status code.
newSearchUsersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchUsersResponse
newSearchUsersResponse pHttpStatus_ =
  SearchUsersResponse'
    { nextToken = Core.Nothing,
      totalCount = Core.Nothing,
      users = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchUsersResponse_nextToken :: Lens.Lens' SearchUsersResponse (Core.Maybe Core.Text)
searchUsersResponse_nextToken = Lens.lens (\SearchUsersResponse' {nextToken} -> nextToken) (\s@SearchUsersResponse' {} a -> s {nextToken = a} :: SearchUsersResponse)

-- | The total number of users returned.
searchUsersResponse_totalCount :: Lens.Lens' SearchUsersResponse (Core.Maybe Core.Int)
searchUsersResponse_totalCount = Lens.lens (\SearchUsersResponse' {totalCount} -> totalCount) (\s@SearchUsersResponse' {} a -> s {totalCount = a} :: SearchUsersResponse)

-- | The users that meet the specified set of filter criteria, in sort order.
searchUsersResponse_users :: Lens.Lens' SearchUsersResponse (Core.Maybe [UserData])
searchUsersResponse_users = Lens.lens (\SearchUsersResponse' {users} -> users) (\s@SearchUsersResponse' {} a -> s {users = a} :: SearchUsersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchUsersResponse_httpStatus :: Lens.Lens' SearchUsersResponse Core.Int
searchUsersResponse_httpStatus = Lens.lens (\SearchUsersResponse' {httpStatus} -> httpStatus) (\s@SearchUsersResponse' {} a -> s {httpStatus = a} :: SearchUsersResponse)

instance Core.NFData SearchUsersResponse
