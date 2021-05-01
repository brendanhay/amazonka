{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchUsers' smart constructor.
data SearchUsers = SearchUsers'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@. Required.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order to use in listing the filtered set of users. Required.
    -- Supported sort keys are UserId, FirstName, LastName, Email, and
    -- EnrollmentStatus.
    sortCriteria :: Prelude.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    -- Required.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The filters to use for listing a specific set of users. Required.
    -- Supported filter keys are UserId, FirstName, LastName, Email, and
    -- EnrollmentStatus.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@. Required.
searchUsers_nextToken :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Text)
searchUsers_nextToken = Lens.lens (\SearchUsers' {nextToken} -> nextToken) (\s@SearchUsers' {} a -> s {nextToken = a} :: SearchUsers)

-- | The sort order to use in listing the filtered set of users. Required.
-- Supported sort keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
searchUsers_sortCriteria :: Lens.Lens' SearchUsers (Prelude.Maybe [Sort])
searchUsers_sortCriteria = Lens.lens (\SearchUsers' {sortCriteria} -> sortCriteria) (\s@SearchUsers' {} a -> s {sortCriteria = a} :: SearchUsers) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
-- Required.
searchUsers_maxResults :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Natural)
searchUsers_maxResults = Lens.lens (\SearchUsers' {maxResults} -> maxResults) (\s@SearchUsers' {} a -> s {maxResults = a} :: SearchUsers)

-- | The filters to use for listing a specific set of users. Required.
-- Supported filter keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
searchUsers_filters :: Lens.Lens' SearchUsers (Prelude.Maybe [Filter])
searchUsers_filters = Lens.lens (\SearchUsers' {filters} -> filters) (\s@SearchUsers' {} a -> s {filters = a} :: SearchUsers) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager SearchUsers where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? searchUsersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? searchUsersResponse_users Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& searchUsers_nextToken
          Lens..~ rs
          Lens.^? searchUsersResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest SearchUsers where
  type Rs SearchUsers = SearchUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchUsersResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "TotalCount")
            Prelude.<*> (x Prelude..?> "Users" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchUsers

instance Prelude.NFData SearchUsers

instance Prelude.ToHeaders SearchUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.SearchUsers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SearchUsers where
  toJSON SearchUsers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("SortCriteria" Prelude..=) Prelude.<$> sortCriteria,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath SearchUsers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SearchUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchUsersResponse' smart constructor.
data SearchUsersResponse = SearchUsersResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of users returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The users that meet the specified set of filter criteria, in sort order.
    users :: Prelude.Maybe [UserData],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  SearchUsersResponse
newSearchUsersResponse pHttpStatus_ =
  SearchUsersResponse'
    { nextToken = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchUsersResponse_nextToken :: Lens.Lens' SearchUsersResponse (Prelude.Maybe Prelude.Text)
searchUsersResponse_nextToken = Lens.lens (\SearchUsersResponse' {nextToken} -> nextToken) (\s@SearchUsersResponse' {} a -> s {nextToken = a} :: SearchUsersResponse)

-- | The total number of users returned.
searchUsersResponse_totalCount :: Lens.Lens' SearchUsersResponse (Prelude.Maybe Prelude.Int)
searchUsersResponse_totalCount = Lens.lens (\SearchUsersResponse' {totalCount} -> totalCount) (\s@SearchUsersResponse' {} a -> s {totalCount = a} :: SearchUsersResponse)

-- | The users that meet the specified set of filter criteria, in sort order.
searchUsersResponse_users :: Lens.Lens' SearchUsersResponse (Prelude.Maybe [UserData])
searchUsersResponse_users = Lens.lens (\SearchUsersResponse' {users} -> users) (\s@SearchUsersResponse' {} a -> s {users = a} :: SearchUsersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
searchUsersResponse_httpStatus :: Lens.Lens' SearchUsersResponse Prelude.Int
searchUsersResponse_httpStatus = Lens.lens (\SearchUsersResponse' {httpStatus} -> httpStatus) (\s@SearchUsersResponse' {} a -> s {httpStatus = a} :: SearchUsersResponse)

instance Prelude.NFData SearchUsersResponse
