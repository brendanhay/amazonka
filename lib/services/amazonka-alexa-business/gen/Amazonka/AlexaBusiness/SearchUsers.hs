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
-- Module      : Amazonka.AlexaBusiness.SearchUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches users and lists the ones that meet a set of filter and sort
-- criteria.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.SearchUsers
  ( -- * Creating a Request
    SearchUsers (..),
    newSearchUsers,

    -- * Request Lenses
    searchUsers_filters,
    searchUsers_maxResults,
    searchUsers_nextToken,
    searchUsers_sortCriteria,

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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchUsers' smart constructor.
data SearchUsers = SearchUsers'
  { -- | The filters to use for listing a specific set of users. Required.
    -- Supported filter keys are UserId, FirstName, LastName, Email, and
    -- EnrollmentStatus.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    -- Required.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@. Required.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order to use in listing the filtered set of users. Required.
    -- Supported sort keys are UserId, FirstName, LastName, Email, and
    -- EnrollmentStatus.
    sortCriteria :: Prelude.Maybe [Sort]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchUsers_filters' - The filters to use for listing a specific set of users. Required.
-- Supported filter keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
--
-- 'maxResults', 'searchUsers_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
-- Required.
--
-- 'nextToken', 'searchUsers_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@. Required.
--
-- 'sortCriteria', 'searchUsers_sortCriteria' - The sort order to use in listing the filtered set of users. Required.
-- Supported sort keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
newSearchUsers ::
  SearchUsers
newSearchUsers =
  SearchUsers'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing
    }

-- | The filters to use for listing a specific set of users. Required.
-- Supported filter keys are UserId, FirstName, LastName, Email, and
-- EnrollmentStatus.
searchUsers_filters :: Lens.Lens' SearchUsers (Prelude.Maybe [Filter])
searchUsers_filters = Lens.lens (\SearchUsers' {filters} -> filters) (\s@SearchUsers' {} a -> s {filters = a} :: SearchUsers) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
-- Required.
searchUsers_maxResults :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Natural)
searchUsers_maxResults = Lens.lens (\SearchUsers' {maxResults} -> maxResults) (\s@SearchUsers' {} a -> s {maxResults = a} :: SearchUsers)

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
searchUsers_sortCriteria = Lens.lens (\SearchUsers' {sortCriteria} -> sortCriteria) (\s@SearchUsers' {} a -> s {sortCriteria = a} :: SearchUsers) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager SearchUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchUsersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchUsersResponse_users
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchUsers_nextToken
          Lens..~ rs
          Lens.^? searchUsersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchUsers where
  type AWSResponse SearchUsers = SearchUsersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchUsersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TotalCount")
            Prelude.<*> (x Data..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchUsers where
  hashWithSalt _salt SearchUsers' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortCriteria

instance Prelude.NFData SearchUsers where
  rnf SearchUsers' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortCriteria

instance Data.ToHeaders SearchUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.SearchUsers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchUsers where
  toJSON SearchUsers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortCriteria" Data..=) Prelude.<$> sortCriteria
          ]
      )

instance Data.ToPath SearchUsers where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchUsers where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
searchUsersResponse_users = Lens.lens (\SearchUsersResponse' {users} -> users) (\s@SearchUsersResponse' {} a -> s {users = a} :: SearchUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchUsersResponse_httpStatus :: Lens.Lens' SearchUsersResponse Prelude.Int
searchUsersResponse_httpStatus = Lens.lens (\SearchUsersResponse' {httpStatus} -> httpStatus) (\s@SearchUsersResponse' {} a -> s {httpStatus = a} :: SearchUsersResponse)

instance Prelude.NFData SearchUsersResponse where
  rnf SearchUsersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf users
      `Prelude.seq` Prelude.rnf httpStatus
