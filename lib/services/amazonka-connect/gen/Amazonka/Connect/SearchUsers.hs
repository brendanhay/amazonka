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
-- Module      : Amazonka.Connect.SearchUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches users in an Amazon Connect instance, with optional filtering.
--
-- @AfterContactWorkTimeLimit@ is returned in milliseconds.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchUsers
  ( -- * Creating a Request
    SearchUsers (..),
    newSearchUsers,

    -- * Request Lenses
    searchUsers_instanceId,
    searchUsers_maxResults,
    searchUsers_nextToken,
    searchUsers_searchCriteria,
    searchUsers_searchFilter,

    -- * Destructuring the Response
    SearchUsersResponse (..),
    newSearchUsersResponse,

    -- * Response Lenses
    searchUsersResponse_approximateTotalCount,
    searchUsersResponse_nextToken,
    searchUsersResponse_users,
    searchUsersResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchUsers' smart constructor.
data SearchUsers = SearchUsers'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    searchCriteria :: Prelude.Maybe UserSearchCriteria,
    -- | Filters to be applied to search results.
    searchFilter :: Prelude.Maybe UserSearchFilter
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
-- 'instanceId', 'searchUsers_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'maxResults', 'searchUsers_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchUsers_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'searchCriteria', 'searchUsers_searchCriteria' - Undocumented member.
--
-- 'searchFilter', 'searchUsers_searchFilter' - Filters to be applied to search results.
newSearchUsers ::
  SearchUsers
newSearchUsers =
  SearchUsers'
    { instanceId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      searchCriteria = Prelude.Nothing,
      searchFilter = Prelude.Nothing
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
searchUsers_instanceId :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Text)
searchUsers_instanceId = Lens.lens (\SearchUsers' {instanceId} -> instanceId) (\s@SearchUsers' {} a -> s {instanceId = a} :: SearchUsers)

-- | The maximum number of results to return per page.
searchUsers_maxResults :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Natural)
searchUsers_maxResults = Lens.lens (\SearchUsers' {maxResults} -> maxResults) (\s@SearchUsers' {} a -> s {maxResults = a} :: SearchUsers)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchUsers_nextToken :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Text)
searchUsers_nextToken = Lens.lens (\SearchUsers' {nextToken} -> nextToken) (\s@SearchUsers' {} a -> s {nextToken = a} :: SearchUsers)

-- | Undocumented member.
searchUsers_searchCriteria :: Lens.Lens' SearchUsers (Prelude.Maybe UserSearchCriteria)
searchUsers_searchCriteria = Lens.lens (\SearchUsers' {searchCriteria} -> searchCriteria) (\s@SearchUsers' {} a -> s {searchCriteria = a} :: SearchUsers)

-- | Filters to be applied to search results.
searchUsers_searchFilter :: Lens.Lens' SearchUsers (Prelude.Maybe UserSearchFilter)
searchUsers_searchFilter = Lens.lens (\SearchUsers' {searchFilter} -> searchFilter) (\s@SearchUsers' {} a -> s {searchFilter = a} :: SearchUsers)

instance Core.AWSPager SearchUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchUsersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchUsersResponse_users Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchUsers_nextToken
          Lens..~ rs
          Lens.^? searchUsersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest SearchUsers where
  type AWSResponse SearchUsers = SearchUsersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchUsersResponse'
            Prelude.<$> (x Data..?> "ApproximateTotalCount")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchUsers where
  hashWithSalt _salt SearchUsers' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` searchCriteria
      `Prelude.hashWithSalt` searchFilter

instance Prelude.NFData SearchUsers where
  rnf SearchUsers' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchCriteria
      `Prelude.seq` Prelude.rnf searchFilter

instance Data.ToHeaders SearchUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchUsers where
  toJSON SearchUsers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SearchCriteria" Data..=)
              Prelude.<$> searchCriteria,
            ("SearchFilter" Data..=) Prelude.<$> searchFilter
          ]
      )

instance Data.ToPath SearchUsers where
  toPath = Prelude.const "/search-users"

instance Data.ToQuery SearchUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchUsersResponse' smart constructor.
data SearchUsersResponse = SearchUsersResponse'
  { -- | The total number of users who matched your search query.
    approximateTotalCount :: Prelude.Maybe Prelude.Integer,
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the users.
    users :: Prelude.Maybe [UserSearchSummary],
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
-- 'approximateTotalCount', 'searchUsersResponse_approximateTotalCount' - The total number of users who matched your search query.
--
-- 'nextToken', 'searchUsersResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'users', 'searchUsersResponse_users' - Information about the users.
--
-- 'httpStatus', 'searchUsersResponse_httpStatus' - The response's http status code.
newSearchUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchUsersResponse
newSearchUsersResponse pHttpStatus_ =
  SearchUsersResponse'
    { approximateTotalCount =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of users who matched your search query.
searchUsersResponse_approximateTotalCount :: Lens.Lens' SearchUsersResponse (Prelude.Maybe Prelude.Integer)
searchUsersResponse_approximateTotalCount = Lens.lens (\SearchUsersResponse' {approximateTotalCount} -> approximateTotalCount) (\s@SearchUsersResponse' {} a -> s {approximateTotalCount = a} :: SearchUsersResponse)

-- | If there are additional results, this is the token for the next set of
-- results.
searchUsersResponse_nextToken :: Lens.Lens' SearchUsersResponse (Prelude.Maybe Prelude.Text)
searchUsersResponse_nextToken = Lens.lens (\SearchUsersResponse' {nextToken} -> nextToken) (\s@SearchUsersResponse' {} a -> s {nextToken = a} :: SearchUsersResponse)

-- | Information about the users.
searchUsersResponse_users :: Lens.Lens' SearchUsersResponse (Prelude.Maybe [UserSearchSummary])
searchUsersResponse_users = Lens.lens (\SearchUsersResponse' {users} -> users) (\s@SearchUsersResponse' {} a -> s {users = a} :: SearchUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchUsersResponse_httpStatus :: Lens.Lens' SearchUsersResponse Prelude.Int
searchUsersResponse_httpStatus = Lens.lens (\SearchUsersResponse' {httpStatus} -> httpStatus) (\s@SearchUsersResponse' {} a -> s {httpStatus = a} :: SearchUsersResponse)

instance Prelude.NFData SearchUsersResponse where
  rnf SearchUsersResponse' {..} =
    Prelude.rnf approximateTotalCount
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf users
      `Prelude.seq` Prelude.rnf httpStatus
