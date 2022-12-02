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
-- Module      : Amazonka.AlexaBusiness.SearchSkillGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches skill groups and lists the ones that meet a set of filter and
-- sort criteria.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.SearchSkillGroups
  ( -- * Creating a Request
    SearchSkillGroups (..),
    newSearchSkillGroups,

    -- * Request Lenses
    searchSkillGroups_sortCriteria,
    searchSkillGroups_nextToken,
    searchSkillGroups_filters,
    searchSkillGroups_maxResults,

    -- * Destructuring the Response
    SearchSkillGroupsResponse (..),
    newSearchSkillGroupsResponse,

    -- * Response Lenses
    searchSkillGroupsResponse_nextToken,
    searchSkillGroupsResponse_skillGroups,
    searchSkillGroupsResponse_totalCount,
    searchSkillGroupsResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchSkillGroups' smart constructor.
data SearchSkillGroups = SearchSkillGroups'
  { -- | The sort order to use in listing the specified set of skill groups. The
    -- supported sort key is SkillGroupName.
    sortCriteria :: Prelude.Maybe [Sort],
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@. Required.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filters to use to list a specified set of skill groups. The
    -- supported filter key is SkillGroupName.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSkillGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortCriteria', 'searchSkillGroups_sortCriteria' - The sort order to use in listing the specified set of skill groups. The
-- supported sort key is SkillGroupName.
--
-- 'nextToken', 'searchSkillGroups_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@. Required.
--
-- 'filters', 'searchSkillGroups_filters' - The filters to use to list a specified set of skill groups. The
-- supported filter key is SkillGroupName.
--
-- 'maxResults', 'searchSkillGroups_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
newSearchSkillGroups ::
  SearchSkillGroups
newSearchSkillGroups =
  SearchSkillGroups'
    { sortCriteria = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The sort order to use in listing the specified set of skill groups. The
-- supported sort key is SkillGroupName.
searchSkillGroups_sortCriteria :: Lens.Lens' SearchSkillGroups (Prelude.Maybe [Sort])
searchSkillGroups_sortCriteria = Lens.lens (\SearchSkillGroups' {sortCriteria} -> sortCriteria) (\s@SearchSkillGroups' {} a -> s {sortCriteria = a} :: SearchSkillGroups) Prelude.. Lens.mapping Lens.coerced

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@. Required.
searchSkillGroups_nextToken :: Lens.Lens' SearchSkillGroups (Prelude.Maybe Prelude.Text)
searchSkillGroups_nextToken = Lens.lens (\SearchSkillGroups' {nextToken} -> nextToken) (\s@SearchSkillGroups' {} a -> s {nextToken = a} :: SearchSkillGroups)

-- | The filters to use to list a specified set of skill groups. The
-- supported filter key is SkillGroupName.
searchSkillGroups_filters :: Lens.Lens' SearchSkillGroups (Prelude.Maybe [Filter])
searchSkillGroups_filters = Lens.lens (\SearchSkillGroups' {filters} -> filters) (\s@SearchSkillGroups' {} a -> s {filters = a} :: SearchSkillGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchSkillGroups_maxResults :: Lens.Lens' SearchSkillGroups (Prelude.Maybe Prelude.Natural)
searchSkillGroups_maxResults = Lens.lens (\SearchSkillGroups' {maxResults} -> maxResults) (\s@SearchSkillGroups' {} a -> s {maxResults = a} :: SearchSkillGroups)

instance Core.AWSPager SearchSkillGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchSkillGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchSkillGroupsResponse_skillGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchSkillGroups_nextToken
          Lens..~ rs
          Lens.^? searchSkillGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchSkillGroups where
  type
    AWSResponse SearchSkillGroups =
      SearchSkillGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchSkillGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SkillGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TotalCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchSkillGroups where
  hashWithSalt _salt SearchSkillGroups' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData SearchSkillGroups where
  rnf SearchSkillGroups' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders SearchSkillGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.SearchSkillGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchSkillGroups where
  toJSON SearchSkillGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortCriteria" Data..=) Prelude.<$> sortCriteria,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath SearchSkillGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchSkillGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchSkillGroupsResponse' smart constructor.
data SearchSkillGroupsResponse = SearchSkillGroupsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The skill groups that meet the filter criteria, in sort order.
    skillGroups :: Prelude.Maybe [SkillGroupData],
    -- | The total number of skill groups returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSkillGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchSkillGroupsResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'skillGroups', 'searchSkillGroupsResponse_skillGroups' - The skill groups that meet the filter criteria, in sort order.
--
-- 'totalCount', 'searchSkillGroupsResponse_totalCount' - The total number of skill groups returned.
--
-- 'httpStatus', 'searchSkillGroupsResponse_httpStatus' - The response's http status code.
newSearchSkillGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchSkillGroupsResponse
newSearchSkillGroupsResponse pHttpStatus_ =
  SearchSkillGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      skillGroups = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchSkillGroupsResponse_nextToken :: Lens.Lens' SearchSkillGroupsResponse (Prelude.Maybe Prelude.Text)
searchSkillGroupsResponse_nextToken = Lens.lens (\SearchSkillGroupsResponse' {nextToken} -> nextToken) (\s@SearchSkillGroupsResponse' {} a -> s {nextToken = a} :: SearchSkillGroupsResponse)

-- | The skill groups that meet the filter criteria, in sort order.
searchSkillGroupsResponse_skillGroups :: Lens.Lens' SearchSkillGroupsResponse (Prelude.Maybe [SkillGroupData])
searchSkillGroupsResponse_skillGroups = Lens.lens (\SearchSkillGroupsResponse' {skillGroups} -> skillGroups) (\s@SearchSkillGroupsResponse' {} a -> s {skillGroups = a} :: SearchSkillGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of skill groups returned.
searchSkillGroupsResponse_totalCount :: Lens.Lens' SearchSkillGroupsResponse (Prelude.Maybe Prelude.Int)
searchSkillGroupsResponse_totalCount = Lens.lens (\SearchSkillGroupsResponse' {totalCount} -> totalCount) (\s@SearchSkillGroupsResponse' {} a -> s {totalCount = a} :: SearchSkillGroupsResponse)

-- | The response's http status code.
searchSkillGroupsResponse_httpStatus :: Lens.Lens' SearchSkillGroupsResponse Prelude.Int
searchSkillGroupsResponse_httpStatus = Lens.lens (\SearchSkillGroupsResponse' {httpStatus} -> httpStatus) (\s@SearchSkillGroupsResponse' {} a -> s {httpStatus = a} :: SearchSkillGroupsResponse)

instance Prelude.NFData SearchSkillGroupsResponse where
  rnf SearchSkillGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf skillGroups
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf httpStatus
