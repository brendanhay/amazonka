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
-- Module      : Network.AWS.AlexaBusiness.SearchSkillGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches skill groups and lists the ones that meet a set of filter and
-- sort criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchSkillGroups
  ( -- * Creating a Request
    SearchSkillGroups (..),
    newSearchSkillGroups,

    -- * Request Lenses
    searchSkillGroups_nextToken,
    searchSkillGroups_sortCriteria,
    searchSkillGroups_maxResults,
    searchSkillGroups_filters,

    -- * Destructuring the Response
    SearchSkillGroupsResponse (..),
    newSearchSkillGroupsResponse,

    -- * Response Lenses
    searchSkillGroupsResponse_nextToken,
    searchSkillGroupsResponse_totalCount,
    searchSkillGroupsResponse_skillGroups,
    searchSkillGroupsResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchSkillGroups' smart constructor.
data SearchSkillGroups = SearchSkillGroups'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@. Required.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order to use in listing the specified set of skill groups. The
    -- supported sort key is SkillGroupName.
    sortCriteria :: Prelude.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The filters to use to list a specified set of skill groups. The
    -- supported filter key is SkillGroupName.
    filters :: Prelude.Maybe [Filter]
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
-- 'nextToken', 'searchSkillGroups_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@. Required.
--
-- 'sortCriteria', 'searchSkillGroups_sortCriteria' - The sort order to use in listing the specified set of skill groups. The
-- supported sort key is SkillGroupName.
--
-- 'maxResults', 'searchSkillGroups_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'filters', 'searchSkillGroups_filters' - The filters to use to list a specified set of skill groups. The
-- supported filter key is SkillGroupName.
newSearchSkillGroups ::
  SearchSkillGroups
newSearchSkillGroups =
  SearchSkillGroups'
    { nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@. Required.
searchSkillGroups_nextToken :: Lens.Lens' SearchSkillGroups (Prelude.Maybe Prelude.Text)
searchSkillGroups_nextToken = Lens.lens (\SearchSkillGroups' {nextToken} -> nextToken) (\s@SearchSkillGroups' {} a -> s {nextToken = a} :: SearchSkillGroups)

-- | The sort order to use in listing the specified set of skill groups. The
-- supported sort key is SkillGroupName.
searchSkillGroups_sortCriteria :: Lens.Lens' SearchSkillGroups (Prelude.Maybe [Sort])
searchSkillGroups_sortCriteria = Lens.lens (\SearchSkillGroups' {sortCriteria} -> sortCriteria) (\s@SearchSkillGroups' {} a -> s {sortCriteria = a} :: SearchSkillGroups) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchSkillGroups_maxResults :: Lens.Lens' SearchSkillGroups (Prelude.Maybe Prelude.Natural)
searchSkillGroups_maxResults = Lens.lens (\SearchSkillGroups' {maxResults} -> maxResults) (\s@SearchSkillGroups' {} a -> s {maxResults = a} :: SearchSkillGroups)

-- | The filters to use to list a specified set of skill groups. The
-- supported filter key is SkillGroupName.
searchSkillGroups_filters :: Lens.Lens' SearchSkillGroups (Prelude.Maybe [Filter])
searchSkillGroups_filters = Lens.lens (\SearchSkillGroups' {filters} -> filters) (\s@SearchSkillGroups' {} a -> s {filters = a} :: SearchSkillGroups) Prelude.. Lens.mapping Lens._Coerce

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchSkillGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "TotalCount")
            Prelude.<*> (x Core..?> "SkillGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchSkillGroups

instance Prelude.NFData SearchSkillGroups

instance Core.ToHeaders SearchSkillGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SearchSkillGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchSkillGroups where
  toJSON SearchSkillGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortCriteria" Core..=) Prelude.<$> sortCriteria,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath SearchSkillGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchSkillGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchSkillGroupsResponse' smart constructor.
data SearchSkillGroupsResponse = SearchSkillGroupsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of skill groups returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The skill groups that meet the filter criteria, in sort order.
    skillGroups :: Prelude.Maybe [SkillGroupData],
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
-- 'totalCount', 'searchSkillGroupsResponse_totalCount' - The total number of skill groups returned.
--
-- 'skillGroups', 'searchSkillGroupsResponse_skillGroups' - The skill groups that meet the filter criteria, in sort order.
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
      totalCount = Prelude.Nothing,
      skillGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchSkillGroupsResponse_nextToken :: Lens.Lens' SearchSkillGroupsResponse (Prelude.Maybe Prelude.Text)
searchSkillGroupsResponse_nextToken = Lens.lens (\SearchSkillGroupsResponse' {nextToken} -> nextToken) (\s@SearchSkillGroupsResponse' {} a -> s {nextToken = a} :: SearchSkillGroupsResponse)

-- | The total number of skill groups returned.
searchSkillGroupsResponse_totalCount :: Lens.Lens' SearchSkillGroupsResponse (Prelude.Maybe Prelude.Int)
searchSkillGroupsResponse_totalCount = Lens.lens (\SearchSkillGroupsResponse' {totalCount} -> totalCount) (\s@SearchSkillGroupsResponse' {} a -> s {totalCount = a} :: SearchSkillGroupsResponse)

-- | The skill groups that meet the filter criteria, in sort order.
searchSkillGroupsResponse_skillGroups :: Lens.Lens' SearchSkillGroupsResponse (Prelude.Maybe [SkillGroupData])
searchSkillGroupsResponse_skillGroups = Lens.lens (\SearchSkillGroupsResponse' {skillGroups} -> skillGroups) (\s@SearchSkillGroupsResponse' {} a -> s {skillGroups = a} :: SearchSkillGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchSkillGroupsResponse_httpStatus :: Lens.Lens' SearchSkillGroupsResponse Prelude.Int
searchSkillGroupsResponse_httpStatus = Lens.lens (\SearchSkillGroupsResponse' {httpStatus} -> httpStatus) (\s@SearchSkillGroupsResponse' {} a -> s {httpStatus = a} :: SearchSkillGroupsResponse)

instance Prelude.NFData SearchSkillGroupsResponse
