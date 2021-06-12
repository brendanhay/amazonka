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
-- Module      : Network.AWS.AlexaBusiness.SearchProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches room profiles and lists the ones that meet a set of filter
-- criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchProfiles
  ( -- * Creating a Request
    SearchProfiles (..),
    newSearchProfiles,

    -- * Request Lenses
    searchProfiles_nextToken,
    searchProfiles_sortCriteria,
    searchProfiles_maxResults,
    searchProfiles_filters,

    -- * Destructuring the Response
    SearchProfilesResponse (..),
    newSearchProfilesResponse,

    -- * Response Lenses
    searchProfilesResponse_nextToken,
    searchProfilesResponse_profiles,
    searchProfilesResponse_totalCount,
    searchProfilesResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchProfiles' smart constructor.
data SearchProfiles = SearchProfiles'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Core.Maybe Core.Text,
    -- | The sort order to use in listing the specified set of room profiles.
    -- Supported sort keys are ProfileName and Address.
    sortCriteria :: Core.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | The filters to use to list a specified set of room profiles. Supported
    -- filter keys are ProfileName and Address. Required.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchProfiles_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'sortCriteria', 'searchProfiles_sortCriteria' - The sort order to use in listing the specified set of room profiles.
-- Supported sort keys are ProfileName and Address.
--
-- 'maxResults', 'searchProfiles_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'filters', 'searchProfiles_filters' - The filters to use to list a specified set of room profiles. Supported
-- filter keys are ProfileName and Address. Required.
newSearchProfiles ::
  SearchProfiles
newSearchProfiles =
  SearchProfiles'
    { nextToken = Core.Nothing,
      sortCriteria = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
searchProfiles_nextToken :: Lens.Lens' SearchProfiles (Core.Maybe Core.Text)
searchProfiles_nextToken = Lens.lens (\SearchProfiles' {nextToken} -> nextToken) (\s@SearchProfiles' {} a -> s {nextToken = a} :: SearchProfiles)

-- | The sort order to use in listing the specified set of room profiles.
-- Supported sort keys are ProfileName and Address.
searchProfiles_sortCriteria :: Lens.Lens' SearchProfiles (Core.Maybe [Sort])
searchProfiles_sortCriteria = Lens.lens (\SearchProfiles' {sortCriteria} -> sortCriteria) (\s@SearchProfiles' {} a -> s {sortCriteria = a} :: SearchProfiles) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchProfiles_maxResults :: Lens.Lens' SearchProfiles (Core.Maybe Core.Natural)
searchProfiles_maxResults = Lens.lens (\SearchProfiles' {maxResults} -> maxResults) (\s@SearchProfiles' {} a -> s {maxResults = a} :: SearchProfiles)

-- | The filters to use to list a specified set of room profiles. Supported
-- filter keys are ProfileName and Address. Required.
searchProfiles_filters :: Lens.Lens' SearchProfiles (Core.Maybe [Filter])
searchProfiles_filters = Lens.lens (\SearchProfiles' {filters} -> filters) (\s@SearchProfiles' {} a -> s {filters = a} :: SearchProfiles) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager SearchProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchProfilesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? searchProfilesResponse_profiles Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& searchProfiles_nextToken
          Lens..~ rs
          Lens.^? searchProfilesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest SearchProfiles where
  type
    AWSResponse SearchProfiles =
      SearchProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProfilesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Profiles" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TotalCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchProfiles

instance Core.NFData SearchProfiles

instance Core.ToHeaders SearchProfiles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SearchProfiles" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchProfiles where
  toJSON SearchProfiles' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath SearchProfiles where
  toPath = Core.const "/"

instance Core.ToQuery SearchProfiles where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchProfilesResponse' smart constructor.
data SearchProfilesResponse = SearchProfilesResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The profiles that meet the specified set of filter criteria, in sort
    -- order.
    profiles :: Core.Maybe [ProfileData],
    -- | The total number of room profiles returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchProfilesResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'profiles', 'searchProfilesResponse_profiles' - The profiles that meet the specified set of filter criteria, in sort
-- order.
--
-- 'totalCount', 'searchProfilesResponse_totalCount' - The total number of room profiles returned.
--
-- 'httpStatus', 'searchProfilesResponse_httpStatus' - The response's http status code.
newSearchProfilesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchProfilesResponse
newSearchProfilesResponse pHttpStatus_ =
  SearchProfilesResponse'
    { nextToken = Core.Nothing,
      profiles = Core.Nothing,
      totalCount = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchProfilesResponse_nextToken :: Lens.Lens' SearchProfilesResponse (Core.Maybe Core.Text)
searchProfilesResponse_nextToken = Lens.lens (\SearchProfilesResponse' {nextToken} -> nextToken) (\s@SearchProfilesResponse' {} a -> s {nextToken = a} :: SearchProfilesResponse)

-- | The profiles that meet the specified set of filter criteria, in sort
-- order.
searchProfilesResponse_profiles :: Lens.Lens' SearchProfilesResponse (Core.Maybe [ProfileData])
searchProfilesResponse_profiles = Lens.lens (\SearchProfilesResponse' {profiles} -> profiles) (\s@SearchProfilesResponse' {} a -> s {profiles = a} :: SearchProfilesResponse) Core.. Lens.mapping Lens._Coerce

-- | The total number of room profiles returned.
searchProfilesResponse_totalCount :: Lens.Lens' SearchProfilesResponse (Core.Maybe Core.Int)
searchProfilesResponse_totalCount = Lens.lens (\SearchProfilesResponse' {totalCount} -> totalCount) (\s@SearchProfilesResponse' {} a -> s {totalCount = a} :: SearchProfilesResponse)

-- | The response's http status code.
searchProfilesResponse_httpStatus :: Lens.Lens' SearchProfilesResponse Core.Int
searchProfilesResponse_httpStatus = Lens.lens (\SearchProfilesResponse' {httpStatus} -> httpStatus) (\s@SearchProfilesResponse' {} a -> s {httpStatus = a} :: SearchProfilesResponse)

instance Core.NFData SearchProfilesResponse
