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
-- Module      : Amazonka.AlexaBusiness.SearchProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches room profiles and lists the ones that meet a set of filter
-- criteria.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.SearchProfiles
  ( -- * Creating a Request
    SearchProfiles (..),
    newSearchProfiles,

    -- * Request Lenses
    searchProfiles_filters,
    searchProfiles_maxResults,
    searchProfiles_nextToken,
    searchProfiles_sortCriteria,

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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchProfiles' smart constructor.
data SearchProfiles = SearchProfiles'
  { -- | The filters to use to list a specified set of room profiles. Supported
    -- filter keys are ProfileName and Address. Required.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order to use in listing the specified set of room profiles.
    -- Supported sort keys are ProfileName and Address.
    sortCriteria :: Prelude.Maybe [Sort]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchProfiles_filters' - The filters to use to list a specified set of room profiles. Supported
-- filter keys are ProfileName and Address. Required.
--
-- 'maxResults', 'searchProfiles_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'searchProfiles_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'sortCriteria', 'searchProfiles_sortCriteria' - The sort order to use in listing the specified set of room profiles.
-- Supported sort keys are ProfileName and Address.
newSearchProfiles ::
  SearchProfiles
newSearchProfiles =
  SearchProfiles'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing
    }

-- | The filters to use to list a specified set of room profiles. Supported
-- filter keys are ProfileName and Address. Required.
searchProfiles_filters :: Lens.Lens' SearchProfiles (Prelude.Maybe [Filter])
searchProfiles_filters = Lens.lens (\SearchProfiles' {filters} -> filters) (\s@SearchProfiles' {} a -> s {filters = a} :: SearchProfiles) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchProfiles_maxResults :: Lens.Lens' SearchProfiles (Prelude.Maybe Prelude.Natural)
searchProfiles_maxResults = Lens.lens (\SearchProfiles' {maxResults} -> maxResults) (\s@SearchProfiles' {} a -> s {maxResults = a} :: SearchProfiles)

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
searchProfiles_nextToken :: Lens.Lens' SearchProfiles (Prelude.Maybe Prelude.Text)
searchProfiles_nextToken = Lens.lens (\SearchProfiles' {nextToken} -> nextToken) (\s@SearchProfiles' {} a -> s {nextToken = a} :: SearchProfiles)

-- | The sort order to use in listing the specified set of room profiles.
-- Supported sort keys are ProfileName and Address.
searchProfiles_sortCriteria :: Lens.Lens' SearchProfiles (Prelude.Maybe [Sort])
searchProfiles_sortCriteria = Lens.lens (\SearchProfiles' {sortCriteria} -> sortCriteria) (\s@SearchProfiles' {} a -> s {sortCriteria = a} :: SearchProfiles) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager SearchProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchProfilesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchProfilesResponse_profiles
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchProfiles_nextToken
          Lens..~ rs
          Lens.^? searchProfilesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchProfiles where
  type
    AWSResponse SearchProfiles =
      SearchProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProfilesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Profiles" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TotalCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchProfiles where
  hashWithSalt _salt SearchProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortCriteria

instance Prelude.NFData SearchProfiles where
  rnf SearchProfiles' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortCriteria

instance Data.ToHeaders SearchProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.SearchProfiles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchProfiles where
  toJSON SearchProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortCriteria" Data..=) Prelude.<$> sortCriteria
          ]
      )

instance Data.ToPath SearchProfiles where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchProfilesResponse' smart constructor.
data SearchProfilesResponse = SearchProfilesResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The profiles that meet the specified set of filter criteria, in sort
    -- order.
    profiles :: Prelude.Maybe [ProfileData],
    -- | The total number of room profiles returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SearchProfilesResponse
newSearchProfilesResponse pHttpStatus_ =
  SearchProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      profiles = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchProfilesResponse_nextToken :: Lens.Lens' SearchProfilesResponse (Prelude.Maybe Prelude.Text)
searchProfilesResponse_nextToken = Lens.lens (\SearchProfilesResponse' {nextToken} -> nextToken) (\s@SearchProfilesResponse' {} a -> s {nextToken = a} :: SearchProfilesResponse)

-- | The profiles that meet the specified set of filter criteria, in sort
-- order.
searchProfilesResponse_profiles :: Lens.Lens' SearchProfilesResponse (Prelude.Maybe [ProfileData])
searchProfilesResponse_profiles = Lens.lens (\SearchProfilesResponse' {profiles} -> profiles) (\s@SearchProfilesResponse' {} a -> s {profiles = a} :: SearchProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of room profiles returned.
searchProfilesResponse_totalCount :: Lens.Lens' SearchProfilesResponse (Prelude.Maybe Prelude.Int)
searchProfilesResponse_totalCount = Lens.lens (\SearchProfilesResponse' {totalCount} -> totalCount) (\s@SearchProfilesResponse' {} a -> s {totalCount = a} :: SearchProfilesResponse)

-- | The response's http status code.
searchProfilesResponse_httpStatus :: Lens.Lens' SearchProfilesResponse Prelude.Int
searchProfilesResponse_httpStatus = Lens.lens (\SearchProfilesResponse' {httpStatus} -> httpStatus) (\s@SearchProfilesResponse' {} a -> s {httpStatus = a} :: SearchProfilesResponse)

instance Prelude.NFData SearchProfilesResponse where
  rnf SearchProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf profiles
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf httpStatus
