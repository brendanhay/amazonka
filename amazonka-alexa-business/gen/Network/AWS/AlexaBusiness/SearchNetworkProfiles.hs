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
-- Module      : Network.AWS.AlexaBusiness.SearchNetworkProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches network profiles and lists the ones that meet a set of filter
-- and sort criteria.
module Network.AWS.AlexaBusiness.SearchNetworkProfiles
  ( -- * Creating a Request
    SearchNetworkProfiles (..),
    newSearchNetworkProfiles,

    -- * Request Lenses
    searchNetworkProfiles_nextToken,
    searchNetworkProfiles_sortCriteria,
    searchNetworkProfiles_maxResults,
    searchNetworkProfiles_filters,

    -- * Destructuring the Response
    SearchNetworkProfilesResponse (..),
    newSearchNetworkProfilesResponse,

    -- * Response Lenses
    searchNetworkProfilesResponse_nextToken,
    searchNetworkProfilesResponse_networkProfiles,
    searchNetworkProfilesResponse_totalCount,
    searchNetworkProfilesResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchNetworkProfiles' smart constructor.
data SearchNetworkProfiles = SearchNetworkProfiles'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by MaxResults.
    nextToken :: Core.Maybe Core.Text,
    -- | The sort order to use to list the specified set of network profiles.
    -- Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
    sortCriteria :: Core.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | The filters to use to list a specified set of network profiles. Valid
    -- filters are NetworkProfileName, Ssid, and SecurityType.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchNetworkProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchNetworkProfiles_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by MaxResults.
--
-- 'sortCriteria', 'searchNetworkProfiles_sortCriteria' - The sort order to use to list the specified set of network profiles.
-- Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
--
-- 'maxResults', 'searchNetworkProfiles_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'filters', 'searchNetworkProfiles_filters' - The filters to use to list a specified set of network profiles. Valid
-- filters are NetworkProfileName, Ssid, and SecurityType.
newSearchNetworkProfiles ::
  SearchNetworkProfiles
newSearchNetworkProfiles =
  SearchNetworkProfiles'
    { nextToken = Core.Nothing,
      sortCriteria = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by MaxResults.
searchNetworkProfiles_nextToken :: Lens.Lens' SearchNetworkProfiles (Core.Maybe Core.Text)
searchNetworkProfiles_nextToken = Lens.lens (\SearchNetworkProfiles' {nextToken} -> nextToken) (\s@SearchNetworkProfiles' {} a -> s {nextToken = a} :: SearchNetworkProfiles)

-- | The sort order to use to list the specified set of network profiles.
-- Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
searchNetworkProfiles_sortCriteria :: Lens.Lens' SearchNetworkProfiles (Core.Maybe [Sort])
searchNetworkProfiles_sortCriteria = Lens.lens (\SearchNetworkProfiles' {sortCriteria} -> sortCriteria) (\s@SearchNetworkProfiles' {} a -> s {sortCriteria = a} :: SearchNetworkProfiles) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
searchNetworkProfiles_maxResults :: Lens.Lens' SearchNetworkProfiles (Core.Maybe Core.Natural)
searchNetworkProfiles_maxResults = Lens.lens (\SearchNetworkProfiles' {maxResults} -> maxResults) (\s@SearchNetworkProfiles' {} a -> s {maxResults = a} :: SearchNetworkProfiles)

-- | The filters to use to list a specified set of network profiles. Valid
-- filters are NetworkProfileName, Ssid, and SecurityType.
searchNetworkProfiles_filters :: Lens.Lens' SearchNetworkProfiles (Core.Maybe [Filter])
searchNetworkProfiles_filters = Lens.lens (\SearchNetworkProfiles' {filters} -> filters) (\s@SearchNetworkProfiles' {} a -> s {filters = a} :: SearchNetworkProfiles) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest SearchNetworkProfiles where
  type
    AWSResponse SearchNetworkProfiles =
      SearchNetworkProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchNetworkProfilesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "NetworkProfiles" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TotalCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchNetworkProfiles

instance Core.NFData SearchNetworkProfiles

instance Core.ToHeaders SearchNetworkProfiles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SearchNetworkProfiles" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchNetworkProfiles where
  toJSON SearchNetworkProfiles' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath SearchNetworkProfiles where
  toPath = Core.const "/"

instance Core.ToQuery SearchNetworkProfiles where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchNetworkProfilesResponse' smart constructor.
data SearchNetworkProfilesResponse = SearchNetworkProfilesResponse'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by MaxResults.
    nextToken :: Core.Maybe Core.Text,
    -- | The network profiles that meet the specified set of filter criteria, in
    -- sort order. It is a list of NetworkProfileData objects.
    networkProfiles :: Core.Maybe [NetworkProfileData],
    -- | The total number of network profiles returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchNetworkProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchNetworkProfilesResponse_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by MaxResults.
--
-- 'networkProfiles', 'searchNetworkProfilesResponse_networkProfiles' - The network profiles that meet the specified set of filter criteria, in
-- sort order. It is a list of NetworkProfileData objects.
--
-- 'totalCount', 'searchNetworkProfilesResponse_totalCount' - The total number of network profiles returned.
--
-- 'httpStatus', 'searchNetworkProfilesResponse_httpStatus' - The response's http status code.
newSearchNetworkProfilesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchNetworkProfilesResponse
newSearchNetworkProfilesResponse pHttpStatus_ =
  SearchNetworkProfilesResponse'
    { nextToken =
        Core.Nothing,
      networkProfiles = Core.Nothing,
      totalCount = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by MaxResults.
searchNetworkProfilesResponse_nextToken :: Lens.Lens' SearchNetworkProfilesResponse (Core.Maybe Core.Text)
searchNetworkProfilesResponse_nextToken = Lens.lens (\SearchNetworkProfilesResponse' {nextToken} -> nextToken) (\s@SearchNetworkProfilesResponse' {} a -> s {nextToken = a} :: SearchNetworkProfilesResponse)

-- | The network profiles that meet the specified set of filter criteria, in
-- sort order. It is a list of NetworkProfileData objects.
searchNetworkProfilesResponse_networkProfiles :: Lens.Lens' SearchNetworkProfilesResponse (Core.Maybe [NetworkProfileData])
searchNetworkProfilesResponse_networkProfiles = Lens.lens (\SearchNetworkProfilesResponse' {networkProfiles} -> networkProfiles) (\s@SearchNetworkProfilesResponse' {} a -> s {networkProfiles = a} :: SearchNetworkProfilesResponse) Core.. Lens.mapping Lens._Coerce

-- | The total number of network profiles returned.
searchNetworkProfilesResponse_totalCount :: Lens.Lens' SearchNetworkProfilesResponse (Core.Maybe Core.Int)
searchNetworkProfilesResponse_totalCount = Lens.lens (\SearchNetworkProfilesResponse' {totalCount} -> totalCount) (\s@SearchNetworkProfilesResponse' {} a -> s {totalCount = a} :: SearchNetworkProfilesResponse)

-- | The response's http status code.
searchNetworkProfilesResponse_httpStatus :: Lens.Lens' SearchNetworkProfilesResponse Core.Int
searchNetworkProfilesResponse_httpStatus = Lens.lens (\SearchNetworkProfilesResponse' {httpStatus} -> httpStatus) (\s@SearchNetworkProfilesResponse' {} a -> s {httpStatus = a} :: SearchNetworkProfilesResponse)

instance Core.NFData SearchNetworkProfilesResponse
