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
-- Module      : Amazonka.Connect.SearchRoutingProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Searches routing profiles in an Amazon Connect instance, with optional
-- filtering.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchRoutingProfiles
  ( -- * Creating a Request
    SearchRoutingProfiles (..),
    newSearchRoutingProfiles,

    -- * Request Lenses
    searchRoutingProfiles_maxResults,
    searchRoutingProfiles_nextToken,
    searchRoutingProfiles_searchCriteria,
    searchRoutingProfiles_searchFilter,
    searchRoutingProfiles_instanceId,

    -- * Destructuring the Response
    SearchRoutingProfilesResponse (..),
    newSearchRoutingProfilesResponse,

    -- * Response Lenses
    searchRoutingProfilesResponse_approximateTotalCount,
    searchRoutingProfilesResponse_nextToken,
    searchRoutingProfilesResponse_routingProfiles,
    searchRoutingProfilesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchRoutingProfiles' smart constructor.
data SearchRoutingProfiles = SearchRoutingProfiles'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The search criteria to be used to return routing profiles.
    --
    -- The @name@ and @description@ fields support \"contains\" queries with a
    -- minimum of 2 characters and a maximum of 25 characters. Any queries with
    -- character lengths outside of this range will throw invalid results.
    searchCriteria :: Prelude.Maybe RoutingProfileSearchCriteria,
    -- | Filters to be applied to search results.
    searchFilter :: Prelude.Maybe RoutingProfileSearchFilter,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRoutingProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchRoutingProfiles_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchRoutingProfiles_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'searchCriteria', 'searchRoutingProfiles_searchCriteria' - The search criteria to be used to return routing profiles.
--
-- The @name@ and @description@ fields support \"contains\" queries with a
-- minimum of 2 characters and a maximum of 25 characters. Any queries with
-- character lengths outside of this range will throw invalid results.
--
-- 'searchFilter', 'searchRoutingProfiles_searchFilter' - Filters to be applied to search results.
--
-- 'instanceId', 'searchRoutingProfiles_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newSearchRoutingProfiles ::
  -- | 'instanceId'
  Prelude.Text ->
  SearchRoutingProfiles
newSearchRoutingProfiles pInstanceId_ =
  SearchRoutingProfiles'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      searchCriteria = Prelude.Nothing,
      searchFilter = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
searchRoutingProfiles_maxResults :: Lens.Lens' SearchRoutingProfiles (Prelude.Maybe Prelude.Natural)
searchRoutingProfiles_maxResults = Lens.lens (\SearchRoutingProfiles' {maxResults} -> maxResults) (\s@SearchRoutingProfiles' {} a -> s {maxResults = a} :: SearchRoutingProfiles)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchRoutingProfiles_nextToken :: Lens.Lens' SearchRoutingProfiles (Prelude.Maybe Prelude.Text)
searchRoutingProfiles_nextToken = Lens.lens (\SearchRoutingProfiles' {nextToken} -> nextToken) (\s@SearchRoutingProfiles' {} a -> s {nextToken = a} :: SearchRoutingProfiles)

-- | The search criteria to be used to return routing profiles.
--
-- The @name@ and @description@ fields support \"contains\" queries with a
-- minimum of 2 characters and a maximum of 25 characters. Any queries with
-- character lengths outside of this range will throw invalid results.
searchRoutingProfiles_searchCriteria :: Lens.Lens' SearchRoutingProfiles (Prelude.Maybe RoutingProfileSearchCriteria)
searchRoutingProfiles_searchCriteria = Lens.lens (\SearchRoutingProfiles' {searchCriteria} -> searchCriteria) (\s@SearchRoutingProfiles' {} a -> s {searchCriteria = a} :: SearchRoutingProfiles)

-- | Filters to be applied to search results.
searchRoutingProfiles_searchFilter :: Lens.Lens' SearchRoutingProfiles (Prelude.Maybe RoutingProfileSearchFilter)
searchRoutingProfiles_searchFilter = Lens.lens (\SearchRoutingProfiles' {searchFilter} -> searchFilter) (\s@SearchRoutingProfiles' {} a -> s {searchFilter = a} :: SearchRoutingProfiles)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
searchRoutingProfiles_instanceId :: Lens.Lens' SearchRoutingProfiles Prelude.Text
searchRoutingProfiles_instanceId = Lens.lens (\SearchRoutingProfiles' {instanceId} -> instanceId) (\s@SearchRoutingProfiles' {} a -> s {instanceId = a} :: SearchRoutingProfiles)

instance Core.AWSPager SearchRoutingProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchRoutingProfilesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchRoutingProfilesResponse_routingProfiles
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchRoutingProfiles_nextToken
          Lens..~ rs
          Lens.^? searchRoutingProfilesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchRoutingProfiles where
  type
    AWSResponse SearchRoutingProfiles =
      SearchRoutingProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchRoutingProfilesResponse'
            Prelude.<$> (x Data..?> "ApproximateTotalCount")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "RoutingProfiles"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchRoutingProfiles where
  hashWithSalt _salt SearchRoutingProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` searchCriteria
      `Prelude.hashWithSalt` searchFilter
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SearchRoutingProfiles where
  rnf SearchRoutingProfiles' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchCriteria
      `Prelude.seq` Prelude.rnf searchFilter
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders SearchRoutingProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchRoutingProfiles where
  toJSON SearchRoutingProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SearchCriteria" Data..=)
              Prelude.<$> searchCriteria,
            ("SearchFilter" Data..=) Prelude.<$> searchFilter,
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath SearchRoutingProfiles where
  toPath = Prelude.const "/search-routing-profiles"

instance Data.ToQuery SearchRoutingProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchRoutingProfilesResponse' smart constructor.
data SearchRoutingProfilesResponse = SearchRoutingProfilesResponse'
  { -- | The total number of routing profiles which matched your search query.
    approximateTotalCount :: Prelude.Maybe Prelude.Integer,
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the routing profiles.
    routingProfiles :: Prelude.Maybe [RoutingProfile],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRoutingProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateTotalCount', 'searchRoutingProfilesResponse_approximateTotalCount' - The total number of routing profiles which matched your search query.
--
-- 'nextToken', 'searchRoutingProfilesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'routingProfiles', 'searchRoutingProfilesResponse_routingProfiles' - Information about the routing profiles.
--
-- 'httpStatus', 'searchRoutingProfilesResponse_httpStatus' - The response's http status code.
newSearchRoutingProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchRoutingProfilesResponse
newSearchRoutingProfilesResponse pHttpStatus_ =
  SearchRoutingProfilesResponse'
    { approximateTotalCount =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      routingProfiles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of routing profiles which matched your search query.
searchRoutingProfilesResponse_approximateTotalCount :: Lens.Lens' SearchRoutingProfilesResponse (Prelude.Maybe Prelude.Integer)
searchRoutingProfilesResponse_approximateTotalCount = Lens.lens (\SearchRoutingProfilesResponse' {approximateTotalCount} -> approximateTotalCount) (\s@SearchRoutingProfilesResponse' {} a -> s {approximateTotalCount = a} :: SearchRoutingProfilesResponse)

-- | If there are additional results, this is the token for the next set of
-- results.
searchRoutingProfilesResponse_nextToken :: Lens.Lens' SearchRoutingProfilesResponse (Prelude.Maybe Prelude.Text)
searchRoutingProfilesResponse_nextToken = Lens.lens (\SearchRoutingProfilesResponse' {nextToken} -> nextToken) (\s@SearchRoutingProfilesResponse' {} a -> s {nextToken = a} :: SearchRoutingProfilesResponse)

-- | Information about the routing profiles.
searchRoutingProfilesResponse_routingProfiles :: Lens.Lens' SearchRoutingProfilesResponse (Prelude.Maybe [RoutingProfile])
searchRoutingProfilesResponse_routingProfiles = Lens.lens (\SearchRoutingProfilesResponse' {routingProfiles} -> routingProfiles) (\s@SearchRoutingProfilesResponse' {} a -> s {routingProfiles = a} :: SearchRoutingProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchRoutingProfilesResponse_httpStatus :: Lens.Lens' SearchRoutingProfilesResponse Prelude.Int
searchRoutingProfilesResponse_httpStatus = Lens.lens (\SearchRoutingProfilesResponse' {httpStatus} -> httpStatus) (\s@SearchRoutingProfilesResponse' {} a -> s {httpStatus = a} :: SearchRoutingProfilesResponse)

instance Prelude.NFData SearchRoutingProfilesResponse where
  rnf SearchRoutingProfilesResponse' {..} =
    Prelude.rnf approximateTotalCount
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf routingProfiles
      `Prelude.seq` Prelude.rnf httpStatus
