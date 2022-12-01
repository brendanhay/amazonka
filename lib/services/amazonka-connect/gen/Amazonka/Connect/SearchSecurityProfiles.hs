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
-- Module      : Amazonka.Connect.SearchSecurityProfiles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Searches security profiles in an Amazon Connect instance, with optional
-- filtering.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchSecurityProfiles
  ( -- * Creating a Request
    SearchSecurityProfiles (..),
    newSearchSecurityProfiles,

    -- * Request Lenses
    searchSecurityProfiles_nextToken,
    searchSecurityProfiles_searchCriteria,
    searchSecurityProfiles_searchFilter,
    searchSecurityProfiles_maxResults,
    searchSecurityProfiles_instanceId,

    -- * Destructuring the Response
    SearchSecurityProfilesResponse (..),
    newSearchSecurityProfilesResponse,

    -- * Response Lenses
    searchSecurityProfilesResponse_nextToken,
    searchSecurityProfilesResponse_approximateTotalCount,
    searchSecurityProfilesResponse_securityProfiles,
    searchSecurityProfilesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchSecurityProfiles' smart constructor.
data SearchSecurityProfiles = SearchSecurityProfiles'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The search criteria to be used to return security profiles.
    --
    -- The @name@ field support \"contains\" queries with a minimum of 2
    -- characters and maximum of 25 characters. Any queries with character
    -- lengths outside of this range will throw invalid results.
    --
    -- The currently supported value for @FieldName@: @name@
    searchCriteria :: Prelude.Maybe SecurityProfileSearchCriteria,
    -- | Filters to be applied to search results.
    searchFilter :: Prelude.Maybe SecurityProfilesSearchFilter,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSecurityProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchSecurityProfiles_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'searchCriteria', 'searchSecurityProfiles_searchCriteria' - The search criteria to be used to return security profiles.
--
-- The @name@ field support \"contains\" queries with a minimum of 2
-- characters and maximum of 25 characters. Any queries with character
-- lengths outside of this range will throw invalid results.
--
-- The currently supported value for @FieldName@: @name@
--
-- 'searchFilter', 'searchSecurityProfiles_searchFilter' - Filters to be applied to search results.
--
-- 'maxResults', 'searchSecurityProfiles_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'searchSecurityProfiles_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newSearchSecurityProfiles ::
  -- | 'instanceId'
  Prelude.Text ->
  SearchSecurityProfiles
newSearchSecurityProfiles pInstanceId_ =
  SearchSecurityProfiles'
    { nextToken =
        Prelude.Nothing,
      searchCriteria = Prelude.Nothing,
      searchFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchSecurityProfiles_nextToken :: Lens.Lens' SearchSecurityProfiles (Prelude.Maybe Prelude.Text)
searchSecurityProfiles_nextToken = Lens.lens (\SearchSecurityProfiles' {nextToken} -> nextToken) (\s@SearchSecurityProfiles' {} a -> s {nextToken = a} :: SearchSecurityProfiles)

-- | The search criteria to be used to return security profiles.
--
-- The @name@ field support \"contains\" queries with a minimum of 2
-- characters and maximum of 25 characters. Any queries with character
-- lengths outside of this range will throw invalid results.
--
-- The currently supported value for @FieldName@: @name@
searchSecurityProfiles_searchCriteria :: Lens.Lens' SearchSecurityProfiles (Prelude.Maybe SecurityProfileSearchCriteria)
searchSecurityProfiles_searchCriteria = Lens.lens (\SearchSecurityProfiles' {searchCriteria} -> searchCriteria) (\s@SearchSecurityProfiles' {} a -> s {searchCriteria = a} :: SearchSecurityProfiles)

-- | Filters to be applied to search results.
searchSecurityProfiles_searchFilter :: Lens.Lens' SearchSecurityProfiles (Prelude.Maybe SecurityProfilesSearchFilter)
searchSecurityProfiles_searchFilter = Lens.lens (\SearchSecurityProfiles' {searchFilter} -> searchFilter) (\s@SearchSecurityProfiles' {} a -> s {searchFilter = a} :: SearchSecurityProfiles)

-- | The maximum number of results to return per page.
searchSecurityProfiles_maxResults :: Lens.Lens' SearchSecurityProfiles (Prelude.Maybe Prelude.Natural)
searchSecurityProfiles_maxResults = Lens.lens (\SearchSecurityProfiles' {maxResults} -> maxResults) (\s@SearchSecurityProfiles' {} a -> s {maxResults = a} :: SearchSecurityProfiles)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
searchSecurityProfiles_instanceId :: Lens.Lens' SearchSecurityProfiles Prelude.Text
searchSecurityProfiles_instanceId = Lens.lens (\SearchSecurityProfiles' {instanceId} -> instanceId) (\s@SearchSecurityProfiles' {} a -> s {instanceId = a} :: SearchSecurityProfiles)

instance Core.AWSPager SearchSecurityProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchSecurityProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchSecurityProfilesResponse_securityProfiles
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchSecurityProfiles_nextToken
          Lens..~ rs
          Lens.^? searchSecurityProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchSecurityProfiles where
  type
    AWSResponse SearchSecurityProfiles =
      SearchSecurityProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchSecurityProfilesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ApproximateTotalCount")
            Prelude.<*> ( x Core..?> "SecurityProfiles"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchSecurityProfiles where
  hashWithSalt _salt SearchSecurityProfiles' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` searchCriteria
      `Prelude.hashWithSalt` searchFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SearchSecurityProfiles where
  rnf SearchSecurityProfiles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchCriteria
      `Prelude.seq` Prelude.rnf searchFilter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders SearchSecurityProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchSecurityProfiles where
  toJSON SearchSecurityProfiles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SearchCriteria" Core..=)
              Prelude.<$> searchCriteria,
            ("SearchFilter" Core..=) Prelude.<$> searchFilter,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.ToPath SearchSecurityProfiles where
  toPath = Prelude.const "/search-security-profiles"

instance Core.ToQuery SearchSecurityProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchSecurityProfilesResponse' smart constructor.
data SearchSecurityProfilesResponse = SearchSecurityProfilesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of security profiles which matched your search query.
    approximateTotalCount :: Prelude.Maybe Prelude.Integer,
    -- | Information about the security profiles.
    securityProfiles :: Prelude.Maybe [SecurityProfileSearchSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSecurityProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchSecurityProfilesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'approximateTotalCount', 'searchSecurityProfilesResponse_approximateTotalCount' - The total number of security profiles which matched your search query.
--
-- 'securityProfiles', 'searchSecurityProfilesResponse_securityProfiles' - Information about the security profiles.
--
-- 'httpStatus', 'searchSecurityProfilesResponse_httpStatus' - The response's http status code.
newSearchSecurityProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchSecurityProfilesResponse
newSearchSecurityProfilesResponse pHttpStatus_ =
  SearchSecurityProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      approximateTotalCount = Prelude.Nothing,
      securityProfiles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
searchSecurityProfilesResponse_nextToken :: Lens.Lens' SearchSecurityProfilesResponse (Prelude.Maybe Prelude.Text)
searchSecurityProfilesResponse_nextToken = Lens.lens (\SearchSecurityProfilesResponse' {nextToken} -> nextToken) (\s@SearchSecurityProfilesResponse' {} a -> s {nextToken = a} :: SearchSecurityProfilesResponse)

-- | The total number of security profiles which matched your search query.
searchSecurityProfilesResponse_approximateTotalCount :: Lens.Lens' SearchSecurityProfilesResponse (Prelude.Maybe Prelude.Integer)
searchSecurityProfilesResponse_approximateTotalCount = Lens.lens (\SearchSecurityProfilesResponse' {approximateTotalCount} -> approximateTotalCount) (\s@SearchSecurityProfilesResponse' {} a -> s {approximateTotalCount = a} :: SearchSecurityProfilesResponse)

-- | Information about the security profiles.
searchSecurityProfilesResponse_securityProfiles :: Lens.Lens' SearchSecurityProfilesResponse (Prelude.Maybe [SecurityProfileSearchSummary])
searchSecurityProfilesResponse_securityProfiles = Lens.lens (\SearchSecurityProfilesResponse' {securityProfiles} -> securityProfiles) (\s@SearchSecurityProfilesResponse' {} a -> s {securityProfiles = a} :: SearchSecurityProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchSecurityProfilesResponse_httpStatus :: Lens.Lens' SearchSecurityProfilesResponse Prelude.Int
searchSecurityProfilesResponse_httpStatus = Lens.lens (\SearchSecurityProfilesResponse' {httpStatus} -> httpStatus) (\s@SearchSecurityProfilesResponse' {} a -> s {httpStatus = a} :: SearchSecurityProfilesResponse)

instance
  Prelude.NFData
    SearchSecurityProfilesResponse
  where
  rnf SearchSecurityProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf approximateTotalCount
      `Prelude.seq` Prelude.rnf securityProfiles
      `Prelude.seq` Prelude.rnf httpStatus
