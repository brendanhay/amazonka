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
-- Module      : Amazonka.IoTThingsGraph.SearchSystemInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for system instances in the user\'s account.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.SearchSystemInstances
  ( -- * Creating a Request
    SearchSystemInstances (..),
    newSearchSystemInstances,

    -- * Request Lenses
    searchSystemInstances_filters,
    searchSystemInstances_nextToken,
    searchSystemInstances_maxResults,

    -- * Destructuring the Response
    SearchSystemInstancesResponse (..),
    newSearchSystemInstancesResponse,

    -- * Response Lenses
    searchSystemInstancesResponse_nextToken,
    searchSystemInstancesResponse_summaries,
    searchSystemInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchSystemInstances' smart constructor.
data SearchSystemInstances = SearchSystemInstances'
  { -- | Optional filter to apply to the search. Valid filters are
    -- @SYSTEM_TEMPLATE_ID@, @STATUS@, and @GREENGRASS_GROUP_NAME@.
    --
    -- Multiple filters function as OR criteria in the query. Multiple values
    -- passed inside the filter function as AND criteria.
    filters :: Prelude.Maybe [SystemInstanceFilter],
    -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSystemInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchSystemInstances_filters' - Optional filter to apply to the search. Valid filters are
-- @SYSTEM_TEMPLATE_ID@, @STATUS@, and @GREENGRASS_GROUP_NAME@.
--
-- Multiple filters function as OR criteria in the query. Multiple values
-- passed inside the filter function as AND criteria.
--
-- 'nextToken', 'searchSystemInstances_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'maxResults', 'searchSystemInstances_maxResults' - The maximum number of results to return in the response.
newSearchSystemInstances ::
  SearchSystemInstances
newSearchSystemInstances =
  SearchSystemInstances'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Optional filter to apply to the search. Valid filters are
-- @SYSTEM_TEMPLATE_ID@, @STATUS@, and @GREENGRASS_GROUP_NAME@.
--
-- Multiple filters function as OR criteria in the query. Multiple values
-- passed inside the filter function as AND criteria.
searchSystemInstances_filters :: Lens.Lens' SearchSystemInstances (Prelude.Maybe [SystemInstanceFilter])
searchSystemInstances_filters = Lens.lens (\SearchSystemInstances' {filters} -> filters) (\s@SearchSystemInstances' {} a -> s {filters = a} :: SearchSystemInstances) Prelude.. Lens.mapping Lens.coerced

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
searchSystemInstances_nextToken :: Lens.Lens' SearchSystemInstances (Prelude.Maybe Prelude.Text)
searchSystemInstances_nextToken = Lens.lens (\SearchSystemInstances' {nextToken} -> nextToken) (\s@SearchSystemInstances' {} a -> s {nextToken = a} :: SearchSystemInstances)

-- | The maximum number of results to return in the response.
searchSystemInstances_maxResults :: Lens.Lens' SearchSystemInstances (Prelude.Maybe Prelude.Natural)
searchSystemInstances_maxResults = Lens.lens (\SearchSystemInstances' {maxResults} -> maxResults) (\s@SearchSystemInstances' {} a -> s {maxResults = a} :: SearchSystemInstances)

instance Core.AWSPager SearchSystemInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchSystemInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchSystemInstancesResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchSystemInstances_nextToken
          Lens..~ rs
          Lens.^? searchSystemInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchSystemInstances where
  type
    AWSResponse SearchSystemInstances =
      SearchSystemInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchSystemInstancesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchSystemInstances where
  hashWithSalt salt' SearchSystemInstances' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchSystemInstances where
  rnf SearchSystemInstances' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders SearchSystemInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.SearchSystemInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchSystemInstances where
  toJSON SearchSystemInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath SearchSystemInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchSystemInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchSystemInstancesResponse' smart constructor.
data SearchSystemInstancesResponse = SearchSystemInstancesResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain summary data abour the system instances
    -- in the result set.
    summaries :: Prelude.Maybe [SystemInstanceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSystemInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchSystemInstancesResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'summaries', 'searchSystemInstancesResponse_summaries' - An array of objects that contain summary data abour the system instances
-- in the result set.
--
-- 'httpStatus', 'searchSystemInstancesResponse_httpStatus' - The response's http status code.
newSearchSystemInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchSystemInstancesResponse
newSearchSystemInstancesResponse pHttpStatus_ =
  SearchSystemInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
searchSystemInstancesResponse_nextToken :: Lens.Lens' SearchSystemInstancesResponse (Prelude.Maybe Prelude.Text)
searchSystemInstancesResponse_nextToken = Lens.lens (\SearchSystemInstancesResponse' {nextToken} -> nextToken) (\s@SearchSystemInstancesResponse' {} a -> s {nextToken = a} :: SearchSystemInstancesResponse)

-- | An array of objects that contain summary data abour the system instances
-- in the result set.
searchSystemInstancesResponse_summaries :: Lens.Lens' SearchSystemInstancesResponse (Prelude.Maybe [SystemInstanceSummary])
searchSystemInstancesResponse_summaries = Lens.lens (\SearchSystemInstancesResponse' {summaries} -> summaries) (\s@SearchSystemInstancesResponse' {} a -> s {summaries = a} :: SearchSystemInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchSystemInstancesResponse_httpStatus :: Lens.Lens' SearchSystemInstancesResponse Prelude.Int
searchSystemInstancesResponse_httpStatus = Lens.lens (\SearchSystemInstancesResponse' {httpStatus} -> httpStatus) (\s@SearchSystemInstancesResponse' {} a -> s {httpStatus = a} :: SearchSystemInstancesResponse)

instance Prelude.NFData SearchSystemInstancesResponse where
  rnf SearchSystemInstancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf summaries
