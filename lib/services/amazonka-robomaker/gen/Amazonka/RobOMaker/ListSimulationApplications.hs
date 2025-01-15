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
-- Module      : Amazonka.RobOMaker.ListSimulationApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of simulation applications. You can optionally provide
-- filters to retrieve specific simulation applications.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListSimulationApplications
  ( -- * Creating a Request
    ListSimulationApplications (..),
    newListSimulationApplications,

    -- * Request Lenses
    listSimulationApplications_filters,
    listSimulationApplications_maxResults,
    listSimulationApplications_nextToken,
    listSimulationApplications_versionQualifier,

    -- * Destructuring the Response
    ListSimulationApplicationsResponse (..),
    newListSimulationApplicationsResponse,

    -- * Response Lenses
    listSimulationApplicationsResponse_nextToken,
    listSimulationApplicationsResponse_simulationApplicationSummaries,
    listSimulationApplicationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListSimulationApplications' smart constructor.
data ListSimulationApplications = ListSimulationApplications'
  { -- | Optional list of filters to limit results.
    --
    -- The filter name @name@ is supported. When filtering, you must use the
    -- complete value of the filtered item. You can use up to three filters.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | When this parameter is used, @ListSimulationApplications@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListSimulationApplications@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListSimulationApplications@ returns up to
    -- 100 results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call
    -- @ListSimulationApplications@ again and assign that token to the request
    -- object\'s @nextToken@ parameter. If there are no remaining results, the
    -- previous response object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version qualifier of the simulation application.
    versionQualifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSimulationApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listSimulationApplications_filters' - Optional list of filters to limit results.
--
-- The filter name @name@ is supported. When filtering, you must use the
-- complete value of the filtered item. You can use up to three filters.
--
-- 'maxResults', 'listSimulationApplications_maxResults' - When this parameter is used, @ListSimulationApplications@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListSimulationApplications@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListSimulationApplications@ returns up to
-- 100 results and a @nextToken@ value if applicable.
--
-- 'nextToken', 'listSimulationApplications_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListSimulationApplications@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
--
-- 'versionQualifier', 'listSimulationApplications_versionQualifier' - The version qualifier of the simulation application.
newListSimulationApplications ::
  ListSimulationApplications
newListSimulationApplications =
  ListSimulationApplications'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      versionQualifier = Prelude.Nothing
    }

-- | Optional list of filters to limit results.
--
-- The filter name @name@ is supported. When filtering, you must use the
-- complete value of the filtered item. You can use up to three filters.
listSimulationApplications_filters :: Lens.Lens' ListSimulationApplications (Prelude.Maybe (Prelude.NonEmpty Filter))
listSimulationApplications_filters = Lens.lens (\ListSimulationApplications' {filters} -> filters) (\s@ListSimulationApplications' {} a -> s {filters = a} :: ListSimulationApplications) Prelude.. Lens.mapping Lens.coerced

-- | When this parameter is used, @ListSimulationApplications@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListSimulationApplications@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListSimulationApplications@ returns up to
-- 100 results and a @nextToken@ value if applicable.
listSimulationApplications_maxResults :: Lens.Lens' ListSimulationApplications (Prelude.Maybe Prelude.Int)
listSimulationApplications_maxResults = Lens.lens (\ListSimulationApplications' {maxResults} -> maxResults) (\s@ListSimulationApplications' {} a -> s {maxResults = a} :: ListSimulationApplications)

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListSimulationApplications@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
listSimulationApplications_nextToken :: Lens.Lens' ListSimulationApplications (Prelude.Maybe Prelude.Text)
listSimulationApplications_nextToken = Lens.lens (\ListSimulationApplications' {nextToken} -> nextToken) (\s@ListSimulationApplications' {} a -> s {nextToken = a} :: ListSimulationApplications)

-- | The version qualifier of the simulation application.
listSimulationApplications_versionQualifier :: Lens.Lens' ListSimulationApplications (Prelude.Maybe Prelude.Text)
listSimulationApplications_versionQualifier = Lens.lens (\ListSimulationApplications' {versionQualifier} -> versionQualifier) (\s@ListSimulationApplications' {} a -> s {versionQualifier = a} :: ListSimulationApplications)

instance Core.AWSPager ListSimulationApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSimulationApplicationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSimulationApplicationsResponse_simulationApplicationSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listSimulationApplications_nextToken
              Lens..~ rs
              Lens.^? listSimulationApplicationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListSimulationApplications where
  type
    AWSResponse ListSimulationApplications =
      ListSimulationApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSimulationApplicationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "simulationApplicationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSimulationApplications where
  hashWithSalt _salt ListSimulationApplications' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` versionQualifier

instance Prelude.NFData ListSimulationApplications where
  rnf ListSimulationApplications' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf versionQualifier

instance Data.ToHeaders ListSimulationApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSimulationApplications where
  toJSON ListSimulationApplications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("versionQualifier" Data..=)
              Prelude.<$> versionQualifier
          ]
      )

instance Data.ToPath ListSimulationApplications where
  toPath = Prelude.const "/listSimulationApplications"

instance Data.ToQuery ListSimulationApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSimulationApplicationsResponse' smart constructor.
data ListSimulationApplicationsResponse = ListSimulationApplicationsResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call
    -- @ListSimulationApplications@ again and assign that token to the request
    -- object\'s @nextToken@ parameter. If there are no remaining results, the
    -- previous response object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of simulation application summaries that meet the criteria of the
    -- request.
    simulationApplicationSummaries :: Prelude.Maybe [SimulationApplicationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSimulationApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSimulationApplicationsResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListSimulationApplications@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
--
-- 'simulationApplicationSummaries', 'listSimulationApplicationsResponse_simulationApplicationSummaries' - A list of simulation application summaries that meet the criteria of the
-- request.
--
-- 'httpStatus', 'listSimulationApplicationsResponse_httpStatus' - The response's http status code.
newListSimulationApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSimulationApplicationsResponse
newListSimulationApplicationsResponse pHttpStatus_ =
  ListSimulationApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      simulationApplicationSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListSimulationApplications@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
listSimulationApplicationsResponse_nextToken :: Lens.Lens' ListSimulationApplicationsResponse (Prelude.Maybe Prelude.Text)
listSimulationApplicationsResponse_nextToken = Lens.lens (\ListSimulationApplicationsResponse' {nextToken} -> nextToken) (\s@ListSimulationApplicationsResponse' {} a -> s {nextToken = a} :: ListSimulationApplicationsResponse)

-- | A list of simulation application summaries that meet the criteria of the
-- request.
listSimulationApplicationsResponse_simulationApplicationSummaries :: Lens.Lens' ListSimulationApplicationsResponse (Prelude.Maybe [SimulationApplicationSummary])
listSimulationApplicationsResponse_simulationApplicationSummaries = Lens.lens (\ListSimulationApplicationsResponse' {simulationApplicationSummaries} -> simulationApplicationSummaries) (\s@ListSimulationApplicationsResponse' {} a -> s {simulationApplicationSummaries = a} :: ListSimulationApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSimulationApplicationsResponse_httpStatus :: Lens.Lens' ListSimulationApplicationsResponse Prelude.Int
listSimulationApplicationsResponse_httpStatus = Lens.lens (\ListSimulationApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListSimulationApplicationsResponse' {} a -> s {httpStatus = a} :: ListSimulationApplicationsResponse)

instance
  Prelude.NFData
    ListSimulationApplicationsResponse
  where
  rnf ListSimulationApplicationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf simulationApplicationSummaries `Prelude.seq`
        Prelude.rnf httpStatus
