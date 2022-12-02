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
-- Module      : Amazonka.RobOMaker.ListWorldGenerationJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists world generator jobs.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListWorldGenerationJobs
  ( -- * Creating a Request
    ListWorldGenerationJobs (..),
    newListWorldGenerationJobs,

    -- * Request Lenses
    listWorldGenerationJobs_nextToken,
    listWorldGenerationJobs_filters,
    listWorldGenerationJobs_maxResults,

    -- * Destructuring the Response
    ListWorldGenerationJobsResponse (..),
    newListWorldGenerationJobsResponse,

    -- * Response Lenses
    listWorldGenerationJobsResponse_nextToken,
    listWorldGenerationJobsResponse_httpStatus,
    listWorldGenerationJobsResponse_worldGenerationJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListWorldGenerationJobs' smart constructor.
data ListWorldGenerationJobs = ListWorldGenerationJobs'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call
    -- @ListWorldGenerationJobsRequest@ again and assign that token to the
    -- request object\'s @nextToken@ parameter. If there are no remaining
    -- results, the previous response object\'s NextToken parameter is set to
    -- null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional filters to limit results. You can use @status@ and
    -- @templateId@.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | When this parameter is used, @ListWorldGeneratorJobs@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListWorldGeneratorJobs@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListWorldGeneratorJobs@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorldGenerationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorldGenerationJobs_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListWorldGenerationJobsRequest@ again and assign that token to the
-- request object\'s @nextToken@ parameter. If there are no remaining
-- results, the previous response object\'s NextToken parameter is set to
-- null.
--
-- 'filters', 'listWorldGenerationJobs_filters' - Optional filters to limit results. You can use @status@ and
-- @templateId@.
--
-- 'maxResults', 'listWorldGenerationJobs_maxResults' - When this parameter is used, @ListWorldGeneratorJobs@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListWorldGeneratorJobs@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListWorldGeneratorJobs@ returns up to 100
-- results and a @nextToken@ value if applicable.
newListWorldGenerationJobs ::
  ListWorldGenerationJobs
newListWorldGenerationJobs =
  ListWorldGenerationJobs'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListWorldGenerationJobsRequest@ again and assign that token to the
-- request object\'s @nextToken@ parameter. If there are no remaining
-- results, the previous response object\'s NextToken parameter is set to
-- null.
listWorldGenerationJobs_nextToken :: Lens.Lens' ListWorldGenerationJobs (Prelude.Maybe Prelude.Text)
listWorldGenerationJobs_nextToken = Lens.lens (\ListWorldGenerationJobs' {nextToken} -> nextToken) (\s@ListWorldGenerationJobs' {} a -> s {nextToken = a} :: ListWorldGenerationJobs)

-- | Optional filters to limit results. You can use @status@ and
-- @templateId@.
listWorldGenerationJobs_filters :: Lens.Lens' ListWorldGenerationJobs (Prelude.Maybe (Prelude.NonEmpty Filter))
listWorldGenerationJobs_filters = Lens.lens (\ListWorldGenerationJobs' {filters} -> filters) (\s@ListWorldGenerationJobs' {} a -> s {filters = a} :: ListWorldGenerationJobs) Prelude.. Lens.mapping Lens.coerced

-- | When this parameter is used, @ListWorldGeneratorJobs@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListWorldGeneratorJobs@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListWorldGeneratorJobs@ returns up to 100
-- results and a @nextToken@ value if applicable.
listWorldGenerationJobs_maxResults :: Lens.Lens' ListWorldGenerationJobs (Prelude.Maybe Prelude.Int)
listWorldGenerationJobs_maxResults = Lens.lens (\ListWorldGenerationJobs' {maxResults} -> maxResults) (\s@ListWorldGenerationJobs' {} a -> s {maxResults = a} :: ListWorldGenerationJobs)

instance Core.AWSPager ListWorldGenerationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorldGenerationJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listWorldGenerationJobsResponse_worldGenerationJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorldGenerationJobs_nextToken
          Lens..~ rs
          Lens.^? listWorldGenerationJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListWorldGenerationJobs where
  type
    AWSResponse ListWorldGenerationJobs =
      ListWorldGenerationJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorldGenerationJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "worldGenerationJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListWorldGenerationJobs where
  hashWithSalt _salt ListWorldGenerationJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListWorldGenerationJobs where
  rnf ListWorldGenerationJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListWorldGenerationJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWorldGenerationJobs where
  toJSON ListWorldGenerationJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListWorldGenerationJobs where
  toPath = Prelude.const "/listWorldGenerationJobs"

instance Data.ToQuery ListWorldGenerationJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorldGenerationJobsResponse' smart constructor.
data ListWorldGenerationJobsResponse = ListWorldGenerationJobsResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call
    -- @ListWorldGeneratorJobsRequest@ again and assign that token to the
    -- request object\'s @nextToken@ parameter. If there are no remaining
    -- results, the previous response object\'s NextToken parameter is set to
    -- null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary information for world generator jobs.
    worldGenerationJobSummaries :: [WorldGenerationJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorldGenerationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorldGenerationJobsResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListWorldGeneratorJobsRequest@ again and assign that token to the
-- request object\'s @nextToken@ parameter. If there are no remaining
-- results, the previous response object\'s NextToken parameter is set to
-- null.
--
-- 'httpStatus', 'listWorldGenerationJobsResponse_httpStatus' - The response's http status code.
--
-- 'worldGenerationJobSummaries', 'listWorldGenerationJobsResponse_worldGenerationJobSummaries' - Summary information for world generator jobs.
newListWorldGenerationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorldGenerationJobsResponse
newListWorldGenerationJobsResponse pHttpStatus_ =
  ListWorldGenerationJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      worldGenerationJobSummaries =
        Prelude.mempty
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListWorldGeneratorJobsRequest@ again and assign that token to the
-- request object\'s @nextToken@ parameter. If there are no remaining
-- results, the previous response object\'s NextToken parameter is set to
-- null.
listWorldGenerationJobsResponse_nextToken :: Lens.Lens' ListWorldGenerationJobsResponse (Prelude.Maybe Prelude.Text)
listWorldGenerationJobsResponse_nextToken = Lens.lens (\ListWorldGenerationJobsResponse' {nextToken} -> nextToken) (\s@ListWorldGenerationJobsResponse' {} a -> s {nextToken = a} :: ListWorldGenerationJobsResponse)

-- | The response's http status code.
listWorldGenerationJobsResponse_httpStatus :: Lens.Lens' ListWorldGenerationJobsResponse Prelude.Int
listWorldGenerationJobsResponse_httpStatus = Lens.lens (\ListWorldGenerationJobsResponse' {httpStatus} -> httpStatus) (\s@ListWorldGenerationJobsResponse' {} a -> s {httpStatus = a} :: ListWorldGenerationJobsResponse)

-- | Summary information for world generator jobs.
listWorldGenerationJobsResponse_worldGenerationJobSummaries :: Lens.Lens' ListWorldGenerationJobsResponse [WorldGenerationJobSummary]
listWorldGenerationJobsResponse_worldGenerationJobSummaries = Lens.lens (\ListWorldGenerationJobsResponse' {worldGenerationJobSummaries} -> worldGenerationJobSummaries) (\s@ListWorldGenerationJobsResponse' {} a -> s {worldGenerationJobSummaries = a} :: ListWorldGenerationJobsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListWorldGenerationJobsResponse
  where
  rnf ListWorldGenerationJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf worldGenerationJobSummaries
