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
-- Module      : Amazonka.RobOMaker.ListWorldExportJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists world export jobs.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListWorldExportJobs
  ( -- * Creating a Request
    ListWorldExportJobs (..),
    newListWorldExportJobs,

    -- * Request Lenses
    listWorldExportJobs_filters,
    listWorldExportJobs_maxResults,
    listWorldExportJobs_nextToken,

    -- * Destructuring the Response
    ListWorldExportJobsResponse (..),
    newListWorldExportJobsResponse,

    -- * Response Lenses
    listWorldExportJobsResponse_nextToken,
    listWorldExportJobsResponse_httpStatus,
    listWorldExportJobsResponse_worldExportJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListWorldExportJobs' smart constructor.
data ListWorldExportJobs = ListWorldExportJobs'
  { -- | Optional filters to limit results. You can use @generationJobId@ and
    -- @templateId@.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | When this parameter is used, @ListWorldExportJobs@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListWorldExportJobs@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListWorldExportJobs@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListWorldExportJobs@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorldExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listWorldExportJobs_filters' - Optional filters to limit results. You can use @generationJobId@ and
-- @templateId@.
--
-- 'maxResults', 'listWorldExportJobs_maxResults' - When this parameter is used, @ListWorldExportJobs@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListWorldExportJobs@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListWorldExportJobs@ returns up to 100
-- results and a @nextToken@ value if applicable.
--
-- 'nextToken', 'listWorldExportJobs_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldExportJobs@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
newListWorldExportJobs ::
  ListWorldExportJobs
newListWorldExportJobs =
  ListWorldExportJobs'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Optional filters to limit results. You can use @generationJobId@ and
-- @templateId@.
listWorldExportJobs_filters :: Lens.Lens' ListWorldExportJobs (Prelude.Maybe (Prelude.NonEmpty Filter))
listWorldExportJobs_filters = Lens.lens (\ListWorldExportJobs' {filters} -> filters) (\s@ListWorldExportJobs' {} a -> s {filters = a} :: ListWorldExportJobs) Prelude.. Lens.mapping Lens.coerced

-- | When this parameter is used, @ListWorldExportJobs@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListWorldExportJobs@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListWorldExportJobs@ returns up to 100
-- results and a @nextToken@ value if applicable.
listWorldExportJobs_maxResults :: Lens.Lens' ListWorldExportJobs (Prelude.Maybe Prelude.Int)
listWorldExportJobs_maxResults = Lens.lens (\ListWorldExportJobs' {maxResults} -> maxResults) (\s@ListWorldExportJobs' {} a -> s {maxResults = a} :: ListWorldExportJobs)

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldExportJobs@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listWorldExportJobs_nextToken :: Lens.Lens' ListWorldExportJobs (Prelude.Maybe Prelude.Text)
listWorldExportJobs_nextToken = Lens.lens (\ListWorldExportJobs' {nextToken} -> nextToken) (\s@ListWorldExportJobs' {} a -> s {nextToken = a} :: ListWorldExportJobs)

instance Core.AWSPager ListWorldExportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorldExportJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listWorldExportJobsResponse_worldExportJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorldExportJobs_nextToken
          Lens..~ rs
          Lens.^? listWorldExportJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListWorldExportJobs where
  type
    AWSResponse ListWorldExportJobs =
      ListWorldExportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorldExportJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "worldExportJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListWorldExportJobs where
  hashWithSalt _salt ListWorldExportJobs' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListWorldExportJobs where
  rnf ListWorldExportJobs' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListWorldExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWorldExportJobs where
  toJSON ListWorldExportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListWorldExportJobs where
  toPath = Prelude.const "/listWorldExportJobs"

instance Data.ToQuery ListWorldExportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorldExportJobsResponse' smart constructor.
data ListWorldExportJobsResponse = ListWorldExportJobsResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call
    -- @ListWorldExportJobsRequest@ again and assign that token to the request
    -- object\'s @nextToken@ parameter. If there are no remaining results, the
    -- previous response object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary information for world export jobs.
    worldExportJobSummaries :: [WorldExportJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorldExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorldExportJobsResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListWorldExportJobsRequest@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
--
-- 'httpStatus', 'listWorldExportJobsResponse_httpStatus' - The response's http status code.
--
-- 'worldExportJobSummaries', 'listWorldExportJobsResponse_worldExportJobSummaries' - Summary information for world export jobs.
newListWorldExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorldExportJobsResponse
newListWorldExportJobsResponse pHttpStatus_ =
  ListWorldExportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      worldExportJobSummaries = Prelude.mempty
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListWorldExportJobsRequest@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
listWorldExportJobsResponse_nextToken :: Lens.Lens' ListWorldExportJobsResponse (Prelude.Maybe Prelude.Text)
listWorldExportJobsResponse_nextToken = Lens.lens (\ListWorldExportJobsResponse' {nextToken} -> nextToken) (\s@ListWorldExportJobsResponse' {} a -> s {nextToken = a} :: ListWorldExportJobsResponse)

-- | The response's http status code.
listWorldExportJobsResponse_httpStatus :: Lens.Lens' ListWorldExportJobsResponse Prelude.Int
listWorldExportJobsResponse_httpStatus = Lens.lens (\ListWorldExportJobsResponse' {httpStatus} -> httpStatus) (\s@ListWorldExportJobsResponse' {} a -> s {httpStatus = a} :: ListWorldExportJobsResponse)

-- | Summary information for world export jobs.
listWorldExportJobsResponse_worldExportJobSummaries :: Lens.Lens' ListWorldExportJobsResponse [WorldExportJobSummary]
listWorldExportJobsResponse_worldExportJobSummaries = Lens.lens (\ListWorldExportJobsResponse' {worldExportJobSummaries} -> worldExportJobSummaries) (\s@ListWorldExportJobsResponse' {} a -> s {worldExportJobSummaries = a} :: ListWorldExportJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListWorldExportJobsResponse where
  rnf ListWorldExportJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf worldExportJobSummaries
