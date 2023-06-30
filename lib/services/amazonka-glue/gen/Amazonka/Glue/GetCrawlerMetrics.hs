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
-- Module      : Amazonka.Glue.GetCrawlerMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metrics about specified crawlers.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetCrawlerMetrics
  ( -- * Creating a Request
    GetCrawlerMetrics (..),
    newGetCrawlerMetrics,

    -- * Request Lenses
    getCrawlerMetrics_crawlerNameList,
    getCrawlerMetrics_maxResults,
    getCrawlerMetrics_nextToken,

    -- * Destructuring the Response
    GetCrawlerMetricsResponse (..),
    newGetCrawlerMetricsResponse,

    -- * Response Lenses
    getCrawlerMetricsResponse_crawlerMetricsList,
    getCrawlerMetricsResponse_nextToken,
    getCrawlerMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCrawlerMetrics' smart constructor.
data GetCrawlerMetrics = GetCrawlerMetrics'
  { -- | A list of the names of crawlers about which to retrieve metrics.
    crawlerNameList :: Prelude.Maybe [Prelude.Text],
    -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCrawlerMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerNameList', 'getCrawlerMetrics_crawlerNameList' - A list of the names of crawlers about which to retrieve metrics.
--
-- 'maxResults', 'getCrawlerMetrics_maxResults' - The maximum size of a list to return.
--
-- 'nextToken', 'getCrawlerMetrics_nextToken' - A continuation token, if this is a continuation call.
newGetCrawlerMetrics ::
  GetCrawlerMetrics
newGetCrawlerMetrics =
  GetCrawlerMetrics'
    { crawlerNameList =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A list of the names of crawlers about which to retrieve metrics.
getCrawlerMetrics_crawlerNameList :: Lens.Lens' GetCrawlerMetrics (Prelude.Maybe [Prelude.Text])
getCrawlerMetrics_crawlerNameList = Lens.lens (\GetCrawlerMetrics' {crawlerNameList} -> crawlerNameList) (\s@GetCrawlerMetrics' {} a -> s {crawlerNameList = a} :: GetCrawlerMetrics) Prelude.. Lens.mapping Lens.coerced

-- | The maximum size of a list to return.
getCrawlerMetrics_maxResults :: Lens.Lens' GetCrawlerMetrics (Prelude.Maybe Prelude.Natural)
getCrawlerMetrics_maxResults = Lens.lens (\GetCrawlerMetrics' {maxResults} -> maxResults) (\s@GetCrawlerMetrics' {} a -> s {maxResults = a} :: GetCrawlerMetrics)

-- | A continuation token, if this is a continuation call.
getCrawlerMetrics_nextToken :: Lens.Lens' GetCrawlerMetrics (Prelude.Maybe Prelude.Text)
getCrawlerMetrics_nextToken = Lens.lens (\GetCrawlerMetrics' {nextToken} -> nextToken) (\s@GetCrawlerMetrics' {} a -> s {nextToken = a} :: GetCrawlerMetrics)

instance Core.AWSPager GetCrawlerMetrics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCrawlerMetricsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCrawlerMetricsResponse_crawlerMetricsList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getCrawlerMetrics_nextToken
          Lens..~ rs
          Lens.^? getCrawlerMetricsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetCrawlerMetrics where
  type
    AWSResponse GetCrawlerMetrics =
      GetCrawlerMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlerMetricsResponse'
            Prelude.<$> ( x
                            Data..?> "CrawlerMetricsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCrawlerMetrics where
  hashWithSalt _salt GetCrawlerMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` crawlerNameList
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetCrawlerMetrics where
  rnf GetCrawlerMetrics' {..} =
    Prelude.rnf crawlerNameList
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetCrawlerMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetCrawlerMetrics" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCrawlerMetrics where
  toJSON GetCrawlerMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrawlerNameList" Data..=)
              Prelude.<$> crawlerNameList,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetCrawlerMetrics where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCrawlerMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCrawlerMetricsResponse' smart constructor.
data GetCrawlerMetricsResponse = GetCrawlerMetricsResponse'
  { -- | A list of metrics for the specified crawler.
    crawlerMetricsList :: Prelude.Maybe [CrawlerMetrics],
    -- | A continuation token, if the returned list does not contain the last
    -- metric available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCrawlerMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerMetricsList', 'getCrawlerMetricsResponse_crawlerMetricsList' - A list of metrics for the specified crawler.
--
-- 'nextToken', 'getCrawlerMetricsResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- metric available.
--
-- 'httpStatus', 'getCrawlerMetricsResponse_httpStatus' - The response's http status code.
newGetCrawlerMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCrawlerMetricsResponse
newGetCrawlerMetricsResponse pHttpStatus_ =
  GetCrawlerMetricsResponse'
    { crawlerMetricsList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of metrics for the specified crawler.
getCrawlerMetricsResponse_crawlerMetricsList :: Lens.Lens' GetCrawlerMetricsResponse (Prelude.Maybe [CrawlerMetrics])
getCrawlerMetricsResponse_crawlerMetricsList = Lens.lens (\GetCrawlerMetricsResponse' {crawlerMetricsList} -> crawlerMetricsList) (\s@GetCrawlerMetricsResponse' {} a -> s {crawlerMetricsList = a} :: GetCrawlerMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if the returned list does not contain the last
-- metric available.
getCrawlerMetricsResponse_nextToken :: Lens.Lens' GetCrawlerMetricsResponse (Prelude.Maybe Prelude.Text)
getCrawlerMetricsResponse_nextToken = Lens.lens (\GetCrawlerMetricsResponse' {nextToken} -> nextToken) (\s@GetCrawlerMetricsResponse' {} a -> s {nextToken = a} :: GetCrawlerMetricsResponse)

-- | The response's http status code.
getCrawlerMetricsResponse_httpStatus :: Lens.Lens' GetCrawlerMetricsResponse Prelude.Int
getCrawlerMetricsResponse_httpStatus = Lens.lens (\GetCrawlerMetricsResponse' {httpStatus} -> httpStatus) (\s@GetCrawlerMetricsResponse' {} a -> s {httpStatus = a} :: GetCrawlerMetricsResponse)

instance Prelude.NFData GetCrawlerMetricsResponse where
  rnf GetCrawlerMetricsResponse' {..} =
    Prelude.rnf crawlerMetricsList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
