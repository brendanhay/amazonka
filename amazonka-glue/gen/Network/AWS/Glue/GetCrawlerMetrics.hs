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
-- Module      : Network.AWS.Glue.GetCrawlerMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metrics about specified crawlers.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetCrawlerMetrics
  ( -- * Creating a Request
    GetCrawlerMetrics (..),
    newGetCrawlerMetrics,

    -- * Request Lenses
    getCrawlerMetrics_nextToken,
    getCrawlerMetrics_crawlerNameList,
    getCrawlerMetrics_maxResults,

    -- * Destructuring the Response
    GetCrawlerMetricsResponse (..),
    newGetCrawlerMetricsResponse,

    -- * Response Lenses
    getCrawlerMetricsResponse_nextToken,
    getCrawlerMetricsResponse_crawlerMetricsList,
    getCrawlerMetricsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCrawlerMetrics' smart constructor.
data GetCrawlerMetrics = GetCrawlerMetrics'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the names of crawlers about which to retrieve metrics.
    crawlerNameList :: Prelude.Maybe [Prelude.Text],
    -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'getCrawlerMetrics_nextToken' - A continuation token, if this is a continuation call.
--
-- 'crawlerNameList', 'getCrawlerMetrics_crawlerNameList' - A list of the names of crawlers about which to retrieve metrics.
--
-- 'maxResults', 'getCrawlerMetrics_maxResults' - The maximum size of a list to return.
newGetCrawlerMetrics ::
  GetCrawlerMetrics
newGetCrawlerMetrics =
  GetCrawlerMetrics'
    { nextToken = Prelude.Nothing,
      crawlerNameList = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation call.
getCrawlerMetrics_nextToken :: Lens.Lens' GetCrawlerMetrics (Prelude.Maybe Prelude.Text)
getCrawlerMetrics_nextToken = Lens.lens (\GetCrawlerMetrics' {nextToken} -> nextToken) (\s@GetCrawlerMetrics' {} a -> s {nextToken = a} :: GetCrawlerMetrics)

-- | A list of the names of crawlers about which to retrieve metrics.
getCrawlerMetrics_crawlerNameList :: Lens.Lens' GetCrawlerMetrics (Prelude.Maybe [Prelude.Text])
getCrawlerMetrics_crawlerNameList = Lens.lens (\GetCrawlerMetrics' {crawlerNameList} -> crawlerNameList) (\s@GetCrawlerMetrics' {} a -> s {crawlerNameList = a} :: GetCrawlerMetrics) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum size of a list to return.
getCrawlerMetrics_maxResults :: Lens.Lens' GetCrawlerMetrics (Prelude.Maybe Prelude.Natural)
getCrawlerMetrics_maxResults = Lens.lens (\GetCrawlerMetrics' {maxResults} -> maxResults) (\s@GetCrawlerMetrics' {} a -> s {maxResults = a} :: GetCrawlerMetrics)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& getCrawlerMetrics_nextToken
          Lens..~ rs
          Lens.^? getCrawlerMetricsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetCrawlerMetrics where
  type
    AWSResponse GetCrawlerMetrics =
      GetCrawlerMetricsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlerMetricsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CrawlerMetricsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCrawlerMetrics

instance Prelude.NFData GetCrawlerMetrics

instance Core.ToHeaders GetCrawlerMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetCrawlerMetrics" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCrawlerMetrics where
  toJSON GetCrawlerMetrics' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("CrawlerNameList" Core..=)
              Prelude.<$> crawlerNameList,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetCrawlerMetrics where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCrawlerMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCrawlerMetricsResponse' smart constructor.
data GetCrawlerMetricsResponse = GetCrawlerMetricsResponse'
  { -- | A continuation token, if the returned list does not contain the last
    -- metric available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of metrics for the specified crawler.
    crawlerMetricsList :: Prelude.Maybe [CrawlerMetrics],
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
-- 'nextToken', 'getCrawlerMetricsResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- metric available.
--
-- 'crawlerMetricsList', 'getCrawlerMetricsResponse_crawlerMetricsList' - A list of metrics for the specified crawler.
--
-- 'httpStatus', 'getCrawlerMetricsResponse_httpStatus' - The response's http status code.
newGetCrawlerMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCrawlerMetricsResponse
newGetCrawlerMetricsResponse pHttpStatus_ =
  GetCrawlerMetricsResponse'
    { nextToken =
        Prelude.Nothing,
      crawlerMetricsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the returned list does not contain the last
-- metric available.
getCrawlerMetricsResponse_nextToken :: Lens.Lens' GetCrawlerMetricsResponse (Prelude.Maybe Prelude.Text)
getCrawlerMetricsResponse_nextToken = Lens.lens (\GetCrawlerMetricsResponse' {nextToken} -> nextToken) (\s@GetCrawlerMetricsResponse' {} a -> s {nextToken = a} :: GetCrawlerMetricsResponse)

-- | A list of metrics for the specified crawler.
getCrawlerMetricsResponse_crawlerMetricsList :: Lens.Lens' GetCrawlerMetricsResponse (Prelude.Maybe [CrawlerMetrics])
getCrawlerMetricsResponse_crawlerMetricsList = Lens.lens (\GetCrawlerMetricsResponse' {crawlerMetricsList} -> crawlerMetricsList) (\s@GetCrawlerMetricsResponse' {} a -> s {crawlerMetricsList = a} :: GetCrawlerMetricsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCrawlerMetricsResponse_httpStatus :: Lens.Lens' GetCrawlerMetricsResponse Prelude.Int
getCrawlerMetricsResponse_httpStatus = Lens.lens (\GetCrawlerMetricsResponse' {httpStatus} -> httpStatus) (\s@GetCrawlerMetricsResponse' {} a -> s {httpStatus = a} :: GetCrawlerMetricsResponse)

instance Prelude.NFData GetCrawlerMetricsResponse
