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
-- Module      : Amazonka.Personalize.ListMetricAttributions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists metric attributions.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListMetricAttributions
  ( -- * Creating a Request
    ListMetricAttributions (..),
    newListMetricAttributions,

    -- * Request Lenses
    listMetricAttributions_datasetGroupArn,
    listMetricAttributions_maxResults,
    listMetricAttributions_nextToken,

    -- * Destructuring the Response
    ListMetricAttributionsResponse (..),
    newListMetricAttributionsResponse,

    -- * Response Lenses
    listMetricAttributionsResponse_metricAttributions,
    listMetricAttributionsResponse_nextToken,
    listMetricAttributionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMetricAttributions' smart constructor.
data ListMetricAttributions = ListMetricAttributions'
  { -- | The metric attributions\' dataset group Amazon Resource Name (ARN).
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of metric attributions to return in one page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricAttributions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetGroupArn', 'listMetricAttributions_datasetGroupArn' - The metric attributions\' dataset group Amazon Resource Name (ARN).
--
-- 'maxResults', 'listMetricAttributions_maxResults' - The maximum number of metric attributions to return in one page of
-- results.
--
-- 'nextToken', 'listMetricAttributions_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListMetricAttributions ::
  ListMetricAttributions
newListMetricAttributions =
  ListMetricAttributions'
    { datasetGroupArn =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The metric attributions\' dataset group Amazon Resource Name (ARN).
listMetricAttributions_datasetGroupArn :: Lens.Lens' ListMetricAttributions (Prelude.Maybe Prelude.Text)
listMetricAttributions_datasetGroupArn = Lens.lens (\ListMetricAttributions' {datasetGroupArn} -> datasetGroupArn) (\s@ListMetricAttributions' {} a -> s {datasetGroupArn = a} :: ListMetricAttributions)

-- | The maximum number of metric attributions to return in one page of
-- results.
listMetricAttributions_maxResults :: Lens.Lens' ListMetricAttributions (Prelude.Maybe Prelude.Natural)
listMetricAttributions_maxResults = Lens.lens (\ListMetricAttributions' {maxResults} -> maxResults) (\s@ListMetricAttributions' {} a -> s {maxResults = a} :: ListMetricAttributions)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listMetricAttributions_nextToken :: Lens.Lens' ListMetricAttributions (Prelude.Maybe Prelude.Text)
listMetricAttributions_nextToken = Lens.lens (\ListMetricAttributions' {nextToken} -> nextToken) (\s@ListMetricAttributions' {} a -> s {nextToken = a} :: ListMetricAttributions)

instance Core.AWSPager ListMetricAttributions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMetricAttributionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMetricAttributionsResponse_metricAttributions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listMetricAttributions_nextToken
          Lens..~ rs
          Lens.^? listMetricAttributionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListMetricAttributions where
  type
    AWSResponse ListMetricAttributions =
      ListMetricAttributionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMetricAttributionsResponse'
            Prelude.<$> ( x
                            Data..?> "metricAttributions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMetricAttributions where
  hashWithSalt _salt ListMetricAttributions' {..} =
    _salt
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMetricAttributions where
  rnf ListMetricAttributions' {..} =
    Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListMetricAttributions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListMetricAttributions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMetricAttributions where
  toJSON ListMetricAttributions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("datasetGroupArn" Data..=)
              Prelude.<$> datasetGroupArn,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListMetricAttributions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMetricAttributions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMetricAttributionsResponse' smart constructor.
data ListMetricAttributionsResponse = ListMetricAttributionsResponse'
  { -- | The list of metric attributions.
    metricAttributions :: Prelude.Maybe [MetricAttributionSummary],
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricAttributionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricAttributions', 'listMetricAttributionsResponse_metricAttributions' - The list of metric attributions.
--
-- 'nextToken', 'listMetricAttributionsResponse_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'httpStatus', 'listMetricAttributionsResponse_httpStatus' - The response's http status code.
newListMetricAttributionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMetricAttributionsResponse
newListMetricAttributionsResponse pHttpStatus_ =
  ListMetricAttributionsResponse'
    { metricAttributions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of metric attributions.
listMetricAttributionsResponse_metricAttributions :: Lens.Lens' ListMetricAttributionsResponse (Prelude.Maybe [MetricAttributionSummary])
listMetricAttributionsResponse_metricAttributions = Lens.lens (\ListMetricAttributionsResponse' {metricAttributions} -> metricAttributions) (\s@ListMetricAttributionsResponse' {} a -> s {metricAttributions = a} :: ListMetricAttributionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listMetricAttributionsResponse_nextToken :: Lens.Lens' ListMetricAttributionsResponse (Prelude.Maybe Prelude.Text)
listMetricAttributionsResponse_nextToken = Lens.lens (\ListMetricAttributionsResponse' {nextToken} -> nextToken) (\s@ListMetricAttributionsResponse' {} a -> s {nextToken = a} :: ListMetricAttributionsResponse)

-- | The response's http status code.
listMetricAttributionsResponse_httpStatus :: Lens.Lens' ListMetricAttributionsResponse Prelude.Int
listMetricAttributionsResponse_httpStatus = Lens.lens (\ListMetricAttributionsResponse' {httpStatus} -> httpStatus) (\s@ListMetricAttributionsResponse' {} a -> s {httpStatus = a} :: ListMetricAttributionsResponse)

instance
  Prelude.NFData
    ListMetricAttributionsResponse
  where
  rnf ListMetricAttributionsResponse' {..} =
    Prelude.rnf metricAttributions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
