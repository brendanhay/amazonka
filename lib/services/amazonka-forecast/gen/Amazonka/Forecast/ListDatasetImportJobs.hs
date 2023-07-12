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
-- Module      : Amazonka.Forecast.ListDatasetImportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of dataset import jobs created using the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation. For each import job, this operation returns a summary of its
-- properties, including its Amazon Resource Name (ARN). You can retrieve
-- the complete set of properties by using the ARN with the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DescribeDatasetImportJob.html DescribeDatasetImportJob>
-- operation. You can filter the list by providing an array of
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_Filter.html Filter>
-- objects.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListDatasetImportJobs
  ( -- * Creating a Request
    ListDatasetImportJobs (..),
    newListDatasetImportJobs,

    -- * Request Lenses
    listDatasetImportJobs_filters,
    listDatasetImportJobs_maxResults,
    listDatasetImportJobs_nextToken,

    -- * Destructuring the Response
    ListDatasetImportJobsResponse (..),
    newListDatasetImportJobsResponse,

    -- * Response Lenses
    listDatasetImportJobsResponse_datasetImportJobs,
    listDatasetImportJobsResponse_nextToken,
    listDatasetImportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasetImportJobs' smart constructor.
data ListDatasetImportJobs = ListDatasetImportJobs'
  { -- | An array of filters. For each filter, you provide a condition and a
    -- match statement. The condition is either @IS@ or @IS_NOT@, which
    -- specifies whether to include or exclude the datasets that match the
    -- statement from the list, respectively. The match statement consists of a
    -- key and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@. To include the datasets that match the statement, specify
    --     @IS@. To exclude matching datasets, specify @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. Valid values are
    --     @DatasetArn@ and @Status@.
    --
    -- -   @Value@ - The value to match.
    --
    -- For example, to list all dataset import jobs whose status is ACTIVE, you
    -- specify the following filter:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
    filters :: Prelude.Maybe [Filter],
    -- | The number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listDatasetImportJobs_filters' - An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the datasets that match the
-- statement from the list, respectively. The match statement consists of a
-- key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the datasets that match the statement, specify
--     @IS@. To exclude matching datasets, specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @DatasetArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all dataset import jobs whose status is ACTIVE, you
-- specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
--
-- 'maxResults', 'listDatasetImportJobs_maxResults' - The number of items to return in the response.
--
-- 'nextToken', 'listDatasetImportJobs_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
newListDatasetImportJobs ::
  ListDatasetImportJobs
newListDatasetImportJobs =
  ListDatasetImportJobs'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the datasets that match the
-- statement from the list, respectively. The match statement consists of a
-- key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the datasets that match the statement, specify
--     @IS@. To exclude matching datasets, specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @DatasetArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all dataset import jobs whose status is ACTIVE, you
-- specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
listDatasetImportJobs_filters :: Lens.Lens' ListDatasetImportJobs (Prelude.Maybe [Filter])
listDatasetImportJobs_filters = Lens.lens (\ListDatasetImportJobs' {filters} -> filters) (\s@ListDatasetImportJobs' {} a -> s {filters = a} :: ListDatasetImportJobs) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to return in the response.
listDatasetImportJobs_maxResults :: Lens.Lens' ListDatasetImportJobs (Prelude.Maybe Prelude.Natural)
listDatasetImportJobs_maxResults = Lens.lens (\ListDatasetImportJobs' {maxResults} -> maxResults) (\s@ListDatasetImportJobs' {} a -> s {maxResults = a} :: ListDatasetImportJobs)

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listDatasetImportJobs_nextToken :: Lens.Lens' ListDatasetImportJobs (Prelude.Maybe Prelude.Text)
listDatasetImportJobs_nextToken = Lens.lens (\ListDatasetImportJobs' {nextToken} -> nextToken) (\s@ListDatasetImportJobs' {} a -> s {nextToken = a} :: ListDatasetImportJobs)

instance Core.AWSPager ListDatasetImportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetImportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetImportJobsResponse_datasetImportJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDatasetImportJobs_nextToken
          Lens..~ rs
          Lens.^? listDatasetImportJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDatasetImportJobs where
  type
    AWSResponse ListDatasetImportJobs =
      ListDatasetImportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetImportJobsResponse'
            Prelude.<$> ( x
                            Data..?> "DatasetImportJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasetImportJobs where
  hashWithSalt _salt ListDatasetImportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDatasetImportJobs where
  rnf ListDatasetImportJobs' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDatasetImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListDatasetImportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatasetImportJobs where
  toJSON ListDatasetImportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDatasetImportJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDatasetImportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasetImportJobsResponse' smart constructor.
data ListDatasetImportJobsResponse = ListDatasetImportJobsResponse'
  { -- | An array of objects that summarize each dataset import job\'s
    -- properties.
    datasetImportJobs :: Prelude.Maybe [DatasetImportJobSummary],
    -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetImportJobs', 'listDatasetImportJobsResponse_datasetImportJobs' - An array of objects that summarize each dataset import job\'s
-- properties.
--
-- 'nextToken', 'listDatasetImportJobsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'httpStatus', 'listDatasetImportJobsResponse_httpStatus' - The response's http status code.
newListDatasetImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetImportJobsResponse
newListDatasetImportJobsResponse pHttpStatus_ =
  ListDatasetImportJobsResponse'
    { datasetImportJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that summarize each dataset import job\'s
-- properties.
listDatasetImportJobsResponse_datasetImportJobs :: Lens.Lens' ListDatasetImportJobsResponse (Prelude.Maybe [DatasetImportJobSummary])
listDatasetImportJobsResponse_datasetImportJobs = Lens.lens (\ListDatasetImportJobsResponse' {datasetImportJobs} -> datasetImportJobs) (\s@ListDatasetImportJobsResponse' {} a -> s {datasetImportJobs = a} :: ListDatasetImportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
listDatasetImportJobsResponse_nextToken :: Lens.Lens' ListDatasetImportJobsResponse (Prelude.Maybe Prelude.Text)
listDatasetImportJobsResponse_nextToken = Lens.lens (\ListDatasetImportJobsResponse' {nextToken} -> nextToken) (\s@ListDatasetImportJobsResponse' {} a -> s {nextToken = a} :: ListDatasetImportJobsResponse)

-- | The response's http status code.
listDatasetImportJobsResponse_httpStatus :: Lens.Lens' ListDatasetImportJobsResponse Prelude.Int
listDatasetImportJobsResponse_httpStatus = Lens.lens (\ListDatasetImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetImportJobsResponse' {} a -> s {httpStatus = a} :: ListDatasetImportJobsResponse)

instance Prelude.NFData ListDatasetImportJobsResponse where
  rnf ListDatasetImportJobsResponse' {..} =
    Prelude.rnf datasetImportJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
