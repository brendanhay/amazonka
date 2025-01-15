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
-- Module      : Amazonka.Kendra.GetSnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves search metrics data. The data provides a snapshot of how your
-- users interact with your search application and how effective the
-- application is.
module Amazonka.Kendra.GetSnapshots
  ( -- * Creating a Request
    GetSnapshots (..),
    newGetSnapshots,

    -- * Request Lenses
    getSnapshots_maxResults,
    getSnapshots_nextToken,
    getSnapshots_indexId,
    getSnapshots_interval,
    getSnapshots_metricType,

    -- * Destructuring the Response
    GetSnapshotsResponse (..),
    newGetSnapshotsResponse,

    -- * Response Lenses
    getSnapshotsResponse_nextToken,
    getSnapshotsResponse_snapShotTimeFilter,
    getSnapshotsResponse_snapshotsData,
    getSnapshotsResponse_snapshotsDataHeader,
    getSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSnapshots' smart constructor.
data GetSnapshots = GetSnapshots'
  { -- | The maximum number of returned data for the metric.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of search metrics
    -- data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index to get search metrics data.
    indexId :: Prelude.Text,
    -- | The time interval or time window to get search metrics data. The time
    -- interval uses the time zone of your index. You can view data in the
    -- following time windows:
    --
    -- -   @THIS_WEEK@: The current week, starting on the Sunday and ending on
    --     the day before the current date.
    --
    -- -   @ONE_WEEK_AGO@: The previous week, starting on the Sunday and ending
    --     on the following Saturday.
    --
    -- -   @TWO_WEEKS_AGO@: The week before the previous week, starting on the
    --     Sunday and ending on the following Saturday.
    --
    -- -   @THIS_MONTH@: The current month, starting on the first day of the
    --     month and ending on the day before the current date.
    --
    -- -   @ONE_MONTH_AGO@: The previous month, starting on the first day of
    --     the month and ending on the last day of the month.
    --
    -- -   @TWO_MONTHS_AGO@: The month before the previous month, starting on
    --     the first day of the month and ending on last day of the month.
    interval :: Interval,
    -- | The metric you want to retrieve. You can specify only one metric per
    -- call.
    --
    -- For more information about the metrics you can view, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/search-analytics.html Gaining insights with search analytics>.
    metricType :: MetricType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getSnapshots_maxResults' - The maximum number of returned data for the metric.
--
-- 'nextToken', 'getSnapshots_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of search metrics
-- data.
--
-- 'indexId', 'getSnapshots_indexId' - The identifier of the index to get search metrics data.
--
-- 'interval', 'getSnapshots_interval' - The time interval or time window to get search metrics data. The time
-- interval uses the time zone of your index. You can view data in the
-- following time windows:
--
-- -   @THIS_WEEK@: The current week, starting on the Sunday and ending on
--     the day before the current date.
--
-- -   @ONE_WEEK_AGO@: The previous week, starting on the Sunday and ending
--     on the following Saturday.
--
-- -   @TWO_WEEKS_AGO@: The week before the previous week, starting on the
--     Sunday and ending on the following Saturday.
--
-- -   @THIS_MONTH@: The current month, starting on the first day of the
--     month and ending on the day before the current date.
--
-- -   @ONE_MONTH_AGO@: The previous month, starting on the first day of
--     the month and ending on the last day of the month.
--
-- -   @TWO_MONTHS_AGO@: The month before the previous month, starting on
--     the first day of the month and ending on last day of the month.
--
-- 'metricType', 'getSnapshots_metricType' - The metric you want to retrieve. You can specify only one metric per
-- call.
--
-- For more information about the metrics you can view, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/search-analytics.html Gaining insights with search analytics>.
newGetSnapshots ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'interval'
  Interval ->
  -- | 'metricType'
  MetricType ->
  GetSnapshots
newGetSnapshots pIndexId_ pInterval_ pMetricType_ =
  GetSnapshots'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      indexId = pIndexId_,
      interval = pInterval_,
      metricType = pMetricType_
    }

-- | The maximum number of returned data for the metric.
getSnapshots_maxResults :: Lens.Lens' GetSnapshots (Prelude.Maybe Prelude.Int)
getSnapshots_maxResults = Lens.lens (\GetSnapshots' {maxResults} -> maxResults) (\s@GetSnapshots' {} a -> s {maxResults = a} :: GetSnapshots)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of search metrics
-- data.
getSnapshots_nextToken :: Lens.Lens' GetSnapshots (Prelude.Maybe Prelude.Text)
getSnapshots_nextToken = Lens.lens (\GetSnapshots' {nextToken} -> nextToken) (\s@GetSnapshots' {} a -> s {nextToken = a} :: GetSnapshots)

-- | The identifier of the index to get search metrics data.
getSnapshots_indexId :: Lens.Lens' GetSnapshots Prelude.Text
getSnapshots_indexId = Lens.lens (\GetSnapshots' {indexId} -> indexId) (\s@GetSnapshots' {} a -> s {indexId = a} :: GetSnapshots)

-- | The time interval or time window to get search metrics data. The time
-- interval uses the time zone of your index. You can view data in the
-- following time windows:
--
-- -   @THIS_WEEK@: The current week, starting on the Sunday and ending on
--     the day before the current date.
--
-- -   @ONE_WEEK_AGO@: The previous week, starting on the Sunday and ending
--     on the following Saturday.
--
-- -   @TWO_WEEKS_AGO@: The week before the previous week, starting on the
--     Sunday and ending on the following Saturday.
--
-- -   @THIS_MONTH@: The current month, starting on the first day of the
--     month and ending on the day before the current date.
--
-- -   @ONE_MONTH_AGO@: The previous month, starting on the first day of
--     the month and ending on the last day of the month.
--
-- -   @TWO_MONTHS_AGO@: The month before the previous month, starting on
--     the first day of the month and ending on last day of the month.
getSnapshots_interval :: Lens.Lens' GetSnapshots Interval
getSnapshots_interval = Lens.lens (\GetSnapshots' {interval} -> interval) (\s@GetSnapshots' {} a -> s {interval = a} :: GetSnapshots)

-- | The metric you want to retrieve. You can specify only one metric per
-- call.
--
-- For more information about the metrics you can view, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/search-analytics.html Gaining insights with search analytics>.
getSnapshots_metricType :: Lens.Lens' GetSnapshots MetricType
getSnapshots_metricType = Lens.lens (\GetSnapshots' {metricType} -> metricType) (\s@GetSnapshots' {} a -> s {metricType = a} :: GetSnapshots)

instance Core.AWSRequest GetSnapshots where
  type AWSResponse GetSnapshots = GetSnapshotsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSnapshotsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SnapShotTimeFilter")
            Prelude.<*> (x Data..?> "SnapshotsData" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "SnapshotsDataHeader"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSnapshots where
  hashWithSalt _salt GetSnapshots' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` metricType

instance Prelude.NFData GetSnapshots where
  rnf GetSnapshots' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf indexId `Prelude.seq`
          Prelude.rnf interval `Prelude.seq`
            Prelude.rnf metricType

instance Data.ToHeaders GetSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.GetSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSnapshots where
  toJSON GetSnapshots' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Interval" Data..= interval),
            Prelude.Just ("MetricType" Data..= metricType)
          ]
      )

instance Data.ToPath GetSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSnapshotsResponse' smart constructor.
data GetSnapshotsResponse = GetSnapshotsResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token, which
    -- you can use in a later request to retrieve the next set of search
    -- metrics data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The date-time for the beginning and end of the time window for the
    -- search metrics data.
    snapShotTimeFilter :: Prelude.Maybe TimeRange,
    -- | The search metrics data. The data returned depends on the metric type
    -- you requested.
    snapshotsData :: Prelude.Maybe [[Prelude.Text]],
    -- | The column headers for the search metrics data.
    snapshotsDataHeader :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSnapshotsResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token, which
-- you can use in a later request to retrieve the next set of search
-- metrics data.
--
-- 'snapShotTimeFilter', 'getSnapshotsResponse_snapShotTimeFilter' - The date-time for the beginning and end of the time window for the
-- search metrics data.
--
-- 'snapshotsData', 'getSnapshotsResponse_snapshotsData' - The search metrics data. The data returned depends on the metric type
-- you requested.
--
-- 'snapshotsDataHeader', 'getSnapshotsResponse_snapshotsDataHeader' - The column headers for the search metrics data.
--
-- 'httpStatus', 'getSnapshotsResponse_httpStatus' - The response's http status code.
newGetSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSnapshotsResponse
newGetSnapshotsResponse pHttpStatus_ =
  GetSnapshotsResponse'
    { nextToken = Prelude.Nothing,
      snapShotTimeFilter = Prelude.Nothing,
      snapshotsData = Prelude.Nothing,
      snapshotsDataHeader = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Kendra returns this token, which
-- you can use in a later request to retrieve the next set of search
-- metrics data.
getSnapshotsResponse_nextToken :: Lens.Lens' GetSnapshotsResponse (Prelude.Maybe Prelude.Text)
getSnapshotsResponse_nextToken = Lens.lens (\GetSnapshotsResponse' {nextToken} -> nextToken) (\s@GetSnapshotsResponse' {} a -> s {nextToken = a} :: GetSnapshotsResponse)

-- | The date-time for the beginning and end of the time window for the
-- search metrics data.
getSnapshotsResponse_snapShotTimeFilter :: Lens.Lens' GetSnapshotsResponse (Prelude.Maybe TimeRange)
getSnapshotsResponse_snapShotTimeFilter = Lens.lens (\GetSnapshotsResponse' {snapShotTimeFilter} -> snapShotTimeFilter) (\s@GetSnapshotsResponse' {} a -> s {snapShotTimeFilter = a} :: GetSnapshotsResponse)

-- | The search metrics data. The data returned depends on the metric type
-- you requested.
getSnapshotsResponse_snapshotsData :: Lens.Lens' GetSnapshotsResponse (Prelude.Maybe [[Prelude.Text]])
getSnapshotsResponse_snapshotsData = Lens.lens (\GetSnapshotsResponse' {snapshotsData} -> snapshotsData) (\s@GetSnapshotsResponse' {} a -> s {snapshotsData = a} :: GetSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The column headers for the search metrics data.
getSnapshotsResponse_snapshotsDataHeader :: Lens.Lens' GetSnapshotsResponse (Prelude.Maybe [Prelude.Text])
getSnapshotsResponse_snapshotsDataHeader = Lens.lens (\GetSnapshotsResponse' {snapshotsDataHeader} -> snapshotsDataHeader) (\s@GetSnapshotsResponse' {} a -> s {snapshotsDataHeader = a} :: GetSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSnapshotsResponse_httpStatus :: Lens.Lens' GetSnapshotsResponse Prelude.Int
getSnapshotsResponse_httpStatus = Lens.lens (\GetSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetSnapshotsResponse' {} a -> s {httpStatus = a} :: GetSnapshotsResponse)

instance Prelude.NFData GetSnapshotsResponse where
  rnf GetSnapshotsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf snapShotTimeFilter `Prelude.seq`
        Prelude.rnf snapshotsData `Prelude.seq`
          Prelude.rnf snapshotsDataHeader `Prelude.seq`
            Prelude.rnf httpStatus
