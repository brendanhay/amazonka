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
-- Module      : Amazonka.LookoutMetrics.DescribeAnomalyDetectionExecutions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the status of the specified anomaly detection
-- jobs.
module Amazonka.LookoutMetrics.DescribeAnomalyDetectionExecutions
  ( -- * Creating a Request
    DescribeAnomalyDetectionExecutions (..),
    newDescribeAnomalyDetectionExecutions,

    -- * Request Lenses
    describeAnomalyDetectionExecutions_maxResults,
    describeAnomalyDetectionExecutions_nextToken,
    describeAnomalyDetectionExecutions_timestamp,
    describeAnomalyDetectionExecutions_anomalyDetectorArn,

    -- * Destructuring the Response
    DescribeAnomalyDetectionExecutionsResponse (..),
    newDescribeAnomalyDetectionExecutionsResponse,

    -- * Response Lenses
    describeAnomalyDetectionExecutionsResponse_executionList,
    describeAnomalyDetectionExecutionsResponse_nextToken,
    describeAnomalyDetectionExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAnomalyDetectionExecutions' smart constructor.
data DescribeAnomalyDetectionExecutions = DescribeAnomalyDetectionExecutions'
  { -- | The number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the anomaly detection job.
    timestamp :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the anomaly detector.
    anomalyDetectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnomalyDetectionExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeAnomalyDetectionExecutions_maxResults' - The number of items to return in the response.
--
-- 'nextToken', 'describeAnomalyDetectionExecutions_nextToken' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'timestamp', 'describeAnomalyDetectionExecutions_timestamp' - The timestamp of the anomaly detection job.
--
-- 'anomalyDetectorArn', 'describeAnomalyDetectionExecutions_anomalyDetectorArn' - The Amazon Resource Name (ARN) of the anomaly detector.
newDescribeAnomalyDetectionExecutions ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  DescribeAnomalyDetectionExecutions
newDescribeAnomalyDetectionExecutions
  pAnomalyDetectorArn_ =
    DescribeAnomalyDetectionExecutions'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        timestamp = Prelude.Nothing,
        anomalyDetectorArn =
          pAnomalyDetectorArn_
      }

-- | The number of items to return in the response.
describeAnomalyDetectionExecutions_maxResults :: Lens.Lens' DescribeAnomalyDetectionExecutions (Prelude.Maybe Prelude.Natural)
describeAnomalyDetectionExecutions_maxResults = Lens.lens (\DescribeAnomalyDetectionExecutions' {maxResults} -> maxResults) (\s@DescribeAnomalyDetectionExecutions' {} a -> s {maxResults = a} :: DescribeAnomalyDetectionExecutions)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
describeAnomalyDetectionExecutions_nextToken :: Lens.Lens' DescribeAnomalyDetectionExecutions (Prelude.Maybe Prelude.Text)
describeAnomalyDetectionExecutions_nextToken = Lens.lens (\DescribeAnomalyDetectionExecutions' {nextToken} -> nextToken) (\s@DescribeAnomalyDetectionExecutions' {} a -> s {nextToken = a} :: DescribeAnomalyDetectionExecutions)

-- | The timestamp of the anomaly detection job.
describeAnomalyDetectionExecutions_timestamp :: Lens.Lens' DescribeAnomalyDetectionExecutions (Prelude.Maybe Prelude.Text)
describeAnomalyDetectionExecutions_timestamp = Lens.lens (\DescribeAnomalyDetectionExecutions' {timestamp} -> timestamp) (\s@DescribeAnomalyDetectionExecutions' {} a -> s {timestamp = a} :: DescribeAnomalyDetectionExecutions)

-- | The Amazon Resource Name (ARN) of the anomaly detector.
describeAnomalyDetectionExecutions_anomalyDetectorArn :: Lens.Lens' DescribeAnomalyDetectionExecutions Prelude.Text
describeAnomalyDetectionExecutions_anomalyDetectorArn = Lens.lens (\DescribeAnomalyDetectionExecutions' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@DescribeAnomalyDetectionExecutions' {} a -> s {anomalyDetectorArn = a} :: DescribeAnomalyDetectionExecutions)

instance
  Core.AWSRequest
    DescribeAnomalyDetectionExecutions
  where
  type
    AWSResponse DescribeAnomalyDetectionExecutions =
      DescribeAnomalyDetectionExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAnomalyDetectionExecutionsResponse'
            Prelude.<$> (x Data..?> "ExecutionList" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAnomalyDetectionExecutions
  where
  hashWithSalt
    _salt
    DescribeAnomalyDetectionExecutions' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` timestamp
        `Prelude.hashWithSalt` anomalyDetectorArn

instance
  Prelude.NFData
    DescribeAnomalyDetectionExecutions
  where
  rnf DescribeAnomalyDetectionExecutions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf anomalyDetectorArn

instance
  Data.ToHeaders
    DescribeAnomalyDetectionExecutions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeAnomalyDetectionExecutions
  where
  toJSON DescribeAnomalyDetectionExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Timestamp" Data..=) Prelude.<$> timestamp,
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn)
          ]
      )

instance
  Data.ToPath
    DescribeAnomalyDetectionExecutions
  where
  toPath =
    Prelude.const "/DescribeAnomalyDetectionExecutions"

instance
  Data.ToQuery
    DescribeAnomalyDetectionExecutions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAnomalyDetectionExecutionsResponse' smart constructor.
data DescribeAnomalyDetectionExecutionsResponse = DescribeAnomalyDetectionExecutionsResponse'
  { -- | A list of detection jobs.
    executionList :: Prelude.Maybe [ExecutionStatus],
    -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnomalyDetectionExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionList', 'describeAnomalyDetectionExecutionsResponse_executionList' - A list of detection jobs.
--
-- 'nextToken', 'describeAnomalyDetectionExecutionsResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'describeAnomalyDetectionExecutionsResponse_httpStatus' - The response's http status code.
newDescribeAnomalyDetectionExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAnomalyDetectionExecutionsResponse
newDescribeAnomalyDetectionExecutionsResponse
  pHttpStatus_ =
    DescribeAnomalyDetectionExecutionsResponse'
      { executionList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of detection jobs.
describeAnomalyDetectionExecutionsResponse_executionList :: Lens.Lens' DescribeAnomalyDetectionExecutionsResponse (Prelude.Maybe [ExecutionStatus])
describeAnomalyDetectionExecutionsResponse_executionList = Lens.lens (\DescribeAnomalyDetectionExecutionsResponse' {executionList} -> executionList) (\s@DescribeAnomalyDetectionExecutionsResponse' {} a -> s {executionList = a} :: DescribeAnomalyDetectionExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s included if more results are available.
describeAnomalyDetectionExecutionsResponse_nextToken :: Lens.Lens' DescribeAnomalyDetectionExecutionsResponse (Prelude.Maybe Prelude.Text)
describeAnomalyDetectionExecutionsResponse_nextToken = Lens.lens (\DescribeAnomalyDetectionExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeAnomalyDetectionExecutionsResponse' {} a -> s {nextToken = a} :: DescribeAnomalyDetectionExecutionsResponse)

-- | The response's http status code.
describeAnomalyDetectionExecutionsResponse_httpStatus :: Lens.Lens' DescribeAnomalyDetectionExecutionsResponse Prelude.Int
describeAnomalyDetectionExecutionsResponse_httpStatus = Lens.lens (\DescribeAnomalyDetectionExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAnomalyDetectionExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeAnomalyDetectionExecutionsResponse)

instance
  Prelude.NFData
    DescribeAnomalyDetectionExecutionsResponse
  where
  rnf DescribeAnomalyDetectionExecutionsResponse' {..} =
    Prelude.rnf executionList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
