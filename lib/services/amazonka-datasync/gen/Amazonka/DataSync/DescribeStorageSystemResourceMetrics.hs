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
-- Module      : Amazonka.DataSync.DescribeStorageSystemResourceMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information, including performance data and capacity usage,
-- which DataSync Discovery collects about a specific resource in
-- your-premises storage system.
--
-- This operation returns paginated results.
module Amazonka.DataSync.DescribeStorageSystemResourceMetrics
  ( -- * Creating a Request
    DescribeStorageSystemResourceMetrics (..),
    newDescribeStorageSystemResourceMetrics,

    -- * Request Lenses
    describeStorageSystemResourceMetrics_endTime,
    describeStorageSystemResourceMetrics_maxResults,
    describeStorageSystemResourceMetrics_nextToken,
    describeStorageSystemResourceMetrics_startTime,
    describeStorageSystemResourceMetrics_discoveryJobArn,
    describeStorageSystemResourceMetrics_resourceType,
    describeStorageSystemResourceMetrics_resourceId,

    -- * Destructuring the Response
    DescribeStorageSystemResourceMetricsResponse (..),
    newDescribeStorageSystemResourceMetricsResponse,

    -- * Response Lenses
    describeStorageSystemResourceMetricsResponse_metrics,
    describeStorageSystemResourceMetricsResponse_nextToken,
    describeStorageSystemResourceMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStorageSystemResourceMetrics' smart constructor.
data DescribeStorageSystemResourceMetrics = DescribeStorageSystemResourceMetrics'
  { -- | Specifies a time within the total duration that the discovery job ran.
    -- To see information gathered during a certain time frame, use this
    -- parameter with @StartTime@.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies how many results that you want in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies an opaque string that indicates the position to begin the next
    -- list of results in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a time within the total duration that the discovery job ran.
    -- To see information gathered during a certain time frame, use this
    -- parameter with @EndTime@.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the Amazon Resource Name (ARN) of the discovery job that
    -- collects information about your on-premises storage system.
    discoveryJobArn :: Prelude.Text,
    -- | Specifies the kind of storage system resource that you want information
    -- about.
    resourceType :: DiscoveryResourceType,
    -- | Specifies the universally unique identifier (UUID) of the storage system
    -- resource that you want information about.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageSystemResourceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'describeStorageSystemResourceMetrics_endTime' - Specifies a time within the total duration that the discovery job ran.
-- To see information gathered during a certain time frame, use this
-- parameter with @StartTime@.
--
-- 'maxResults', 'describeStorageSystemResourceMetrics_maxResults' - Specifies how many results that you want in the response.
--
-- 'nextToken', 'describeStorageSystemResourceMetrics_nextToken' - Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
--
-- 'startTime', 'describeStorageSystemResourceMetrics_startTime' - Specifies a time within the total duration that the discovery job ran.
-- To see information gathered during a certain time frame, use this
-- parameter with @EndTime@.
--
-- 'discoveryJobArn', 'describeStorageSystemResourceMetrics_discoveryJobArn' - Specifies the Amazon Resource Name (ARN) of the discovery job that
-- collects information about your on-premises storage system.
--
-- 'resourceType', 'describeStorageSystemResourceMetrics_resourceType' - Specifies the kind of storage system resource that you want information
-- about.
--
-- 'resourceId', 'describeStorageSystemResourceMetrics_resourceId' - Specifies the universally unique identifier (UUID) of the storage system
-- resource that you want information about.
newDescribeStorageSystemResourceMetrics ::
  -- | 'discoveryJobArn'
  Prelude.Text ->
  -- | 'resourceType'
  DiscoveryResourceType ->
  -- | 'resourceId'
  Prelude.Text ->
  DescribeStorageSystemResourceMetrics
newDescribeStorageSystemResourceMetrics
  pDiscoveryJobArn_
  pResourceType_
  pResourceId_ =
    DescribeStorageSystemResourceMetrics'
      { endTime =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        startTime = Prelude.Nothing,
        discoveryJobArn = pDiscoveryJobArn_,
        resourceType = pResourceType_,
        resourceId = pResourceId_
      }

-- | Specifies a time within the total duration that the discovery job ran.
-- To see information gathered during a certain time frame, use this
-- parameter with @StartTime@.
describeStorageSystemResourceMetrics_endTime :: Lens.Lens' DescribeStorageSystemResourceMetrics (Prelude.Maybe Prelude.UTCTime)
describeStorageSystemResourceMetrics_endTime = Lens.lens (\DescribeStorageSystemResourceMetrics' {endTime} -> endTime) (\s@DescribeStorageSystemResourceMetrics' {} a -> s {endTime = a} :: DescribeStorageSystemResourceMetrics) Prelude.. Lens.mapping Data._Time

-- | Specifies how many results that you want in the response.
describeStorageSystemResourceMetrics_maxResults :: Lens.Lens' DescribeStorageSystemResourceMetrics (Prelude.Maybe Prelude.Natural)
describeStorageSystemResourceMetrics_maxResults = Lens.lens (\DescribeStorageSystemResourceMetrics' {maxResults} -> maxResults) (\s@DescribeStorageSystemResourceMetrics' {} a -> s {maxResults = a} :: DescribeStorageSystemResourceMetrics)

-- | Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
describeStorageSystemResourceMetrics_nextToken :: Lens.Lens' DescribeStorageSystemResourceMetrics (Prelude.Maybe Prelude.Text)
describeStorageSystemResourceMetrics_nextToken = Lens.lens (\DescribeStorageSystemResourceMetrics' {nextToken} -> nextToken) (\s@DescribeStorageSystemResourceMetrics' {} a -> s {nextToken = a} :: DescribeStorageSystemResourceMetrics)

-- | Specifies a time within the total duration that the discovery job ran.
-- To see information gathered during a certain time frame, use this
-- parameter with @EndTime@.
describeStorageSystemResourceMetrics_startTime :: Lens.Lens' DescribeStorageSystemResourceMetrics (Prelude.Maybe Prelude.UTCTime)
describeStorageSystemResourceMetrics_startTime = Lens.lens (\DescribeStorageSystemResourceMetrics' {startTime} -> startTime) (\s@DescribeStorageSystemResourceMetrics' {} a -> s {startTime = a} :: DescribeStorageSystemResourceMetrics) Prelude.. Lens.mapping Data._Time

-- | Specifies the Amazon Resource Name (ARN) of the discovery job that
-- collects information about your on-premises storage system.
describeStorageSystemResourceMetrics_discoveryJobArn :: Lens.Lens' DescribeStorageSystemResourceMetrics Prelude.Text
describeStorageSystemResourceMetrics_discoveryJobArn = Lens.lens (\DescribeStorageSystemResourceMetrics' {discoveryJobArn} -> discoveryJobArn) (\s@DescribeStorageSystemResourceMetrics' {} a -> s {discoveryJobArn = a} :: DescribeStorageSystemResourceMetrics)

-- | Specifies the kind of storage system resource that you want information
-- about.
describeStorageSystemResourceMetrics_resourceType :: Lens.Lens' DescribeStorageSystemResourceMetrics DiscoveryResourceType
describeStorageSystemResourceMetrics_resourceType = Lens.lens (\DescribeStorageSystemResourceMetrics' {resourceType} -> resourceType) (\s@DescribeStorageSystemResourceMetrics' {} a -> s {resourceType = a} :: DescribeStorageSystemResourceMetrics)

-- | Specifies the universally unique identifier (UUID) of the storage system
-- resource that you want information about.
describeStorageSystemResourceMetrics_resourceId :: Lens.Lens' DescribeStorageSystemResourceMetrics Prelude.Text
describeStorageSystemResourceMetrics_resourceId = Lens.lens (\DescribeStorageSystemResourceMetrics' {resourceId} -> resourceId) (\s@DescribeStorageSystemResourceMetrics' {} a -> s {resourceId = a} :: DescribeStorageSystemResourceMetrics)

instance
  Core.AWSPager
    DescribeStorageSystemResourceMetrics
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStorageSystemResourceMetricsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStorageSystemResourceMetricsResponse_metrics
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeStorageSystemResourceMetrics_nextToken
          Lens..~ rs
          Lens.^? describeStorageSystemResourceMetricsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeStorageSystemResourceMetrics
  where
  type
    AWSResponse DescribeStorageSystemResourceMetrics =
      DescribeStorageSystemResourceMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStorageSystemResourceMetricsResponse'
            Prelude.<$> (x Data..?> "Metrics" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeStorageSystemResourceMetrics
  where
  hashWithSalt
    _salt
    DescribeStorageSystemResourceMetrics' {..} =
      _salt
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` discoveryJobArn
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` resourceId

instance
  Prelude.NFData
    DescribeStorageSystemResourceMetrics
  where
  rnf DescribeStorageSystemResourceMetrics' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf discoveryJobArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceId

instance
  Data.ToHeaders
    DescribeStorageSystemResourceMetrics
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.DescribeStorageSystemResourceMetrics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeStorageSystemResourceMetrics
  where
  toJSON DescribeStorageSystemResourceMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartTime" Data..=) Prelude.<$> startTime,
            Prelude.Just
              ("DiscoveryJobArn" Data..= discoveryJobArn),
            Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance
  Data.ToPath
    DescribeStorageSystemResourceMetrics
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeStorageSystemResourceMetrics
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStorageSystemResourceMetricsResponse' smart constructor.
data DescribeStorageSystemResourceMetricsResponse = DescribeStorageSystemResourceMetricsResponse'
  { -- | The details that your discovery job collected about your storage system
    -- resource.
    metrics :: Prelude.Maybe [ResourceMetrics],
    -- | The opaque string that indicates the position to begin the next list of
    -- results in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageSystemResourceMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'describeStorageSystemResourceMetricsResponse_metrics' - The details that your discovery job collected about your storage system
-- resource.
--
-- 'nextToken', 'describeStorageSystemResourceMetricsResponse_nextToken' - The opaque string that indicates the position to begin the next list of
-- results in the response.
--
-- 'httpStatus', 'describeStorageSystemResourceMetricsResponse_httpStatus' - The response's http status code.
newDescribeStorageSystemResourceMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStorageSystemResourceMetricsResponse
newDescribeStorageSystemResourceMetricsResponse
  pHttpStatus_ =
    DescribeStorageSystemResourceMetricsResponse'
      { metrics =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The details that your discovery job collected about your storage system
-- resource.
describeStorageSystemResourceMetricsResponse_metrics :: Lens.Lens' DescribeStorageSystemResourceMetricsResponse (Prelude.Maybe [ResourceMetrics])
describeStorageSystemResourceMetricsResponse_metrics = Lens.lens (\DescribeStorageSystemResourceMetricsResponse' {metrics} -> metrics) (\s@DescribeStorageSystemResourceMetricsResponse' {} a -> s {metrics = a} :: DescribeStorageSystemResourceMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The opaque string that indicates the position to begin the next list of
-- results in the response.
describeStorageSystemResourceMetricsResponse_nextToken :: Lens.Lens' DescribeStorageSystemResourceMetricsResponse (Prelude.Maybe Prelude.Text)
describeStorageSystemResourceMetricsResponse_nextToken = Lens.lens (\DescribeStorageSystemResourceMetricsResponse' {nextToken} -> nextToken) (\s@DescribeStorageSystemResourceMetricsResponse' {} a -> s {nextToken = a} :: DescribeStorageSystemResourceMetricsResponse)

-- | The response's http status code.
describeStorageSystemResourceMetricsResponse_httpStatus :: Lens.Lens' DescribeStorageSystemResourceMetricsResponse Prelude.Int
describeStorageSystemResourceMetricsResponse_httpStatus = Lens.lens (\DescribeStorageSystemResourceMetricsResponse' {httpStatus} -> httpStatus) (\s@DescribeStorageSystemResourceMetricsResponse' {} a -> s {httpStatus = a} :: DescribeStorageSystemResourceMetricsResponse)

instance
  Prelude.NFData
    DescribeStorageSystemResourceMetricsResponse
  where
  rnf DescribeStorageSystemResourceMetricsResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
