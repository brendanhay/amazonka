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
-- Module      : Amazonka.CodeGuruProfiler.BatchGetFrameMetricData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the time series of values for a requested list of frame metrics
-- from a time period.
module Amazonka.CodeGuruProfiler.BatchGetFrameMetricData
  ( -- * Creating a Request
    BatchGetFrameMetricData (..),
    newBatchGetFrameMetricData,

    -- * Request Lenses
    batchGetFrameMetricData_period,
    batchGetFrameMetricData_endTime,
    batchGetFrameMetricData_frameMetrics,
    batchGetFrameMetricData_targetResolution,
    batchGetFrameMetricData_startTime,
    batchGetFrameMetricData_profilingGroupName,

    -- * Destructuring the Response
    BatchGetFrameMetricDataResponse (..),
    newBatchGetFrameMetricDataResponse,

    -- * Response Lenses
    batchGetFrameMetricDataResponse_httpStatus,
    batchGetFrameMetricDataResponse_endTime,
    batchGetFrameMetricDataResponse_endTimes,
    batchGetFrameMetricDataResponse_frameMetricData,
    batchGetFrameMetricDataResponse_resolution,
    batchGetFrameMetricDataResponse_startTime,
    batchGetFrameMetricDataResponse_unprocessedEndTimes,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the BatchGetFrameMetricDataRequest.
--
-- /See:/ 'newBatchGetFrameMetricData' smart constructor.
data BatchGetFrameMetricData = BatchGetFrameMetricData'
  { -- | The duration of the frame metrics used to return the time series values.
    -- Specify using the ISO 8601 format. The maximum period duration is one
    -- day (@PT24H@ or @P1D@).
    period :: Prelude.Maybe Prelude.Text,
    -- | The end time of the time period for the returned time series values.
    -- This is specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The details of the metrics that are used to request a time series of
    -- values. The metric includes the name of the frame, the aggregation type
    -- to calculate the metric value for the frame, and the thread states to
    -- use to get the count for the metric value of the frame.
    frameMetrics :: Prelude.Maybe [FrameMetric],
    -- | The requested resolution of time steps for the returned time series of
    -- values. If the requested target resolution is not available due to data
    -- not being retained we provide a best effort result by falling back to
    -- the most granular available resolution after the target resolution.
    -- There are 3 valid values.
    --
    -- -   @P1D@ — 1 day
    --
    -- -   @PT1H@ — 1 hour
    --
    -- -   @PT5M@ — 5 minutes
    targetResolution :: Prelude.Maybe AggregationPeriod,
    -- | The start time of the time period for the frame metrics used to return
    -- the time series values. This is specified using the ISO 8601 format. For
    -- example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1,
    -- 2020 1:15:02 PM UTC.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the profiling group associated with the the frame metrics
    -- used to return the time series values.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetFrameMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'period', 'batchGetFrameMetricData_period' - The duration of the frame metrics used to return the time series values.
-- Specify using the ISO 8601 format. The maximum period duration is one
-- day (@PT24H@ or @P1D@).
--
-- 'endTime', 'batchGetFrameMetricData_endTime' - The end time of the time period for the returned time series values.
-- This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
--
-- 'frameMetrics', 'batchGetFrameMetricData_frameMetrics' - The details of the metrics that are used to request a time series of
-- values. The metric includes the name of the frame, the aggregation type
-- to calculate the metric value for the frame, and the thread states to
-- use to get the count for the metric value of the frame.
--
-- 'targetResolution', 'batchGetFrameMetricData_targetResolution' - The requested resolution of time steps for the returned time series of
-- values. If the requested target resolution is not available due to data
-- not being retained we provide a best effort result by falling back to
-- the most granular available resolution after the target resolution.
-- There are 3 valid values.
--
-- -   @P1D@ — 1 day
--
-- -   @PT1H@ — 1 hour
--
-- -   @PT5M@ — 5 minutes
--
-- 'startTime', 'batchGetFrameMetricData_startTime' - The start time of the time period for the frame metrics used to return
-- the time series values. This is specified using the ISO 8601 format. For
-- example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1,
-- 2020 1:15:02 PM UTC.
--
-- 'profilingGroupName', 'batchGetFrameMetricData_profilingGroupName' - The name of the profiling group associated with the the frame metrics
-- used to return the time series values.
newBatchGetFrameMetricData ::
  -- | 'profilingGroupName'
  Prelude.Text ->
  BatchGetFrameMetricData
newBatchGetFrameMetricData pProfilingGroupName_ =
  BatchGetFrameMetricData'
    { period = Prelude.Nothing,
      endTime = Prelude.Nothing,
      frameMetrics = Prelude.Nothing,
      targetResolution = Prelude.Nothing,
      startTime = Prelude.Nothing,
      profilingGroupName = pProfilingGroupName_
    }

-- | The duration of the frame metrics used to return the time series values.
-- Specify using the ISO 8601 format. The maximum period duration is one
-- day (@PT24H@ or @P1D@).
batchGetFrameMetricData_period :: Lens.Lens' BatchGetFrameMetricData (Prelude.Maybe Prelude.Text)
batchGetFrameMetricData_period = Lens.lens (\BatchGetFrameMetricData' {period} -> period) (\s@BatchGetFrameMetricData' {} a -> s {period = a} :: BatchGetFrameMetricData)

-- | The end time of the time period for the returned time series values.
-- This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
batchGetFrameMetricData_endTime :: Lens.Lens' BatchGetFrameMetricData (Prelude.Maybe Prelude.UTCTime)
batchGetFrameMetricData_endTime = Lens.lens (\BatchGetFrameMetricData' {endTime} -> endTime) (\s@BatchGetFrameMetricData' {} a -> s {endTime = a} :: BatchGetFrameMetricData) Prelude.. Lens.mapping Core._Time

-- | The details of the metrics that are used to request a time series of
-- values. The metric includes the name of the frame, the aggregation type
-- to calculate the metric value for the frame, and the thread states to
-- use to get the count for the metric value of the frame.
batchGetFrameMetricData_frameMetrics :: Lens.Lens' BatchGetFrameMetricData (Prelude.Maybe [FrameMetric])
batchGetFrameMetricData_frameMetrics = Lens.lens (\BatchGetFrameMetricData' {frameMetrics} -> frameMetrics) (\s@BatchGetFrameMetricData' {} a -> s {frameMetrics = a} :: BatchGetFrameMetricData) Prelude.. Lens.mapping Lens.coerced

-- | The requested resolution of time steps for the returned time series of
-- values. If the requested target resolution is not available due to data
-- not being retained we provide a best effort result by falling back to
-- the most granular available resolution after the target resolution.
-- There are 3 valid values.
--
-- -   @P1D@ — 1 day
--
-- -   @PT1H@ — 1 hour
--
-- -   @PT5M@ — 5 minutes
batchGetFrameMetricData_targetResolution :: Lens.Lens' BatchGetFrameMetricData (Prelude.Maybe AggregationPeriod)
batchGetFrameMetricData_targetResolution = Lens.lens (\BatchGetFrameMetricData' {targetResolution} -> targetResolution) (\s@BatchGetFrameMetricData' {} a -> s {targetResolution = a} :: BatchGetFrameMetricData)

-- | The start time of the time period for the frame metrics used to return
-- the time series values. This is specified using the ISO 8601 format. For
-- example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1,
-- 2020 1:15:02 PM UTC.
batchGetFrameMetricData_startTime :: Lens.Lens' BatchGetFrameMetricData (Prelude.Maybe Prelude.UTCTime)
batchGetFrameMetricData_startTime = Lens.lens (\BatchGetFrameMetricData' {startTime} -> startTime) (\s@BatchGetFrameMetricData' {} a -> s {startTime = a} :: BatchGetFrameMetricData) Prelude.. Lens.mapping Core._Time

-- | The name of the profiling group associated with the the frame metrics
-- used to return the time series values.
batchGetFrameMetricData_profilingGroupName :: Lens.Lens' BatchGetFrameMetricData Prelude.Text
batchGetFrameMetricData_profilingGroupName = Lens.lens (\BatchGetFrameMetricData' {profilingGroupName} -> profilingGroupName) (\s@BatchGetFrameMetricData' {} a -> s {profilingGroupName = a} :: BatchGetFrameMetricData)

instance Core.AWSRequest BatchGetFrameMetricData where
  type
    AWSResponse BatchGetFrameMetricData =
      BatchGetFrameMetricDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetFrameMetricDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "endTime")
            Prelude.<*> (x Core..?> "endTimes" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "frameMetricData"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..:> "resolution")
            Prelude.<*> (x Core..:> "startTime")
            Prelude.<*> ( x Core..?> "unprocessedEndTimes"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchGetFrameMetricData where
  hashWithSalt _salt BatchGetFrameMetricData' {..} =
    _salt `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` frameMetrics
      `Prelude.hashWithSalt` targetResolution
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData BatchGetFrameMetricData where
  rnf BatchGetFrameMetricData' {..} =
    Prelude.rnf period
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf frameMetrics
      `Prelude.seq` Prelude.rnf targetResolution
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf profilingGroupName

instance Core.ToHeaders BatchGetFrameMetricData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetFrameMetricData where
  toJSON BatchGetFrameMetricData' {..} =
    Core.object
      ( Prelude.catMaybes
          [("frameMetrics" Core..=) Prelude.<$> frameMetrics]
      )

instance Core.ToPath BatchGetFrameMetricData where
  toPath BatchGetFrameMetricData' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Core.toBS profilingGroupName,
        "/frames/-/metrics"
      ]

instance Core.ToQuery BatchGetFrameMetricData where
  toQuery BatchGetFrameMetricData' {..} =
    Prelude.mconcat
      [ "period" Core.=: period,
        "endTime" Core.=: endTime,
        "targetResolution" Core.=: targetResolution,
        "startTime" Core.=: startTime
      ]

-- | The structure representing the BatchGetFrameMetricDataResponse.
--
-- /See:/ 'newBatchGetFrameMetricDataResponse' smart constructor.
data BatchGetFrameMetricDataResponse = BatchGetFrameMetricDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The end time of the time period for the returned time series values.
    -- This is specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    endTime :: Core.POSIX,
    -- | List of instances, or time steps, in the time series. For example, if
    -- the @period@ is one day (@PT24H)@), and the @resolution@ is five minutes
    -- (@PT5M@), then there are 288 @endTimes@ in the list that are each five
    -- minutes appart.
    endTimes :: [TimestampStructure],
    -- | Details of the metrics to request a time series of values. The metric
    -- includes the name of the frame, the aggregation type to calculate the
    -- metric value for the frame, and the thread states to use to get the
    -- count for the metric value of the frame.
    frameMetricData :: [FrameMetricDatum],
    -- | Resolution or granularity of the profile data used to generate the time
    -- series. This is the value used to jump through time steps in a time
    -- series. There are 3 valid values.
    --
    -- -   @P1D@ — 1 day
    --
    -- -   @PT1H@ — 1 hour
    --
    -- -   @PT5M@ — 5 minutes
    resolution :: AggregationPeriod,
    -- | The start time of the time period for the returned time series values.
    -- This is specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    startTime :: Core.POSIX,
    -- | List of instances which remained unprocessed. This will create a missing
    -- time step in the list of end times.
    unprocessedEndTimes :: Prelude.HashMap Prelude.Text [TimestampStructure]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetFrameMetricDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchGetFrameMetricDataResponse_httpStatus' - The response's http status code.
--
-- 'endTime', 'batchGetFrameMetricDataResponse_endTime' - The end time of the time period for the returned time series values.
-- This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
--
-- 'endTimes', 'batchGetFrameMetricDataResponse_endTimes' - List of instances, or time steps, in the time series. For example, if
-- the @period@ is one day (@PT24H)@), and the @resolution@ is five minutes
-- (@PT5M@), then there are 288 @endTimes@ in the list that are each five
-- minutes appart.
--
-- 'frameMetricData', 'batchGetFrameMetricDataResponse_frameMetricData' - Details of the metrics to request a time series of values. The metric
-- includes the name of the frame, the aggregation type to calculate the
-- metric value for the frame, and the thread states to use to get the
-- count for the metric value of the frame.
--
-- 'resolution', 'batchGetFrameMetricDataResponse_resolution' - Resolution or granularity of the profile data used to generate the time
-- series. This is the value used to jump through time steps in a time
-- series. There are 3 valid values.
--
-- -   @P1D@ — 1 day
--
-- -   @PT1H@ — 1 hour
--
-- -   @PT5M@ — 5 minutes
--
-- 'startTime', 'batchGetFrameMetricDataResponse_startTime' - The start time of the time period for the returned time series values.
-- This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
--
-- 'unprocessedEndTimes', 'batchGetFrameMetricDataResponse_unprocessedEndTimes' - List of instances which remained unprocessed. This will create a missing
-- time step in the list of end times.
newBatchGetFrameMetricDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'resolution'
  AggregationPeriod ->
  -- | 'startTime'
  Prelude.UTCTime ->
  BatchGetFrameMetricDataResponse
newBatchGetFrameMetricDataResponse
  pHttpStatus_
  pEndTime_
  pResolution_
  pStartTime_ =
    BatchGetFrameMetricDataResponse'
      { httpStatus =
          pHttpStatus_,
        endTime = Core._Time Lens.# pEndTime_,
        endTimes = Prelude.mempty,
        frameMetricData = Prelude.mempty,
        resolution = pResolution_,
        startTime = Core._Time Lens.# pStartTime_,
        unprocessedEndTimes = Prelude.mempty
      }

-- | The response's http status code.
batchGetFrameMetricDataResponse_httpStatus :: Lens.Lens' BatchGetFrameMetricDataResponse Prelude.Int
batchGetFrameMetricDataResponse_httpStatus = Lens.lens (\BatchGetFrameMetricDataResponse' {httpStatus} -> httpStatus) (\s@BatchGetFrameMetricDataResponse' {} a -> s {httpStatus = a} :: BatchGetFrameMetricDataResponse)

-- | The end time of the time period for the returned time series values.
-- This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
batchGetFrameMetricDataResponse_endTime :: Lens.Lens' BatchGetFrameMetricDataResponse Prelude.UTCTime
batchGetFrameMetricDataResponse_endTime = Lens.lens (\BatchGetFrameMetricDataResponse' {endTime} -> endTime) (\s@BatchGetFrameMetricDataResponse' {} a -> s {endTime = a} :: BatchGetFrameMetricDataResponse) Prelude.. Core._Time

-- | List of instances, or time steps, in the time series. For example, if
-- the @period@ is one day (@PT24H)@), and the @resolution@ is five minutes
-- (@PT5M@), then there are 288 @endTimes@ in the list that are each five
-- minutes appart.
batchGetFrameMetricDataResponse_endTimes :: Lens.Lens' BatchGetFrameMetricDataResponse [TimestampStructure]
batchGetFrameMetricDataResponse_endTimes = Lens.lens (\BatchGetFrameMetricDataResponse' {endTimes} -> endTimes) (\s@BatchGetFrameMetricDataResponse' {} a -> s {endTimes = a} :: BatchGetFrameMetricDataResponse) Prelude.. Lens.coerced

-- | Details of the metrics to request a time series of values. The metric
-- includes the name of the frame, the aggregation type to calculate the
-- metric value for the frame, and the thread states to use to get the
-- count for the metric value of the frame.
batchGetFrameMetricDataResponse_frameMetricData :: Lens.Lens' BatchGetFrameMetricDataResponse [FrameMetricDatum]
batchGetFrameMetricDataResponse_frameMetricData = Lens.lens (\BatchGetFrameMetricDataResponse' {frameMetricData} -> frameMetricData) (\s@BatchGetFrameMetricDataResponse' {} a -> s {frameMetricData = a} :: BatchGetFrameMetricDataResponse) Prelude.. Lens.coerced

-- | Resolution or granularity of the profile data used to generate the time
-- series. This is the value used to jump through time steps in a time
-- series. There are 3 valid values.
--
-- -   @P1D@ — 1 day
--
-- -   @PT1H@ — 1 hour
--
-- -   @PT5M@ — 5 minutes
batchGetFrameMetricDataResponse_resolution :: Lens.Lens' BatchGetFrameMetricDataResponse AggregationPeriod
batchGetFrameMetricDataResponse_resolution = Lens.lens (\BatchGetFrameMetricDataResponse' {resolution} -> resolution) (\s@BatchGetFrameMetricDataResponse' {} a -> s {resolution = a} :: BatchGetFrameMetricDataResponse)

-- | The start time of the time period for the returned time series values.
-- This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
batchGetFrameMetricDataResponse_startTime :: Lens.Lens' BatchGetFrameMetricDataResponse Prelude.UTCTime
batchGetFrameMetricDataResponse_startTime = Lens.lens (\BatchGetFrameMetricDataResponse' {startTime} -> startTime) (\s@BatchGetFrameMetricDataResponse' {} a -> s {startTime = a} :: BatchGetFrameMetricDataResponse) Prelude.. Core._Time

-- | List of instances which remained unprocessed. This will create a missing
-- time step in the list of end times.
batchGetFrameMetricDataResponse_unprocessedEndTimes :: Lens.Lens' BatchGetFrameMetricDataResponse (Prelude.HashMap Prelude.Text [TimestampStructure])
batchGetFrameMetricDataResponse_unprocessedEndTimes = Lens.lens (\BatchGetFrameMetricDataResponse' {unprocessedEndTimes} -> unprocessedEndTimes) (\s@BatchGetFrameMetricDataResponse' {} a -> s {unprocessedEndTimes = a} :: BatchGetFrameMetricDataResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchGetFrameMetricDataResponse
  where
  rnf BatchGetFrameMetricDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf endTimes
      `Prelude.seq` Prelude.rnf frameMetricData
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf unprocessedEndTimes
