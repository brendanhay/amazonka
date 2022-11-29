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
-- Module      : Amazonka.LookoutMetrics.DescribeMetricSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a dataset.
--
-- Amazon Lookout for Metrics API actions are eventually consistent. If you
-- do a read operation on a resource immediately after creating or
-- modifying it, use retries to allow time for the write operation to
-- complete.
module Amazonka.LookoutMetrics.DescribeMetricSet
  ( -- * Creating a Request
    DescribeMetricSet (..),
    newDescribeMetricSet,

    -- * Request Lenses
    describeMetricSet_metricSetArn,

    -- * Destructuring the Response
    DescribeMetricSetResponse (..),
    newDescribeMetricSetResponse,

    -- * Response Lenses
    describeMetricSetResponse_lastModificationTime,
    describeMetricSetResponse_timestampColumn,
    describeMetricSetResponse_metricSetDescription,
    describeMetricSetResponse_anomalyDetectorArn,
    describeMetricSetResponse_metricSetName,
    describeMetricSetResponse_timezone,
    describeMetricSetResponse_offset,
    describeMetricSetResponse_metricSource,
    describeMetricSetResponse_dimensionFilterList,
    describeMetricSetResponse_dimensionList,
    describeMetricSetResponse_metricSetArn,
    describeMetricSetResponse_metricSetFrequency,
    describeMetricSetResponse_creationTime,
    describeMetricSetResponse_metricList,
    describeMetricSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMetricSet' smart constructor.
data DescribeMetricSet = DescribeMetricSet'
  { -- | The ARN of the dataset.
    metricSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMetricSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricSetArn', 'describeMetricSet_metricSetArn' - The ARN of the dataset.
newDescribeMetricSet ::
  -- | 'metricSetArn'
  Prelude.Text ->
  DescribeMetricSet
newDescribeMetricSet pMetricSetArn_ =
  DescribeMetricSet' {metricSetArn = pMetricSetArn_}

-- | The ARN of the dataset.
describeMetricSet_metricSetArn :: Lens.Lens' DescribeMetricSet Prelude.Text
describeMetricSet_metricSetArn = Lens.lens (\DescribeMetricSet' {metricSetArn} -> metricSetArn) (\s@DescribeMetricSet' {} a -> s {metricSetArn = a} :: DescribeMetricSet)

instance Core.AWSRequest DescribeMetricSet where
  type
    AWSResponse DescribeMetricSet =
      DescribeMetricSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMetricSetResponse'
            Prelude.<$> (x Core..?> "LastModificationTime")
            Prelude.<*> (x Core..?> "TimestampColumn")
            Prelude.<*> (x Core..?> "MetricSetDescription")
            Prelude.<*> (x Core..?> "AnomalyDetectorArn")
            Prelude.<*> (x Core..?> "MetricSetName")
            Prelude.<*> (x Core..?> "Timezone")
            Prelude.<*> (x Core..?> "Offset")
            Prelude.<*> (x Core..?> "MetricSource")
            Prelude.<*> ( x Core..?> "DimensionFilterList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "DimensionList")
            Prelude.<*> (x Core..?> "MetricSetArn")
            Prelude.<*> (x Core..?> "MetricSetFrequency")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "MetricList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMetricSet where
  hashWithSalt _salt DescribeMetricSet' {..} =
    _salt `Prelude.hashWithSalt` metricSetArn

instance Prelude.NFData DescribeMetricSet where
  rnf DescribeMetricSet' {..} = Prelude.rnf metricSetArn

instance Core.ToHeaders DescribeMetricSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeMetricSet where
  toJSON DescribeMetricSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("MetricSetArn" Core..= metricSetArn)]
      )

instance Core.ToPath DescribeMetricSet where
  toPath = Prelude.const "/DescribeMetricSet"

instance Core.ToQuery DescribeMetricSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMetricSetResponse' smart constructor.
data DescribeMetricSetResponse = DescribeMetricSetResponse'
  { -- | The time at which the dataset was last modified.
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | Contains information about the column used for tracking time in your
    -- source data.
    timestampColumn :: Prelude.Maybe TimestampColumn,
    -- | The dataset\'s description.
    metricSetDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the detector that contains the dataset.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset.
    metricSetName :: Prelude.Maybe Prelude.Text,
    -- | The time zone in which the dataset\'s data was recorded.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | After an interval ends, the amount of seconds that the detector waits
    -- before importing data. Offset is only supported for S3, Redshift, Athena
    -- and datasources.
    offset :: Prelude.Maybe Prelude.Natural,
    -- | Contains information about the dataset\'s source data.
    metricSource :: Prelude.Maybe MetricSource,
    -- | The dimensions and their values that were used to filter the dataset.
    dimensionFilterList :: Prelude.Maybe [MetricSetDimensionFilter],
    -- | A list of the dimensions chosen for analysis.
    dimensionList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ARN of the dataset.
    metricSetArn :: Prelude.Maybe Prelude.Text,
    -- | The interval at which the data will be analyzed for anomalies.
    metricSetFrequency :: Prelude.Maybe Frequency,
    -- | The time at which the dataset was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A list of the metrics defined by the dataset.
    metricList :: Prelude.Maybe (Prelude.NonEmpty Metric),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMetricSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'describeMetricSetResponse_lastModificationTime' - The time at which the dataset was last modified.
--
-- 'timestampColumn', 'describeMetricSetResponse_timestampColumn' - Contains information about the column used for tracking time in your
-- source data.
--
-- 'metricSetDescription', 'describeMetricSetResponse_metricSetDescription' - The dataset\'s description.
--
-- 'anomalyDetectorArn', 'describeMetricSetResponse_anomalyDetectorArn' - The ARN of the detector that contains the dataset.
--
-- 'metricSetName', 'describeMetricSetResponse_metricSetName' - The name of the dataset.
--
-- 'timezone', 'describeMetricSetResponse_timezone' - The time zone in which the dataset\'s data was recorded.
--
-- 'offset', 'describeMetricSetResponse_offset' - After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3, Redshift, Athena
-- and datasources.
--
-- 'metricSource', 'describeMetricSetResponse_metricSource' - Contains information about the dataset\'s source data.
--
-- 'dimensionFilterList', 'describeMetricSetResponse_dimensionFilterList' - The dimensions and their values that were used to filter the dataset.
--
-- 'dimensionList', 'describeMetricSetResponse_dimensionList' - A list of the dimensions chosen for analysis.
--
-- 'metricSetArn', 'describeMetricSetResponse_metricSetArn' - The ARN of the dataset.
--
-- 'metricSetFrequency', 'describeMetricSetResponse_metricSetFrequency' - The interval at which the data will be analyzed for anomalies.
--
-- 'creationTime', 'describeMetricSetResponse_creationTime' - The time at which the dataset was created.
--
-- 'metricList', 'describeMetricSetResponse_metricList' - A list of the metrics defined by the dataset.
--
-- 'httpStatus', 'describeMetricSetResponse_httpStatus' - The response's http status code.
newDescribeMetricSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMetricSetResponse
newDescribeMetricSetResponse pHttpStatus_ =
  DescribeMetricSetResponse'
    { lastModificationTime =
        Prelude.Nothing,
      timestampColumn = Prelude.Nothing,
      metricSetDescription = Prelude.Nothing,
      anomalyDetectorArn = Prelude.Nothing,
      metricSetName = Prelude.Nothing,
      timezone = Prelude.Nothing,
      offset = Prelude.Nothing,
      metricSource = Prelude.Nothing,
      dimensionFilterList = Prelude.Nothing,
      dimensionList = Prelude.Nothing,
      metricSetArn = Prelude.Nothing,
      metricSetFrequency = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      metricList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the dataset was last modified.
describeMetricSetResponse_lastModificationTime :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Prelude.UTCTime)
describeMetricSetResponse_lastModificationTime = Lens.lens (\DescribeMetricSetResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeMetricSetResponse' {} a -> s {lastModificationTime = a} :: DescribeMetricSetResponse) Prelude.. Lens.mapping Core._Time

-- | Contains information about the column used for tracking time in your
-- source data.
describeMetricSetResponse_timestampColumn :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe TimestampColumn)
describeMetricSetResponse_timestampColumn = Lens.lens (\DescribeMetricSetResponse' {timestampColumn} -> timestampColumn) (\s@DescribeMetricSetResponse' {} a -> s {timestampColumn = a} :: DescribeMetricSetResponse)

-- | The dataset\'s description.
describeMetricSetResponse_metricSetDescription :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Prelude.Text)
describeMetricSetResponse_metricSetDescription = Lens.lens (\DescribeMetricSetResponse' {metricSetDescription} -> metricSetDescription) (\s@DescribeMetricSetResponse' {} a -> s {metricSetDescription = a} :: DescribeMetricSetResponse)

-- | The ARN of the detector that contains the dataset.
describeMetricSetResponse_anomalyDetectorArn :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Prelude.Text)
describeMetricSetResponse_anomalyDetectorArn = Lens.lens (\DescribeMetricSetResponse' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@DescribeMetricSetResponse' {} a -> s {anomalyDetectorArn = a} :: DescribeMetricSetResponse)

-- | The name of the dataset.
describeMetricSetResponse_metricSetName :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Prelude.Text)
describeMetricSetResponse_metricSetName = Lens.lens (\DescribeMetricSetResponse' {metricSetName} -> metricSetName) (\s@DescribeMetricSetResponse' {} a -> s {metricSetName = a} :: DescribeMetricSetResponse)

-- | The time zone in which the dataset\'s data was recorded.
describeMetricSetResponse_timezone :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Prelude.Text)
describeMetricSetResponse_timezone = Lens.lens (\DescribeMetricSetResponse' {timezone} -> timezone) (\s@DescribeMetricSetResponse' {} a -> s {timezone = a} :: DescribeMetricSetResponse)

-- | After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3, Redshift, Athena
-- and datasources.
describeMetricSetResponse_offset :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Prelude.Natural)
describeMetricSetResponse_offset = Lens.lens (\DescribeMetricSetResponse' {offset} -> offset) (\s@DescribeMetricSetResponse' {} a -> s {offset = a} :: DescribeMetricSetResponse)

-- | Contains information about the dataset\'s source data.
describeMetricSetResponse_metricSource :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe MetricSource)
describeMetricSetResponse_metricSource = Lens.lens (\DescribeMetricSetResponse' {metricSource} -> metricSource) (\s@DescribeMetricSetResponse' {} a -> s {metricSource = a} :: DescribeMetricSetResponse)

-- | The dimensions and their values that were used to filter the dataset.
describeMetricSetResponse_dimensionFilterList :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe [MetricSetDimensionFilter])
describeMetricSetResponse_dimensionFilterList = Lens.lens (\DescribeMetricSetResponse' {dimensionFilterList} -> dimensionFilterList) (\s@DescribeMetricSetResponse' {} a -> s {dimensionFilterList = a} :: DescribeMetricSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the dimensions chosen for analysis.
describeMetricSetResponse_dimensionList :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeMetricSetResponse_dimensionList = Lens.lens (\DescribeMetricSetResponse' {dimensionList} -> dimensionList) (\s@DescribeMetricSetResponse' {} a -> s {dimensionList = a} :: DescribeMetricSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the dataset.
describeMetricSetResponse_metricSetArn :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Prelude.Text)
describeMetricSetResponse_metricSetArn = Lens.lens (\DescribeMetricSetResponse' {metricSetArn} -> metricSetArn) (\s@DescribeMetricSetResponse' {} a -> s {metricSetArn = a} :: DescribeMetricSetResponse)

-- | The interval at which the data will be analyzed for anomalies.
describeMetricSetResponse_metricSetFrequency :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Frequency)
describeMetricSetResponse_metricSetFrequency = Lens.lens (\DescribeMetricSetResponse' {metricSetFrequency} -> metricSetFrequency) (\s@DescribeMetricSetResponse' {} a -> s {metricSetFrequency = a} :: DescribeMetricSetResponse)

-- | The time at which the dataset was created.
describeMetricSetResponse_creationTime :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe Prelude.UTCTime)
describeMetricSetResponse_creationTime = Lens.lens (\DescribeMetricSetResponse' {creationTime} -> creationTime) (\s@DescribeMetricSetResponse' {} a -> s {creationTime = a} :: DescribeMetricSetResponse) Prelude.. Lens.mapping Core._Time

-- | A list of the metrics defined by the dataset.
describeMetricSetResponse_metricList :: Lens.Lens' DescribeMetricSetResponse (Prelude.Maybe (Prelude.NonEmpty Metric))
describeMetricSetResponse_metricList = Lens.lens (\DescribeMetricSetResponse' {metricList} -> metricList) (\s@DescribeMetricSetResponse' {} a -> s {metricList = a} :: DescribeMetricSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMetricSetResponse_httpStatus :: Lens.Lens' DescribeMetricSetResponse Prelude.Int
describeMetricSetResponse_httpStatus = Lens.lens (\DescribeMetricSetResponse' {httpStatus} -> httpStatus) (\s@DescribeMetricSetResponse' {} a -> s {httpStatus = a} :: DescribeMetricSetResponse)

instance Prelude.NFData DescribeMetricSetResponse where
  rnf DescribeMetricSetResponse' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf timestampColumn
      `Prelude.seq` Prelude.rnf metricSetDescription
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf metricSetName
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf offset
      `Prelude.seq` Prelude.rnf metricSource
      `Prelude.seq` Prelude.rnf dimensionFilterList
      `Prelude.seq` Prelude.rnf dimensionList
      `Prelude.seq` Prelude.rnf metricSetArn
      `Prelude.seq` Prelude.rnf metricSetFrequency
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf metricList
      `Prelude.seq` Prelude.rnf httpStatus
