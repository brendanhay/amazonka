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
-- Module      : Amazonka.LookoutMetrics.CreateMetricSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dataset.
module Amazonka.LookoutMetrics.CreateMetricSet
  ( -- * Creating a Request
    CreateMetricSet (..),
    newCreateMetricSet,

    -- * Request Lenses
    createMetricSet_dimensionList,
    createMetricSet_offset,
    createMetricSet_timestampColumn,
    createMetricSet_metricSetFrequency,
    createMetricSet_metricSetDescription,
    createMetricSet_timezone,
    createMetricSet_tags,
    createMetricSet_anomalyDetectorArn,
    createMetricSet_metricSetName,
    createMetricSet_metricList,
    createMetricSet_metricSource,

    -- * Destructuring the Response
    CreateMetricSetResponse (..),
    newCreateMetricSetResponse,

    -- * Response Lenses
    createMetricSetResponse_metricSetArn,
    createMetricSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMetricSet' smart constructor.
data CreateMetricSet = CreateMetricSet'
  { -- | A list of the fields you want to treat as dimensions.
    dimensionList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | After an interval ends, the amount of seconds that the detector waits
    -- before importing data. Offset is only supported for S3 and Redshift
    -- datasources.
    offset :: Prelude.Maybe Prelude.Natural,
    -- | Contains information about the column used for tracking time in your
    -- source data.
    timestampColumn :: Prelude.Maybe TimestampColumn,
    -- | The frequency with which the source data will be analyzed for anomalies.
    metricSetFrequency :: Prelude.Maybe Frequency,
    -- | A description of the dataset you are creating.
    metricSetDescription :: Prelude.Maybe Prelude.Text,
    -- | The time zone in which your source data was recorded.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | A list of
    -- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
    -- to apply to the dataset.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the anomaly detector that will use the dataset.
    anomalyDetectorArn :: Prelude.Text,
    -- | The name of the dataset.
    metricSetName :: Prelude.Text,
    -- | A list of metrics that the dataset will contain.
    metricList :: Prelude.NonEmpty Metric,
    -- | Contains information about how the source data should be interpreted.
    metricSource :: MetricSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMetricSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionList', 'createMetricSet_dimensionList' - A list of the fields you want to treat as dimensions.
--
-- 'offset', 'createMetricSet_offset' - After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3 and Redshift
-- datasources.
--
-- 'timestampColumn', 'createMetricSet_timestampColumn' - Contains information about the column used for tracking time in your
-- source data.
--
-- 'metricSetFrequency', 'createMetricSet_metricSetFrequency' - The frequency with which the source data will be analyzed for anomalies.
--
-- 'metricSetDescription', 'createMetricSet_metricSetDescription' - A description of the dataset you are creating.
--
-- 'timezone', 'createMetricSet_timezone' - The time zone in which your source data was recorded.
--
-- 'tags', 'createMetricSet_tags' - A list of
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
-- to apply to the dataset.
--
-- 'anomalyDetectorArn', 'createMetricSet_anomalyDetectorArn' - The ARN of the anomaly detector that will use the dataset.
--
-- 'metricSetName', 'createMetricSet_metricSetName' - The name of the dataset.
--
-- 'metricList', 'createMetricSet_metricList' - A list of metrics that the dataset will contain.
--
-- 'metricSource', 'createMetricSet_metricSource' - Contains information about how the source data should be interpreted.
newCreateMetricSet ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  -- | 'metricSetName'
  Prelude.Text ->
  -- | 'metricList'
  Prelude.NonEmpty Metric ->
  -- | 'metricSource'
  MetricSource ->
  CreateMetricSet
newCreateMetricSet
  pAnomalyDetectorArn_
  pMetricSetName_
  pMetricList_
  pMetricSource_ =
    CreateMetricSet'
      { dimensionList = Prelude.Nothing,
        offset = Prelude.Nothing,
        timestampColumn = Prelude.Nothing,
        metricSetFrequency = Prelude.Nothing,
        metricSetDescription = Prelude.Nothing,
        timezone = Prelude.Nothing,
        tags = Prelude.Nothing,
        anomalyDetectorArn = pAnomalyDetectorArn_,
        metricSetName = pMetricSetName_,
        metricList = Lens.coerced Lens.# pMetricList_,
        metricSource = pMetricSource_
      }

-- | A list of the fields you want to treat as dimensions.
createMetricSet_dimensionList :: Lens.Lens' CreateMetricSet (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMetricSet_dimensionList = Lens.lens (\CreateMetricSet' {dimensionList} -> dimensionList) (\s@CreateMetricSet' {} a -> s {dimensionList = a} :: CreateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3 and Redshift
-- datasources.
createMetricSet_offset :: Lens.Lens' CreateMetricSet (Prelude.Maybe Prelude.Natural)
createMetricSet_offset = Lens.lens (\CreateMetricSet' {offset} -> offset) (\s@CreateMetricSet' {} a -> s {offset = a} :: CreateMetricSet)

-- | Contains information about the column used for tracking time in your
-- source data.
createMetricSet_timestampColumn :: Lens.Lens' CreateMetricSet (Prelude.Maybe TimestampColumn)
createMetricSet_timestampColumn = Lens.lens (\CreateMetricSet' {timestampColumn} -> timestampColumn) (\s@CreateMetricSet' {} a -> s {timestampColumn = a} :: CreateMetricSet)

-- | The frequency with which the source data will be analyzed for anomalies.
createMetricSet_metricSetFrequency :: Lens.Lens' CreateMetricSet (Prelude.Maybe Frequency)
createMetricSet_metricSetFrequency = Lens.lens (\CreateMetricSet' {metricSetFrequency} -> metricSetFrequency) (\s@CreateMetricSet' {} a -> s {metricSetFrequency = a} :: CreateMetricSet)

-- | A description of the dataset you are creating.
createMetricSet_metricSetDescription :: Lens.Lens' CreateMetricSet (Prelude.Maybe Prelude.Text)
createMetricSet_metricSetDescription = Lens.lens (\CreateMetricSet' {metricSetDescription} -> metricSetDescription) (\s@CreateMetricSet' {} a -> s {metricSetDescription = a} :: CreateMetricSet)

-- | The time zone in which your source data was recorded.
createMetricSet_timezone :: Lens.Lens' CreateMetricSet (Prelude.Maybe Prelude.Text)
createMetricSet_timezone = Lens.lens (\CreateMetricSet' {timezone} -> timezone) (\s@CreateMetricSet' {} a -> s {timezone = a} :: CreateMetricSet)

-- | A list of
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
-- to apply to the dataset.
createMetricSet_tags :: Lens.Lens' CreateMetricSet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMetricSet_tags = Lens.lens (\CreateMetricSet' {tags} -> tags) (\s@CreateMetricSet' {} a -> s {tags = a} :: CreateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the anomaly detector that will use the dataset.
createMetricSet_anomalyDetectorArn :: Lens.Lens' CreateMetricSet Prelude.Text
createMetricSet_anomalyDetectorArn = Lens.lens (\CreateMetricSet' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@CreateMetricSet' {} a -> s {anomalyDetectorArn = a} :: CreateMetricSet)

-- | The name of the dataset.
createMetricSet_metricSetName :: Lens.Lens' CreateMetricSet Prelude.Text
createMetricSet_metricSetName = Lens.lens (\CreateMetricSet' {metricSetName} -> metricSetName) (\s@CreateMetricSet' {} a -> s {metricSetName = a} :: CreateMetricSet)

-- | A list of metrics that the dataset will contain.
createMetricSet_metricList :: Lens.Lens' CreateMetricSet (Prelude.NonEmpty Metric)
createMetricSet_metricList = Lens.lens (\CreateMetricSet' {metricList} -> metricList) (\s@CreateMetricSet' {} a -> s {metricList = a} :: CreateMetricSet) Prelude.. Lens.coerced

-- | Contains information about how the source data should be interpreted.
createMetricSet_metricSource :: Lens.Lens' CreateMetricSet MetricSource
createMetricSet_metricSource = Lens.lens (\CreateMetricSet' {metricSource} -> metricSource) (\s@CreateMetricSet' {} a -> s {metricSource = a} :: CreateMetricSet)

instance Core.AWSRequest CreateMetricSet where
  type
    AWSResponse CreateMetricSet =
      CreateMetricSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMetricSetResponse'
            Prelude.<$> (x Core..?> "MetricSetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMetricSet

instance Prelude.NFData CreateMetricSet

instance Core.ToHeaders CreateMetricSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateMetricSet where
  toJSON CreateMetricSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DimensionList" Core..=) Prelude.<$> dimensionList,
            ("Offset" Core..=) Prelude.<$> offset,
            ("TimestampColumn" Core..=)
              Prelude.<$> timestampColumn,
            ("MetricSetFrequency" Core..=)
              Prelude.<$> metricSetFrequency,
            ("MetricSetDescription" Core..=)
              Prelude.<$> metricSetDescription,
            ("Timezone" Core..=) Prelude.<$> timezone,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("AnomalyDetectorArn" Core..= anomalyDetectorArn),
            Prelude.Just ("MetricSetName" Core..= metricSetName),
            Prelude.Just ("MetricList" Core..= metricList),
            Prelude.Just ("MetricSource" Core..= metricSource)
          ]
      )

instance Core.ToPath CreateMetricSet where
  toPath = Prelude.const "/CreateMetricSet"

instance Core.ToQuery CreateMetricSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMetricSetResponse' smart constructor.
data CreateMetricSetResponse = CreateMetricSetResponse'
  { -- | The ARN of the dataset.
    metricSetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMetricSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricSetArn', 'createMetricSetResponse_metricSetArn' - The ARN of the dataset.
--
-- 'httpStatus', 'createMetricSetResponse_httpStatus' - The response's http status code.
newCreateMetricSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMetricSetResponse
newCreateMetricSetResponse pHttpStatus_ =
  CreateMetricSetResponse'
    { metricSetArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the dataset.
createMetricSetResponse_metricSetArn :: Lens.Lens' CreateMetricSetResponse (Prelude.Maybe Prelude.Text)
createMetricSetResponse_metricSetArn = Lens.lens (\CreateMetricSetResponse' {metricSetArn} -> metricSetArn) (\s@CreateMetricSetResponse' {} a -> s {metricSetArn = a} :: CreateMetricSetResponse)

-- | The response's http status code.
createMetricSetResponse_httpStatus :: Lens.Lens' CreateMetricSetResponse Prelude.Int
createMetricSetResponse_httpStatus = Lens.lens (\CreateMetricSetResponse' {httpStatus} -> httpStatus) (\s@CreateMetricSetResponse' {} a -> s {httpStatus = a} :: CreateMetricSetResponse)

instance Prelude.NFData CreateMetricSetResponse
