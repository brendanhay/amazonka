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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createMetricSet_tags,
    createMetricSet_timestampColumn,
    createMetricSet_metricSetDescription,
    createMetricSet_timezone,
    createMetricSet_offset,
    createMetricSet_dimensionFilterList,
    createMetricSet_dimensionList,
    createMetricSet_metricSetFrequency,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMetricSet' smart constructor.
data CreateMetricSet = CreateMetricSet'
  { -- | A list of
    -- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
    -- to apply to the dataset.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contains information about the column used for tracking time in your
    -- source data.
    timestampColumn :: Prelude.Maybe TimestampColumn,
    -- | A description of the dataset you are creating.
    metricSetDescription :: Prelude.Maybe Prelude.Text,
    -- | The time zone in which your source data was recorded.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | After an interval ends, the amount of seconds that the detector waits
    -- before importing data. Offset is only supported for S3, Redshift, Athena
    -- and datasources.
    offset :: Prelude.Maybe Prelude.Natural,
    -- | A list of filters that specify which data is kept for anomaly detection.
    dimensionFilterList :: Prelude.Maybe [MetricSetDimensionFilter],
    -- | A list of the fields you want to treat as dimensions.
    dimensionList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The frequency with which the source data will be analyzed for anomalies.
    metricSetFrequency :: Prelude.Maybe Frequency,
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
-- 'tags', 'createMetricSet_tags' - A list of
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
-- to apply to the dataset.
--
-- 'timestampColumn', 'createMetricSet_timestampColumn' - Contains information about the column used for tracking time in your
-- source data.
--
-- 'metricSetDescription', 'createMetricSet_metricSetDescription' - A description of the dataset you are creating.
--
-- 'timezone', 'createMetricSet_timezone' - The time zone in which your source data was recorded.
--
-- 'offset', 'createMetricSet_offset' - After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3, Redshift, Athena
-- and datasources.
--
-- 'dimensionFilterList', 'createMetricSet_dimensionFilterList' - A list of filters that specify which data is kept for anomaly detection.
--
-- 'dimensionList', 'createMetricSet_dimensionList' - A list of the fields you want to treat as dimensions.
--
-- 'metricSetFrequency', 'createMetricSet_metricSetFrequency' - The frequency with which the source data will be analyzed for anomalies.
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
      { tags = Prelude.Nothing,
        timestampColumn = Prelude.Nothing,
        metricSetDescription = Prelude.Nothing,
        timezone = Prelude.Nothing,
        offset = Prelude.Nothing,
        dimensionFilterList = Prelude.Nothing,
        dimensionList = Prelude.Nothing,
        metricSetFrequency = Prelude.Nothing,
        anomalyDetectorArn = pAnomalyDetectorArn_,
        metricSetName = pMetricSetName_,
        metricList = Lens.coerced Lens.# pMetricList_,
        metricSource = pMetricSource_
      }

-- | A list of
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
-- to apply to the dataset.
createMetricSet_tags :: Lens.Lens' CreateMetricSet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMetricSet_tags = Lens.lens (\CreateMetricSet' {tags} -> tags) (\s@CreateMetricSet' {} a -> s {tags = a} :: CreateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the column used for tracking time in your
-- source data.
createMetricSet_timestampColumn :: Lens.Lens' CreateMetricSet (Prelude.Maybe TimestampColumn)
createMetricSet_timestampColumn = Lens.lens (\CreateMetricSet' {timestampColumn} -> timestampColumn) (\s@CreateMetricSet' {} a -> s {timestampColumn = a} :: CreateMetricSet)

-- | A description of the dataset you are creating.
createMetricSet_metricSetDescription :: Lens.Lens' CreateMetricSet (Prelude.Maybe Prelude.Text)
createMetricSet_metricSetDescription = Lens.lens (\CreateMetricSet' {metricSetDescription} -> metricSetDescription) (\s@CreateMetricSet' {} a -> s {metricSetDescription = a} :: CreateMetricSet)

-- | The time zone in which your source data was recorded.
createMetricSet_timezone :: Lens.Lens' CreateMetricSet (Prelude.Maybe Prelude.Text)
createMetricSet_timezone = Lens.lens (\CreateMetricSet' {timezone} -> timezone) (\s@CreateMetricSet' {} a -> s {timezone = a} :: CreateMetricSet)

-- | After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3, Redshift, Athena
-- and datasources.
createMetricSet_offset :: Lens.Lens' CreateMetricSet (Prelude.Maybe Prelude.Natural)
createMetricSet_offset = Lens.lens (\CreateMetricSet' {offset} -> offset) (\s@CreateMetricSet' {} a -> s {offset = a} :: CreateMetricSet)

-- | A list of filters that specify which data is kept for anomaly detection.
createMetricSet_dimensionFilterList :: Lens.Lens' CreateMetricSet (Prelude.Maybe [MetricSetDimensionFilter])
createMetricSet_dimensionFilterList = Lens.lens (\CreateMetricSet' {dimensionFilterList} -> dimensionFilterList) (\s@CreateMetricSet' {} a -> s {dimensionFilterList = a} :: CreateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | A list of the fields you want to treat as dimensions.
createMetricSet_dimensionList :: Lens.Lens' CreateMetricSet (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMetricSet_dimensionList = Lens.lens (\CreateMetricSet' {dimensionList} -> dimensionList) (\s@CreateMetricSet' {} a -> s {dimensionList = a} :: CreateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | The frequency with which the source data will be analyzed for anomalies.
createMetricSet_metricSetFrequency :: Lens.Lens' CreateMetricSet (Prelude.Maybe Frequency)
createMetricSet_metricSetFrequency = Lens.lens (\CreateMetricSet' {metricSetFrequency} -> metricSetFrequency) (\s@CreateMetricSet' {} a -> s {metricSetFrequency = a} :: CreateMetricSet)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMetricSetResponse'
            Prelude.<$> (x Data..?> "MetricSetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMetricSet where
  hashWithSalt _salt CreateMetricSet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timestampColumn
      `Prelude.hashWithSalt` metricSetDescription
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` dimensionFilterList
      `Prelude.hashWithSalt` dimensionList
      `Prelude.hashWithSalt` metricSetFrequency
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` metricSetName
      `Prelude.hashWithSalt` metricList
      `Prelude.hashWithSalt` metricSource

instance Prelude.NFData CreateMetricSet where
  rnf CreateMetricSet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timestampColumn
      `Prelude.seq` Prelude.rnf metricSetDescription
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf offset
      `Prelude.seq` Prelude.rnf dimensionFilterList
      `Prelude.seq` Prelude.rnf dimensionList
      `Prelude.seq` Prelude.rnf metricSetFrequency
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf metricSetName
      `Prelude.seq` Prelude.rnf metricList
      `Prelude.seq` Prelude.rnf metricSource

instance Data.ToHeaders CreateMetricSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMetricSet where
  toJSON CreateMetricSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("TimestampColumn" Data..=)
              Prelude.<$> timestampColumn,
            ("MetricSetDescription" Data..=)
              Prelude.<$> metricSetDescription,
            ("Timezone" Data..=) Prelude.<$> timezone,
            ("Offset" Data..=) Prelude.<$> offset,
            ("DimensionFilterList" Data..=)
              Prelude.<$> dimensionFilterList,
            ("DimensionList" Data..=) Prelude.<$> dimensionList,
            ("MetricSetFrequency" Data..=)
              Prelude.<$> metricSetFrequency,
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn),
            Prelude.Just ("MetricSetName" Data..= metricSetName),
            Prelude.Just ("MetricList" Data..= metricList),
            Prelude.Just ("MetricSource" Data..= metricSource)
          ]
      )

instance Data.ToPath CreateMetricSet where
  toPath = Prelude.const "/CreateMetricSet"

instance Data.ToQuery CreateMetricSet where
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

instance Prelude.NFData CreateMetricSetResponse where
  rnf CreateMetricSetResponse' {..} =
    Prelude.rnf metricSetArn
      `Prelude.seq` Prelude.rnf httpStatus
