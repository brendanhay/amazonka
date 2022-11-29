{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.BatchTransformInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.BatchTransformInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringDatasetFormat
import Amazonka.SageMaker.Types.ProcessingS3DataDistributionType
import Amazonka.SageMaker.Types.ProcessingS3InputMode

-- | Input object for the batch transform job.
--
-- /See:/ 'newBatchTransformInput' smart constructor.
data BatchTransformInput = BatchTransformInput'
  { -- | The threshold for the class probability to be evaluated as a positive
    -- result.
    probabilityThresholdAttribute :: Prelude.Maybe Prelude.Double,
    -- | Whether the @Pipe@ or @File@ is used as the input mode for transferring
    -- data for the monitoring job. @Pipe@ mode is recommended for large
    -- datasets. @File@ mode is useful for small files that fit in memory.
    -- Defaults to @File@.
    s3InputMode :: Prelude.Maybe ProcessingS3InputMode,
    -- | Whether input data distributed in Amazon S3 is fully replicated or
    -- sharded by an S3 key. Defaults to @FullyReplicated@
    s3DataDistributionType :: Prelude.Maybe ProcessingS3DataDistributionType,
    -- | In a classification problem, the attribute that represents the class
    -- probability.
    probabilityAttribute :: Prelude.Maybe Prelude.Text,
    -- | If specified, monitoring jobs substract this time from the start time.
    -- For information about using offsets for scheduling monitoring jobs, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
    startTimeOffset :: Prelude.Maybe Prelude.Text,
    -- | The attributes of the input data that are the input features.
    featuresAttribute :: Prelude.Maybe Prelude.Text,
    -- | The attribute of the input data that represents the ground truth label.
    inferenceAttribute :: Prelude.Maybe Prelude.Text,
    -- | If specified, monitoring jobs substract this time from the end time. For
    -- information about using offsets for scheduling monitoring jobs, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
    endTimeOffset :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location being used to capture the data.
    dataCapturedDestinationS3Uri :: Prelude.Text,
    -- | The dataset format for your batch transform job.
    datasetFormat :: MonitoringDatasetFormat,
    -- | Path to the filesystem where the batch transform data is available to
    -- the container.
    localPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchTransformInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'probabilityThresholdAttribute', 'batchTransformInput_probabilityThresholdAttribute' - The threshold for the class probability to be evaluated as a positive
-- result.
--
-- 's3InputMode', 'batchTransformInput_s3InputMode' - Whether the @Pipe@ or @File@ is used as the input mode for transferring
-- data for the monitoring job. @Pipe@ mode is recommended for large
-- datasets. @File@ mode is useful for small files that fit in memory.
-- Defaults to @File@.
--
-- 's3DataDistributionType', 'batchTransformInput_s3DataDistributionType' - Whether input data distributed in Amazon S3 is fully replicated or
-- sharded by an S3 key. Defaults to @FullyReplicated@
--
-- 'probabilityAttribute', 'batchTransformInput_probabilityAttribute' - In a classification problem, the attribute that represents the class
-- probability.
--
-- 'startTimeOffset', 'batchTransformInput_startTimeOffset' - If specified, monitoring jobs substract this time from the start time.
-- For information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
--
-- 'featuresAttribute', 'batchTransformInput_featuresAttribute' - The attributes of the input data that are the input features.
--
-- 'inferenceAttribute', 'batchTransformInput_inferenceAttribute' - The attribute of the input data that represents the ground truth label.
--
-- 'endTimeOffset', 'batchTransformInput_endTimeOffset' - If specified, monitoring jobs substract this time from the end time. For
-- information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
--
-- 'dataCapturedDestinationS3Uri', 'batchTransformInput_dataCapturedDestinationS3Uri' - The Amazon S3 location being used to capture the data.
--
-- 'datasetFormat', 'batchTransformInput_datasetFormat' - The dataset format for your batch transform job.
--
-- 'localPath', 'batchTransformInput_localPath' - Path to the filesystem where the batch transform data is available to
-- the container.
newBatchTransformInput ::
  -- | 'dataCapturedDestinationS3Uri'
  Prelude.Text ->
  -- | 'datasetFormat'
  MonitoringDatasetFormat ->
  -- | 'localPath'
  Prelude.Text ->
  BatchTransformInput
newBatchTransformInput
  pDataCapturedDestinationS3Uri_
  pDatasetFormat_
  pLocalPath_ =
    BatchTransformInput'
      { probabilityThresholdAttribute =
          Prelude.Nothing,
        s3InputMode = Prelude.Nothing,
        s3DataDistributionType = Prelude.Nothing,
        probabilityAttribute = Prelude.Nothing,
        startTimeOffset = Prelude.Nothing,
        featuresAttribute = Prelude.Nothing,
        inferenceAttribute = Prelude.Nothing,
        endTimeOffset = Prelude.Nothing,
        dataCapturedDestinationS3Uri =
          pDataCapturedDestinationS3Uri_,
        datasetFormat = pDatasetFormat_,
        localPath = pLocalPath_
      }

-- | The threshold for the class probability to be evaluated as a positive
-- result.
batchTransformInput_probabilityThresholdAttribute :: Lens.Lens' BatchTransformInput (Prelude.Maybe Prelude.Double)
batchTransformInput_probabilityThresholdAttribute = Lens.lens (\BatchTransformInput' {probabilityThresholdAttribute} -> probabilityThresholdAttribute) (\s@BatchTransformInput' {} a -> s {probabilityThresholdAttribute = a} :: BatchTransformInput)

-- | Whether the @Pipe@ or @File@ is used as the input mode for transferring
-- data for the monitoring job. @Pipe@ mode is recommended for large
-- datasets. @File@ mode is useful for small files that fit in memory.
-- Defaults to @File@.
batchTransformInput_s3InputMode :: Lens.Lens' BatchTransformInput (Prelude.Maybe ProcessingS3InputMode)
batchTransformInput_s3InputMode = Lens.lens (\BatchTransformInput' {s3InputMode} -> s3InputMode) (\s@BatchTransformInput' {} a -> s {s3InputMode = a} :: BatchTransformInput)

-- | Whether input data distributed in Amazon S3 is fully replicated or
-- sharded by an S3 key. Defaults to @FullyReplicated@
batchTransformInput_s3DataDistributionType :: Lens.Lens' BatchTransformInput (Prelude.Maybe ProcessingS3DataDistributionType)
batchTransformInput_s3DataDistributionType = Lens.lens (\BatchTransformInput' {s3DataDistributionType} -> s3DataDistributionType) (\s@BatchTransformInput' {} a -> s {s3DataDistributionType = a} :: BatchTransformInput)

-- | In a classification problem, the attribute that represents the class
-- probability.
batchTransformInput_probabilityAttribute :: Lens.Lens' BatchTransformInput (Prelude.Maybe Prelude.Text)
batchTransformInput_probabilityAttribute = Lens.lens (\BatchTransformInput' {probabilityAttribute} -> probabilityAttribute) (\s@BatchTransformInput' {} a -> s {probabilityAttribute = a} :: BatchTransformInput)

-- | If specified, monitoring jobs substract this time from the start time.
-- For information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
batchTransformInput_startTimeOffset :: Lens.Lens' BatchTransformInput (Prelude.Maybe Prelude.Text)
batchTransformInput_startTimeOffset = Lens.lens (\BatchTransformInput' {startTimeOffset} -> startTimeOffset) (\s@BatchTransformInput' {} a -> s {startTimeOffset = a} :: BatchTransformInput)

-- | The attributes of the input data that are the input features.
batchTransformInput_featuresAttribute :: Lens.Lens' BatchTransformInput (Prelude.Maybe Prelude.Text)
batchTransformInput_featuresAttribute = Lens.lens (\BatchTransformInput' {featuresAttribute} -> featuresAttribute) (\s@BatchTransformInput' {} a -> s {featuresAttribute = a} :: BatchTransformInput)

-- | The attribute of the input data that represents the ground truth label.
batchTransformInput_inferenceAttribute :: Lens.Lens' BatchTransformInput (Prelude.Maybe Prelude.Text)
batchTransformInput_inferenceAttribute = Lens.lens (\BatchTransformInput' {inferenceAttribute} -> inferenceAttribute) (\s@BatchTransformInput' {} a -> s {inferenceAttribute = a} :: BatchTransformInput)

-- | If specified, monitoring jobs substract this time from the end time. For
-- information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
batchTransformInput_endTimeOffset :: Lens.Lens' BatchTransformInput (Prelude.Maybe Prelude.Text)
batchTransformInput_endTimeOffset = Lens.lens (\BatchTransformInput' {endTimeOffset} -> endTimeOffset) (\s@BatchTransformInput' {} a -> s {endTimeOffset = a} :: BatchTransformInput)

-- | The Amazon S3 location being used to capture the data.
batchTransformInput_dataCapturedDestinationS3Uri :: Lens.Lens' BatchTransformInput Prelude.Text
batchTransformInput_dataCapturedDestinationS3Uri = Lens.lens (\BatchTransformInput' {dataCapturedDestinationS3Uri} -> dataCapturedDestinationS3Uri) (\s@BatchTransformInput' {} a -> s {dataCapturedDestinationS3Uri = a} :: BatchTransformInput)

-- | The dataset format for your batch transform job.
batchTransformInput_datasetFormat :: Lens.Lens' BatchTransformInput MonitoringDatasetFormat
batchTransformInput_datasetFormat = Lens.lens (\BatchTransformInput' {datasetFormat} -> datasetFormat) (\s@BatchTransformInput' {} a -> s {datasetFormat = a} :: BatchTransformInput)

-- | Path to the filesystem where the batch transform data is available to
-- the container.
batchTransformInput_localPath :: Lens.Lens' BatchTransformInput Prelude.Text
batchTransformInput_localPath = Lens.lens (\BatchTransformInput' {localPath} -> localPath) (\s@BatchTransformInput' {} a -> s {localPath = a} :: BatchTransformInput)

instance Core.FromJSON BatchTransformInput where
  parseJSON =
    Core.withObject
      "BatchTransformInput"
      ( \x ->
          BatchTransformInput'
            Prelude.<$> (x Core..:? "ProbabilityThresholdAttribute")
            Prelude.<*> (x Core..:? "S3InputMode")
            Prelude.<*> (x Core..:? "S3DataDistributionType")
            Prelude.<*> (x Core..:? "ProbabilityAttribute")
            Prelude.<*> (x Core..:? "StartTimeOffset")
            Prelude.<*> (x Core..:? "FeaturesAttribute")
            Prelude.<*> (x Core..:? "InferenceAttribute")
            Prelude.<*> (x Core..:? "EndTimeOffset")
            Prelude.<*> (x Core..: "DataCapturedDestinationS3Uri")
            Prelude.<*> (x Core..: "DatasetFormat")
            Prelude.<*> (x Core..: "LocalPath")
      )

instance Prelude.Hashable BatchTransformInput where
  hashWithSalt _salt BatchTransformInput' {..} =
    _salt
      `Prelude.hashWithSalt` probabilityThresholdAttribute
      `Prelude.hashWithSalt` s3InputMode
      `Prelude.hashWithSalt` s3DataDistributionType
      `Prelude.hashWithSalt` probabilityAttribute
      `Prelude.hashWithSalt` startTimeOffset
      `Prelude.hashWithSalt` featuresAttribute
      `Prelude.hashWithSalt` inferenceAttribute
      `Prelude.hashWithSalt` endTimeOffset
      `Prelude.hashWithSalt` dataCapturedDestinationS3Uri
      `Prelude.hashWithSalt` datasetFormat
      `Prelude.hashWithSalt` localPath

instance Prelude.NFData BatchTransformInput where
  rnf BatchTransformInput' {..} =
    Prelude.rnf probabilityThresholdAttribute
      `Prelude.seq` Prelude.rnf s3InputMode
      `Prelude.seq` Prelude.rnf s3DataDistributionType
      `Prelude.seq` Prelude.rnf probabilityAttribute
      `Prelude.seq` Prelude.rnf startTimeOffset
      `Prelude.seq` Prelude.rnf featuresAttribute
      `Prelude.seq` Prelude.rnf inferenceAttribute
      `Prelude.seq` Prelude.rnf endTimeOffset
      `Prelude.seq` Prelude.rnf dataCapturedDestinationS3Uri
      `Prelude.seq` Prelude.rnf datasetFormat
      `Prelude.seq` Prelude.rnf localPath

instance Core.ToJSON BatchTransformInput where
  toJSON BatchTransformInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProbabilityThresholdAttribute" Core..=)
              Prelude.<$> probabilityThresholdAttribute,
            ("S3InputMode" Core..=) Prelude.<$> s3InputMode,
            ("S3DataDistributionType" Core..=)
              Prelude.<$> s3DataDistributionType,
            ("ProbabilityAttribute" Core..=)
              Prelude.<$> probabilityAttribute,
            ("StartTimeOffset" Core..=)
              Prelude.<$> startTimeOffset,
            ("FeaturesAttribute" Core..=)
              Prelude.<$> featuresAttribute,
            ("InferenceAttribute" Core..=)
              Prelude.<$> inferenceAttribute,
            ("EndTimeOffset" Core..=) Prelude.<$> endTimeOffset,
            Prelude.Just
              ( "DataCapturedDestinationS3Uri"
                  Core..= dataCapturedDestinationS3Uri
              ),
            Prelude.Just ("DatasetFormat" Core..= datasetFormat),
            Prelude.Just ("LocalPath" Core..= localPath)
          ]
      )
