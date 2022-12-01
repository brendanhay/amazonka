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
-- Module      : Amazonka.SageMaker.Types.EndpointInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EndpointInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingS3DataDistributionType
import Amazonka.SageMaker.Types.ProcessingS3InputMode

-- | Input object for the endpoint
--
-- /See:/ 'newEndpointInput' smart constructor.
data EndpointInput = EndpointInput'
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
    -- | An endpoint in customer\'s account which has enabled @DataCaptureConfig@
    -- enabled.
    endpointName :: Prelude.Text,
    -- | Path to the filesystem where the endpoint data is available to the
    -- container.
    localPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'probabilityThresholdAttribute', 'endpointInput_probabilityThresholdAttribute' - The threshold for the class probability to be evaluated as a positive
-- result.
--
-- 's3InputMode', 'endpointInput_s3InputMode' - Whether the @Pipe@ or @File@ is used as the input mode for transferring
-- data for the monitoring job. @Pipe@ mode is recommended for large
-- datasets. @File@ mode is useful for small files that fit in memory.
-- Defaults to @File@.
--
-- 's3DataDistributionType', 'endpointInput_s3DataDistributionType' - Whether input data distributed in Amazon S3 is fully replicated or
-- sharded by an S3 key. Defaults to @FullyReplicated@
--
-- 'probabilityAttribute', 'endpointInput_probabilityAttribute' - In a classification problem, the attribute that represents the class
-- probability.
--
-- 'startTimeOffset', 'endpointInput_startTimeOffset' - If specified, monitoring jobs substract this time from the start time.
-- For information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
--
-- 'featuresAttribute', 'endpointInput_featuresAttribute' - The attributes of the input data that are the input features.
--
-- 'inferenceAttribute', 'endpointInput_inferenceAttribute' - The attribute of the input data that represents the ground truth label.
--
-- 'endTimeOffset', 'endpointInput_endTimeOffset' - If specified, monitoring jobs substract this time from the end time. For
-- information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
--
-- 'endpointName', 'endpointInput_endpointName' - An endpoint in customer\'s account which has enabled @DataCaptureConfig@
-- enabled.
--
-- 'localPath', 'endpointInput_localPath' - Path to the filesystem where the endpoint data is available to the
-- container.
newEndpointInput ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'localPath'
  Prelude.Text ->
  EndpointInput
newEndpointInput pEndpointName_ pLocalPath_ =
  EndpointInput'
    { probabilityThresholdAttribute =
        Prelude.Nothing,
      s3InputMode = Prelude.Nothing,
      s3DataDistributionType = Prelude.Nothing,
      probabilityAttribute = Prelude.Nothing,
      startTimeOffset = Prelude.Nothing,
      featuresAttribute = Prelude.Nothing,
      inferenceAttribute = Prelude.Nothing,
      endTimeOffset = Prelude.Nothing,
      endpointName = pEndpointName_,
      localPath = pLocalPath_
    }

-- | The threshold for the class probability to be evaluated as a positive
-- result.
endpointInput_probabilityThresholdAttribute :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Double)
endpointInput_probabilityThresholdAttribute = Lens.lens (\EndpointInput' {probabilityThresholdAttribute} -> probabilityThresholdAttribute) (\s@EndpointInput' {} a -> s {probabilityThresholdAttribute = a} :: EndpointInput)

-- | Whether the @Pipe@ or @File@ is used as the input mode for transferring
-- data for the monitoring job. @Pipe@ mode is recommended for large
-- datasets. @File@ mode is useful for small files that fit in memory.
-- Defaults to @File@.
endpointInput_s3InputMode :: Lens.Lens' EndpointInput (Prelude.Maybe ProcessingS3InputMode)
endpointInput_s3InputMode = Lens.lens (\EndpointInput' {s3InputMode} -> s3InputMode) (\s@EndpointInput' {} a -> s {s3InputMode = a} :: EndpointInput)

-- | Whether input data distributed in Amazon S3 is fully replicated or
-- sharded by an S3 key. Defaults to @FullyReplicated@
endpointInput_s3DataDistributionType :: Lens.Lens' EndpointInput (Prelude.Maybe ProcessingS3DataDistributionType)
endpointInput_s3DataDistributionType = Lens.lens (\EndpointInput' {s3DataDistributionType} -> s3DataDistributionType) (\s@EndpointInput' {} a -> s {s3DataDistributionType = a} :: EndpointInput)

-- | In a classification problem, the attribute that represents the class
-- probability.
endpointInput_probabilityAttribute :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_probabilityAttribute = Lens.lens (\EndpointInput' {probabilityAttribute} -> probabilityAttribute) (\s@EndpointInput' {} a -> s {probabilityAttribute = a} :: EndpointInput)

-- | If specified, monitoring jobs substract this time from the start time.
-- For information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
endpointInput_startTimeOffset :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_startTimeOffset = Lens.lens (\EndpointInput' {startTimeOffset} -> startTimeOffset) (\s@EndpointInput' {} a -> s {startTimeOffset = a} :: EndpointInput)

-- | The attributes of the input data that are the input features.
endpointInput_featuresAttribute :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_featuresAttribute = Lens.lens (\EndpointInput' {featuresAttribute} -> featuresAttribute) (\s@EndpointInput' {} a -> s {featuresAttribute = a} :: EndpointInput)

-- | The attribute of the input data that represents the ground truth label.
endpointInput_inferenceAttribute :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_inferenceAttribute = Lens.lens (\EndpointInput' {inferenceAttribute} -> inferenceAttribute) (\s@EndpointInput' {} a -> s {inferenceAttribute = a} :: EndpointInput)

-- | If specified, monitoring jobs substract this time from the end time. For
-- information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
endpointInput_endTimeOffset :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_endTimeOffset = Lens.lens (\EndpointInput' {endTimeOffset} -> endTimeOffset) (\s@EndpointInput' {} a -> s {endTimeOffset = a} :: EndpointInput)

-- | An endpoint in customer\'s account which has enabled @DataCaptureConfig@
-- enabled.
endpointInput_endpointName :: Lens.Lens' EndpointInput Prelude.Text
endpointInput_endpointName = Lens.lens (\EndpointInput' {endpointName} -> endpointName) (\s@EndpointInput' {} a -> s {endpointName = a} :: EndpointInput)

-- | Path to the filesystem where the endpoint data is available to the
-- container.
endpointInput_localPath :: Lens.Lens' EndpointInput Prelude.Text
endpointInput_localPath = Lens.lens (\EndpointInput' {localPath} -> localPath) (\s@EndpointInput' {} a -> s {localPath = a} :: EndpointInput)

instance Core.FromJSON EndpointInput where
  parseJSON =
    Core.withObject
      "EndpointInput"
      ( \x ->
          EndpointInput'
            Prelude.<$> (x Core..:? "ProbabilityThresholdAttribute")
            Prelude.<*> (x Core..:? "S3InputMode")
            Prelude.<*> (x Core..:? "S3DataDistributionType")
            Prelude.<*> (x Core..:? "ProbabilityAttribute")
            Prelude.<*> (x Core..:? "StartTimeOffset")
            Prelude.<*> (x Core..:? "FeaturesAttribute")
            Prelude.<*> (x Core..:? "InferenceAttribute")
            Prelude.<*> (x Core..:? "EndTimeOffset")
            Prelude.<*> (x Core..: "EndpointName")
            Prelude.<*> (x Core..: "LocalPath")
      )

instance Prelude.Hashable EndpointInput where
  hashWithSalt _salt EndpointInput' {..} =
    _salt
      `Prelude.hashWithSalt` probabilityThresholdAttribute
      `Prelude.hashWithSalt` s3InputMode
      `Prelude.hashWithSalt` s3DataDistributionType
      `Prelude.hashWithSalt` probabilityAttribute
      `Prelude.hashWithSalt` startTimeOffset
      `Prelude.hashWithSalt` featuresAttribute
      `Prelude.hashWithSalt` inferenceAttribute
      `Prelude.hashWithSalt` endTimeOffset
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` localPath

instance Prelude.NFData EndpointInput where
  rnf EndpointInput' {..} =
    Prelude.rnf probabilityThresholdAttribute
      `Prelude.seq` Prelude.rnf s3InputMode
      `Prelude.seq` Prelude.rnf s3DataDistributionType
      `Prelude.seq` Prelude.rnf probabilityAttribute
      `Prelude.seq` Prelude.rnf startTimeOffset
      `Prelude.seq` Prelude.rnf featuresAttribute
      `Prelude.seq` Prelude.rnf inferenceAttribute
      `Prelude.seq` Prelude.rnf endTimeOffset
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf localPath

instance Core.ToJSON EndpointInput where
  toJSON EndpointInput' {..} =
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
            Prelude.Just ("EndpointName" Core..= endpointName),
            Prelude.Just ("LocalPath" Core..= localPath)
          ]
      )
