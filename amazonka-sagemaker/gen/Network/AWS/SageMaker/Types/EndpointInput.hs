{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.EndpointInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
import Network.AWS.SageMaker.Types.ProcessingS3InputMode

-- | Input object for the endpoint
--
-- /See:/ 'newEndpointInput' smart constructor.
data EndpointInput = EndpointInput'
  { -- | If specified, monitoring jobs substract this time from the end time. For
    -- information about using offsets for scheduling monitoring jobs, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
    endTimeOffset :: Prelude.Maybe Prelude.Text,
    -- | The attribute of the input data that represents the ground truth label.
    inferenceAttribute :: Prelude.Maybe Prelude.Text,
    -- | Whether the @Pipe@ or @File@ is used as the input mode for transfering
    -- data for the monitoring job. @Pipe@ mode is recommended for large
    -- datasets. @File@ mode is useful for small files that fit in memory.
    -- Defaults to @File@.
    s3InputMode :: Prelude.Maybe ProcessingS3InputMode,
    -- | Whether input data distributed in Amazon S3 is fully replicated or
    -- sharded by an S3 key. Defauts to @FullyReplicated@
    s3DataDistributionType :: Prelude.Maybe ProcessingS3DataDistributionType,
    -- | In a classification problem, the attribute that represents the class
    -- probability.
    probabilityAttribute :: Prelude.Maybe Prelude.Text,
    -- | The threshold for the class probability to be evaluated as a positive
    -- result.
    probabilityThresholdAttribute :: Prelude.Maybe Prelude.Double,
    -- | The attributes of the input data that are the input features.
    featuresAttribute :: Prelude.Maybe Prelude.Text,
    -- | If specified, monitoring jobs substract this time from the start time.
    -- For information about using offsets for scheduling monitoring jobs, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
    startTimeOffset :: Prelude.Maybe Prelude.Text,
    -- | An endpoint in customer\'s account which has enabled @DataCaptureConfig@
    -- enabled.
    endpointName :: Prelude.Text,
    -- | Path to the filesystem where the endpoint data is available to the
    -- container.
    localPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndpointInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTimeOffset', 'endpointInput_endTimeOffset' - If specified, monitoring jobs substract this time from the end time. For
-- information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
--
-- 'inferenceAttribute', 'endpointInput_inferenceAttribute' - The attribute of the input data that represents the ground truth label.
--
-- 's3InputMode', 'endpointInput_s3InputMode' - Whether the @Pipe@ or @File@ is used as the input mode for transfering
-- data for the monitoring job. @Pipe@ mode is recommended for large
-- datasets. @File@ mode is useful for small files that fit in memory.
-- Defaults to @File@.
--
-- 's3DataDistributionType', 'endpointInput_s3DataDistributionType' - Whether input data distributed in Amazon S3 is fully replicated or
-- sharded by an S3 key. Defauts to @FullyReplicated@
--
-- 'probabilityAttribute', 'endpointInput_probabilityAttribute' - In a classification problem, the attribute that represents the class
-- probability.
--
-- 'probabilityThresholdAttribute', 'endpointInput_probabilityThresholdAttribute' - The threshold for the class probability to be evaluated as a positive
-- result.
--
-- 'featuresAttribute', 'endpointInput_featuresAttribute' - The attributes of the input data that are the input features.
--
-- 'startTimeOffset', 'endpointInput_startTimeOffset' - If specified, monitoring jobs substract this time from the start time.
-- For information about using offsets for scheduling monitoring jobs, see
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
    { endTimeOffset = Prelude.Nothing,
      inferenceAttribute = Prelude.Nothing,
      s3InputMode = Prelude.Nothing,
      s3DataDistributionType = Prelude.Nothing,
      probabilityAttribute = Prelude.Nothing,
      probabilityThresholdAttribute = Prelude.Nothing,
      featuresAttribute = Prelude.Nothing,
      startTimeOffset = Prelude.Nothing,
      endpointName = pEndpointName_,
      localPath = pLocalPath_
    }

-- | If specified, monitoring jobs substract this time from the end time. For
-- information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
endpointInput_endTimeOffset :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_endTimeOffset = Lens.lens (\EndpointInput' {endTimeOffset} -> endTimeOffset) (\s@EndpointInput' {} a -> s {endTimeOffset = a} :: EndpointInput)

-- | The attribute of the input data that represents the ground truth label.
endpointInput_inferenceAttribute :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_inferenceAttribute = Lens.lens (\EndpointInput' {inferenceAttribute} -> inferenceAttribute) (\s@EndpointInput' {} a -> s {inferenceAttribute = a} :: EndpointInput)

-- | Whether the @Pipe@ or @File@ is used as the input mode for transfering
-- data for the monitoring job. @Pipe@ mode is recommended for large
-- datasets. @File@ mode is useful for small files that fit in memory.
-- Defaults to @File@.
endpointInput_s3InputMode :: Lens.Lens' EndpointInput (Prelude.Maybe ProcessingS3InputMode)
endpointInput_s3InputMode = Lens.lens (\EndpointInput' {s3InputMode} -> s3InputMode) (\s@EndpointInput' {} a -> s {s3InputMode = a} :: EndpointInput)

-- | Whether input data distributed in Amazon S3 is fully replicated or
-- sharded by an S3 key. Defauts to @FullyReplicated@
endpointInput_s3DataDistributionType :: Lens.Lens' EndpointInput (Prelude.Maybe ProcessingS3DataDistributionType)
endpointInput_s3DataDistributionType = Lens.lens (\EndpointInput' {s3DataDistributionType} -> s3DataDistributionType) (\s@EndpointInput' {} a -> s {s3DataDistributionType = a} :: EndpointInput)

-- | In a classification problem, the attribute that represents the class
-- probability.
endpointInput_probabilityAttribute :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_probabilityAttribute = Lens.lens (\EndpointInput' {probabilityAttribute} -> probabilityAttribute) (\s@EndpointInput' {} a -> s {probabilityAttribute = a} :: EndpointInput)

-- | The threshold for the class probability to be evaluated as a positive
-- result.
endpointInput_probabilityThresholdAttribute :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Double)
endpointInput_probabilityThresholdAttribute = Lens.lens (\EndpointInput' {probabilityThresholdAttribute} -> probabilityThresholdAttribute) (\s@EndpointInput' {} a -> s {probabilityThresholdAttribute = a} :: EndpointInput)

-- | The attributes of the input data that are the input features.
endpointInput_featuresAttribute :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_featuresAttribute = Lens.lens (\EndpointInput' {featuresAttribute} -> featuresAttribute) (\s@EndpointInput' {} a -> s {featuresAttribute = a} :: EndpointInput)

-- | If specified, monitoring jobs substract this time from the start time.
-- For information about using offsets for scheduling monitoring jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-model-quality-schedule.html Schedule Model Quality Monitoring Jobs>.
endpointInput_startTimeOffset :: Lens.Lens' EndpointInput (Prelude.Maybe Prelude.Text)
endpointInput_startTimeOffset = Lens.lens (\EndpointInput' {startTimeOffset} -> startTimeOffset) (\s@EndpointInput' {} a -> s {startTimeOffset = a} :: EndpointInput)

-- | An endpoint in customer\'s account which has enabled @DataCaptureConfig@
-- enabled.
endpointInput_endpointName :: Lens.Lens' EndpointInput Prelude.Text
endpointInput_endpointName = Lens.lens (\EndpointInput' {endpointName} -> endpointName) (\s@EndpointInput' {} a -> s {endpointName = a} :: EndpointInput)

-- | Path to the filesystem where the endpoint data is available to the
-- container.
endpointInput_localPath :: Lens.Lens' EndpointInput Prelude.Text
endpointInput_localPath = Lens.lens (\EndpointInput' {localPath} -> localPath) (\s@EndpointInput' {} a -> s {localPath = a} :: EndpointInput)

instance Prelude.FromJSON EndpointInput where
  parseJSON =
    Prelude.withObject
      "EndpointInput"
      ( \x ->
          EndpointInput'
            Prelude.<$> (x Prelude..:? "EndTimeOffset")
            Prelude.<*> (x Prelude..:? "InferenceAttribute")
            Prelude.<*> (x Prelude..:? "S3InputMode")
            Prelude.<*> (x Prelude..:? "S3DataDistributionType")
            Prelude.<*> (x Prelude..:? "ProbabilityAttribute")
            Prelude.<*> (x Prelude..:? "ProbabilityThresholdAttribute")
            Prelude.<*> (x Prelude..:? "FeaturesAttribute")
            Prelude.<*> (x Prelude..:? "StartTimeOffset")
            Prelude.<*> (x Prelude..: "EndpointName")
            Prelude.<*> (x Prelude..: "LocalPath")
      )

instance Prelude.Hashable EndpointInput

instance Prelude.NFData EndpointInput

instance Prelude.ToJSON EndpointInput where
  toJSON EndpointInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EndTimeOffset" Prelude..=)
              Prelude.<$> endTimeOffset,
            ("InferenceAttribute" Prelude..=)
              Prelude.<$> inferenceAttribute,
            ("S3InputMode" Prelude..=) Prelude.<$> s3InputMode,
            ("S3DataDistributionType" Prelude..=)
              Prelude.<$> s3DataDistributionType,
            ("ProbabilityAttribute" Prelude..=)
              Prelude.<$> probabilityAttribute,
            ("ProbabilityThresholdAttribute" Prelude..=)
              Prelude.<$> probabilityThresholdAttribute,
            ("FeaturesAttribute" Prelude..=)
              Prelude.<$> featuresAttribute,
            ("StartTimeOffset" Prelude..=)
              Prelude.<$> startTimeOffset,
            Prelude.Just
              ("EndpointName" Prelude..= endpointName),
            Prelude.Just ("LocalPath" Prelude..= localPath)
          ]
      )
