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
-- Module      : Network.AWS.SageMaker.Types.ModelQualityAppSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelQualityAppSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MonitoringProblemType

-- | Container image configuration object for the monitoring job.
--
-- /See:/ 'newModelQualityAppSpecification' smart constructor.
data ModelQualityAppSpecification = ModelQualityAppSpecification'
  { -- | An array of arguments for the container used to run the monitoring job.
    containerArguments :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Specifies the entrypoint for a container that the monitoring job runs.
    containerEntrypoint :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | An Amazon S3 URI to a script that is called after analysis has been
    -- performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceUri :: Core.Maybe Core.Text,
    -- | Sets the environment variables in the container that the monitoring job
    -- runs.
    environment :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | An Amazon S3 URI to a script that is called per row prior to running
    -- analysis. It can base64 decode the payload and convert it into a flatted
    -- json so that the built-in container can use the converted data.
    -- Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceUri :: Core.Maybe Core.Text,
    -- | The machine learning problem type of the model that the monitoring job
    -- monitors.
    problemType :: Core.Maybe MonitoringProblemType,
    -- | The address of the container image that the monitoring job runs.
    imageUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelQualityAppSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerArguments', 'modelQualityAppSpecification_containerArguments' - An array of arguments for the container used to run the monitoring job.
--
-- 'containerEntrypoint', 'modelQualityAppSpecification_containerEntrypoint' - Specifies the entrypoint for a container that the monitoring job runs.
--
-- 'postAnalyticsProcessorSourceUri', 'modelQualityAppSpecification_postAnalyticsProcessorSourceUri' - An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
--
-- 'environment', 'modelQualityAppSpecification_environment' - Sets the environment variables in the container that the monitoring job
-- runs.
--
-- 'recordPreprocessorSourceUri', 'modelQualityAppSpecification_recordPreprocessorSourceUri' - An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
--
-- 'problemType', 'modelQualityAppSpecification_problemType' - The machine learning problem type of the model that the monitoring job
-- monitors.
--
-- 'imageUri', 'modelQualityAppSpecification_imageUri' - The address of the container image that the monitoring job runs.
newModelQualityAppSpecification ::
  -- | 'imageUri'
  Core.Text ->
  ModelQualityAppSpecification
newModelQualityAppSpecification pImageUri_ =
  ModelQualityAppSpecification'
    { containerArguments =
        Core.Nothing,
      containerEntrypoint = Core.Nothing,
      postAnalyticsProcessorSourceUri =
        Core.Nothing,
      environment = Core.Nothing,
      recordPreprocessorSourceUri = Core.Nothing,
      problemType = Core.Nothing,
      imageUri = pImageUri_
    }

-- | An array of arguments for the container used to run the monitoring job.
modelQualityAppSpecification_containerArguments :: Lens.Lens' ModelQualityAppSpecification (Core.Maybe (Core.NonEmpty Core.Text))
modelQualityAppSpecification_containerArguments = Lens.lens (\ModelQualityAppSpecification' {containerArguments} -> containerArguments) (\s@ModelQualityAppSpecification' {} a -> s {containerArguments = a} :: ModelQualityAppSpecification) Core.. Lens.mapping Lens._Coerce

-- | Specifies the entrypoint for a container that the monitoring job runs.
modelQualityAppSpecification_containerEntrypoint :: Lens.Lens' ModelQualityAppSpecification (Core.Maybe (Core.NonEmpty Core.Text))
modelQualityAppSpecification_containerEntrypoint = Lens.lens (\ModelQualityAppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@ModelQualityAppSpecification' {} a -> s {containerEntrypoint = a} :: ModelQualityAppSpecification) Core.. Lens.mapping Lens._Coerce

-- | An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
modelQualityAppSpecification_postAnalyticsProcessorSourceUri :: Lens.Lens' ModelQualityAppSpecification (Core.Maybe Core.Text)
modelQualityAppSpecification_postAnalyticsProcessorSourceUri = Lens.lens (\ModelQualityAppSpecification' {postAnalyticsProcessorSourceUri} -> postAnalyticsProcessorSourceUri) (\s@ModelQualityAppSpecification' {} a -> s {postAnalyticsProcessorSourceUri = a} :: ModelQualityAppSpecification)

-- | Sets the environment variables in the container that the monitoring job
-- runs.
modelQualityAppSpecification_environment :: Lens.Lens' ModelQualityAppSpecification (Core.Maybe (Core.HashMap Core.Text Core.Text))
modelQualityAppSpecification_environment = Lens.lens (\ModelQualityAppSpecification' {environment} -> environment) (\s@ModelQualityAppSpecification' {} a -> s {environment = a} :: ModelQualityAppSpecification) Core.. Lens.mapping Lens._Coerce

-- | An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
modelQualityAppSpecification_recordPreprocessorSourceUri :: Lens.Lens' ModelQualityAppSpecification (Core.Maybe Core.Text)
modelQualityAppSpecification_recordPreprocessorSourceUri = Lens.lens (\ModelQualityAppSpecification' {recordPreprocessorSourceUri} -> recordPreprocessorSourceUri) (\s@ModelQualityAppSpecification' {} a -> s {recordPreprocessorSourceUri = a} :: ModelQualityAppSpecification)

-- | The machine learning problem type of the model that the monitoring job
-- monitors.
modelQualityAppSpecification_problemType :: Lens.Lens' ModelQualityAppSpecification (Core.Maybe MonitoringProblemType)
modelQualityAppSpecification_problemType = Lens.lens (\ModelQualityAppSpecification' {problemType} -> problemType) (\s@ModelQualityAppSpecification' {} a -> s {problemType = a} :: ModelQualityAppSpecification)

-- | The address of the container image that the monitoring job runs.
modelQualityAppSpecification_imageUri :: Lens.Lens' ModelQualityAppSpecification Core.Text
modelQualityAppSpecification_imageUri = Lens.lens (\ModelQualityAppSpecification' {imageUri} -> imageUri) (\s@ModelQualityAppSpecification' {} a -> s {imageUri = a} :: ModelQualityAppSpecification)

instance Core.FromJSON ModelQualityAppSpecification where
  parseJSON =
    Core.withObject
      "ModelQualityAppSpecification"
      ( \x ->
          ModelQualityAppSpecification'
            Core.<$> (x Core..:? "ContainerArguments")
            Core.<*> (x Core..:? "ContainerEntrypoint")
            Core.<*> (x Core..:? "PostAnalyticsProcessorSourceUri")
            Core.<*> (x Core..:? "Environment" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RecordPreprocessorSourceUri")
            Core.<*> (x Core..:? "ProblemType")
            Core.<*> (x Core..: "ImageUri")
      )

instance Core.Hashable ModelQualityAppSpecification

instance Core.NFData ModelQualityAppSpecification

instance Core.ToJSON ModelQualityAppSpecification where
  toJSON ModelQualityAppSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContainerArguments" Core..=)
              Core.<$> containerArguments,
            ("ContainerEntrypoint" Core..=)
              Core.<$> containerEntrypoint,
            ("PostAnalyticsProcessorSourceUri" Core..=)
              Core.<$> postAnalyticsProcessorSourceUri,
            ("Environment" Core..=) Core.<$> environment,
            ("RecordPreprocessorSourceUri" Core..=)
              Core.<$> recordPreprocessorSourceUri,
            ("ProblemType" Core..=) Core.<$> problemType,
            Core.Just ("ImageUri" Core..= imageUri)
          ]
      )
