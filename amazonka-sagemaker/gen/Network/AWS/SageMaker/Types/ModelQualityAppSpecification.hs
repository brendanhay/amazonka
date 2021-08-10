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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MonitoringProblemType

-- | Container image configuration object for the monitoring job.
--
-- /See:/ 'newModelQualityAppSpecification' smart constructor.
data ModelQualityAppSpecification = ModelQualityAppSpecification'
  { -- | An array of arguments for the container used to run the monitoring job.
    containerArguments :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies the entrypoint for a container that the monitoring job runs.
    containerEntrypoint :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An Amazon S3 URI to a script that is called after analysis has been
    -- performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceUri :: Prelude.Maybe Prelude.Text,
    -- | Sets the environment variables in the container that the monitoring job
    -- runs.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An Amazon S3 URI to a script that is called per row prior to running
    -- analysis. It can base64 decode the payload and convert it into a flatted
    -- json so that the built-in container can use the converted data.
    -- Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceUri :: Prelude.Maybe Prelude.Text,
    -- | The machine learning problem type of the model that the monitoring job
    -- monitors.
    problemType :: Prelude.Maybe MonitoringProblemType,
    -- | The address of the container image that the monitoring job runs.
    imageUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ModelQualityAppSpecification
newModelQualityAppSpecification pImageUri_ =
  ModelQualityAppSpecification'
    { containerArguments =
        Prelude.Nothing,
      containerEntrypoint = Prelude.Nothing,
      postAnalyticsProcessorSourceUri =
        Prelude.Nothing,
      environment = Prelude.Nothing,
      recordPreprocessorSourceUri = Prelude.Nothing,
      problemType = Prelude.Nothing,
      imageUri = pImageUri_
    }

-- | An array of arguments for the container used to run the monitoring job.
modelQualityAppSpecification_containerArguments :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modelQualityAppSpecification_containerArguments = Lens.lens (\ModelQualityAppSpecification' {containerArguments} -> containerArguments) (\s@ModelQualityAppSpecification' {} a -> s {containerArguments = a} :: ModelQualityAppSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the entrypoint for a container that the monitoring job runs.
modelQualityAppSpecification_containerEntrypoint :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modelQualityAppSpecification_containerEntrypoint = Lens.lens (\ModelQualityAppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@ModelQualityAppSpecification' {} a -> s {containerEntrypoint = a} :: ModelQualityAppSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
modelQualityAppSpecification_postAnalyticsProcessorSourceUri :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe Prelude.Text)
modelQualityAppSpecification_postAnalyticsProcessorSourceUri = Lens.lens (\ModelQualityAppSpecification' {postAnalyticsProcessorSourceUri} -> postAnalyticsProcessorSourceUri) (\s@ModelQualityAppSpecification' {} a -> s {postAnalyticsProcessorSourceUri = a} :: ModelQualityAppSpecification)

-- | Sets the environment variables in the container that the monitoring job
-- runs.
modelQualityAppSpecification_environment :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
modelQualityAppSpecification_environment = Lens.lens (\ModelQualityAppSpecification' {environment} -> environment) (\s@ModelQualityAppSpecification' {} a -> s {environment = a} :: ModelQualityAppSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
modelQualityAppSpecification_recordPreprocessorSourceUri :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe Prelude.Text)
modelQualityAppSpecification_recordPreprocessorSourceUri = Lens.lens (\ModelQualityAppSpecification' {recordPreprocessorSourceUri} -> recordPreprocessorSourceUri) (\s@ModelQualityAppSpecification' {} a -> s {recordPreprocessorSourceUri = a} :: ModelQualityAppSpecification)

-- | The machine learning problem type of the model that the monitoring job
-- monitors.
modelQualityAppSpecification_problemType :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe MonitoringProblemType)
modelQualityAppSpecification_problemType = Lens.lens (\ModelQualityAppSpecification' {problemType} -> problemType) (\s@ModelQualityAppSpecification' {} a -> s {problemType = a} :: ModelQualityAppSpecification)

-- | The address of the container image that the monitoring job runs.
modelQualityAppSpecification_imageUri :: Lens.Lens' ModelQualityAppSpecification Prelude.Text
modelQualityAppSpecification_imageUri = Lens.lens (\ModelQualityAppSpecification' {imageUri} -> imageUri) (\s@ModelQualityAppSpecification' {} a -> s {imageUri = a} :: ModelQualityAppSpecification)

instance Core.FromJSON ModelQualityAppSpecification where
  parseJSON =
    Core.withObject
      "ModelQualityAppSpecification"
      ( \x ->
          ModelQualityAppSpecification'
            Prelude.<$> (x Core..:? "ContainerArguments")
            Prelude.<*> (x Core..:? "ContainerEntrypoint")
            Prelude.<*> (x Core..:? "PostAnalyticsProcessorSourceUri")
            Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RecordPreprocessorSourceUri")
            Prelude.<*> (x Core..:? "ProblemType")
            Prelude.<*> (x Core..: "ImageUri")
      )

instance
  Prelude.Hashable
    ModelQualityAppSpecification

instance Prelude.NFData ModelQualityAppSpecification

instance Core.ToJSON ModelQualityAppSpecification where
  toJSON ModelQualityAppSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContainerArguments" Core..=)
              Prelude.<$> containerArguments,
            ("ContainerEntrypoint" Core..=)
              Prelude.<$> containerEntrypoint,
            ("PostAnalyticsProcessorSourceUri" Core..=)
              Prelude.<$> postAnalyticsProcessorSourceUri,
            ("Environment" Core..=) Prelude.<$> environment,
            ("RecordPreprocessorSourceUri" Core..=)
              Prelude.<$> recordPreprocessorSourceUri,
            ("ProblemType" Core..=) Prelude.<$> problemType,
            Prelude.Just ("ImageUri" Core..= imageUri)
          ]
      )
