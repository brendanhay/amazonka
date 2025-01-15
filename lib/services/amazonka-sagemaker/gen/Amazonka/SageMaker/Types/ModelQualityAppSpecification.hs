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
-- Module      : Amazonka.SageMaker.Types.ModelQualityAppSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelQualityAppSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringProblemType

-- | Container image configuration object for the monitoring job.
--
-- /See:/ 'newModelQualityAppSpecification' smart constructor.
data ModelQualityAppSpecification = ModelQualityAppSpecification'
  { -- | An array of arguments for the container used to run the monitoring job.
    containerArguments :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies the entrypoint for a container that the monitoring job runs.
    containerEntrypoint :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Sets the environment variables in the container that the monitoring job
    -- runs.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An Amazon S3 URI to a script that is called after analysis has been
    -- performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceUri :: Prelude.Maybe Prelude.Text,
    -- | The machine learning problem type of the model that the monitoring job
    -- monitors.
    problemType :: Prelude.Maybe MonitoringProblemType,
    -- | An Amazon S3 URI to a script that is called per row prior to running
    -- analysis. It can base64 decode the payload and convert it into a flatted
    -- json so that the built-in container can use the converted data.
    -- Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceUri :: Prelude.Maybe Prelude.Text,
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
-- 'environment', 'modelQualityAppSpecification_environment' - Sets the environment variables in the container that the monitoring job
-- runs.
--
-- 'postAnalyticsProcessorSourceUri', 'modelQualityAppSpecification_postAnalyticsProcessorSourceUri' - An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
--
-- 'problemType', 'modelQualityAppSpecification_problemType' - The machine learning problem type of the model that the monitoring job
-- monitors.
--
-- 'recordPreprocessorSourceUri', 'modelQualityAppSpecification_recordPreprocessorSourceUri' - An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
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
      environment = Prelude.Nothing,
      postAnalyticsProcessorSourceUri =
        Prelude.Nothing,
      problemType = Prelude.Nothing,
      recordPreprocessorSourceUri = Prelude.Nothing,
      imageUri = pImageUri_
    }

-- | An array of arguments for the container used to run the monitoring job.
modelQualityAppSpecification_containerArguments :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modelQualityAppSpecification_containerArguments = Lens.lens (\ModelQualityAppSpecification' {containerArguments} -> containerArguments) (\s@ModelQualityAppSpecification' {} a -> s {containerArguments = a} :: ModelQualityAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the entrypoint for a container that the monitoring job runs.
modelQualityAppSpecification_containerEntrypoint :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modelQualityAppSpecification_containerEntrypoint = Lens.lens (\ModelQualityAppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@ModelQualityAppSpecification' {} a -> s {containerEntrypoint = a} :: ModelQualityAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Sets the environment variables in the container that the monitoring job
-- runs.
modelQualityAppSpecification_environment :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
modelQualityAppSpecification_environment = Lens.lens (\ModelQualityAppSpecification' {environment} -> environment) (\s@ModelQualityAppSpecification' {} a -> s {environment = a} :: ModelQualityAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
modelQualityAppSpecification_postAnalyticsProcessorSourceUri :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe Prelude.Text)
modelQualityAppSpecification_postAnalyticsProcessorSourceUri = Lens.lens (\ModelQualityAppSpecification' {postAnalyticsProcessorSourceUri} -> postAnalyticsProcessorSourceUri) (\s@ModelQualityAppSpecification' {} a -> s {postAnalyticsProcessorSourceUri = a} :: ModelQualityAppSpecification)

-- | The machine learning problem type of the model that the monitoring job
-- monitors.
modelQualityAppSpecification_problemType :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe MonitoringProblemType)
modelQualityAppSpecification_problemType = Lens.lens (\ModelQualityAppSpecification' {problemType} -> problemType) (\s@ModelQualityAppSpecification' {} a -> s {problemType = a} :: ModelQualityAppSpecification)

-- | An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
modelQualityAppSpecification_recordPreprocessorSourceUri :: Lens.Lens' ModelQualityAppSpecification (Prelude.Maybe Prelude.Text)
modelQualityAppSpecification_recordPreprocessorSourceUri = Lens.lens (\ModelQualityAppSpecification' {recordPreprocessorSourceUri} -> recordPreprocessorSourceUri) (\s@ModelQualityAppSpecification' {} a -> s {recordPreprocessorSourceUri = a} :: ModelQualityAppSpecification)

-- | The address of the container image that the monitoring job runs.
modelQualityAppSpecification_imageUri :: Lens.Lens' ModelQualityAppSpecification Prelude.Text
modelQualityAppSpecification_imageUri = Lens.lens (\ModelQualityAppSpecification' {imageUri} -> imageUri) (\s@ModelQualityAppSpecification' {} a -> s {imageUri = a} :: ModelQualityAppSpecification)

instance Data.FromJSON ModelQualityAppSpecification where
  parseJSON =
    Data.withObject
      "ModelQualityAppSpecification"
      ( \x ->
          ModelQualityAppSpecification'
            Prelude.<$> (x Data..:? "ContainerArguments")
            Prelude.<*> (x Data..:? "ContainerEntrypoint")
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PostAnalyticsProcessorSourceUri")
            Prelude.<*> (x Data..:? "ProblemType")
            Prelude.<*> (x Data..:? "RecordPreprocessorSourceUri")
            Prelude.<*> (x Data..: "ImageUri")
      )

instance
  Prelude.Hashable
    ModelQualityAppSpecification
  where
  hashWithSalt _salt ModelQualityAppSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` containerArguments
      `Prelude.hashWithSalt` containerEntrypoint
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` postAnalyticsProcessorSourceUri
      `Prelude.hashWithSalt` problemType
      `Prelude.hashWithSalt` recordPreprocessorSourceUri
      `Prelude.hashWithSalt` imageUri

instance Prelude.NFData ModelQualityAppSpecification where
  rnf ModelQualityAppSpecification' {..} =
    Prelude.rnf containerArguments `Prelude.seq`
      Prelude.rnf containerEntrypoint `Prelude.seq`
        Prelude.rnf environment `Prelude.seq`
          Prelude.rnf postAnalyticsProcessorSourceUri `Prelude.seq`
            Prelude.rnf problemType `Prelude.seq`
              Prelude.rnf recordPreprocessorSourceUri `Prelude.seq`
                Prelude.rnf imageUri

instance Data.ToJSON ModelQualityAppSpecification where
  toJSON ModelQualityAppSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContainerArguments" Data..=)
              Prelude.<$> containerArguments,
            ("ContainerEntrypoint" Data..=)
              Prelude.<$> containerEntrypoint,
            ("Environment" Data..=) Prelude.<$> environment,
            ("PostAnalyticsProcessorSourceUri" Data..=)
              Prelude.<$> postAnalyticsProcessorSourceUri,
            ("ProblemType" Data..=) Prelude.<$> problemType,
            ("RecordPreprocessorSourceUri" Data..=)
              Prelude.<$> recordPreprocessorSourceUri,
            Prelude.Just ("ImageUri" Data..= imageUri)
          ]
      )
