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
-- Module      : Amazonka.SageMaker.Types.DataQualityAppSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DataQualityAppSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the container that a data quality monitoring job runs.
--
-- /See:/ 'newDataQualityAppSpecification' smart constructor.
data DataQualityAppSpecification = DataQualityAppSpecification'
  { -- | The entrypoint for a container used to run a monitoring job.
    containerEntrypoint :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An Amazon S3 URI to a script that is called per row prior to running
    -- analysis. It can base64 decode the payload and convert it into a flatted
    -- json so that the built-in container can use the converted data.
    -- Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceUri :: Prelude.Maybe Prelude.Text,
    -- | Sets the environment variables in the container that the monitoring job
    -- runs.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The arguments to send to the container that the monitoring job runs.
    containerArguments :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An Amazon S3 URI to a script that is called after analysis has been
    -- performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceUri :: Prelude.Maybe Prelude.Text,
    -- | The container image that the data quality monitoring job runs.
    imageUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityAppSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerEntrypoint', 'dataQualityAppSpecification_containerEntrypoint' - The entrypoint for a container used to run a monitoring job.
--
-- 'recordPreprocessorSourceUri', 'dataQualityAppSpecification_recordPreprocessorSourceUri' - An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
--
-- 'environment', 'dataQualityAppSpecification_environment' - Sets the environment variables in the container that the monitoring job
-- runs.
--
-- 'containerArguments', 'dataQualityAppSpecification_containerArguments' - The arguments to send to the container that the monitoring job runs.
--
-- 'postAnalyticsProcessorSourceUri', 'dataQualityAppSpecification_postAnalyticsProcessorSourceUri' - An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
--
-- 'imageUri', 'dataQualityAppSpecification_imageUri' - The container image that the data quality monitoring job runs.
newDataQualityAppSpecification ::
  -- | 'imageUri'
  Prelude.Text ->
  DataQualityAppSpecification
newDataQualityAppSpecification pImageUri_ =
  DataQualityAppSpecification'
    { containerEntrypoint =
        Prelude.Nothing,
      recordPreprocessorSourceUri = Prelude.Nothing,
      environment = Prelude.Nothing,
      containerArguments = Prelude.Nothing,
      postAnalyticsProcessorSourceUri =
        Prelude.Nothing,
      imageUri = pImageUri_
    }

-- | The entrypoint for a container used to run a monitoring job.
dataQualityAppSpecification_containerEntrypoint :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
dataQualityAppSpecification_containerEntrypoint = Lens.lens (\DataQualityAppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@DataQualityAppSpecification' {} a -> s {containerEntrypoint = a} :: DataQualityAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
dataQualityAppSpecification_recordPreprocessorSourceUri :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe Prelude.Text)
dataQualityAppSpecification_recordPreprocessorSourceUri = Lens.lens (\DataQualityAppSpecification' {recordPreprocessorSourceUri} -> recordPreprocessorSourceUri) (\s@DataQualityAppSpecification' {} a -> s {recordPreprocessorSourceUri = a} :: DataQualityAppSpecification)

-- | Sets the environment variables in the container that the monitoring job
-- runs.
dataQualityAppSpecification_environment :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dataQualityAppSpecification_environment = Lens.lens (\DataQualityAppSpecification' {environment} -> environment) (\s@DataQualityAppSpecification' {} a -> s {environment = a} :: DataQualityAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The arguments to send to the container that the monitoring job runs.
dataQualityAppSpecification_containerArguments :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
dataQualityAppSpecification_containerArguments = Lens.lens (\DataQualityAppSpecification' {containerArguments} -> containerArguments) (\s@DataQualityAppSpecification' {} a -> s {containerArguments = a} :: DataQualityAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
dataQualityAppSpecification_postAnalyticsProcessorSourceUri :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe Prelude.Text)
dataQualityAppSpecification_postAnalyticsProcessorSourceUri = Lens.lens (\DataQualityAppSpecification' {postAnalyticsProcessorSourceUri} -> postAnalyticsProcessorSourceUri) (\s@DataQualityAppSpecification' {} a -> s {postAnalyticsProcessorSourceUri = a} :: DataQualityAppSpecification)

-- | The container image that the data quality monitoring job runs.
dataQualityAppSpecification_imageUri :: Lens.Lens' DataQualityAppSpecification Prelude.Text
dataQualityAppSpecification_imageUri = Lens.lens (\DataQualityAppSpecification' {imageUri} -> imageUri) (\s@DataQualityAppSpecification' {} a -> s {imageUri = a} :: DataQualityAppSpecification)

instance Data.FromJSON DataQualityAppSpecification where
  parseJSON =
    Data.withObject
      "DataQualityAppSpecification"
      ( \x ->
          DataQualityAppSpecification'
            Prelude.<$> (x Data..:? "ContainerEntrypoint")
            Prelude.<*> (x Data..:? "RecordPreprocessorSourceUri")
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ContainerArguments")
            Prelude.<*> (x Data..:? "PostAnalyticsProcessorSourceUri")
            Prelude.<*> (x Data..: "ImageUri")
      )

instance Prelude.Hashable DataQualityAppSpecification where
  hashWithSalt _salt DataQualityAppSpecification' {..} =
    _salt `Prelude.hashWithSalt` containerEntrypoint
      `Prelude.hashWithSalt` recordPreprocessorSourceUri
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` containerArguments
      `Prelude.hashWithSalt` postAnalyticsProcessorSourceUri
      `Prelude.hashWithSalt` imageUri

instance Prelude.NFData DataQualityAppSpecification where
  rnf DataQualityAppSpecification' {..} =
    Prelude.rnf containerEntrypoint
      `Prelude.seq` Prelude.rnf recordPreprocessorSourceUri
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf containerArguments
      `Prelude.seq` Prelude.rnf postAnalyticsProcessorSourceUri
      `Prelude.seq` Prelude.rnf imageUri

instance Data.ToJSON DataQualityAppSpecification where
  toJSON DataQualityAppSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContainerEntrypoint" Data..=)
              Prelude.<$> containerEntrypoint,
            ("RecordPreprocessorSourceUri" Data..=)
              Prelude.<$> recordPreprocessorSourceUri,
            ("Environment" Data..=) Prelude.<$> environment,
            ("ContainerArguments" Data..=)
              Prelude.<$> containerArguments,
            ("PostAnalyticsProcessorSourceUri" Data..=)
              Prelude.<$> postAnalyticsProcessorSourceUri,
            Prelude.Just ("ImageUri" Data..= imageUri)
          ]
      )
