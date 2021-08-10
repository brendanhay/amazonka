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
-- Module      : Network.AWS.SageMaker.Types.DataQualityAppSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataQualityAppSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the container that a data quality monitoring job runs.
--
-- /See:/ 'newDataQualityAppSpecification' smart constructor.
data DataQualityAppSpecification = DataQualityAppSpecification'
  { -- | The arguments to send to the container that the monitoring job runs.
    containerArguments :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The entrypoint for a container used to run a monitoring job.
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
-- 'containerArguments', 'dataQualityAppSpecification_containerArguments' - The arguments to send to the container that the monitoring job runs.
--
-- 'containerEntrypoint', 'dataQualityAppSpecification_containerEntrypoint' - The entrypoint for a container used to run a monitoring job.
--
-- 'postAnalyticsProcessorSourceUri', 'dataQualityAppSpecification_postAnalyticsProcessorSourceUri' - An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
--
-- 'environment', 'dataQualityAppSpecification_environment' - Sets the environment variables in the container that the monitoring job
-- runs.
--
-- 'recordPreprocessorSourceUri', 'dataQualityAppSpecification_recordPreprocessorSourceUri' - An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
--
-- 'imageUri', 'dataQualityAppSpecification_imageUri' - The container image that the data quality monitoring job runs.
newDataQualityAppSpecification ::
  -- | 'imageUri'
  Prelude.Text ->
  DataQualityAppSpecification
newDataQualityAppSpecification pImageUri_ =
  DataQualityAppSpecification'
    { containerArguments =
        Prelude.Nothing,
      containerEntrypoint = Prelude.Nothing,
      postAnalyticsProcessorSourceUri =
        Prelude.Nothing,
      environment = Prelude.Nothing,
      recordPreprocessorSourceUri = Prelude.Nothing,
      imageUri = pImageUri_
    }

-- | The arguments to send to the container that the monitoring job runs.
dataQualityAppSpecification_containerArguments :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
dataQualityAppSpecification_containerArguments = Lens.lens (\DataQualityAppSpecification' {containerArguments} -> containerArguments) (\s@DataQualityAppSpecification' {} a -> s {containerArguments = a} :: DataQualityAppSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | The entrypoint for a container used to run a monitoring job.
dataQualityAppSpecification_containerEntrypoint :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
dataQualityAppSpecification_containerEntrypoint = Lens.lens (\DataQualityAppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@DataQualityAppSpecification' {} a -> s {containerEntrypoint = a} :: DataQualityAppSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
dataQualityAppSpecification_postAnalyticsProcessorSourceUri :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe Prelude.Text)
dataQualityAppSpecification_postAnalyticsProcessorSourceUri = Lens.lens (\DataQualityAppSpecification' {postAnalyticsProcessorSourceUri} -> postAnalyticsProcessorSourceUri) (\s@DataQualityAppSpecification' {} a -> s {postAnalyticsProcessorSourceUri = a} :: DataQualityAppSpecification)

-- | Sets the environment variables in the container that the monitoring job
-- runs.
dataQualityAppSpecification_environment :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dataQualityAppSpecification_environment = Lens.lens (\DataQualityAppSpecification' {environment} -> environment) (\s@DataQualityAppSpecification' {} a -> s {environment = a} :: DataQualityAppSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
dataQualityAppSpecification_recordPreprocessorSourceUri :: Lens.Lens' DataQualityAppSpecification (Prelude.Maybe Prelude.Text)
dataQualityAppSpecification_recordPreprocessorSourceUri = Lens.lens (\DataQualityAppSpecification' {recordPreprocessorSourceUri} -> recordPreprocessorSourceUri) (\s@DataQualityAppSpecification' {} a -> s {recordPreprocessorSourceUri = a} :: DataQualityAppSpecification)

-- | The container image that the data quality monitoring job runs.
dataQualityAppSpecification_imageUri :: Lens.Lens' DataQualityAppSpecification Prelude.Text
dataQualityAppSpecification_imageUri = Lens.lens (\DataQualityAppSpecification' {imageUri} -> imageUri) (\s@DataQualityAppSpecification' {} a -> s {imageUri = a} :: DataQualityAppSpecification)

instance Core.FromJSON DataQualityAppSpecification where
  parseJSON =
    Core.withObject
      "DataQualityAppSpecification"
      ( \x ->
          DataQualityAppSpecification'
            Prelude.<$> (x Core..:? "ContainerArguments")
            Prelude.<*> (x Core..:? "ContainerEntrypoint")
            Prelude.<*> (x Core..:? "PostAnalyticsProcessorSourceUri")
            Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RecordPreprocessorSourceUri")
            Prelude.<*> (x Core..: "ImageUri")
      )

instance Prelude.Hashable DataQualityAppSpecification

instance Prelude.NFData DataQualityAppSpecification

instance Core.ToJSON DataQualityAppSpecification where
  toJSON DataQualityAppSpecification' {..} =
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
            Prelude.Just ("ImageUri" Core..= imageUri)
          ]
      )
