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
-- Module      : Amazonka.SageMaker.Types.MonitoringAppSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringAppSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container image configuration object for the monitoring job.
--
-- /See:/ 'newMonitoringAppSpecification' smart constructor.
data MonitoringAppSpecification = MonitoringAppSpecification'
  { -- | An array of arguments for the container used to run the monitoring job.
    containerArguments :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies the entrypoint for a container used to run the monitoring job.
    containerEntrypoint :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An Amazon S3 URI to a script that is called after analysis has been
    -- performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceUri :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 URI to a script that is called per row prior to running
    -- analysis. It can base64 decode the payload and convert it into a flatted
    -- json so that the built-in container can use the converted data.
    -- Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceUri :: Prelude.Maybe Prelude.Text,
    -- | The container image to be run by the monitoring job.
    imageUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringAppSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerArguments', 'monitoringAppSpecification_containerArguments' - An array of arguments for the container used to run the monitoring job.
--
-- 'containerEntrypoint', 'monitoringAppSpecification_containerEntrypoint' - Specifies the entrypoint for a container used to run the monitoring job.
--
-- 'postAnalyticsProcessorSourceUri', 'monitoringAppSpecification_postAnalyticsProcessorSourceUri' - An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
--
-- 'recordPreprocessorSourceUri', 'monitoringAppSpecification_recordPreprocessorSourceUri' - An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
--
-- 'imageUri', 'monitoringAppSpecification_imageUri' - The container image to be run by the monitoring job.
newMonitoringAppSpecification ::
  -- | 'imageUri'
  Prelude.Text ->
  MonitoringAppSpecification
newMonitoringAppSpecification pImageUri_ =
  MonitoringAppSpecification'
    { containerArguments =
        Prelude.Nothing,
      containerEntrypoint = Prelude.Nothing,
      postAnalyticsProcessorSourceUri =
        Prelude.Nothing,
      recordPreprocessorSourceUri = Prelude.Nothing,
      imageUri = pImageUri_
    }

-- | An array of arguments for the container used to run the monitoring job.
monitoringAppSpecification_containerArguments :: Lens.Lens' MonitoringAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
monitoringAppSpecification_containerArguments = Lens.lens (\MonitoringAppSpecification' {containerArguments} -> containerArguments) (\s@MonitoringAppSpecification' {} a -> s {containerArguments = a} :: MonitoringAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the entrypoint for a container used to run the monitoring job.
monitoringAppSpecification_containerEntrypoint :: Lens.Lens' MonitoringAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
monitoringAppSpecification_containerEntrypoint = Lens.lens (\MonitoringAppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@MonitoringAppSpecification' {} a -> s {containerEntrypoint = a} :: MonitoringAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
monitoringAppSpecification_postAnalyticsProcessorSourceUri :: Lens.Lens' MonitoringAppSpecification (Prelude.Maybe Prelude.Text)
monitoringAppSpecification_postAnalyticsProcessorSourceUri = Lens.lens (\MonitoringAppSpecification' {postAnalyticsProcessorSourceUri} -> postAnalyticsProcessorSourceUri) (\s@MonitoringAppSpecification' {} a -> s {postAnalyticsProcessorSourceUri = a} :: MonitoringAppSpecification)

-- | An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
monitoringAppSpecification_recordPreprocessorSourceUri :: Lens.Lens' MonitoringAppSpecification (Prelude.Maybe Prelude.Text)
monitoringAppSpecification_recordPreprocessorSourceUri = Lens.lens (\MonitoringAppSpecification' {recordPreprocessorSourceUri} -> recordPreprocessorSourceUri) (\s@MonitoringAppSpecification' {} a -> s {recordPreprocessorSourceUri = a} :: MonitoringAppSpecification)

-- | The container image to be run by the monitoring job.
monitoringAppSpecification_imageUri :: Lens.Lens' MonitoringAppSpecification Prelude.Text
monitoringAppSpecification_imageUri = Lens.lens (\MonitoringAppSpecification' {imageUri} -> imageUri) (\s@MonitoringAppSpecification' {} a -> s {imageUri = a} :: MonitoringAppSpecification)

instance Data.FromJSON MonitoringAppSpecification where
  parseJSON =
    Data.withObject
      "MonitoringAppSpecification"
      ( \x ->
          MonitoringAppSpecification'
            Prelude.<$> (x Data..:? "ContainerArguments")
            Prelude.<*> (x Data..:? "ContainerEntrypoint")
            Prelude.<*> (x Data..:? "PostAnalyticsProcessorSourceUri")
            Prelude.<*> (x Data..:? "RecordPreprocessorSourceUri")
            Prelude.<*> (x Data..: "ImageUri")
      )

instance Prelude.Hashable MonitoringAppSpecification where
  hashWithSalt _salt MonitoringAppSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` containerArguments
      `Prelude.hashWithSalt` containerEntrypoint
      `Prelude.hashWithSalt` postAnalyticsProcessorSourceUri
      `Prelude.hashWithSalt` recordPreprocessorSourceUri
      `Prelude.hashWithSalt` imageUri

instance Prelude.NFData MonitoringAppSpecification where
  rnf MonitoringAppSpecification' {..} =
    Prelude.rnf containerArguments
      `Prelude.seq` Prelude.rnf containerEntrypoint
      `Prelude.seq` Prelude.rnf postAnalyticsProcessorSourceUri
      `Prelude.seq` Prelude.rnf recordPreprocessorSourceUri
      `Prelude.seq` Prelude.rnf imageUri

instance Data.ToJSON MonitoringAppSpecification where
  toJSON MonitoringAppSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContainerArguments" Data..=)
              Prelude.<$> containerArguments,
            ("ContainerEntrypoint" Data..=)
              Prelude.<$> containerEntrypoint,
            ("PostAnalyticsProcessorSourceUri" Data..=)
              Prelude.<$> postAnalyticsProcessorSourceUri,
            ("RecordPreprocessorSourceUri" Data..=)
              Prelude.<$> recordPreprocessorSourceUri,
            Prelude.Just ("ImageUri" Data..= imageUri)
          ]
      )
