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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringAppSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Container image configuration object for the monitoring job.
--
-- /See:/ 'newMonitoringAppSpecification' smart constructor.
data MonitoringAppSpecification = MonitoringAppSpecification'
  { -- | Specifies the entrypoint for a container used to run the monitoring job.
    containerEntrypoint :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An Amazon S3 URI to a script that is called per row prior to running
    -- analysis. It can base64 decode the payload and convert it into a flatted
    -- json so that the built-in container can use the converted data.
    -- Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceUri :: Prelude.Maybe Prelude.Text,
    -- | An array of arguments for the container used to run the monitoring job.
    containerArguments :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An Amazon S3 URI to a script that is called after analysis has been
    -- performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceUri :: Prelude.Maybe Prelude.Text,
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
-- 'containerEntrypoint', 'monitoringAppSpecification_containerEntrypoint' - Specifies the entrypoint for a container used to run the monitoring job.
--
-- 'recordPreprocessorSourceUri', 'monitoringAppSpecification_recordPreprocessorSourceUri' - An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
--
-- 'containerArguments', 'monitoringAppSpecification_containerArguments' - An array of arguments for the container used to run the monitoring job.
--
-- 'postAnalyticsProcessorSourceUri', 'monitoringAppSpecification_postAnalyticsProcessorSourceUri' - An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
--
-- 'imageUri', 'monitoringAppSpecification_imageUri' - The container image to be run by the monitoring job.
newMonitoringAppSpecification ::
  -- | 'imageUri'
  Prelude.Text ->
  MonitoringAppSpecification
newMonitoringAppSpecification pImageUri_ =
  MonitoringAppSpecification'
    { containerEntrypoint =
        Prelude.Nothing,
      recordPreprocessorSourceUri = Prelude.Nothing,
      containerArguments = Prelude.Nothing,
      postAnalyticsProcessorSourceUri =
        Prelude.Nothing,
      imageUri = pImageUri_
    }

-- | Specifies the entrypoint for a container used to run the monitoring job.
monitoringAppSpecification_containerEntrypoint :: Lens.Lens' MonitoringAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
monitoringAppSpecification_containerEntrypoint = Lens.lens (\MonitoringAppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@MonitoringAppSpecification' {} a -> s {containerEntrypoint = a} :: MonitoringAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
monitoringAppSpecification_recordPreprocessorSourceUri :: Lens.Lens' MonitoringAppSpecification (Prelude.Maybe Prelude.Text)
monitoringAppSpecification_recordPreprocessorSourceUri = Lens.lens (\MonitoringAppSpecification' {recordPreprocessorSourceUri} -> recordPreprocessorSourceUri) (\s@MonitoringAppSpecification' {} a -> s {recordPreprocessorSourceUri = a} :: MonitoringAppSpecification)

-- | An array of arguments for the container used to run the monitoring job.
monitoringAppSpecification_containerArguments :: Lens.Lens' MonitoringAppSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
monitoringAppSpecification_containerArguments = Lens.lens (\MonitoringAppSpecification' {containerArguments} -> containerArguments) (\s@MonitoringAppSpecification' {} a -> s {containerArguments = a} :: MonitoringAppSpecification) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
monitoringAppSpecification_postAnalyticsProcessorSourceUri :: Lens.Lens' MonitoringAppSpecification (Prelude.Maybe Prelude.Text)
monitoringAppSpecification_postAnalyticsProcessorSourceUri = Lens.lens (\MonitoringAppSpecification' {postAnalyticsProcessorSourceUri} -> postAnalyticsProcessorSourceUri) (\s@MonitoringAppSpecification' {} a -> s {postAnalyticsProcessorSourceUri = a} :: MonitoringAppSpecification)

-- | The container image to be run by the monitoring job.
monitoringAppSpecification_imageUri :: Lens.Lens' MonitoringAppSpecification Prelude.Text
monitoringAppSpecification_imageUri = Lens.lens (\MonitoringAppSpecification' {imageUri} -> imageUri) (\s@MonitoringAppSpecification' {} a -> s {imageUri = a} :: MonitoringAppSpecification)

instance Core.FromJSON MonitoringAppSpecification where
  parseJSON =
    Core.withObject
      "MonitoringAppSpecification"
      ( \x ->
          MonitoringAppSpecification'
            Prelude.<$> (x Core..:? "ContainerEntrypoint")
            Prelude.<*> (x Core..:? "RecordPreprocessorSourceUri")
            Prelude.<*> (x Core..:? "ContainerArguments")
            Prelude.<*> (x Core..:? "PostAnalyticsProcessorSourceUri")
            Prelude.<*> (x Core..: "ImageUri")
      )

instance Prelude.Hashable MonitoringAppSpecification where
  hashWithSalt _salt MonitoringAppSpecification' {..} =
    _salt `Prelude.hashWithSalt` containerEntrypoint
      `Prelude.hashWithSalt` recordPreprocessorSourceUri
      `Prelude.hashWithSalt` containerArguments
      `Prelude.hashWithSalt` postAnalyticsProcessorSourceUri
      `Prelude.hashWithSalt` imageUri

instance Prelude.NFData MonitoringAppSpecification where
  rnf MonitoringAppSpecification' {..} =
    Prelude.rnf containerEntrypoint
      `Prelude.seq` Prelude.rnf recordPreprocessorSourceUri
      `Prelude.seq` Prelude.rnf containerArguments
      `Prelude.seq` Prelude.rnf postAnalyticsProcessorSourceUri
      `Prelude.seq` Prelude.rnf imageUri

instance Core.ToJSON MonitoringAppSpecification where
  toJSON MonitoringAppSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContainerEntrypoint" Core..=)
              Prelude.<$> containerEntrypoint,
            ("RecordPreprocessorSourceUri" Core..=)
              Prelude.<$> recordPreprocessorSourceUri,
            ("ContainerArguments" Core..=)
              Prelude.<$> containerArguments,
            ("PostAnalyticsProcessorSourceUri" Core..=)
              Prelude.<$> postAnalyticsProcessorSourceUri,
            Prelude.Just ("ImageUri" Core..= imageUri)
          ]
      )
