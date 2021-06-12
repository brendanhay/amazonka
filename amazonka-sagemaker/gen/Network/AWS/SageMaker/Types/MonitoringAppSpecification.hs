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
-- Module      : Network.AWS.SageMaker.Types.MonitoringAppSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringAppSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Container image configuration object for the monitoring job.
--
-- /See:/ 'newMonitoringAppSpecification' smart constructor.
data MonitoringAppSpecification = MonitoringAppSpecification'
  { -- | An array of arguments for the container used to run the monitoring job.
    containerArguments :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Specifies the entrypoint for a container used to run the monitoring job.
    containerEntrypoint :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | An Amazon S3 URI to a script that is called after analysis has been
    -- performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceUri :: Core.Maybe Core.Text,
    -- | An Amazon S3 URI to a script that is called per row prior to running
    -- analysis. It can base64 decode the payload and convert it into a flatted
    -- json so that the built-in container can use the converted data.
    -- Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceUri :: Core.Maybe Core.Text,
    -- | The container image to be run by the monitoring job.
    imageUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  MonitoringAppSpecification
newMonitoringAppSpecification pImageUri_ =
  MonitoringAppSpecification'
    { containerArguments =
        Core.Nothing,
      containerEntrypoint = Core.Nothing,
      postAnalyticsProcessorSourceUri = Core.Nothing,
      recordPreprocessorSourceUri = Core.Nothing,
      imageUri = pImageUri_
    }

-- | An array of arguments for the container used to run the monitoring job.
monitoringAppSpecification_containerArguments :: Lens.Lens' MonitoringAppSpecification (Core.Maybe (Core.NonEmpty Core.Text))
monitoringAppSpecification_containerArguments = Lens.lens (\MonitoringAppSpecification' {containerArguments} -> containerArguments) (\s@MonitoringAppSpecification' {} a -> s {containerArguments = a} :: MonitoringAppSpecification) Core.. Lens.mapping Lens._Coerce

-- | Specifies the entrypoint for a container used to run the monitoring job.
monitoringAppSpecification_containerEntrypoint :: Lens.Lens' MonitoringAppSpecification (Core.Maybe (Core.NonEmpty Core.Text))
monitoringAppSpecification_containerEntrypoint = Lens.lens (\MonitoringAppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@MonitoringAppSpecification' {} a -> s {containerEntrypoint = a} :: MonitoringAppSpecification) Core.. Lens.mapping Lens._Coerce

-- | An Amazon S3 URI to a script that is called after analysis has been
-- performed. Applicable only for the built-in (first party) containers.
monitoringAppSpecification_postAnalyticsProcessorSourceUri :: Lens.Lens' MonitoringAppSpecification (Core.Maybe Core.Text)
monitoringAppSpecification_postAnalyticsProcessorSourceUri = Lens.lens (\MonitoringAppSpecification' {postAnalyticsProcessorSourceUri} -> postAnalyticsProcessorSourceUri) (\s@MonitoringAppSpecification' {} a -> s {postAnalyticsProcessorSourceUri = a} :: MonitoringAppSpecification)

-- | An Amazon S3 URI to a script that is called per row prior to running
-- analysis. It can base64 decode the payload and convert it into a flatted
-- json so that the built-in container can use the converted data.
-- Applicable only for the built-in (first party) containers.
monitoringAppSpecification_recordPreprocessorSourceUri :: Lens.Lens' MonitoringAppSpecification (Core.Maybe Core.Text)
monitoringAppSpecification_recordPreprocessorSourceUri = Lens.lens (\MonitoringAppSpecification' {recordPreprocessorSourceUri} -> recordPreprocessorSourceUri) (\s@MonitoringAppSpecification' {} a -> s {recordPreprocessorSourceUri = a} :: MonitoringAppSpecification)

-- | The container image to be run by the monitoring job.
monitoringAppSpecification_imageUri :: Lens.Lens' MonitoringAppSpecification Core.Text
monitoringAppSpecification_imageUri = Lens.lens (\MonitoringAppSpecification' {imageUri} -> imageUri) (\s@MonitoringAppSpecification' {} a -> s {imageUri = a} :: MonitoringAppSpecification)

instance Core.FromJSON MonitoringAppSpecification where
  parseJSON =
    Core.withObject
      "MonitoringAppSpecification"
      ( \x ->
          MonitoringAppSpecification'
            Core.<$> (x Core..:? "ContainerArguments")
            Core.<*> (x Core..:? "ContainerEntrypoint")
            Core.<*> (x Core..:? "PostAnalyticsProcessorSourceUri")
            Core.<*> (x Core..:? "RecordPreprocessorSourceUri")
            Core.<*> (x Core..: "ImageUri")
      )

instance Core.Hashable MonitoringAppSpecification

instance Core.NFData MonitoringAppSpecification

instance Core.ToJSON MonitoringAppSpecification where
  toJSON MonitoringAppSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContainerArguments" Core..=)
              Core.<$> containerArguments,
            ("ContainerEntrypoint" Core..=)
              Core.<$> containerEntrypoint,
            ("PostAnalyticsProcessorSourceUri" Core..=)
              Core.<$> postAnalyticsProcessorSourceUri,
            ("RecordPreprocessorSourceUri" Core..=)
              Core.<$> recordPreprocessorSourceUri,
            Core.Just ("ImageUri" Core..= imageUri)
          ]
      )
