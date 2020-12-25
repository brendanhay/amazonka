{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringAppSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringAppSpecification
  ( MonitoringAppSpecification (..),

    -- * Smart constructor
    mkMonitoringAppSpecification,

    -- * Lenses
    masImageUri,
    masContainerArguments,
    masContainerEntrypoint,
    masPostAnalyticsProcessorSourceUri,
    masRecordPreprocessorSourceUri,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ContainerArgument as Types
import qualified Network.AWS.SageMaker.Types.ContainerEntrypointString as Types
import qualified Network.AWS.SageMaker.Types.ImageUri as Types
import qualified Network.AWS.SageMaker.Types.PostAnalyticsProcessorSourceUri as Types
import qualified Network.AWS.SageMaker.Types.RecordPreprocessorSourceUri as Types

-- | Container image configuration object for the monitoring job.
--
-- /See:/ 'mkMonitoringAppSpecification' smart constructor.
data MonitoringAppSpecification = MonitoringAppSpecification'
  { -- | The container image to be run by the monitoring job.
    imageUri :: Types.ImageUri,
    -- | An array of arguments for the container used to run the monitoring job.
    containerArguments :: Core.Maybe (Core.NonEmpty Types.ContainerArgument),
    -- | Specifies the entrypoint for a container used to run the monitoring job.
    containerEntrypoint :: Core.Maybe (Core.NonEmpty Types.ContainerEntrypointString),
    -- | An Amazon S3 URI to a script that is called after analysis has been performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceUri :: Core.Maybe Types.PostAnalyticsProcessorSourceUri,
    -- | An Amazon S3 URI to a script that is called per row prior to running analysis. It can base64 decode the payload and convert it into a flatted json so that the built-in container can use the converted data. Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceUri :: Core.Maybe Types.RecordPreprocessorSourceUri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringAppSpecification' value with any optional fields omitted.
mkMonitoringAppSpecification ::
  -- | 'imageUri'
  Types.ImageUri ->
  MonitoringAppSpecification
mkMonitoringAppSpecification imageUri =
  MonitoringAppSpecification'
    { imageUri,
      containerArguments = Core.Nothing,
      containerEntrypoint = Core.Nothing,
      postAnalyticsProcessorSourceUri = Core.Nothing,
      recordPreprocessorSourceUri = Core.Nothing
    }

-- | The container image to be run by the monitoring job.
--
-- /Note:/ Consider using 'imageUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masImageUri :: Lens.Lens' MonitoringAppSpecification Types.ImageUri
masImageUri = Lens.field @"imageUri"
{-# DEPRECATED masImageUri "Use generic-lens or generic-optics with 'imageUri' instead." #-}

-- | An array of arguments for the container used to run the monitoring job.
--
-- /Note:/ Consider using 'containerArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masContainerArguments :: Lens.Lens' MonitoringAppSpecification (Core.Maybe (Core.NonEmpty Types.ContainerArgument))
masContainerArguments = Lens.field @"containerArguments"
{-# DEPRECATED masContainerArguments "Use generic-lens or generic-optics with 'containerArguments' instead." #-}

-- | Specifies the entrypoint for a container used to run the monitoring job.
--
-- /Note:/ Consider using 'containerEntrypoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masContainerEntrypoint :: Lens.Lens' MonitoringAppSpecification (Core.Maybe (Core.NonEmpty Types.ContainerEntrypointString))
masContainerEntrypoint = Lens.field @"containerEntrypoint"
{-# DEPRECATED masContainerEntrypoint "Use generic-lens or generic-optics with 'containerEntrypoint' instead." #-}

-- | An Amazon S3 URI to a script that is called after analysis has been performed. Applicable only for the built-in (first party) containers.
--
-- /Note:/ Consider using 'postAnalyticsProcessorSourceUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masPostAnalyticsProcessorSourceUri :: Lens.Lens' MonitoringAppSpecification (Core.Maybe Types.PostAnalyticsProcessorSourceUri)
masPostAnalyticsProcessorSourceUri = Lens.field @"postAnalyticsProcessorSourceUri"
{-# DEPRECATED masPostAnalyticsProcessorSourceUri "Use generic-lens or generic-optics with 'postAnalyticsProcessorSourceUri' instead." #-}

-- | An Amazon S3 URI to a script that is called per row prior to running analysis. It can base64 decode the payload and convert it into a flatted json so that the built-in container can use the converted data. Applicable only for the built-in (first party) containers.
--
-- /Note:/ Consider using 'recordPreprocessorSourceUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masRecordPreprocessorSourceUri :: Lens.Lens' MonitoringAppSpecification (Core.Maybe Types.RecordPreprocessorSourceUri)
masRecordPreprocessorSourceUri = Lens.field @"recordPreprocessorSourceUri"
{-# DEPRECATED masRecordPreprocessorSourceUri "Use generic-lens or generic-optics with 'recordPreprocessorSourceUri' instead." #-}

instance Core.FromJSON MonitoringAppSpecification where
  toJSON MonitoringAppSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ImageUri" Core..= imageUri),
            ("ContainerArguments" Core..=) Core.<$> containerArguments,
            ("ContainerEntrypoint" Core..=) Core.<$> containerEntrypoint,
            ("PostAnalyticsProcessorSourceUri" Core..=)
              Core.<$> postAnalyticsProcessorSourceUri,
            ("RecordPreprocessorSourceUri" Core..=)
              Core.<$> recordPreprocessorSourceUri
          ]
      )

instance Core.FromJSON MonitoringAppSpecification where
  parseJSON =
    Core.withObject "MonitoringAppSpecification" Core.$
      \x ->
        MonitoringAppSpecification'
          Core.<$> (x Core..: "ImageUri")
          Core.<*> (x Core..:? "ContainerArguments")
          Core.<*> (x Core..:? "ContainerEntrypoint")
          Core.<*> (x Core..:? "PostAnalyticsProcessorSourceUri")
          Core.<*> (x Core..:? "RecordPreprocessorSourceUri")
