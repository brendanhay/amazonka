{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
  ( DatasetContentDeliveryDestination (..),

    -- * Smart constructor
    mkDatasetContentDeliveryDestination,

    -- * Lenses
    dcddIotEventsDestinationConfiguration,
    dcddS3DestinationConfiguration,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration as Types
import qualified Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The destination to which dataset contents are delivered.
--
-- /See:/ 'mkDatasetContentDeliveryDestination' smart constructor.
data DatasetContentDeliveryDestination = DatasetContentDeliveryDestination'
  { -- | Configuration information for delivery of dataset contents to AWS IoT Events.
    iotEventsDestinationConfiguration :: Core.Maybe Types.IotEventsDestinationConfiguration,
    -- | Configuration information for delivery of dataset contents to Amazon S3.
    s3DestinationConfiguration :: Core.Maybe Types.S3DestinationConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatasetContentDeliveryDestination' value with any optional fields omitted.
mkDatasetContentDeliveryDestination ::
  DatasetContentDeliveryDestination
mkDatasetContentDeliveryDestination =
  DatasetContentDeliveryDestination'
    { iotEventsDestinationConfiguration =
        Core.Nothing,
      s3DestinationConfiguration = Core.Nothing
    }

-- | Configuration information for delivery of dataset contents to AWS IoT Events.
--
-- /Note:/ Consider using 'iotEventsDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcddIotEventsDestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Core.Maybe Types.IotEventsDestinationConfiguration)
dcddIotEventsDestinationConfiguration = Lens.field @"iotEventsDestinationConfiguration"
{-# DEPRECATED dcddIotEventsDestinationConfiguration "Use generic-lens or generic-optics with 'iotEventsDestinationConfiguration' instead." #-}

-- | Configuration information for delivery of dataset contents to Amazon S3.
--
-- /Note:/ Consider using 's3DestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcddS3DestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Core.Maybe Types.S3DestinationConfiguration)
dcddS3DestinationConfiguration = Lens.field @"s3DestinationConfiguration"
{-# DEPRECATED dcddS3DestinationConfiguration "Use generic-lens or generic-optics with 's3DestinationConfiguration' instead." #-}

instance Core.FromJSON DatasetContentDeliveryDestination where
  toJSON DatasetContentDeliveryDestination {..} =
    Core.object
      ( Core.catMaybes
          [ ("iotEventsDestinationConfiguration" Core..=)
              Core.<$> iotEventsDestinationConfiguration,
            ("s3DestinationConfiguration" Core..=)
              Core.<$> s3DestinationConfiguration
          ]
      )

instance Core.FromJSON DatasetContentDeliveryDestination where
  parseJSON =
    Core.withObject "DatasetContentDeliveryDestination" Core.$
      \x ->
        DatasetContentDeliveryDestination'
          Core.<$> (x Core..:? "iotEventsDestinationConfiguration")
          Core.<*> (x Core..:? "s3DestinationConfiguration")
