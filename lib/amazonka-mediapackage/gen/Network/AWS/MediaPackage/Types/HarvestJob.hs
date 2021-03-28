{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.HarvestJob
  ( HarvestJob (..)
  -- * Smart constructor
  , mkHarvestJob
  -- * Lenses
  , hjArn
  , hjChannelId
  , hjCreatedAt
  , hjEndTime
  , hjId
  , hjOriginEndpointId
  , hjS3Destination
  , hjStartTime
  , hjStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.S3Destination as Types
import qualified Network.AWS.MediaPackage.Types.Status as Types
import qualified Network.AWS.Prelude as Core

-- | A HarvestJob resource configuration
--
-- /See:/ 'mkHarvestJob' smart constructor.
data HarvestJob = HarvestJob'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) assigned to the HarvestJob.
  , channelId :: Core.Maybe Core.Text
    -- ^ The ID of the Channel that the HarvestJob will harvest from.
  , createdAt :: Core.Maybe Core.Text
    -- ^ The time the HarvestJob was submitted
  , endTime :: Core.Maybe Core.Text
    -- ^ The end of the time-window which will be harvested.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
  , originEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
  , s3Destination :: Core.Maybe Types.S3Destination
  , startTime :: Core.Maybe Core.Text
    -- ^ The start of the time-window which will be harvested.
  , status :: Core.Maybe Types.Status
    -- ^ The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HarvestJob' value with any optional fields omitted.
mkHarvestJob
    :: HarvestJob
mkHarvestJob
  = HarvestJob'{arn = Core.Nothing, channelId = Core.Nothing,
                createdAt = Core.Nothing, endTime = Core.Nothing,
                id = Core.Nothing, originEndpointId = Core.Nothing,
                s3Destination = Core.Nothing, startTime = Core.Nothing,
                status = Core.Nothing}

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjArn :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
hjArn = Lens.field @"arn"
{-# INLINEABLE hjArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The ID of the Channel that the HarvestJob will harvest from.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjChannelId :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
hjChannelId = Lens.field @"channelId"
{-# INLINEABLE hjChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | The time the HarvestJob was submitted
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjCreatedAt :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
hjCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE hjCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The end of the time-window which will be harvested.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjEndTime :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
hjEndTime = Lens.field @"endTime"
{-# INLINEABLE hjEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjId :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
hjId = Lens.field @"id"
{-# INLINEABLE hjId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'originEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjOriginEndpointId :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
hjOriginEndpointId = Lens.field @"originEndpointId"
{-# INLINEABLE hjOriginEndpointId #-}
{-# DEPRECATED originEndpointId "Use generic-lens or generic-optics with 'originEndpointId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjS3Destination :: Lens.Lens' HarvestJob (Core.Maybe Types.S3Destination)
hjS3Destination = Lens.field @"s3Destination"
{-# INLINEABLE hjS3Destination #-}
{-# DEPRECATED s3Destination "Use generic-lens or generic-optics with 's3Destination' instead"  #-}

-- | The start of the time-window which will be harvested.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjStartTime :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
hjStartTime = Lens.field @"startTime"
{-# INLINEABLE hjStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjStatus :: Lens.Lens' HarvestJob (Core.Maybe Types.Status)
hjStatus = Lens.field @"status"
{-# INLINEABLE hjStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON HarvestJob where
        parseJSON
          = Core.withObject "HarvestJob" Core.$
              \ x ->
                HarvestJob' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "channelId" Core.<*>
                    x Core..:? "createdAt"
                    Core.<*> x Core..:? "endTime"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "originEndpointId"
                    Core.<*> x Core..:? "s3Destination"
                    Core.<*> x Core..:? "startTime"
                    Core.<*> x Core..:? "status"
