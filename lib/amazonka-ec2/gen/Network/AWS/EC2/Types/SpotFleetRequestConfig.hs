{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetRequestConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetRequestConfig
  ( SpotFleetRequestConfig (..),

    -- * Smart constructor
    mkSpotFleetRequestConfig,

    -- * Lenses
    sfrcActivityStatus,
    sfrcCreateTime,
    sfrcSpotFleetRequestConfig,
    sfrcSpotFleetRequestId,
    sfrcSpotFleetRequestState,
    sfrcTags,
  )
where

import qualified Network.AWS.EC2.Types.ActivityStatus as Types
import qualified Network.AWS.EC2.Types.BatchState as Types
import qualified Network.AWS.EC2.Types.SpotFleetRequestConfigData as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Spot Fleet request.
--
-- /See:/ 'mkSpotFleetRequestConfig' smart constructor.
data SpotFleetRequestConfig = SpotFleetRequestConfig'
  { -- | The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
    activityStatus :: Core.Maybe Types.ActivityStatus,
    -- | The creation date and time of the request.
    createTime :: Core.Maybe Core.UTCTime,
    -- | The configuration of the Spot Fleet request.
    spotFleetRequestConfig :: Core.Maybe Types.SpotFleetRequestConfigData,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Types.String,
    -- | The state of the Spot Fleet request.
    spotFleetRequestState :: Core.Maybe Types.BatchState,
    -- | The tags for a Spot Fleet resource.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SpotFleetRequestConfig' value with any optional fields omitted.
mkSpotFleetRequestConfig ::
  SpotFleetRequestConfig
mkSpotFleetRequestConfig =
  SpotFleetRequestConfig'
    { activityStatus = Core.Nothing,
      createTime = Core.Nothing,
      spotFleetRequestConfig = Core.Nothing,
      spotFleetRequestId = Core.Nothing,
      spotFleetRequestState = Core.Nothing,
      tags = Core.Nothing
    }

-- | The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
--
-- /Note:/ Consider using 'activityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcActivityStatus :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Types.ActivityStatus)
sfrcActivityStatus = Lens.field @"activityStatus"
{-# DEPRECATED sfrcActivityStatus "Use generic-lens or generic-optics with 'activityStatus' instead." #-}

-- | The creation date and time of the request.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcCreateTime :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Core.UTCTime)
sfrcCreateTime = Lens.field @"createTime"
{-# DEPRECATED sfrcCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The configuration of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestConfig :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Types.SpotFleetRequestConfigData)
sfrcSpotFleetRequestConfig = Lens.field @"spotFleetRequestConfig"
{-# DEPRECATED sfrcSpotFleetRequestConfig "Use generic-lens or generic-optics with 'spotFleetRequestConfig' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestId :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Types.String)
sfrcSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED sfrcSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The state of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestState :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Types.BatchState)
sfrcSpotFleetRequestState = Lens.field @"spotFleetRequestState"
{-# DEPRECATED sfrcSpotFleetRequestState "Use generic-lens or generic-optics with 'spotFleetRequestState' instead." #-}

-- | The tags for a Spot Fleet resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcTags :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe [Types.Tag])
sfrcTags = Lens.field @"tags"
{-# DEPRECATED sfrcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML SpotFleetRequestConfig where
  parseXML x =
    SpotFleetRequestConfig'
      Core.<$> (x Core..@? "activityStatus")
      Core.<*> (x Core..@? "createTime")
      Core.<*> (x Core..@? "spotFleetRequestConfig")
      Core.<*> (x Core..@? "spotFleetRequestId")
      Core.<*> (x Core..@? "spotFleetRequestState")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
