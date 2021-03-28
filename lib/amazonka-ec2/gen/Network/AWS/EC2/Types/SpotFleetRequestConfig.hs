{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetRequestConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotFleetRequestConfig
  ( SpotFleetRequestConfig (..)
  -- * Smart constructor
  , mkSpotFleetRequestConfig
  -- * Lenses
  , sfrcActivityStatus
  , sfrcCreateTime
  , sfrcSpotFleetRequestConfig
  , sfrcSpotFleetRequestId
  , sfrcSpotFleetRequestState
  , sfrcTags
  ) where

import qualified Network.AWS.EC2.Types.ActivityStatus as Types
import qualified Network.AWS.EC2.Types.BatchState as Types
import qualified Network.AWS.EC2.Types.SpotFleetRequestConfigData as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Spot Fleet request.
--
-- /See:/ 'mkSpotFleetRequestConfig' smart constructor.
data SpotFleetRequestConfig = SpotFleetRequestConfig'
  { activityStatus :: Core.Maybe Types.ActivityStatus
    -- ^ The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
  , createTime :: Core.Maybe Core.UTCTime
    -- ^ The creation date and time of the request.
  , spotFleetRequestConfig :: Core.Maybe Types.SpotFleetRequestConfigData
    -- ^ The configuration of the Spot Fleet request.
  , spotFleetRequestId :: Core.Maybe Core.Text
    -- ^ The ID of the Spot Fleet request.
  , spotFleetRequestState :: Core.Maybe Types.BatchState
    -- ^ The state of the Spot Fleet request.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for a Spot Fleet resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SpotFleetRequestConfig' value with any optional fields omitted.
mkSpotFleetRequestConfig
    :: SpotFleetRequestConfig
mkSpotFleetRequestConfig
  = SpotFleetRequestConfig'{activityStatus = Core.Nothing,
                            createTime = Core.Nothing, spotFleetRequestConfig = Core.Nothing,
                            spotFleetRequestId = Core.Nothing,
                            spotFleetRequestState = Core.Nothing, tags = Core.Nothing}

-- | The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
--
-- /Note:/ Consider using 'activityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcActivityStatus :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Types.ActivityStatus)
sfrcActivityStatus = Lens.field @"activityStatus"
{-# INLINEABLE sfrcActivityStatus #-}
{-# DEPRECATED activityStatus "Use generic-lens or generic-optics with 'activityStatus' instead"  #-}

-- | The creation date and time of the request.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcCreateTime :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Core.UTCTime)
sfrcCreateTime = Lens.field @"createTime"
{-# INLINEABLE sfrcCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The configuration of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestConfig :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Types.SpotFleetRequestConfigData)
sfrcSpotFleetRequestConfig = Lens.field @"spotFleetRequestConfig"
{-# INLINEABLE sfrcSpotFleetRequestConfig #-}
{-# DEPRECATED spotFleetRequestConfig "Use generic-lens or generic-optics with 'spotFleetRequestConfig' instead"  #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestId :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Core.Text)
sfrcSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# INLINEABLE sfrcSpotFleetRequestId #-}
{-# DEPRECATED spotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead"  #-}

-- | The state of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestState :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe Types.BatchState)
sfrcSpotFleetRequestState = Lens.field @"spotFleetRequestState"
{-# INLINEABLE sfrcSpotFleetRequestState #-}
{-# DEPRECATED spotFleetRequestState "Use generic-lens or generic-optics with 'spotFleetRequestState' instead"  #-}

-- | The tags for a Spot Fleet resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcTags :: Lens.Lens' SpotFleetRequestConfig (Core.Maybe [Types.Tag])
sfrcTags = Lens.field @"tags"
{-# INLINEABLE sfrcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML SpotFleetRequestConfig where
        parseXML x
          = SpotFleetRequestConfig' Core.<$>
              (x Core..@? "activityStatus") Core.<*> x Core..@? "createTime"
                Core.<*> x Core..@? "spotFleetRequestConfig"
                Core.<*> x Core..@? "spotFleetRequestId"
                Core.<*> x Core..@? "spotFleetRequestState"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
