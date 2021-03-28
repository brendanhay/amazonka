{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile.
module Network.AWS.DeviceFarm.CreateNetworkProfile
    (
    -- * Creating a request
      CreateNetworkProfile (..)
    , mkCreateNetworkProfile
    -- ** Request lenses
    , cnpProjectArn
    , cnpName
    , cnpDescription
    , cnpDownlinkBandwidthBits
    , cnpDownlinkDelayMs
    , cnpDownlinkJitterMs
    , cnpDownlinkLossPercent
    , cnpType
    , cnpUplinkBandwidthBits
    , cnpUplinkDelayMs
    , cnpUplinkJitterMs
    , cnpUplinkLossPercent

    -- * Destructuring the response
    , CreateNetworkProfileResponse (..)
    , mkCreateNetworkProfileResponse
    -- ** Response lenses
    , cnprrsNetworkProfile
    , cnprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { projectArn :: Types.ProjectArn
    -- ^ The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
  , name :: Types.Name
    -- ^ The name for the new network profile.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the network profile.
  , downlinkBandwidthBits :: Core.Maybe Core.Integer
    -- ^ The data throughput rate in bits per second, as an integer from 0 to 104857600.
  , downlinkDelayMs :: Core.Maybe Core.Integer
    -- ^ Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
  , downlinkJitterMs :: Core.Maybe Core.Integer
    -- ^ Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
  , downlinkLossPercent :: Core.Maybe Core.Natural
    -- ^ Proportion of received packets that fail to arrive from 0 to 100 percent.
  , type' :: Core.Maybe Types.NetworkProfileType
    -- ^ The type of network profile to create. Valid values are listed here.
  , uplinkBandwidthBits :: Core.Maybe Core.Integer
    -- ^ The data throughput rate in bits per second, as an integer from 0 to 104857600.
  , uplinkDelayMs :: Core.Maybe Core.Integer
    -- ^ Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
  , uplinkJitterMs :: Core.Maybe Core.Integer
    -- ^ Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
  , uplinkLossPercent :: Core.Maybe Core.Natural
    -- ^ Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkProfile' value with any optional fields omitted.
mkCreateNetworkProfile
    :: Types.ProjectArn -- ^ 'projectArn'
    -> Types.Name -- ^ 'name'
    -> CreateNetworkProfile
mkCreateNetworkProfile projectArn name
  = CreateNetworkProfile'{projectArn, name,
                          description = Core.Nothing, downlinkBandwidthBits = Core.Nothing,
                          downlinkDelayMs = Core.Nothing, downlinkJitterMs = Core.Nothing,
                          downlinkLossPercent = Core.Nothing, type' = Core.Nothing,
                          uplinkBandwidthBits = Core.Nothing, uplinkDelayMs = Core.Nothing,
                          uplinkJitterMs = Core.Nothing, uplinkLossPercent = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpProjectArn :: Lens.Lens' CreateNetworkProfile Types.ProjectArn
cnpProjectArn = Lens.field @"projectArn"
{-# INLINEABLE cnpProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | The name for the new network profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpName :: Lens.Lens' CreateNetworkProfile Types.Name
cnpName = Lens.field @"name"
{-# INLINEABLE cnpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The description of the network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDescription :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.Description)
cnpDescription = Lens.field @"description"
{-# INLINEABLE cnpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'downlinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpDownlinkBandwidthBits = Lens.field @"downlinkBandwidthBits"
{-# INLINEABLE cnpDownlinkBandwidthBits #-}
{-# DEPRECATED downlinkBandwidthBits "Use generic-lens or generic-optics with 'downlinkBandwidthBits' instead"  #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkDelayMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpDownlinkDelayMs = Lens.field @"downlinkDelayMs"
{-# INLINEABLE cnpDownlinkDelayMs #-}
{-# DEPRECATED downlinkDelayMs "Use generic-lens or generic-optics with 'downlinkDelayMs' instead"  #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkJitterMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpDownlinkJitterMs = Lens.field @"downlinkJitterMs"
{-# INLINEABLE cnpDownlinkJitterMs #-}
{-# DEPRECATED downlinkJitterMs "Use generic-lens or generic-optics with 'downlinkJitterMs' instead"  #-}

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'downlinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkLossPercent :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Natural)
cnpDownlinkLossPercent = Lens.field @"downlinkLossPercent"
{-# INLINEABLE cnpDownlinkLossPercent #-}
{-# DEPRECATED downlinkLossPercent "Use generic-lens or generic-optics with 'downlinkLossPercent' instead"  #-}

-- | The type of network profile to create. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpType :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.NetworkProfileType)
cnpType = Lens.field @"type'"
{-# INLINEABLE cnpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'uplinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpUplinkBandwidthBits = Lens.field @"uplinkBandwidthBits"
{-# INLINEABLE cnpUplinkBandwidthBits #-}
{-# DEPRECATED uplinkBandwidthBits "Use generic-lens or generic-optics with 'uplinkBandwidthBits' instead"  #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkDelayMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpUplinkDelayMs = Lens.field @"uplinkDelayMs"
{-# INLINEABLE cnpUplinkDelayMs #-}
{-# DEPRECATED uplinkDelayMs "Use generic-lens or generic-optics with 'uplinkDelayMs' instead"  #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkJitterMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpUplinkJitterMs = Lens.field @"uplinkJitterMs"
{-# INLINEABLE cnpUplinkJitterMs #-}
{-# DEPRECATED uplinkJitterMs "Use generic-lens or generic-optics with 'uplinkJitterMs' instead"  #-}

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'uplinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkLossPercent :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Natural)
cnpUplinkLossPercent = Lens.field @"uplinkLossPercent"
{-# INLINEABLE cnpUplinkLossPercent #-}
{-# DEPRECATED uplinkLossPercent "Use generic-lens or generic-optics with 'uplinkLossPercent' instead"  #-}

instance Core.ToQuery CreateNetworkProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateNetworkProfile where
        toHeaders CreateNetworkProfile{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.CreateNetworkProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateNetworkProfile where
        toJSON CreateNetworkProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectArn" Core..= projectArn),
                  Core.Just ("name" Core..= name),
                  ("description" Core..=) Core.<$> description,
                  ("downlinkBandwidthBits" Core..=) Core.<$> downlinkBandwidthBits,
                  ("downlinkDelayMs" Core..=) Core.<$> downlinkDelayMs,
                  ("downlinkJitterMs" Core..=) Core.<$> downlinkJitterMs,
                  ("downlinkLossPercent" Core..=) Core.<$> downlinkLossPercent,
                  ("type" Core..=) Core.<$> type',
                  ("uplinkBandwidthBits" Core..=) Core.<$> uplinkBandwidthBits,
                  ("uplinkDelayMs" Core..=) Core.<$> uplinkDelayMs,
                  ("uplinkJitterMs" Core..=) Core.<$> uplinkJitterMs,
                  ("uplinkLossPercent" Core..=) Core.<$> uplinkLossPercent])

instance Core.AWSRequest CreateNetworkProfile where
        type Rs CreateNetworkProfile = CreateNetworkProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateNetworkProfileResponse' Core.<$>
                   (x Core..:? "networkProfile") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { networkProfile :: Core.Maybe Types.NetworkProfile
    -- ^ The network profile that is returned by the create network profile request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkProfileResponse' value with any optional fields omitted.
mkCreateNetworkProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNetworkProfileResponse
mkCreateNetworkProfileResponse responseStatus
  = CreateNetworkProfileResponse'{networkProfile = Core.Nothing,
                                  responseStatus}

-- | The network profile that is returned by the create network profile request.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprrsNetworkProfile :: Lens.Lens' CreateNetworkProfileResponse (Core.Maybe Types.NetworkProfile)
cnprrsNetworkProfile = Lens.field @"networkProfile"
{-# INLINEABLE cnprrsNetworkProfile #-}
{-# DEPRECATED networkProfile "Use generic-lens or generic-optics with 'networkProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprrsResponseStatus :: Lens.Lens' CreateNetworkProfileResponse Core.Int
cnprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cnprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
