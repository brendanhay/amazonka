{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the network profile.
module Network.AWS.DeviceFarm.UpdateNetworkProfile
  ( -- * Creating a request
    UpdateNetworkProfile (..),
    mkUpdateNetworkProfile,

    -- ** Request lenses
    unpArn,
    unpDescription,
    unpDownlinkBandwidthBits,
    unpDownlinkDelayMs,
    unpDownlinkJitterMs,
    unpDownlinkLossPercent,
    unpName,
    unpType,
    unpUplinkBandwidthBits,
    unpUplinkDelayMs,
    unpUplinkJitterMs,
    unpUplinkLossPercent,

    -- * Destructuring the response
    UpdateNetworkProfileResponse (..),
    mkUpdateNetworkProfileResponse,

    -- ** Response lenses
    unprrsNetworkProfile,
    unprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { -- | The Amazon Resource Name (ARN) of the project for which you want to update network profile settings.
    arn :: Types.Arn,
    -- | The description of the network profile about which you are returning information.
    description :: Core.Maybe Types.Description,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    downlinkBandwidthBits :: Core.Maybe Core.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    downlinkDelayMs :: Core.Maybe Core.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    downlinkJitterMs :: Core.Maybe Core.Integer,
    -- | Proportion of received packets that fail to arrive from 0 to 100 percent.
    downlinkLossPercent :: Core.Maybe Core.Natural,
    -- | The name of the network profile about which you are returning information.
    name :: Core.Maybe Types.Name,
    -- | The type of network profile to return information about. Valid values are listed here.
    type' :: Core.Maybe Types.NetworkProfileType,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    uplinkBandwidthBits :: Core.Maybe Core.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    uplinkDelayMs :: Core.Maybe Core.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    uplinkJitterMs :: Core.Maybe Core.Integer,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
    uplinkLossPercent :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNetworkProfile' value with any optional fields omitted.
mkUpdateNetworkProfile ::
  -- | 'arn'
  Types.Arn ->
  UpdateNetworkProfile
mkUpdateNetworkProfile arn =
  UpdateNetworkProfile'
    { arn,
      description = Core.Nothing,
      downlinkBandwidthBits = Core.Nothing,
      downlinkDelayMs = Core.Nothing,
      downlinkJitterMs = Core.Nothing,
      downlinkLossPercent = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing,
      uplinkBandwidthBits = Core.Nothing,
      uplinkDelayMs = Core.Nothing,
      uplinkJitterMs = Core.Nothing,
      uplinkLossPercent = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project for which you want to update network profile settings.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpArn :: Lens.Lens' UpdateNetworkProfile Types.Arn
unpArn = Lens.field @"arn"
{-# DEPRECATED unpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The description of the network profile about which you are returning information.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDescription :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Types.Description)
unpDescription = Lens.field @"description"
{-# DEPRECATED unpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'downlinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDownlinkBandwidthBits :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Integer)
unpDownlinkBandwidthBits = Lens.field @"downlinkBandwidthBits"
{-# DEPRECATED unpDownlinkBandwidthBits "Use generic-lens or generic-optics with 'downlinkBandwidthBits' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDownlinkDelayMs :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Integer)
unpDownlinkDelayMs = Lens.field @"downlinkDelayMs"
{-# DEPRECATED unpDownlinkDelayMs "Use generic-lens or generic-optics with 'downlinkDelayMs' instead." #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDownlinkJitterMs :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Integer)
unpDownlinkJitterMs = Lens.field @"downlinkJitterMs"
{-# DEPRECATED unpDownlinkJitterMs "Use generic-lens or generic-optics with 'downlinkJitterMs' instead." #-}

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'downlinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDownlinkLossPercent :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Natural)
unpDownlinkLossPercent = Lens.field @"downlinkLossPercent"
{-# DEPRECATED unpDownlinkLossPercent "Use generic-lens or generic-optics with 'downlinkLossPercent' instead." #-}

-- | The name of the network profile about which you are returning information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpName :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Types.Name)
unpName = Lens.field @"name"
{-# DEPRECATED unpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of network profile to return information about. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpType :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Types.NetworkProfileType)
unpType = Lens.field @"type'"
{-# DEPRECATED unpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'uplinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpUplinkBandwidthBits :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Integer)
unpUplinkBandwidthBits = Lens.field @"uplinkBandwidthBits"
{-# DEPRECATED unpUplinkBandwidthBits "Use generic-lens or generic-optics with 'uplinkBandwidthBits' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpUplinkDelayMs :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Integer)
unpUplinkDelayMs = Lens.field @"uplinkDelayMs"
{-# DEPRECATED unpUplinkDelayMs "Use generic-lens or generic-optics with 'uplinkDelayMs' instead." #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpUplinkJitterMs :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Integer)
unpUplinkJitterMs = Lens.field @"uplinkJitterMs"
{-# DEPRECATED unpUplinkJitterMs "Use generic-lens or generic-optics with 'uplinkJitterMs' instead." #-}

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'uplinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpUplinkLossPercent :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Natural)
unpUplinkLossPercent = Lens.field @"uplinkLossPercent"
{-# DEPRECATED unpUplinkLossPercent "Use generic-lens or generic-optics with 'uplinkLossPercent' instead." #-}

instance Core.FromJSON UpdateNetworkProfile where
  toJSON UpdateNetworkProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            ("description" Core..=) Core.<$> description,
            ("downlinkBandwidthBits" Core..=) Core.<$> downlinkBandwidthBits,
            ("downlinkDelayMs" Core..=) Core.<$> downlinkDelayMs,
            ("downlinkJitterMs" Core..=) Core.<$> downlinkJitterMs,
            ("downlinkLossPercent" Core..=) Core.<$> downlinkLossPercent,
            ("name" Core..=) Core.<$> name,
            ("type" Core..=) Core.<$> type',
            ("uplinkBandwidthBits" Core..=) Core.<$> uplinkBandwidthBits,
            ("uplinkDelayMs" Core..=) Core.<$> uplinkDelayMs,
            ("uplinkJitterMs" Core..=) Core.<$> uplinkJitterMs,
            ("uplinkLossPercent" Core..=) Core.<$> uplinkLossPercent
          ]
      )

instance Core.AWSRequest UpdateNetworkProfile where
  type Rs UpdateNetworkProfile = UpdateNetworkProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.UpdateNetworkProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNetworkProfileResponse'
            Core.<$> (x Core..:? "networkProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateNetworkProfileResponse' smart constructor.
data UpdateNetworkProfileResponse = UpdateNetworkProfileResponse'
  { -- | A list of the available network profiles.
    networkProfile :: Core.Maybe Types.NetworkProfile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNetworkProfileResponse' value with any optional fields omitted.
mkUpdateNetworkProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateNetworkProfileResponse
mkUpdateNetworkProfileResponse responseStatus =
  UpdateNetworkProfileResponse'
    { networkProfile = Core.Nothing,
      responseStatus
    }

-- | A list of the available network profiles.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unprrsNetworkProfile :: Lens.Lens' UpdateNetworkProfileResponse (Core.Maybe Types.NetworkProfile)
unprrsNetworkProfile = Lens.field @"networkProfile"
{-# DEPRECATED unprrsNetworkProfile "Use generic-lens or generic-optics with 'networkProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unprrsResponseStatus :: Lens.Lens' UpdateNetworkProfileResponse Core.Int
unprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED unprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
