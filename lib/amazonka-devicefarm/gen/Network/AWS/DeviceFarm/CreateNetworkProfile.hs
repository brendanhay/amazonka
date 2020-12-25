{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateNetworkProfile (..),
    mkCreateNetworkProfile,

    -- ** Request lenses
    cnpProjectArn,
    cnpName,
    cnpDescription,
    cnpDownlinkBandwidthBits,
    cnpDownlinkDelayMs,
    cnpDownlinkJitterMs,
    cnpDownlinkLossPercent,
    cnpType,
    cnpUplinkBandwidthBits,
    cnpUplinkDelayMs,
    cnpUplinkJitterMs,
    cnpUplinkLossPercent,

    -- * Destructuring the response
    CreateNetworkProfileResponse (..),
    mkCreateNetworkProfileResponse,

    -- ** Response lenses
    cnprrsNetworkProfile,
    cnprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
    projectArn :: Types.ProjectArn,
    -- | The name for the new network profile.
    name :: Types.Name,
    -- | The description of the network profile.
    description :: Core.Maybe Types.Description,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    downlinkBandwidthBits :: Core.Maybe Core.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    downlinkDelayMs :: Core.Maybe Core.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    downlinkJitterMs :: Core.Maybe Core.Integer,
    -- | Proportion of received packets that fail to arrive from 0 to 100 percent.
    downlinkLossPercent :: Core.Maybe Core.Natural,
    -- | The type of network profile to create. Valid values are listed here.
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

-- | Creates a 'CreateNetworkProfile' value with any optional fields omitted.
mkCreateNetworkProfile ::
  -- | 'projectArn'
  Types.ProjectArn ->
  -- | 'name'
  Types.Name ->
  CreateNetworkProfile
mkCreateNetworkProfile projectArn name =
  CreateNetworkProfile'
    { projectArn,
      name,
      description = Core.Nothing,
      downlinkBandwidthBits = Core.Nothing,
      downlinkDelayMs = Core.Nothing,
      downlinkJitterMs = Core.Nothing,
      downlinkLossPercent = Core.Nothing,
      type' = Core.Nothing,
      uplinkBandwidthBits = Core.Nothing,
      uplinkDelayMs = Core.Nothing,
      uplinkJitterMs = Core.Nothing,
      uplinkLossPercent = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpProjectArn :: Lens.Lens' CreateNetworkProfile Types.ProjectArn
cnpProjectArn = Lens.field @"projectArn"
{-# DEPRECATED cnpProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | The name for the new network profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpName :: Lens.Lens' CreateNetworkProfile Types.Name
cnpName = Lens.field @"name"
{-# DEPRECATED cnpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDescription :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.Description)
cnpDescription = Lens.field @"description"
{-# DEPRECATED cnpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'downlinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpDownlinkBandwidthBits = Lens.field @"downlinkBandwidthBits"
{-# DEPRECATED cnpDownlinkBandwidthBits "Use generic-lens or generic-optics with 'downlinkBandwidthBits' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkDelayMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpDownlinkDelayMs = Lens.field @"downlinkDelayMs"
{-# DEPRECATED cnpDownlinkDelayMs "Use generic-lens or generic-optics with 'downlinkDelayMs' instead." #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkJitterMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpDownlinkJitterMs = Lens.field @"downlinkJitterMs"
{-# DEPRECATED cnpDownlinkJitterMs "Use generic-lens or generic-optics with 'downlinkJitterMs' instead." #-}

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'downlinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkLossPercent :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Natural)
cnpDownlinkLossPercent = Lens.field @"downlinkLossPercent"
{-# DEPRECATED cnpDownlinkLossPercent "Use generic-lens or generic-optics with 'downlinkLossPercent' instead." #-}

-- | The type of network profile to create. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpType :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.NetworkProfileType)
cnpType = Lens.field @"type'"
{-# DEPRECATED cnpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'uplinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpUplinkBandwidthBits = Lens.field @"uplinkBandwidthBits"
{-# DEPRECATED cnpUplinkBandwidthBits "Use generic-lens or generic-optics with 'uplinkBandwidthBits' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkDelayMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpUplinkDelayMs = Lens.field @"uplinkDelayMs"
{-# DEPRECATED cnpUplinkDelayMs "Use generic-lens or generic-optics with 'uplinkDelayMs' instead." #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkJitterMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
cnpUplinkJitterMs = Lens.field @"uplinkJitterMs"
{-# DEPRECATED cnpUplinkJitterMs "Use generic-lens or generic-optics with 'uplinkJitterMs' instead." #-}

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'uplinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkLossPercent :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Natural)
cnpUplinkLossPercent = Lens.field @"uplinkLossPercent"
{-# DEPRECATED cnpUplinkLossPercent "Use generic-lens or generic-optics with 'uplinkLossPercent' instead." #-}

instance Core.FromJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectArn" Core..= projectArn),
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
            ("uplinkLossPercent" Core..=) Core.<$> uplinkLossPercent
          ]
      )

instance Core.AWSRequest CreateNetworkProfile where
  type Rs CreateNetworkProfile = CreateNetworkProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.CreateNetworkProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkProfileResponse'
            Core.<$> (x Core..:? "networkProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { -- | The network profile that is returned by the create network profile request.
    networkProfile :: Core.Maybe Types.NetworkProfile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkProfileResponse' value with any optional fields omitted.
mkCreateNetworkProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateNetworkProfileResponse
mkCreateNetworkProfileResponse responseStatus =
  CreateNetworkProfileResponse'
    { networkProfile = Core.Nothing,
      responseStatus
    }

-- | The network profile that is returned by the create network profile request.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprrsNetworkProfile :: Lens.Lens' CreateNetworkProfileResponse (Core.Maybe Types.NetworkProfile)
cnprrsNetworkProfile = Lens.field @"networkProfile"
{-# DEPRECATED cnprrsNetworkProfile "Use generic-lens or generic-optics with 'networkProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprrsResponseStatus :: Lens.Lens' CreateNetworkProfileResponse Core.Int
cnprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cnprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
