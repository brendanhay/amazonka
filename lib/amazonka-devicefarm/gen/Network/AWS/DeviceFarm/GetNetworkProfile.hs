{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a network profile.
module Network.AWS.DeviceFarm.GetNetworkProfile
  ( -- * Creating a request
    GetNetworkProfile (..),
    mkGetNetworkProfile,

    -- ** Request lenses
    gnpArn,

    -- * Destructuring the response
    GetNetworkProfileResponse (..),
    mkGetNetworkProfileResponse,

    -- ** Response lenses
    gnprrsNetworkProfile,
    gnprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetNetworkProfile' smart constructor.
newtype GetNetworkProfile = GetNetworkProfile'
  { -- | The ARN of the network profile to return information about.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetNetworkProfile' value with any optional fields omitted.
mkGetNetworkProfile ::
  -- | 'arn'
  Types.Arn ->
  GetNetworkProfile
mkGetNetworkProfile arn = GetNetworkProfile' {arn}

-- | The ARN of the network profile to return information about.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnpArn :: Lens.Lens' GetNetworkProfile Types.Arn
gnpArn = Lens.field @"arn"
{-# DEPRECATED gnpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetNetworkProfile where
  toJSON GetNetworkProfile {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetNetworkProfile where
  type Rs GetNetworkProfile = GetNetworkProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetNetworkProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkProfileResponse'
            Core.<$> (x Core..:? "networkProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetNetworkProfileResponse' smart constructor.
data GetNetworkProfileResponse = GetNetworkProfileResponse'
  { -- | The network profile.
    networkProfile :: Core.Maybe Types.NetworkProfile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetNetworkProfileResponse' value with any optional fields omitted.
mkGetNetworkProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetNetworkProfileResponse
mkGetNetworkProfileResponse responseStatus =
  GetNetworkProfileResponse'
    { networkProfile = Core.Nothing,
      responseStatus
    }

-- | The network profile.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnprrsNetworkProfile :: Lens.Lens' GetNetworkProfileResponse (Core.Maybe Types.NetworkProfile)
gnprrsNetworkProfile = Lens.field @"networkProfile"
{-# DEPRECATED gnprrsNetworkProfile "Use generic-lens or generic-optics with 'networkProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnprrsResponseStatus :: Lens.Lens' GetNetworkProfileResponse Core.Int
gnprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gnprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
