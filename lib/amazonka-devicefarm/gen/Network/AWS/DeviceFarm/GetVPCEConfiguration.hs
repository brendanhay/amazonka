{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetVPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configuration settings for your Amazon Virtual Private Cloud (VPC) endpoint.
module Network.AWS.DeviceFarm.GetVPCEConfiguration
  ( -- * Creating a request
    GetVPCEConfiguration (..),
    mkGetVPCEConfiguration,

    -- ** Request lenses
    gvpcecArn,

    -- * Destructuring the response
    GetVPCEConfigurationResponse (..),
    mkGetVPCEConfigurationResponse,

    -- ** Response lenses
    gvpcecrrsVpceConfiguration,
    gvpcecrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetVPCEConfiguration' smart constructor.
newtype GetVPCEConfiguration = GetVPCEConfiguration'
  { -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to describe.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetVPCEConfiguration' value with any optional fields omitted.
mkGetVPCEConfiguration ::
  -- | 'arn'
  Types.Arn ->
  GetVPCEConfiguration
mkGetVPCEConfiguration arn = GetVPCEConfiguration' {arn}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to describe.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvpcecArn :: Lens.Lens' GetVPCEConfiguration Types.Arn
gvpcecArn = Lens.field @"arn"
{-# DEPRECATED gvpcecArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetVPCEConfiguration where
  toJSON GetVPCEConfiguration {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetVPCEConfiguration where
  type Rs GetVPCEConfiguration = GetVPCEConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.GetVPCEConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVPCEConfigurationResponse'
            Core.<$> (x Core..:? "vpceConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetVPCEConfigurationResponse' smart constructor.
data GetVPCEConfigurationResponse = GetVPCEConfigurationResponse'
  { -- | An object that contains information about your VPC endpoint configuration.
    vpceConfiguration :: Core.Maybe Types.VPCEConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVPCEConfigurationResponse' value with any optional fields omitted.
mkGetVPCEConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetVPCEConfigurationResponse
mkGetVPCEConfigurationResponse responseStatus =
  GetVPCEConfigurationResponse'
    { vpceConfiguration = Core.Nothing,
      responseStatus
    }

-- | An object that contains information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvpcecrrsVpceConfiguration :: Lens.Lens' GetVPCEConfigurationResponse (Core.Maybe Types.VPCEConfiguration)
gvpcecrrsVpceConfiguration = Lens.field @"vpceConfiguration"
{-# DEPRECATED gvpcecrrsVpceConfiguration "Use generic-lens or generic-optics with 'vpceConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvpcecrrsResponseStatus :: Lens.Lens' GetVPCEConfigurationResponse Core.Int
gvpcecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gvpcecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
