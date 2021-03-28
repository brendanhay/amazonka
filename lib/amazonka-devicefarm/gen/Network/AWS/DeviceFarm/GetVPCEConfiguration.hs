{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetVPCEConfiguration (..)
    , mkGetVPCEConfiguration
    -- ** Request lenses
    , gvpcecArn

    -- * Destructuring the response
    , GetVPCEConfigurationResponse (..)
    , mkGetVPCEConfigurationResponse
    -- ** Response lenses
    , gvpcecrrsVpceConfiguration
    , gvpcecrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetVPCEConfiguration' smart constructor.
newtype GetVPCEConfiguration = GetVPCEConfiguration'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetVPCEConfiguration' value with any optional fields omitted.
mkGetVPCEConfiguration
    :: Types.Arn -- ^ 'arn'
    -> GetVPCEConfiguration
mkGetVPCEConfiguration arn = GetVPCEConfiguration'{arn}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to describe.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvpcecArn :: Lens.Lens' GetVPCEConfiguration Types.Arn
gvpcecArn = Lens.field @"arn"
{-# INLINEABLE gvpcecArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetVPCEConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetVPCEConfiguration where
        toHeaders GetVPCEConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.GetVPCEConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetVPCEConfiguration where
        toJSON GetVPCEConfiguration{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetVPCEConfiguration where
        type Rs GetVPCEConfiguration = GetVPCEConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetVPCEConfigurationResponse' Core.<$>
                   (x Core..:? "vpceConfiguration") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetVPCEConfigurationResponse' smart constructor.
data GetVPCEConfigurationResponse = GetVPCEConfigurationResponse'
  { vpceConfiguration :: Core.Maybe Types.VPCEConfiguration
    -- ^ An object that contains information about your VPC endpoint configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVPCEConfigurationResponse' value with any optional fields omitted.
mkGetVPCEConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetVPCEConfigurationResponse
mkGetVPCEConfigurationResponse responseStatus
  = GetVPCEConfigurationResponse'{vpceConfiguration = Core.Nothing,
                                  responseStatus}

-- | An object that contains information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvpcecrrsVpceConfiguration :: Lens.Lens' GetVPCEConfigurationResponse (Core.Maybe Types.VPCEConfiguration)
gvpcecrrsVpceConfiguration = Lens.field @"vpceConfiguration"
{-# INLINEABLE gvpcecrrsVpceConfiguration #-}
{-# DEPRECATED vpceConfiguration "Use generic-lens or generic-optics with 'vpceConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvpcecrrsResponseStatus :: Lens.Lens' GetVPCEConfigurationResponse Core.Int
gvpcecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gvpcecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
