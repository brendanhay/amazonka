{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateVPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an Amazon Virtual Private Cloud (VPC) endpoint configuration.
module Network.AWS.DeviceFarm.UpdateVPCEConfiguration
    (
    -- * Creating a request
      UpdateVPCEConfiguration (..)
    , mkUpdateVPCEConfiguration
    -- ** Request lenses
    , uvpcecArn
    , uvpcecServiceDnsName
    , uvpcecVpceConfigurationDescription
    , uvpcecVpceConfigurationName
    , uvpcecVpceServiceName

    -- * Destructuring the response
    , UpdateVPCEConfigurationResponse (..)
    , mkUpdateVPCEConfigurationResponse
    -- ** Response lenses
    , uvpcecrrsVpceConfiguration
    , uvpcecrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateVPCEConfiguration' smart constructor.
data UpdateVPCEConfiguration = UpdateVPCEConfiguration'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to update.
  , serviceDnsName :: Core.Maybe Types.ServiceDnsName
    -- ^ The DNS (domain) name used to connect to your private service in your VPC. The DNS name must not already be in use on the internet.
  , vpceConfigurationDescription :: Core.Maybe Types.VpceConfigurationDescription
    -- ^ An optional description that provides details about your VPC endpoint configuration.
  , vpceConfigurationName :: Core.Maybe Types.VpceConfigurationName
    -- ^ The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
  , vpceServiceName :: Core.Maybe Types.VPCEServiceName
    -- ^ The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVPCEConfiguration' value with any optional fields omitted.
mkUpdateVPCEConfiguration
    :: Types.Arn -- ^ 'arn'
    -> UpdateVPCEConfiguration
mkUpdateVPCEConfiguration arn
  = UpdateVPCEConfiguration'{arn, serviceDnsName = Core.Nothing,
                             vpceConfigurationDescription = Core.Nothing,
                             vpceConfigurationName = Core.Nothing,
                             vpceServiceName = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvpcecArn :: Lens.Lens' UpdateVPCEConfiguration Types.Arn
uvpcecArn = Lens.field @"arn"
{-# INLINEABLE uvpcecArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The DNS (domain) name used to connect to your private service in your VPC. The DNS name must not already be in use on the internet.
--
-- /Note:/ Consider using 'serviceDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvpcecServiceDnsName :: Lens.Lens' UpdateVPCEConfiguration (Core.Maybe Types.ServiceDnsName)
uvpcecServiceDnsName = Lens.field @"serviceDnsName"
{-# INLINEABLE uvpcecServiceDnsName #-}
{-# DEPRECATED serviceDnsName "Use generic-lens or generic-optics with 'serviceDnsName' instead"  #-}

-- | An optional description that provides details about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvpcecVpceConfigurationDescription :: Lens.Lens' UpdateVPCEConfiguration (Core.Maybe Types.VpceConfigurationDescription)
uvpcecVpceConfigurationDescription = Lens.field @"vpceConfigurationDescription"
{-# INLINEABLE uvpcecVpceConfigurationDescription #-}
{-# DEPRECATED vpceConfigurationDescription "Use generic-lens or generic-optics with 'vpceConfigurationDescription' instead"  #-}

-- | The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
--
-- /Note:/ Consider using 'vpceConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvpcecVpceConfigurationName :: Lens.Lens' UpdateVPCEConfiguration (Core.Maybe Types.VpceConfigurationName)
uvpcecVpceConfigurationName = Lens.field @"vpceConfigurationName"
{-# INLINEABLE uvpcecVpceConfigurationName #-}
{-# DEPRECATED vpceConfigurationName "Use generic-lens or generic-optics with 'vpceConfigurationName' instead"  #-}

-- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
--
-- /Note:/ Consider using 'vpceServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvpcecVpceServiceName :: Lens.Lens' UpdateVPCEConfiguration (Core.Maybe Types.VPCEServiceName)
uvpcecVpceServiceName = Lens.field @"vpceServiceName"
{-# INLINEABLE uvpcecVpceServiceName #-}
{-# DEPRECATED vpceServiceName "Use generic-lens or generic-optics with 'vpceServiceName' instead"  #-}

instance Core.ToQuery UpdateVPCEConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateVPCEConfiguration where
        toHeaders UpdateVPCEConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.UpdateVPCEConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateVPCEConfiguration where
        toJSON UpdateVPCEConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn),
                  ("serviceDnsName" Core..=) Core.<$> serviceDnsName,
                  ("vpceConfigurationDescription" Core..=) Core.<$>
                    vpceConfigurationDescription,
                  ("vpceConfigurationName" Core..=) Core.<$> vpceConfigurationName,
                  ("vpceServiceName" Core..=) Core.<$> vpceServiceName])

instance Core.AWSRequest UpdateVPCEConfiguration where
        type Rs UpdateVPCEConfiguration = UpdateVPCEConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateVPCEConfigurationResponse' Core.<$>
                   (x Core..:? "vpceConfiguration") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateVPCEConfigurationResponse' smart constructor.
data UpdateVPCEConfigurationResponse = UpdateVPCEConfigurationResponse'
  { vpceConfiguration :: Core.Maybe Types.VPCEConfiguration
    -- ^ An object that contains information about your VPC endpoint configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVPCEConfigurationResponse' value with any optional fields omitted.
mkUpdateVPCEConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateVPCEConfigurationResponse
mkUpdateVPCEConfigurationResponse responseStatus
  = UpdateVPCEConfigurationResponse'{vpceConfiguration =
                                       Core.Nothing,
                                     responseStatus}

-- | An object that contains information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvpcecrrsVpceConfiguration :: Lens.Lens' UpdateVPCEConfigurationResponse (Core.Maybe Types.VPCEConfiguration)
uvpcecrrsVpceConfiguration = Lens.field @"vpceConfiguration"
{-# INLINEABLE uvpcecrrsVpceConfiguration #-}
{-# DEPRECATED vpceConfiguration "Use generic-lens or generic-optics with 'vpceConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvpcecrrsResponseStatus :: Lens.Lens' UpdateVPCEConfigurationResponse Core.Int
uvpcecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uvpcecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
