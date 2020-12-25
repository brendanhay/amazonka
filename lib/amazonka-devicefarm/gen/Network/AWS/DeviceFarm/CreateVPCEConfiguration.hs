{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateVPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration record in Device Farm for your Amazon Virtual Private Cloud (VPC) endpoint.
module Network.AWS.DeviceFarm.CreateVPCEConfiguration
  ( -- * Creating a request
    CreateVPCEConfiguration (..),
    mkCreateVPCEConfiguration,

    -- ** Request lenses
    cvpcecVpceConfigurationName,
    cvpcecVpceServiceName,
    cvpcecServiceDnsName,
    cvpcecVpceConfigurationDescription,

    -- * Destructuring the response
    CreateVPCEConfigurationResponse (..),
    mkCreateVPCEConfigurationResponse,

    -- ** Response lenses
    cvpcecrrsVpceConfiguration,
    cvpcecrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateVPCEConfiguration' smart constructor.
data CreateVPCEConfiguration = CreateVPCEConfiguration'
  { -- | The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
    vpceConfigurationName :: Types.VpceConfigurationName,
    -- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
    vpceServiceName :: Types.VPCEServiceName,
    -- | The DNS name of the service running in your VPC that you want Device Farm to test.
    serviceDnsName :: Types.ServiceDnsName,
    -- | An optional description that provides details about your VPC endpoint configuration.
    vpceConfigurationDescription :: Core.Maybe Types.VpceConfigurationDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVPCEConfiguration' value with any optional fields omitted.
mkCreateVPCEConfiguration ::
  -- | 'vpceConfigurationName'
  Types.VpceConfigurationName ->
  -- | 'vpceServiceName'
  Types.VPCEServiceName ->
  -- | 'serviceDnsName'
  Types.ServiceDnsName ->
  CreateVPCEConfiguration
mkCreateVPCEConfiguration
  vpceConfigurationName
  vpceServiceName
  serviceDnsName =
    CreateVPCEConfiguration'
      { vpceConfigurationName,
        vpceServiceName,
        serviceDnsName,
        vpceConfigurationDescription = Core.Nothing
      }

-- | The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
--
-- /Note:/ Consider using 'vpceConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcecVpceConfigurationName :: Lens.Lens' CreateVPCEConfiguration Types.VpceConfigurationName
cvpcecVpceConfigurationName = Lens.field @"vpceConfigurationName"
{-# DEPRECATED cvpcecVpceConfigurationName "Use generic-lens or generic-optics with 'vpceConfigurationName' instead." #-}

-- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
--
-- /Note:/ Consider using 'vpceServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcecVpceServiceName :: Lens.Lens' CreateVPCEConfiguration Types.VPCEServiceName
cvpcecVpceServiceName = Lens.field @"vpceServiceName"
{-# DEPRECATED cvpcecVpceServiceName "Use generic-lens or generic-optics with 'vpceServiceName' instead." #-}

-- | The DNS name of the service running in your VPC that you want Device Farm to test.
--
-- /Note:/ Consider using 'serviceDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcecServiceDnsName :: Lens.Lens' CreateVPCEConfiguration Types.ServiceDnsName
cvpcecServiceDnsName = Lens.field @"serviceDnsName"
{-# DEPRECATED cvpcecServiceDnsName "Use generic-lens or generic-optics with 'serviceDnsName' instead." #-}

-- | An optional description that provides details about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcecVpceConfigurationDescription :: Lens.Lens' CreateVPCEConfiguration (Core.Maybe Types.VpceConfigurationDescription)
cvpcecVpceConfigurationDescription = Lens.field @"vpceConfigurationDescription"
{-# DEPRECATED cvpcecVpceConfigurationDescription "Use generic-lens or generic-optics with 'vpceConfigurationDescription' instead." #-}

instance Core.FromJSON CreateVPCEConfiguration where
  toJSON CreateVPCEConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("vpceConfigurationName" Core..= vpceConfigurationName),
            Core.Just ("vpceServiceName" Core..= vpceServiceName),
            Core.Just ("serviceDnsName" Core..= serviceDnsName),
            ("vpceConfigurationDescription" Core..=)
              Core.<$> vpceConfigurationDescription
          ]
      )

instance Core.AWSRequest CreateVPCEConfiguration where
  type Rs CreateVPCEConfiguration = CreateVPCEConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.CreateVPCEConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVPCEConfigurationResponse'
            Core.<$> (x Core..:? "vpceConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateVPCEConfigurationResponse' smart constructor.
data CreateVPCEConfigurationResponse = CreateVPCEConfigurationResponse'
  { -- | An object that contains information about your VPC endpoint configuration.
    vpceConfiguration :: Core.Maybe Types.VPCEConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVPCEConfigurationResponse' value with any optional fields omitted.
mkCreateVPCEConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateVPCEConfigurationResponse
mkCreateVPCEConfigurationResponse responseStatus =
  CreateVPCEConfigurationResponse'
    { vpceConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | An object that contains information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcecrrsVpceConfiguration :: Lens.Lens' CreateVPCEConfigurationResponse (Core.Maybe Types.VPCEConfiguration)
cvpcecrrsVpceConfiguration = Lens.field @"vpceConfiguration"
{-# DEPRECATED cvpcecrrsVpceConfiguration "Use generic-lens or generic-optics with 'vpceConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcecrrsResponseStatus :: Lens.Lens' CreateVPCEConfigurationResponse Core.Int
cvpcecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cvpcecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
