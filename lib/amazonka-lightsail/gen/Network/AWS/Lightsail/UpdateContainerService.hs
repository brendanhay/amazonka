{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of your Amazon Lightsail container service, such as its power, scale, and public domain names.
module Network.AWS.Lightsail.UpdateContainerService
  ( -- * Creating a request
    UpdateContainerService (..),
    mkUpdateContainerService,

    -- ** Request lenses
    ucsServiceName,
    ucsIsDisabled,
    ucsPower,
    ucsPublicDomainNames,
    ucsScale,

    -- * Destructuring the response
    UpdateContainerServiceResponse (..),
    mkUpdateContainerServiceResponse,

    -- ** Response lenses
    ucsrrsContainerService,
    ucsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateContainerService' smart constructor.
data UpdateContainerService = UpdateContainerService'
  { -- | The name of the container service to update.
    serviceName :: Types.ServiceName,
    -- | A Boolean value to indicate whether the container service is disabled.
    isDisabled :: Core.Maybe Core.Bool,
    -- | The power for the container service.
    --
    -- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
    -- Use the @GetContainerServicePowers@ action to view the specifications of each power option.
    power :: Core.Maybe Types.ContainerServicePowerName,
    -- | The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
    --
    -- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
    -- If you don't specify public domain names, then you can use the default domain of the container service.
    -- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
    -- You can specify public domain names using a string to array map as shown in the example later on this page.
    publicDomainNames :: Core.Maybe (Core.HashMap Types.String [Types.String]),
    -- | The scale for the container service.
    --
    -- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
    scale :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContainerService' value with any optional fields omitted.
mkUpdateContainerService ::
  -- | 'serviceName'
  Types.ServiceName ->
  UpdateContainerService
mkUpdateContainerService serviceName =
  UpdateContainerService'
    { serviceName,
      isDisabled = Core.Nothing,
      power = Core.Nothing,
      publicDomainNames = Core.Nothing,
      scale = Core.Nothing
    }

-- | The name of the container service to update.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsServiceName :: Lens.Lens' UpdateContainerService Types.ServiceName
ucsServiceName = Lens.field @"serviceName"
{-# DEPRECATED ucsServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | A Boolean value to indicate whether the container service is disabled.
--
-- /Note:/ Consider using 'isDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsIsDisabled :: Lens.Lens' UpdateContainerService (Core.Maybe Core.Bool)
ucsIsDisabled = Lens.field @"isDisabled"
{-# DEPRECATED ucsIsDisabled "Use generic-lens or generic-optics with 'isDisabled' instead." #-}

-- | The power for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- Use the @GetContainerServicePowers@ action to view the specifications of each power option.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsPower :: Lens.Lens' UpdateContainerService (Core.Maybe Types.ContainerServicePowerName)
ucsPower = Lens.field @"power"
{-# DEPRECATED ucsPower "Use generic-lens or generic-optics with 'power' instead." #-}

-- | The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- You can specify public domain names using a string to array map as shown in the example later on this page.
--
-- /Note:/ Consider using 'publicDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsPublicDomainNames :: Lens.Lens' UpdateContainerService (Core.Maybe (Core.HashMap Types.String [Types.String]))
ucsPublicDomainNames = Lens.field @"publicDomainNames"
{-# DEPRECATED ucsPublicDomainNames "Use generic-lens or generic-optics with 'publicDomainNames' instead." #-}

-- | The scale for the container service.
--
-- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsScale :: Lens.Lens' UpdateContainerService (Core.Maybe Core.Natural)
ucsScale = Lens.field @"scale"
{-# DEPRECATED ucsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

instance Core.FromJSON UpdateContainerService where
  toJSON UpdateContainerService {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("serviceName" Core..= serviceName),
            ("isDisabled" Core..=) Core.<$> isDisabled,
            ("power" Core..=) Core.<$> power,
            ("publicDomainNames" Core..=) Core.<$> publicDomainNames,
            ("scale" Core..=) Core.<$> scale
          ]
      )

instance Core.AWSRequest UpdateContainerService where
  type Rs UpdateContainerService = UpdateContainerServiceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.UpdateContainerService")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContainerServiceResponse'
            Core.<$> (x Core..:? "containerService")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateContainerServiceResponse' smart constructor.
data UpdateContainerServiceResponse = UpdateContainerServiceResponse'
  { -- | An object that describes a container service.
    containerService :: Core.Maybe Types.ContainerService,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateContainerServiceResponse' value with any optional fields omitted.
mkUpdateContainerServiceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateContainerServiceResponse
mkUpdateContainerServiceResponse responseStatus =
  UpdateContainerServiceResponse'
    { containerService = Core.Nothing,
      responseStatus
    }

-- | An object that describes a container service.
--
-- /Note:/ Consider using 'containerService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrrsContainerService :: Lens.Lens' UpdateContainerServiceResponse (Core.Maybe Types.ContainerService)
ucsrrsContainerService = Lens.field @"containerService"
{-# DEPRECATED ucsrrsContainerService "Use generic-lens or generic-optics with 'containerService' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrrsResponseStatus :: Lens.Lens' UpdateContainerServiceResponse Core.Int
ucsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
