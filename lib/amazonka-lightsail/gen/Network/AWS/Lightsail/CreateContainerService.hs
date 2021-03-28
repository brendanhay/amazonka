{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lightsail container service.
--
-- A Lightsail container service is a compute resource to which you can deploy containers. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-containers Container services in Amazon Lightsail> in the /Lightsail Dev Guide/ .
module Network.AWS.Lightsail.CreateContainerService
    (
    -- * Creating a request
      CreateContainerService (..)
    , mkCreateContainerService
    -- ** Request lenses
    , ccsServiceName
    , ccsPower
    , ccsScale
    , ccsDeployment
    , ccsPublicDomainNames
    , ccsTags

    -- * Destructuring the response
    , CreateContainerServiceResponse (..)
    , mkCreateContainerServiceResponse
    -- ** Response lenses
    , ccsrrsContainerService
    , ccsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateContainerService' smart constructor.
data CreateContainerService = CreateContainerService'
  { serviceName :: Types.ServiceName
    -- ^ The name for the container service.
--
-- The name that you specify for your container service will make up part of its default domain. The default domain of a container service is typically @https://<ServiceName>.<RandomGUID>.<AWSRegion>.cs.amazonlightsail.com@ . If the name of your container service is @container-service-1@ , and it's located in the US East (Ohio) AWS region (@us-east-2@ ), then the domain for your container service will be like the following example: @https://container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@ 
-- The following are the requirements for container service names:
--
--     * Must be unique within each AWS Region in your Lightsail account.
--
--
--     * Must contain 1 to 63 characters.
--
--
--     * Must contain only alphanumeric characters and hyphens.
--
--
--     * A hyphen (-) can separate words but cannot be at the start or end of the name.
--
--
  , power :: Types.ContainerServicePowerName
    -- ^ The power specification for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- Use the @GetContainerServicePowers@ action to get a list of power options that you can specify using this parameter, and their base monthly cost.
  , scale :: Core.Natural
    -- ^ The scale specification for the container service.
--
-- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
  , deployment :: Core.Maybe Types.ContainerServiceDeploymentRequest
    -- ^ An object that describes a deployment for the container service.
--
-- A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
  , publicDomainNames :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- You can specify public domain names using a string to array map as shown in the example later on this page.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the container service.
--
-- For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContainerService' value with any optional fields omitted.
mkCreateContainerService
    :: Types.ServiceName -- ^ 'serviceName'
    -> Types.ContainerServicePowerName -- ^ 'power'
    -> Core.Natural -- ^ 'scale'
    -> CreateContainerService
mkCreateContainerService serviceName power scale
  = CreateContainerService'{serviceName, power, scale,
                            deployment = Core.Nothing, publicDomainNames = Core.Nothing,
                            tags = Core.Nothing}

-- | The name for the container service.
--
-- The name that you specify for your container service will make up part of its default domain. The default domain of a container service is typically @https://<ServiceName>.<RandomGUID>.<AWSRegion>.cs.amazonlightsail.com@ . If the name of your container service is @container-service-1@ , and it's located in the US East (Ohio) AWS region (@us-east-2@ ), then the domain for your container service will be like the following example: @https://container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@ 
-- The following are the requirements for container service names:
--
--     * Must be unique within each AWS Region in your Lightsail account.
--
--
--     * Must contain 1 to 63 characters.
--
--
--     * Must contain only alphanumeric characters and hyphens.
--
--
--     * A hyphen (-) can separate words but cannot be at the start or end of the name.
--
--
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsServiceName :: Lens.Lens' CreateContainerService Types.ServiceName
ccsServiceName = Lens.field @"serviceName"
{-# INLINEABLE ccsServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The power specification for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- Use the @GetContainerServicePowers@ action to get a list of power options that you can specify using this parameter, and their base monthly cost.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsPower :: Lens.Lens' CreateContainerService Types.ContainerServicePowerName
ccsPower = Lens.field @"power"
{-# INLINEABLE ccsPower #-}
{-# DEPRECATED power "Use generic-lens or generic-optics with 'power' instead"  #-}

-- | The scale specification for the container service.
--
-- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsScale :: Lens.Lens' CreateContainerService Core.Natural
ccsScale = Lens.field @"scale"
{-# INLINEABLE ccsScale #-}
{-# DEPRECATED scale "Use generic-lens or generic-optics with 'scale' instead"  #-}

-- | An object that describes a deployment for the container service.
--
-- A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
--
-- /Note:/ Consider using 'deployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsDeployment :: Lens.Lens' CreateContainerService (Core.Maybe Types.ContainerServiceDeploymentRequest)
ccsDeployment = Lens.field @"deployment"
{-# INLINEABLE ccsDeployment #-}
{-# DEPRECATED deployment "Use generic-lens or generic-optics with 'deployment' instead"  #-}

-- | The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- You can specify public domain names using a string to array map as shown in the example later on this page.
--
-- /Note:/ Consider using 'publicDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsPublicDomainNames :: Lens.Lens' CreateContainerService (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
ccsPublicDomainNames = Lens.field @"publicDomainNames"
{-# INLINEABLE ccsPublicDomainNames #-}
{-# DEPRECATED publicDomainNames "Use generic-lens or generic-optics with 'publicDomainNames' instead"  #-}

-- | The tag keys and optional values for the container service.
--
-- For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTags :: Lens.Lens' CreateContainerService (Core.Maybe [Types.Tag])
ccsTags = Lens.field @"tags"
{-# INLINEABLE ccsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateContainerService where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateContainerService where
        toHeaders CreateContainerService{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateContainerService")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateContainerService where
        toJSON CreateContainerService{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("serviceName" Core..= serviceName),
                  Core.Just ("power" Core..= power),
                  Core.Just ("scale" Core..= scale),
                  ("deployment" Core..=) Core.<$> deployment,
                  ("publicDomainNames" Core..=) Core.<$> publicDomainNames,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateContainerService where
        type Rs CreateContainerService = CreateContainerServiceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateContainerServiceResponse' Core.<$>
                   (x Core..:? "containerService") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateContainerServiceResponse' smart constructor.
data CreateContainerServiceResponse = CreateContainerServiceResponse'
  { containerService :: Core.Maybe Types.ContainerService
    -- ^ An object that describes a container service.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateContainerServiceResponse' value with any optional fields omitted.
mkCreateContainerServiceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateContainerServiceResponse
mkCreateContainerServiceResponse responseStatus
  = CreateContainerServiceResponse'{containerService = Core.Nothing,
                                    responseStatus}

-- | An object that describes a container service.
--
-- /Note:/ Consider using 'containerService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsContainerService :: Lens.Lens' CreateContainerServiceResponse (Core.Maybe Types.ContainerService)
ccsrrsContainerService = Lens.field @"containerService"
{-# INLINEABLE ccsrrsContainerService #-}
{-# DEPRECATED containerService "Use generic-lens or generic-optics with 'containerService' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsResponseStatus :: Lens.Lens' CreateContainerServiceResponse Core.Int
ccsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
