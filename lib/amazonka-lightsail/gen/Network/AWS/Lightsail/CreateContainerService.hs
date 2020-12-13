{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateContainerService (..),
    mkCreateContainerService,

    -- ** Request lenses
    ccsScale,
    ccsPower,
    ccsServiceName,
    ccsPublicDomainNames,
    ccsTags,
    ccsDeployment,

    -- * Destructuring the response
    CreateContainerServiceResponse (..),
    mkCreateContainerServiceResponse,

    -- ** Response lenses
    ccsrsContainerService,
    ccsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateContainerService' smart constructor.
data CreateContainerService = CreateContainerService'
  { -- | The scale specification for the container service.
    --
    -- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
    scale :: Lude.Natural,
    -- | The power specification for the container service.
    --
    -- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
    -- Use the @GetContainerServicePowers@ action to get a list of power options that you can specify using this parameter, and their base monthly cost.
    power :: ContainerServicePowerName,
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
    serviceName :: Lude.Text,
    -- | The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
    --
    -- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
    -- If you don't specify public domain names, then you can use the default domain of the container service.
    -- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
    -- You can specify public domain names using a string to array map as shown in the example later on this page.
    publicDomainNames :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The tag keys and optional values for the container service.
    --
    -- For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Lude.Maybe [Tag],
    -- | An object that describes a deployment for the container service.
    --
    -- A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
    deployment :: Lude.Maybe ContainerServiceDeploymentRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContainerService' with the minimum fields required to make a request.
--
-- * 'scale' - The scale specification for the container service.
--
-- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- * 'power' - The power specification for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- Use the @GetContainerServicePowers@ action to get a list of power options that you can specify using this parameter, and their base monthly cost.
-- * 'serviceName' - The name for the container service.
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
-- * 'publicDomainNames' - The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- You can specify public domain names using a string to array map as shown in the example later on this page.
-- * 'tags' - The tag keys and optional values for the container service.
--
-- For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
-- * 'deployment' - An object that describes a deployment for the container service.
--
-- A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
mkCreateContainerService ::
  -- | 'scale'
  Lude.Natural ->
  -- | 'power'
  ContainerServicePowerName ->
  -- | 'serviceName'
  Lude.Text ->
  CreateContainerService
mkCreateContainerService pScale_ pPower_ pServiceName_ =
  CreateContainerService'
    { scale = pScale_,
      power = pPower_,
      serviceName = pServiceName_,
      publicDomainNames = Lude.Nothing,
      tags = Lude.Nothing,
      deployment = Lude.Nothing
    }

-- | The scale specification for the container service.
--
-- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsScale :: Lens.Lens' CreateContainerService Lude.Natural
ccsScale = Lens.lens (scale :: CreateContainerService -> Lude.Natural) (\s a -> s {scale = a} :: CreateContainerService)
{-# DEPRECATED ccsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | The power specification for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- Use the @GetContainerServicePowers@ action to get a list of power options that you can specify using this parameter, and their base monthly cost.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsPower :: Lens.Lens' CreateContainerService ContainerServicePowerName
ccsPower = Lens.lens (power :: CreateContainerService -> ContainerServicePowerName) (\s a -> s {power = a} :: CreateContainerService)
{-# DEPRECATED ccsPower "Use generic-lens or generic-optics with 'power' instead." #-}

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
ccsServiceName :: Lens.Lens' CreateContainerService Lude.Text
ccsServiceName = Lens.lens (serviceName :: CreateContainerService -> Lude.Text) (\s a -> s {serviceName = a} :: CreateContainerService)
{-# DEPRECATED ccsServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- You can specify public domain names using a string to array map as shown in the example later on this page.
--
-- /Note:/ Consider using 'publicDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsPublicDomainNames :: Lens.Lens' CreateContainerService (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
ccsPublicDomainNames = Lens.lens (publicDomainNames :: CreateContainerService -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {publicDomainNames = a} :: CreateContainerService)
{-# DEPRECATED ccsPublicDomainNames "Use generic-lens or generic-optics with 'publicDomainNames' instead." #-}

-- | The tag keys and optional values for the container service.
--
-- For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTags :: Lens.Lens' CreateContainerService (Lude.Maybe [Tag])
ccsTags = Lens.lens (tags :: CreateContainerService -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateContainerService)
{-# DEPRECATED ccsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | An object that describes a deployment for the container service.
--
-- A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
--
-- /Note:/ Consider using 'deployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsDeployment :: Lens.Lens' CreateContainerService (Lude.Maybe ContainerServiceDeploymentRequest)
ccsDeployment = Lens.lens (deployment :: CreateContainerService -> Lude.Maybe ContainerServiceDeploymentRequest) (\s a -> s {deployment = a} :: CreateContainerService)
{-# DEPRECATED ccsDeployment "Use generic-lens or generic-optics with 'deployment' instead." #-}

instance Lude.AWSRequest CreateContainerService where
  type Rs CreateContainerService = CreateContainerServiceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateContainerServiceResponse'
            Lude.<$> (x Lude..?> "containerService")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateContainerService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateContainerService" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateContainerService where
  toJSON CreateContainerService' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("scale" Lude..= scale),
            Lude.Just ("power" Lude..= power),
            Lude.Just ("serviceName" Lude..= serviceName),
            ("publicDomainNames" Lude..=) Lude.<$> publicDomainNames,
            ("tags" Lude..=) Lude.<$> tags,
            ("deployment" Lude..=) Lude.<$> deployment
          ]
      )

instance Lude.ToPath CreateContainerService where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateContainerService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateContainerServiceResponse' smart constructor.
data CreateContainerServiceResponse = CreateContainerServiceResponse'
  { -- | An object that describes a container service.
    containerService :: Lude.Maybe ContainerService,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContainerServiceResponse' with the minimum fields required to make a request.
--
-- * 'containerService' - An object that describes a container service.
-- * 'responseStatus' - The response status code.
mkCreateContainerServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateContainerServiceResponse
mkCreateContainerServiceResponse pResponseStatus_ =
  CreateContainerServiceResponse'
    { containerService = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes a container service.
--
-- /Note:/ Consider using 'containerService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrsContainerService :: Lens.Lens' CreateContainerServiceResponse (Lude.Maybe ContainerService)
ccsrsContainerService = Lens.lens (containerService :: CreateContainerServiceResponse -> Lude.Maybe ContainerService) (\s a -> s {containerService = a} :: CreateContainerServiceResponse)
{-# DEPRECATED ccsrsContainerService "Use generic-lens or generic-optics with 'containerService' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrsResponseStatus :: Lens.Lens' CreateContainerServiceResponse Lude.Int
ccsrsResponseStatus = Lens.lens (responseStatus :: CreateContainerServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateContainerServiceResponse)
{-# DEPRECATED ccsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
