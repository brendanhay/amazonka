{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateContainerServiceDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment for your Amazon Lightsail container service.
--
-- A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
-- You can deploy containers to your container service using container images from a public registry like Docker Hub, or from your local machine. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-creating-container-images Creating container images for your Amazon Lightsail container services> in the /Lightsail Dev Guide/ .
module Network.AWS.Lightsail.CreateContainerServiceDeployment
  ( -- * Creating a request
    CreateContainerServiceDeployment (..),
    mkCreateContainerServiceDeployment,

    -- ** Request lenses
    ccsdPublicEndpoint,
    ccsdContainers,
    ccsdServiceName,

    -- * Destructuring the response
    CreateContainerServiceDeploymentResponse (..),
    mkCreateContainerServiceDeploymentResponse,

    -- ** Response lenses
    ccsdrsContainerService,
    ccsdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateContainerServiceDeployment' smart constructor.
data CreateContainerServiceDeployment = CreateContainerServiceDeployment'
  { publicEndpoint ::
      Lude.Maybe
        EndpointRequest,
    containers ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Container)
        ),
    serviceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContainerServiceDeployment' with the minimum fields required to make a request.
--
-- * 'containers' - An object that describes the settings of the containers that will be launched on the container service.
-- * 'publicEndpoint' - An object that describes the settings of the public endpoint for the container service.
-- * 'serviceName' - The name of the container service for which to create the deployment.
mkCreateContainerServiceDeployment ::
  -- | 'serviceName'
  Lude.Text ->
  CreateContainerServiceDeployment
mkCreateContainerServiceDeployment pServiceName_ =
  CreateContainerServiceDeployment'
    { publicEndpoint = Lude.Nothing,
      containers = Lude.Nothing,
      serviceName = pServiceName_
    }

-- | An object that describes the settings of the public endpoint for the container service.
--
-- /Note:/ Consider using 'publicEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdPublicEndpoint :: Lens.Lens' CreateContainerServiceDeployment (Lude.Maybe EndpointRequest)
ccsdPublicEndpoint = Lens.lens (publicEndpoint :: CreateContainerServiceDeployment -> Lude.Maybe EndpointRequest) (\s a -> s {publicEndpoint = a} :: CreateContainerServiceDeployment)
{-# DEPRECATED ccsdPublicEndpoint "Use generic-lens or generic-optics with 'publicEndpoint' instead." #-}

-- | An object that describes the settings of the containers that will be launched on the container service.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdContainers :: Lens.Lens' CreateContainerServiceDeployment (Lude.Maybe (Lude.HashMap Lude.Text (Container)))
ccsdContainers = Lens.lens (containers :: CreateContainerServiceDeployment -> Lude.Maybe (Lude.HashMap Lude.Text (Container))) (\s a -> s {containers = a} :: CreateContainerServiceDeployment)
{-# DEPRECATED ccsdContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | The name of the container service for which to create the deployment.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdServiceName :: Lens.Lens' CreateContainerServiceDeployment Lude.Text
ccsdServiceName = Lens.lens (serviceName :: CreateContainerServiceDeployment -> Lude.Text) (\s a -> s {serviceName = a} :: CreateContainerServiceDeployment)
{-# DEPRECATED ccsdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest CreateContainerServiceDeployment where
  type
    Rs CreateContainerServiceDeployment =
      CreateContainerServiceDeploymentResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateContainerServiceDeploymentResponse'
            Lude.<$> (x Lude..?> "containerService")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateContainerServiceDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.CreateContainerServiceDeployment" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateContainerServiceDeployment where
  toJSON CreateContainerServiceDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("publicEndpoint" Lude..=) Lude.<$> publicEndpoint,
            ("containers" Lude..=) Lude.<$> containers,
            Lude.Just ("serviceName" Lude..= serviceName)
          ]
      )

instance Lude.ToPath CreateContainerServiceDeployment where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateContainerServiceDeployment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateContainerServiceDeploymentResponse' smart constructor.
data CreateContainerServiceDeploymentResponse = CreateContainerServiceDeploymentResponse'
  { containerService ::
      Lude.Maybe
        ContainerService,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContainerServiceDeploymentResponse' with the minimum fields required to make a request.
--
-- * 'containerService' - An object that describes a container service.
-- * 'responseStatus' - The response status code.
mkCreateContainerServiceDeploymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateContainerServiceDeploymentResponse
mkCreateContainerServiceDeploymentResponse pResponseStatus_ =
  CreateContainerServiceDeploymentResponse'
    { containerService =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes a container service.
--
-- /Note:/ Consider using 'containerService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdrsContainerService :: Lens.Lens' CreateContainerServiceDeploymentResponse (Lude.Maybe ContainerService)
ccsdrsContainerService = Lens.lens (containerService :: CreateContainerServiceDeploymentResponse -> Lude.Maybe ContainerService) (\s a -> s {containerService = a} :: CreateContainerServiceDeploymentResponse)
{-# DEPRECATED ccsdrsContainerService "Use generic-lens or generic-optics with 'containerService' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdrsResponseStatus :: Lens.Lens' CreateContainerServiceDeploymentResponse Lude.Int
ccsdrsResponseStatus = Lens.lens (responseStatus :: CreateContainerServiceDeploymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateContainerServiceDeploymentResponse)
{-# DEPRECATED ccsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
