-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
  ( ContainerServiceDeploymentRequest (..),

    -- * Smart constructor
    mkContainerServiceDeploymentRequest,

    -- * Lenses
    csdrPublicEndpoint,
    csdrContainers,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.EndpointRequest
import qualified Network.AWS.Prelude as Lude

-- | Describes a container deployment configuration of an Amazon Lightsail container service.
--
-- A deployment specifies the settings, such as the ports and launch command, of containers that are deployed to your container service.
--
-- /See:/ 'mkContainerServiceDeploymentRequest' smart constructor.
data ContainerServiceDeploymentRequest = ContainerServiceDeploymentRequest'
  { publicEndpoint ::
      Lude.Maybe
        EndpointRequest,
    containers ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Container)
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerServiceDeploymentRequest' with the minimum fields required to make a request.
--
-- * 'containers' - An object that describes the configuration for the containers of the deployment.
-- * 'publicEndpoint' - An object that describes the endpoint of the deployment.
mkContainerServiceDeploymentRequest ::
  ContainerServiceDeploymentRequest
mkContainerServiceDeploymentRequest =
  ContainerServiceDeploymentRequest'
    { publicEndpoint = Lude.Nothing,
      containers = Lude.Nothing
    }

-- | An object that describes the endpoint of the deployment.
--
-- /Note:/ Consider using 'publicEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrPublicEndpoint :: Lens.Lens' ContainerServiceDeploymentRequest (Lude.Maybe EndpointRequest)
csdrPublicEndpoint = Lens.lens (publicEndpoint :: ContainerServiceDeploymentRequest -> Lude.Maybe EndpointRequest) (\s a -> s {publicEndpoint = a} :: ContainerServiceDeploymentRequest)
{-# DEPRECATED csdrPublicEndpoint "Use generic-lens or generic-optics with 'publicEndpoint' instead." #-}

-- | An object that describes the configuration for the containers of the deployment.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrContainers :: Lens.Lens' ContainerServiceDeploymentRequest (Lude.Maybe (Lude.HashMap Lude.Text (Container)))
csdrContainers = Lens.lens (containers :: ContainerServiceDeploymentRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Container))) (\s a -> s {containers = a} :: ContainerServiceDeploymentRequest)
{-# DEPRECATED csdrContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

instance Lude.ToJSON ContainerServiceDeploymentRequest where
  toJSON ContainerServiceDeploymentRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("publicEndpoint" Lude..=) Lude.<$> publicEndpoint,
            ("containers" Lude..=) Lude.<$> containers
          ]
      )
