{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceEndpoint
  ( ContainerServiceEndpoint (..),

    -- * Smart constructor
    mkContainerServiceEndpoint,

    -- * Lenses
    cseHealthCheck,
    cseContainerName,
    cseContainerPort,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
import qualified Network.AWS.Prelude as Lude

-- | Describes the public endpoint configuration of a deployment of an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerServiceEndpoint' smart constructor.
data ContainerServiceEndpoint = ContainerServiceEndpoint'
  { -- | An object that describes the health check configuration of the container.
    healthCheck :: Lude.Maybe ContainerServiceHealthCheckConfig,
    -- | The name of the container entry of the deployment that the endpoint configuration applies to.
    containerName :: Lude.Maybe Lude.Text,
    -- | The port of the specified container to which traffic is forwarded to.
    containerPort :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerServiceEndpoint' with the minimum fields required to make a request.
--
-- * 'healthCheck' - An object that describes the health check configuration of the container.
-- * 'containerName' - The name of the container entry of the deployment that the endpoint configuration applies to.
-- * 'containerPort' - The port of the specified container to which traffic is forwarded to.
mkContainerServiceEndpoint ::
  ContainerServiceEndpoint
mkContainerServiceEndpoint =
  ContainerServiceEndpoint'
    { healthCheck = Lude.Nothing,
      containerName = Lude.Nothing,
      containerPort = Lude.Nothing
    }

-- | An object that describes the health check configuration of the container.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseHealthCheck :: Lens.Lens' ContainerServiceEndpoint (Lude.Maybe ContainerServiceHealthCheckConfig)
cseHealthCheck = Lens.lens (healthCheck :: ContainerServiceEndpoint -> Lude.Maybe ContainerServiceHealthCheckConfig) (\s a -> s {healthCheck = a} :: ContainerServiceEndpoint)
{-# DEPRECATED cseHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The name of the container entry of the deployment that the endpoint configuration applies to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseContainerName :: Lens.Lens' ContainerServiceEndpoint (Lude.Maybe Lude.Text)
cseContainerName = Lens.lens (containerName :: ContainerServiceEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {containerName = a} :: ContainerServiceEndpoint)
{-# DEPRECATED cseContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The port of the specified container to which traffic is forwarded to.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseContainerPort :: Lens.Lens' ContainerServiceEndpoint (Lude.Maybe Lude.Int)
cseContainerPort = Lens.lens (containerPort :: ContainerServiceEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {containerPort = a} :: ContainerServiceEndpoint)
{-# DEPRECATED cseContainerPort "Use generic-lens or generic-optics with 'containerPort' instead." #-}

instance Lude.FromJSON ContainerServiceEndpoint where
  parseJSON =
    Lude.withObject
      "ContainerServiceEndpoint"
      ( \x ->
          ContainerServiceEndpoint'
            Lude.<$> (x Lude..:? "healthCheck")
            Lude.<*> (x Lude..:? "containerName")
            Lude.<*> (x Lude..:? "containerPort")
      )
