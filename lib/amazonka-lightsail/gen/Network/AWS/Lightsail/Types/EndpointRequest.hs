-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.EndpointRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.EndpointRequest
  ( EndpointRequest (..),

    -- * Smart constructor
    mkEndpointRequest,

    -- * Lenses
    erHealthCheck,
    erContainerName,
    erContainerPort,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
import qualified Network.AWS.Prelude as Lude

-- | Describes the settings of a public endpoint for an Amazon Lightsail container service.
--
-- /See:/ 'mkEndpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { healthCheck ::
      Lude.Maybe ContainerServiceHealthCheckConfig,
    containerName :: Lude.Text,
    containerPort :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointRequest' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container for the endpoint.
-- * 'containerPort' - The port of the container to which traffic is forwarded to.
-- * 'healthCheck' - An object that describes the health check configuration of the container.
mkEndpointRequest ::
  -- | 'containerName'
  Lude.Text ->
  -- | 'containerPort'
  Lude.Int ->
  EndpointRequest
mkEndpointRequest pContainerName_ pContainerPort_ =
  EndpointRequest'
    { healthCheck = Lude.Nothing,
      containerName = pContainerName_,
      containerPort = pContainerPort_
    }

-- | An object that describes the health check configuration of the container.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erHealthCheck :: Lens.Lens' EndpointRequest (Lude.Maybe ContainerServiceHealthCheckConfig)
erHealthCheck = Lens.lens (healthCheck :: EndpointRequest -> Lude.Maybe ContainerServiceHealthCheckConfig) (\s a -> s {healthCheck = a} :: EndpointRequest)
{-# DEPRECATED erHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The name of the container for the endpoint.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erContainerName :: Lens.Lens' EndpointRequest Lude.Text
erContainerName = Lens.lens (containerName :: EndpointRequest -> Lude.Text) (\s a -> s {containerName = a} :: EndpointRequest)
{-# DEPRECATED erContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The port of the container to which traffic is forwarded to.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erContainerPort :: Lens.Lens' EndpointRequest Lude.Int
erContainerPort = Lens.lens (containerPort :: EndpointRequest -> Lude.Int) (\s a -> s {containerPort = a} :: EndpointRequest)
{-# DEPRECATED erContainerPort "Use generic-lens or generic-optics with 'containerPort' instead." #-}

instance Lude.ToJSON EndpointRequest where
  toJSON EndpointRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("healthCheck" Lude..=) Lude.<$> healthCheck,
            Lude.Just ("containerName" Lude..= containerName),
            Lude.Just ("containerPort" Lude..= containerPort)
          ]
      )
