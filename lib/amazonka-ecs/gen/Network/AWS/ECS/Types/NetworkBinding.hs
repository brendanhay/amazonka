{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.NetworkBinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkBinding
  ( NetworkBinding (..),

    -- * Smart constructor
    mkNetworkBinding,

    -- * Lenses
    nbBindIP,
    nbProtocol,
    nbHostPort,
    nbContainerPort,
  )
where

import Network.AWS.ECS.Types.TransportProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on the network bindings between a container and its host container instance. After a task reaches the @RUNNING@ status, manual and automatic host and container port assignments are visible in the @networkBindings@ section of 'DescribeTasks' API responses.
--
-- /See:/ 'mkNetworkBinding' smart constructor.
data NetworkBinding = NetworkBinding'
  { -- | The IP address that the container is bound to on the container instance.
    bindIP :: Lude.Maybe Lude.Text,
    -- | The protocol used for the network binding.
    protocol :: Lude.Maybe TransportProtocol,
    -- | The port number on the host that is used with the network binding.
    hostPort :: Lude.Maybe Lude.Int,
    -- | The port number on the container that is used with the network binding.
    containerPort :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkBinding' with the minimum fields required to make a request.
--
-- * 'bindIP' - The IP address that the container is bound to on the container instance.
-- * 'protocol' - The protocol used for the network binding.
-- * 'hostPort' - The port number on the host that is used with the network binding.
-- * 'containerPort' - The port number on the container that is used with the network binding.
mkNetworkBinding ::
  NetworkBinding
mkNetworkBinding =
  NetworkBinding'
    { bindIP = Lude.Nothing,
      protocol = Lude.Nothing,
      hostPort = Lude.Nothing,
      containerPort = Lude.Nothing
    }

-- | The IP address that the container is bound to on the container instance.
--
-- /Note:/ Consider using 'bindIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbBindIP :: Lens.Lens' NetworkBinding (Lude.Maybe Lude.Text)
nbBindIP = Lens.lens (bindIP :: NetworkBinding -> Lude.Maybe Lude.Text) (\s a -> s {bindIP = a} :: NetworkBinding)
{-# DEPRECATED nbBindIP "Use generic-lens or generic-optics with 'bindIP' instead." #-}

-- | The protocol used for the network binding.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbProtocol :: Lens.Lens' NetworkBinding (Lude.Maybe TransportProtocol)
nbProtocol = Lens.lens (protocol :: NetworkBinding -> Lude.Maybe TransportProtocol) (\s a -> s {protocol = a} :: NetworkBinding)
{-# DEPRECATED nbProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The port number on the host that is used with the network binding.
--
-- /Note:/ Consider using 'hostPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbHostPort :: Lens.Lens' NetworkBinding (Lude.Maybe Lude.Int)
nbHostPort = Lens.lens (hostPort :: NetworkBinding -> Lude.Maybe Lude.Int) (\s a -> s {hostPort = a} :: NetworkBinding)
{-# DEPRECATED nbHostPort "Use generic-lens or generic-optics with 'hostPort' instead." #-}

-- | The port number on the container that is used with the network binding.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbContainerPort :: Lens.Lens' NetworkBinding (Lude.Maybe Lude.Int)
nbContainerPort = Lens.lens (containerPort :: NetworkBinding -> Lude.Maybe Lude.Int) (\s a -> s {containerPort = a} :: NetworkBinding)
{-# DEPRECATED nbContainerPort "Use generic-lens or generic-optics with 'containerPort' instead." #-}

instance Lude.FromJSON NetworkBinding where
  parseJSON =
    Lude.withObject
      "NetworkBinding"
      ( \x ->
          NetworkBinding'
            Lude.<$> (x Lude..:? "bindIP")
            Lude.<*> (x Lude..:? "protocol")
            Lude.<*> (x Lude..:? "hostPort")
            Lude.<*> (x Lude..:? "containerPort")
      )

instance Lude.ToJSON NetworkBinding where
  toJSON NetworkBinding' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bindIP" Lude..=) Lude.<$> bindIP,
            ("protocol" Lude..=) Lude.<$> protocol,
            ("hostPort" Lude..=) Lude.<$> hostPort,
            ("containerPort" Lude..=) Lude.<$> containerPort
          ]
      )
