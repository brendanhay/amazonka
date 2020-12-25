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
    nbContainerPort,
    nbHostPort,
    nbProtocol,
  )
where

import qualified Network.AWS.ECS.Types.BindIP as Types
import qualified Network.AWS.ECS.Types.TransportProtocol as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on the network bindings between a container and its host container instance. After a task reaches the @RUNNING@ status, manual and automatic host and container port assignments are visible in the @networkBindings@ section of 'DescribeTasks' API responses.
--
-- /See:/ 'mkNetworkBinding' smart constructor.
data NetworkBinding = NetworkBinding'
  { -- | The IP address that the container is bound to on the container instance.
    bindIP :: Core.Maybe Types.BindIP,
    -- | The port number on the container that is used with the network binding.
    containerPort :: Core.Maybe Core.Int,
    -- | The port number on the host that is used with the network binding.
    hostPort :: Core.Maybe Core.Int,
    -- | The protocol used for the network binding.
    protocol :: Core.Maybe Types.TransportProtocol
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkBinding' value with any optional fields omitted.
mkNetworkBinding ::
  NetworkBinding
mkNetworkBinding =
  NetworkBinding'
    { bindIP = Core.Nothing,
      containerPort = Core.Nothing,
      hostPort = Core.Nothing,
      protocol = Core.Nothing
    }

-- | The IP address that the container is bound to on the container instance.
--
-- /Note:/ Consider using 'bindIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbBindIP :: Lens.Lens' NetworkBinding (Core.Maybe Types.BindIP)
nbBindIP = Lens.field @"bindIP"
{-# DEPRECATED nbBindIP "Use generic-lens or generic-optics with 'bindIP' instead." #-}

-- | The port number on the container that is used with the network binding.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbContainerPort :: Lens.Lens' NetworkBinding (Core.Maybe Core.Int)
nbContainerPort = Lens.field @"containerPort"
{-# DEPRECATED nbContainerPort "Use generic-lens or generic-optics with 'containerPort' instead." #-}

-- | The port number on the host that is used with the network binding.
--
-- /Note:/ Consider using 'hostPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbHostPort :: Lens.Lens' NetworkBinding (Core.Maybe Core.Int)
nbHostPort = Lens.field @"hostPort"
{-# DEPRECATED nbHostPort "Use generic-lens or generic-optics with 'hostPort' instead." #-}

-- | The protocol used for the network binding.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbProtocol :: Lens.Lens' NetworkBinding (Core.Maybe Types.TransportProtocol)
nbProtocol = Lens.field @"protocol"
{-# DEPRECATED nbProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

instance Core.FromJSON NetworkBinding where
  toJSON NetworkBinding {..} =
    Core.object
      ( Core.catMaybes
          [ ("bindIP" Core..=) Core.<$> bindIP,
            ("containerPort" Core..=) Core.<$> containerPort,
            ("hostPort" Core..=) Core.<$> hostPort,
            ("protocol" Core..=) Core.<$> protocol
          ]
      )

instance Core.FromJSON NetworkBinding where
  parseJSON =
    Core.withObject "NetworkBinding" Core.$
      \x ->
        NetworkBinding'
          Core.<$> (x Core..:? "bindIP")
          Core.<*> (x Core..:? "containerPort")
          Core.<*> (x Core..:? "hostPort")
          Core.<*> (x Core..:? "protocol")
