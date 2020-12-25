{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Container
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Container
  ( Container (..),

    -- * Smart constructor
    mkContainer,

    -- * Lenses
    cCommand,
    cEnvironment,
    cImage,
    cPorts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ContainerServiceProtocol as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the settings of a container that will be launched, or that is launched, to an Amazon Lightsail container service.
--
-- /See:/ 'mkContainer' smart constructor.
data Container = Container'
  { -- | The launch command for the container.
    command :: Core.Maybe [Types.String],
    -- | The environment variables of the container.
    environment :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The name of the image used for the container.
    --
    -- Container images sourced from your Lightsail container service, that are registered and stored on your service, start with a colon (@:@ ). For example, @:container-service-1.mystaticwebsite.1@ . Container images sourced from a public registry like Docker Hub don't start with a colon. For example, @nginx:latest@ or @nginx@ .
    image :: Core.Maybe Types.String,
    -- | The open firewall ports of the container.
    ports :: Core.Maybe (Core.HashMap Types.String Types.ContainerServiceProtocol)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Container' value with any optional fields omitted.
mkContainer ::
  Container
mkContainer =
  Container'
    { command = Core.Nothing,
      environment = Core.Nothing,
      image = Core.Nothing,
      ports = Core.Nothing
    }

-- | The launch command for the container.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommand :: Lens.Lens' Container (Core.Maybe [Types.String])
cCommand = Lens.field @"command"
{-# DEPRECATED cCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The environment variables of the container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnvironment :: Lens.Lens' Container (Core.Maybe (Core.HashMap Types.String Types.String))
cEnvironment = Lens.field @"environment"
{-# DEPRECATED cEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are registered and stored on your service, start with a colon (@:@ ). For example, @:container-service-1.mystaticwebsite.1@ . Container images sourced from a public registry like Docker Hub don't start with a colon. For example, @nginx:latest@ or @nginx@ .
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cImage :: Lens.Lens' Container (Core.Maybe Types.String)
cImage = Lens.field @"image"
{-# DEPRECATED cImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The open firewall ports of the container.
--
-- /Note:/ Consider using 'ports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPorts :: Lens.Lens' Container (Core.Maybe (Core.HashMap Types.String Types.ContainerServiceProtocol))
cPorts = Lens.field @"ports"
{-# DEPRECATED cPorts "Use generic-lens or generic-optics with 'ports' instead." #-}

instance Core.FromJSON Container where
  toJSON Container {..} =
    Core.object
      ( Core.catMaybes
          [ ("command" Core..=) Core.<$> command,
            ("environment" Core..=) Core.<$> environment,
            ("image" Core..=) Core.<$> image,
            ("ports" Core..=) Core.<$> ports
          ]
      )

instance Core.FromJSON Container where
  parseJSON =
    Core.withObject "Container" Core.$
      \x ->
        Container'
          Core.<$> (x Core..:? "command")
          Core.<*> (x Core..:? "environment")
          Core.<*> (x Core..:? "image")
          Core.<*> (x Core..:? "ports")
