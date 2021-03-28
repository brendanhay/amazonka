{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Container
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Container
  ( Container (..)
  -- * Smart constructor
  , mkContainer
  -- * Lenses
  , cCommand
  , cEnvironment
  , cImage
  , cPorts
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ContainerServiceProtocol as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the settings of a container that will be launched, or that is launched, to an Amazon Lightsail container service.
--
-- /See:/ 'mkContainer' smart constructor.
data Container = Container'
  { command :: Core.Maybe [Core.Text]
    -- ^ The launch command for the container.
  , environment :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The environment variables of the container.
  , image :: Core.Maybe Core.Text
    -- ^ The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are registered and stored on your service, start with a colon (@:@ ). For example, @:container-service-1.mystaticwebsite.1@ . Container images sourced from a public registry like Docker Hub don't start with a colon. For example, @nginx:latest@ or @nginx@ .
  , ports :: Core.Maybe (Core.HashMap Core.Text Types.ContainerServiceProtocol)
    -- ^ The open firewall ports of the container.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Container' value with any optional fields omitted.
mkContainer
    :: Container
mkContainer
  = Container'{command = Core.Nothing, environment = Core.Nothing,
               image = Core.Nothing, ports = Core.Nothing}

-- | The launch command for the container.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommand :: Lens.Lens' Container (Core.Maybe [Core.Text])
cCommand = Lens.field @"command"
{-# INLINEABLE cCommand #-}
{-# DEPRECATED command "Use generic-lens or generic-optics with 'command' instead"  #-}

-- | The environment variables of the container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnvironment :: Lens.Lens' Container (Core.Maybe (Core.HashMap Core.Text Core.Text))
cEnvironment = Lens.field @"environment"
{-# INLINEABLE cEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are registered and stored on your service, start with a colon (@:@ ). For example, @:container-service-1.mystaticwebsite.1@ . Container images sourced from a public registry like Docker Hub don't start with a colon. For example, @nginx:latest@ or @nginx@ .
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cImage :: Lens.Lens' Container (Core.Maybe Core.Text)
cImage = Lens.field @"image"
{-# INLINEABLE cImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The open firewall ports of the container.
--
-- /Note:/ Consider using 'ports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPorts :: Lens.Lens' Container (Core.Maybe (Core.HashMap Core.Text Types.ContainerServiceProtocol))
cPorts = Lens.field @"ports"
{-# INLINEABLE cPorts #-}
{-# DEPRECATED ports "Use generic-lens or generic-optics with 'ports' instead"  #-}

instance Core.FromJSON Container where
        toJSON Container{..}
          = Core.object
              (Core.catMaybes
                 [("command" Core..=) Core.<$> command,
                  ("environment" Core..=) Core.<$> environment,
                  ("image" Core..=) Core.<$> image,
                  ("ports" Core..=) Core.<$> ports])

instance Core.FromJSON Container where
        parseJSON
          = Core.withObject "Container" Core.$
              \ x ->
                Container' Core.<$>
                  (x Core..:? "command") Core.<*> x Core..:? "environment" Core.<*>
                    x Core..:? "image"
                    Core.<*> x Core..:? "ports"
