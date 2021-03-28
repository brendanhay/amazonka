{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputDestination
  ( InputDestination (..)
  -- * Smart constructor
  , mkInputDestination
  -- * Lenses
  , idIp
  , idPort
  , idUrl
  , idVpc
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputDestinationVpc as Types
import qualified Network.AWS.Prelude as Core

-- | The settings for a PUSH type input.
--
-- /See:/ 'mkInputDestination' smart constructor.
data InputDestination = InputDestination'
  { ip :: Core.Maybe Core.Text
    -- ^ The system-generated static IP address of endpoint.
--
-- It remains fixed for the lifetime of the input.
  , port :: Core.Maybe Core.Text
    -- ^ The port number for the input.
  , url :: Core.Maybe Core.Text
    -- ^ This represents the endpoint that the customer stream will be
--
-- pushed to.
  , vpc :: Core.Maybe Types.InputDestinationVpc
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDestination' value with any optional fields omitted.
mkInputDestination
    :: InputDestination
mkInputDestination
  = InputDestination'{ip = Core.Nothing, port = Core.Nothing,
                      url = Core.Nothing, vpc = Core.Nothing}

-- | The system-generated static IP address of endpoint.
--
-- It remains fixed for the lifetime of the input.
--
-- /Note:/ Consider using 'ip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idIp :: Lens.Lens' InputDestination (Core.Maybe Core.Text)
idIp = Lens.field @"ip"
{-# INLINEABLE idIp #-}
{-# DEPRECATED ip "Use generic-lens or generic-optics with 'ip' instead"  #-}

-- | The port number for the input.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idPort :: Lens.Lens' InputDestination (Core.Maybe Core.Text)
idPort = Lens.field @"port"
{-# INLINEABLE idPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | This represents the endpoint that the customer stream will be
--
-- pushed to.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idUrl :: Lens.Lens' InputDestination (Core.Maybe Core.Text)
idUrl = Lens.field @"url"
{-# INLINEABLE idUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idVpc :: Lens.Lens' InputDestination (Core.Maybe Types.InputDestinationVpc)
idVpc = Lens.field @"vpc"
{-# INLINEABLE idVpc #-}
{-# DEPRECATED vpc "Use generic-lens or generic-optics with 'vpc' instead"  #-}

instance Core.FromJSON InputDestination where
        parseJSON
          = Core.withObject "InputDestination" Core.$
              \ x ->
                InputDestination' Core.<$>
                  (x Core..:? "ip") Core.<*> x Core..:? "port" Core.<*>
                    x Core..:? "url"
                    Core.<*> x Core..:? "vpc"
