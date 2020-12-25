{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingConnectivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingConnectivity
  ( ThingConnectivity (..),

    -- * Smart constructor
    mkThingConnectivity,

    -- * Lenses
    tcConnected,
    tcTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The connectivity status of the thing.
--
-- /See:/ 'mkThingConnectivity' smart constructor.
data ThingConnectivity = ThingConnectivity'
  { -- | True if the thing is connected to the AWS IoT service; false if it is not connected.
    connected :: Core.Maybe Core.Bool,
    -- | The epoch time (in milliseconds) when the thing last connected or disconnected. If the thing has been disconnected for more than a few weeks, the time value might be missing.
    timestamp :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThingConnectivity' value with any optional fields omitted.
mkThingConnectivity ::
  ThingConnectivity
mkThingConnectivity =
  ThingConnectivity'
    { connected = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | True if the thing is connected to the AWS IoT service; false if it is not connected.
--
-- /Note:/ Consider using 'connected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcConnected :: Lens.Lens' ThingConnectivity (Core.Maybe Core.Bool)
tcConnected = Lens.field @"connected"
{-# DEPRECATED tcConnected "Use generic-lens or generic-optics with 'connected' instead." #-}

-- | The epoch time (in milliseconds) when the thing last connected or disconnected. If the thing has been disconnected for more than a few weeks, the time value might be missing.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTimestamp :: Lens.Lens' ThingConnectivity (Core.Maybe Core.Integer)
tcTimestamp = Lens.field @"timestamp"
{-# DEPRECATED tcTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromJSON ThingConnectivity where
  parseJSON =
    Core.withObject "ThingConnectivity" Core.$
      \x ->
        ThingConnectivity'
          Core.<$> (x Core..:? "connected") Core.<*> (x Core..:? "timestamp")
