{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexOutputDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexOutputDestination
  ( MultiplexOutputDestination (..)
  -- * Smart constructor
  , mkMultiplexOutputDestination
  -- * Lenses
  , modMediaConnectSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Multiplex output destination settings
--
-- /See:/ 'mkMultiplexOutputDestination' smart constructor.
newtype MultiplexOutputDestination = MultiplexOutputDestination'
  { mediaConnectSettings :: Core.Maybe Types.MultiplexMediaConnectOutputDestinationSettings
    -- ^ Multiplex MediaConnect output destination settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexOutputDestination' value with any optional fields omitted.
mkMultiplexOutputDestination
    :: MultiplexOutputDestination
mkMultiplexOutputDestination
  = MultiplexOutputDestination'{mediaConnectSettings = Core.Nothing}

-- | Multiplex MediaConnect output destination settings.
--
-- /Note:/ Consider using 'mediaConnectSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
modMediaConnectSettings :: Lens.Lens' MultiplexOutputDestination (Core.Maybe Types.MultiplexMediaConnectOutputDestinationSettings)
modMediaConnectSettings = Lens.field @"mediaConnectSettings"
{-# INLINEABLE modMediaConnectSettings #-}
{-# DEPRECATED mediaConnectSettings "Use generic-lens or generic-optics with 'mediaConnectSettings' instead"  #-}

instance Core.FromJSON MultiplexOutputDestination where
        parseJSON
          = Core.withObject "MultiplexOutputDestination" Core.$
              \ x ->
                MultiplexOutputDestination' Core.<$>
                  (x Core..:? "mediaConnectSettings")
