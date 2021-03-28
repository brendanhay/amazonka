{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexOutputSettings
  ( MultiplexOutputSettings (..)
  -- * Smart constructor
  , mkMultiplexOutputSettings
  -- * Lenses
  , mosDestination
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.OutputLocationRef as Types
import qualified Network.AWS.Prelude as Core

-- | Multiplex Output Settings
--
-- /See:/ 'mkMultiplexOutputSettings' smart constructor.
newtype MultiplexOutputSettings = MultiplexOutputSettings'
  { destination :: Types.OutputLocationRef
    -- ^ Destination is a Multiplex.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexOutputSettings' value with any optional fields omitted.
mkMultiplexOutputSettings
    :: Types.OutputLocationRef -- ^ 'destination'
    -> MultiplexOutputSettings
mkMultiplexOutputSettings destination
  = MultiplexOutputSettings'{destination}

-- | Destination is a Multiplex.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mosDestination :: Lens.Lens' MultiplexOutputSettings Types.OutputLocationRef
mosDestination = Lens.field @"destination"
{-# INLINEABLE mosDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

instance Core.FromJSON MultiplexOutputSettings where
        toJSON MultiplexOutputSettings{..}
          = Core.object
              (Core.catMaybes [Core.Just ("destination" Core..= destination)])

instance Core.FromJSON MultiplexOutputSettings where
        parseJSON
          = Core.withObject "MultiplexOutputSettings" Core.$
              \ x -> MultiplexOutputSettings' Core.<$> (x Core..: "destination")
