{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.StreamSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.StreamSelection
  ( StreamSelection (..)
  -- * Smart constructor
  , mkStreamSelection
  -- * Lenses
  , ssMaxVideoBitsPerSecond
  , ssMinVideoBitsPerSecond
  , ssStreamOrder
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.StreamOrder as Types
import qualified Network.AWS.Prelude as Core

-- | A StreamSelection configuration.
--
-- /See:/ 'mkStreamSelection' smart constructor.
data StreamSelection = StreamSelection'
  { maxVideoBitsPerSecond :: Core.Maybe Core.Int
    -- ^ The maximum video bitrate (bps) to include in output.
  , minVideoBitsPerSecond :: Core.Maybe Core.Int
    -- ^ The minimum video bitrate (bps) to include in output.
  , streamOrder :: Core.Maybe Types.StreamOrder
    -- ^ A directive that determines the order of streams in the output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamSelection' value with any optional fields omitted.
mkStreamSelection
    :: StreamSelection
mkStreamSelection
  = StreamSelection'{maxVideoBitsPerSecond = Core.Nothing,
                     minVideoBitsPerSecond = Core.Nothing, streamOrder = Core.Nothing}

-- | The maximum video bitrate (bps) to include in output.
--
-- /Note:/ Consider using 'maxVideoBitsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMaxVideoBitsPerSecond :: Lens.Lens' StreamSelection (Core.Maybe Core.Int)
ssMaxVideoBitsPerSecond = Lens.field @"maxVideoBitsPerSecond"
{-# INLINEABLE ssMaxVideoBitsPerSecond #-}
{-# DEPRECATED maxVideoBitsPerSecond "Use generic-lens or generic-optics with 'maxVideoBitsPerSecond' instead"  #-}

-- | The minimum video bitrate (bps) to include in output.
--
-- /Note:/ Consider using 'minVideoBitsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMinVideoBitsPerSecond :: Lens.Lens' StreamSelection (Core.Maybe Core.Int)
ssMinVideoBitsPerSecond = Lens.field @"minVideoBitsPerSecond"
{-# INLINEABLE ssMinVideoBitsPerSecond #-}
{-# DEPRECATED minVideoBitsPerSecond "Use generic-lens or generic-optics with 'minVideoBitsPerSecond' instead"  #-}

-- | A directive that determines the order of streams in the output.
--
-- /Note:/ Consider using 'streamOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamOrder :: Lens.Lens' StreamSelection (Core.Maybe Types.StreamOrder)
ssStreamOrder = Lens.field @"streamOrder"
{-# INLINEABLE ssStreamOrder #-}
{-# DEPRECATED streamOrder "Use generic-lens or generic-optics with 'streamOrder' instead"  #-}

instance Core.FromJSON StreamSelection where
        toJSON StreamSelection{..}
          = Core.object
              (Core.catMaybes
                 [("maxVideoBitsPerSecond" Core..=) Core.<$> maxVideoBitsPerSecond,
                  ("minVideoBitsPerSecond" Core..=) Core.<$> minVideoBitsPerSecond,
                  ("streamOrder" Core..=) Core.<$> streamOrder])

instance Core.FromJSON StreamSelection where
        parseJSON
          = Core.withObject "StreamSelection" Core.$
              \ x ->
                StreamSelection' Core.<$>
                  (x Core..:? "maxVideoBitsPerSecond") Core.<*>
                    x Core..:? "minVideoBitsPerSecond"
                    Core.<*> x Core..:? "streamOrder"
