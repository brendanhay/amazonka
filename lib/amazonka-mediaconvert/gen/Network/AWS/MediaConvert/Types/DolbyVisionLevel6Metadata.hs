{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
  ( DolbyVisionLevel6Metadata (..)
  -- * Smart constructor
  , mkDolbyVisionLevel6Metadata
  -- * Lenses
  , dvlmMaxCll
  , dvlmMaxFall
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to override the MaxCLL and MaxFALL values in your input with new values.
--
-- /See:/ 'mkDolbyVisionLevel6Metadata' smart constructor.
data DolbyVisionLevel6Metadata = DolbyVisionLevel6Metadata'
  { maxCll :: Core.Maybe Core.Natural
    -- ^ Maximum Content Light Level. Static HDR metadata that corresponds to the brightest pixel in the entire stream. Measured in nits.
  , maxFall :: Core.Maybe Core.Natural
    -- ^ Maximum Frame-Average Light Level. Static HDR metadata that corresponds to the highest frame-average brightness in the entire stream. Measured in nits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DolbyVisionLevel6Metadata' value with any optional fields omitted.
mkDolbyVisionLevel6Metadata
    :: DolbyVisionLevel6Metadata
mkDolbyVisionLevel6Metadata
  = DolbyVisionLevel6Metadata'{maxCll = Core.Nothing,
                               maxFall = Core.Nothing}

-- | Maximum Content Light Level. Static HDR metadata that corresponds to the brightest pixel in the entire stream. Measured in nits.
--
-- /Note:/ Consider using 'maxCll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvlmMaxCll :: Lens.Lens' DolbyVisionLevel6Metadata (Core.Maybe Core.Natural)
dvlmMaxCll = Lens.field @"maxCll"
{-# INLINEABLE dvlmMaxCll #-}
{-# DEPRECATED maxCll "Use generic-lens or generic-optics with 'maxCll' instead"  #-}

-- | Maximum Frame-Average Light Level. Static HDR metadata that corresponds to the highest frame-average brightness in the entire stream. Measured in nits.
--
-- /Note:/ Consider using 'maxFall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvlmMaxFall :: Lens.Lens' DolbyVisionLevel6Metadata (Core.Maybe Core.Natural)
dvlmMaxFall = Lens.field @"maxFall"
{-# INLINEABLE dvlmMaxFall #-}
{-# DEPRECATED maxFall "Use generic-lens or generic-optics with 'maxFall' instead"  #-}

instance Core.FromJSON DolbyVisionLevel6Metadata where
        toJSON DolbyVisionLevel6Metadata{..}
          = Core.object
              (Core.catMaybes
                 [("maxCll" Core..=) Core.<$> maxCll,
                  ("maxFall" Core..=) Core.<$> maxFall])

instance Core.FromJSON DolbyVisionLevel6Metadata where
        parseJSON
          = Core.withObject "DolbyVisionLevel6Metadata" Core.$
              \ x ->
                DolbyVisionLevel6Metadata' Core.<$>
                  (x Core..:? "maxCll") Core.<*> x Core..:? "maxFall"
