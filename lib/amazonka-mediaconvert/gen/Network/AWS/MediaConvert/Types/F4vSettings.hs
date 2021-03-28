{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.F4vSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.F4vSettings
  ( F4vSettings (..)
  -- * Smart constructor
  , mkF4vSettings
  -- * Lenses
  , fsMoovPlacement
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.F4vMoovPlacement as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for F4v container
--
-- /See:/ 'mkF4vSettings' smart constructor.
newtype F4vSettings = F4vSettings'
  { moovPlacement :: Core.Maybe Types.F4vMoovPlacement
    -- ^ If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'F4vSettings' value with any optional fields omitted.
mkF4vSettings
    :: F4vSettings
mkF4vSettings = F4vSettings'{moovPlacement = Core.Nothing}

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
--
-- /Note:/ Consider using 'moovPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMoovPlacement :: Lens.Lens' F4vSettings (Core.Maybe Types.F4vMoovPlacement)
fsMoovPlacement = Lens.field @"moovPlacement"
{-# INLINEABLE fsMoovPlacement #-}
{-# DEPRECATED moovPlacement "Use generic-lens or generic-optics with 'moovPlacement' instead"  #-}

instance Core.FromJSON F4vSettings where
        toJSON F4vSettings{..}
          = Core.object
              (Core.catMaybes [("moovPlacement" Core..=) Core.<$> moovPlacement])

instance Core.FromJSON F4vSettings where
        parseJSON
          = Core.withObject "F4vSettings" Core.$
              \ x -> F4vSettings' Core.<$> (x Core..:? "moovPlacement")
