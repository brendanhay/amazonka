{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImscDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ImscDestinationSettings
  ( ImscDestinationSettings (..)
  -- * Smart constructor
  , mkImscDestinationSettings
  -- * Lenses
  , idsStylePassthrough
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.ImscStylePassthrough as Types
import qualified Network.AWS.Prelude as Core

-- | Settings specific to IMSC caption outputs.
--
-- /See:/ 'mkImscDestinationSettings' smart constructor.
newtype ImscDestinationSettings = ImscDestinationSettings'
  { stylePassthrough :: Core.Maybe Types.ImscStylePassthrough
    -- ^ Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImscDestinationSettings' value with any optional fields omitted.
mkImscDestinationSettings
    :: ImscDestinationSettings
mkImscDestinationSettings
  = ImscDestinationSettings'{stylePassthrough = Core.Nothing}

-- | Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
--
-- /Note:/ Consider using 'stylePassthrough' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsStylePassthrough :: Lens.Lens' ImscDestinationSettings (Core.Maybe Types.ImscStylePassthrough)
idsStylePassthrough = Lens.field @"stylePassthrough"
{-# INLINEABLE idsStylePassthrough #-}
{-# DEPRECATED stylePassthrough "Use generic-lens or generic-optics with 'stylePassthrough' instead"  #-}

instance Core.FromJSON ImscDestinationSettings where
        toJSON ImscDestinationSettings{..}
          = Core.object
              (Core.catMaybes
                 [("stylePassthrough" Core..=) Core.<$> stylePassthrough])

instance Core.FromJSON ImscDestinationSettings where
        parseJSON
          = Core.withObject "ImscDestinationSettings" Core.$
              \ x ->
                ImscDestinationSettings' Core.<$> (x Core..:? "stylePassthrough")
