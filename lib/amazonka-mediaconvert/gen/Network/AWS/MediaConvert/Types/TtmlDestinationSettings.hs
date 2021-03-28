{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.TtmlDestinationSettings
  ( TtmlDestinationSettings (..)
  -- * Smart constructor
  , mkTtmlDestinationSettings
  -- * Lenses
  , tdsStylePassthrough
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.TtmlStylePassthrough as Types
import qualified Network.AWS.Prelude as Core

-- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
--
-- /See:/ 'mkTtmlDestinationSettings' smart constructor.
newtype TtmlDestinationSettings = TtmlDestinationSettings'
  { stylePassthrough :: Core.Maybe Types.TtmlStylePassthrough
    -- ^ Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT) to the TTML output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TtmlDestinationSettings' value with any optional fields omitted.
mkTtmlDestinationSettings
    :: TtmlDestinationSettings
mkTtmlDestinationSettings
  = TtmlDestinationSettings'{stylePassthrough = Core.Nothing}

-- | Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT) to the TTML output.
--
-- /Note:/ Consider using 'stylePassthrough' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsStylePassthrough :: Lens.Lens' TtmlDestinationSettings (Core.Maybe Types.TtmlStylePassthrough)
tdsStylePassthrough = Lens.field @"stylePassthrough"
{-# INLINEABLE tdsStylePassthrough #-}
{-# DEPRECATED stylePassthrough "Use generic-lens or generic-optics with 'stylePassthrough' instead"  #-}

instance Core.FromJSON TtmlDestinationSettings where
        toJSON TtmlDestinationSettings{..}
          = Core.object
              (Core.catMaybes
                 [("stylePassthrough" Core..=) Core.<$> stylePassthrough])

instance Core.FromJSON TtmlDestinationSettings where
        parseJSON
          = Core.withObject "TtmlDestinationSettings" Core.$
              \ x ->
                TtmlDestinationSettings' Core.<$> (x Core..:? "stylePassthrough")
