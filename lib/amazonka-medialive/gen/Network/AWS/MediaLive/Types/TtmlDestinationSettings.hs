{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.TtmlDestinationSettings
  ( TtmlDestinationSettings (..)
  -- * Smart constructor
  , mkTtmlDestinationSettings
  -- * Lenses
  , tdsStyleControl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.TtmlDestinationStyleControl as Types
import qualified Network.AWS.Prelude as Core

-- | Ttml Destination Settings
--
-- /See:/ 'mkTtmlDestinationSettings' smart constructor.
newtype TtmlDestinationSettings = TtmlDestinationSettings'
  { styleControl :: Core.Maybe Types.TtmlDestinationStyleControl
    -- ^ When set to passthrough, passes through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TtmlDestinationSettings' value with any optional fields omitted.
mkTtmlDestinationSettings
    :: TtmlDestinationSettings
mkTtmlDestinationSettings
  = TtmlDestinationSettings'{styleControl = Core.Nothing}

-- | When set to passthrough, passes through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
--
-- /Note:/ Consider using 'styleControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsStyleControl :: Lens.Lens' TtmlDestinationSettings (Core.Maybe Types.TtmlDestinationStyleControl)
tdsStyleControl = Lens.field @"styleControl"
{-# INLINEABLE tdsStyleControl #-}
{-# DEPRECATED styleControl "Use generic-lens or generic-optics with 'styleControl' instead"  #-}

instance Core.FromJSON TtmlDestinationSettings where
        toJSON TtmlDestinationSettings{..}
          = Core.object
              (Core.catMaybes [("styleControl" Core..=) Core.<$> styleControl])

instance Core.FromJSON TtmlDestinationSettings where
        parseJSON
          = Core.withObject "TtmlDestinationSettings" Core.$
              \ x ->
                TtmlDestinationSettings' Core.<$> (x Core..:? "styleControl")
