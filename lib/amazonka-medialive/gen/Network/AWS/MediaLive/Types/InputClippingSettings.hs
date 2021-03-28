{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputClippingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputClippingSettings
  ( InputClippingSettings (..)
  -- * Smart constructor
  , mkInputClippingSettings
  -- * Lenses
  , icsInputTimecodeSource
  , icsStartTimecode
  , icsStopTimecode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputTimecodeSource as Types
import qualified Network.AWS.MediaLive.Types.StartTimecode as Types
import qualified Network.AWS.MediaLive.Types.StopTimecode as Types
import qualified Network.AWS.Prelude as Core

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- /See:/ 'mkInputClippingSettings' smart constructor.
data InputClippingSettings = InputClippingSettings'
  { inputTimecodeSource :: Types.InputTimecodeSource
    -- ^ The source of the timecodes in the source being clipped.
  , startTimecode :: Core.Maybe Types.StartTimecode
    -- ^ Settings to identify the start of the clip.
  , stopTimecode :: Core.Maybe Types.StopTimecode
    -- ^ Settings to identify the end of the clip.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputClippingSettings' value with any optional fields omitted.
mkInputClippingSettings
    :: Types.InputTimecodeSource -- ^ 'inputTimecodeSource'
    -> InputClippingSettings
mkInputClippingSettings inputTimecodeSource
  = InputClippingSettings'{inputTimecodeSource,
                           startTimecode = Core.Nothing, stopTimecode = Core.Nothing}

-- | The source of the timecodes in the source being clipped.
--
-- /Note:/ Consider using 'inputTimecodeSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsInputTimecodeSource :: Lens.Lens' InputClippingSettings Types.InputTimecodeSource
icsInputTimecodeSource = Lens.field @"inputTimecodeSource"
{-# INLINEABLE icsInputTimecodeSource #-}
{-# DEPRECATED inputTimecodeSource "Use generic-lens or generic-optics with 'inputTimecodeSource' instead"  #-}

-- | Settings to identify the start of the clip.
--
-- /Note:/ Consider using 'startTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsStartTimecode :: Lens.Lens' InputClippingSettings (Core.Maybe Types.StartTimecode)
icsStartTimecode = Lens.field @"startTimecode"
{-# INLINEABLE icsStartTimecode #-}
{-# DEPRECATED startTimecode "Use generic-lens or generic-optics with 'startTimecode' instead"  #-}

-- | Settings to identify the end of the clip.
--
-- /Note:/ Consider using 'stopTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsStopTimecode :: Lens.Lens' InputClippingSettings (Core.Maybe Types.StopTimecode)
icsStopTimecode = Lens.field @"stopTimecode"
{-# INLINEABLE icsStopTimecode #-}
{-# DEPRECATED stopTimecode "Use generic-lens or generic-optics with 'stopTimecode' instead"  #-}

instance Core.FromJSON InputClippingSettings where
        toJSON InputClippingSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("inputTimecodeSource" Core..= inputTimecodeSource),
                  ("startTimecode" Core..=) Core.<$> startTimecode,
                  ("stopTimecode" Core..=) Core.<$> stopTimecode])

instance Core.FromJSON InputClippingSettings where
        parseJSON
          = Core.withObject "InputClippingSettings" Core.$
              \ x ->
                InputClippingSettings' Core.<$>
                  (x Core..: "inputTimecodeSource") Core.<*>
                    x Core..:? "startTimecode"
                    Core.<*> x Core..:? "stopTimecode"
