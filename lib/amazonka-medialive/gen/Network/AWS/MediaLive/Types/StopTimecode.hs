{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StopTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.StopTimecode
  ( StopTimecode (..)
  -- * Smart constructor
  , mkStopTimecode
  -- * Lenses
  , sLastFrameClippingBehavior
  , sTimecode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.LastFrameClippingBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | Settings to identify the end of the clip.
--
-- /See:/ 'mkStopTimecode' smart constructor.
data StopTimecode = StopTimecode'
  { lastFrameClippingBehavior :: Core.Maybe Types.LastFrameClippingBehavior
    -- ^ If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
  , timecode :: Core.Maybe Core.Text
    -- ^ The timecode for the frame where you want to stop the clip. Optional; if not specified, the clip continues to the end of the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopTimecode' value with any optional fields omitted.
mkStopTimecode
    :: StopTimecode
mkStopTimecode
  = StopTimecode'{lastFrameClippingBehavior = Core.Nothing,
                  timecode = Core.Nothing}

-- | If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
--
-- /Note:/ Consider using 'lastFrameClippingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLastFrameClippingBehavior :: Lens.Lens' StopTimecode (Core.Maybe Types.LastFrameClippingBehavior)
sLastFrameClippingBehavior = Lens.field @"lastFrameClippingBehavior"
{-# INLINEABLE sLastFrameClippingBehavior #-}
{-# DEPRECATED lastFrameClippingBehavior "Use generic-lens or generic-optics with 'lastFrameClippingBehavior' instead"  #-}

-- | The timecode for the frame where you want to stop the clip. Optional; if not specified, the clip continues to the end of the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
--
-- /Note:/ Consider using 'timecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimecode :: Lens.Lens' StopTimecode (Core.Maybe Core.Text)
sTimecode = Lens.field @"timecode"
{-# INLINEABLE sTimecode #-}
{-# DEPRECATED timecode "Use generic-lens or generic-optics with 'timecode' instead"  #-}

instance Core.FromJSON StopTimecode where
        toJSON StopTimecode{..}
          = Core.object
              (Core.catMaybes
                 [("lastFrameClippingBehavior" Core..=) Core.<$>
                    lastFrameClippingBehavior,
                  ("timecode" Core..=) Core.<$> timecode])

instance Core.FromJSON StopTimecode where
        parseJSON
          = Core.withObject "StopTimecode" Core.$
              \ x ->
                StopTimecode' Core.<$>
                  (x Core..:? "lastFrameClippingBehavior") Core.<*>
                    x Core..:? "timecode"
