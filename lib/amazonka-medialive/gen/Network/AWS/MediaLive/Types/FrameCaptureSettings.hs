{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.FrameCaptureSettings
  ( FrameCaptureSettings (..)
  -- * Smart constructor
  , mkFrameCaptureSettings
  -- * Lenses
  , fcsCaptureInterval
  , fcsCaptureIntervalUnits
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit as Types
import qualified Network.AWS.Prelude as Core

-- | Frame Capture Settings
--
-- /See:/ 'mkFrameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { captureInterval :: Core.Natural
    -- ^ The frequency at which to capture frames for inclusion in the output. May be specified in either seconds or milliseconds, as specified by captureIntervalUnits.
  , captureIntervalUnits :: Core.Maybe Types.FrameCaptureIntervalUnit
    -- ^ Unit for the frame capture interval.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FrameCaptureSettings' value with any optional fields omitted.
mkFrameCaptureSettings
    :: Core.Natural -- ^ 'captureInterval'
    -> FrameCaptureSettings
mkFrameCaptureSettings captureInterval
  = FrameCaptureSettings'{captureInterval,
                          captureIntervalUnits = Core.Nothing}

-- | The frequency at which to capture frames for inclusion in the output. May be specified in either seconds or milliseconds, as specified by captureIntervalUnits.
--
-- /Note:/ Consider using 'captureInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsCaptureInterval :: Lens.Lens' FrameCaptureSettings Core.Natural
fcsCaptureInterval = Lens.field @"captureInterval"
{-# INLINEABLE fcsCaptureInterval #-}
{-# DEPRECATED captureInterval "Use generic-lens or generic-optics with 'captureInterval' instead"  #-}

-- | Unit for the frame capture interval.
--
-- /Note:/ Consider using 'captureIntervalUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsCaptureIntervalUnits :: Lens.Lens' FrameCaptureSettings (Core.Maybe Types.FrameCaptureIntervalUnit)
fcsCaptureIntervalUnits = Lens.field @"captureIntervalUnits"
{-# INLINEABLE fcsCaptureIntervalUnits #-}
{-# DEPRECATED captureIntervalUnits "Use generic-lens or generic-optics with 'captureIntervalUnits' instead"  #-}

instance Core.FromJSON FrameCaptureSettings where
        toJSON FrameCaptureSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("captureInterval" Core..= captureInterval),
                  ("captureIntervalUnits" Core..=) Core.<$> captureIntervalUnits])

instance Core.FromJSON FrameCaptureSettings where
        parseJSON
          = Core.withObject "FrameCaptureSettings" Core.$
              \ x ->
                FrameCaptureSettings' Core.<$>
                  (x Core..: "captureInterval") Core.<*>
                    x Core..:? "captureIntervalUnits"
