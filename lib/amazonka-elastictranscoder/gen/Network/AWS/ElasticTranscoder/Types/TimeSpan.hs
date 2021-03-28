{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.TimeSpan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.TimeSpan
  ( TimeSpan (..)
  -- * Smart constructor
  , mkTimeSpan
  -- * Lenses
  , tsDuration
  , tsStartTime
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.Time as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings that determine when a clip begins and how long it lasts.
--
-- /See:/ 'mkTimeSpan' smart constructor.
data TimeSpan = TimeSpan'
  { duration :: Core.Maybe Types.Time
    -- ^ The duration of the clip. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder creates an output file from StartTime to the end of the file.
--
-- If you specify a value longer than the duration of the input file, Elastic Transcoder transcodes the file and returns a warning message.
  , startTime :: Core.Maybe Types.Time
    -- ^ The place in the input file where you want a clip to start. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder starts at the beginning of the input file.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimeSpan' value with any optional fields omitted.
mkTimeSpan
    :: TimeSpan
mkTimeSpan
  = TimeSpan'{duration = Core.Nothing, startTime = Core.Nothing}

-- | The duration of the clip. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder creates an output file from StartTime to the end of the file.
--
-- If you specify a value longer than the duration of the input file, Elastic Transcoder transcodes the file and returns a warning message.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDuration :: Lens.Lens' TimeSpan (Core.Maybe Types.Time)
tsDuration = Lens.field @"duration"
{-# INLINEABLE tsDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The place in the input file where you want a clip to start. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder starts at the beginning of the input file.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStartTime :: Lens.Lens' TimeSpan (Core.Maybe Types.Time)
tsStartTime = Lens.field @"startTime"
{-# INLINEABLE tsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.FromJSON TimeSpan where
        toJSON TimeSpan{..}
          = Core.object
              (Core.catMaybes
                 [("Duration" Core..=) Core.<$> duration,
                  ("StartTime" Core..=) Core.<$> startTime])

instance Core.FromJSON TimeSpan where
        parseJSON
          = Core.withObject "TimeSpan" Core.$
              \ x ->
                TimeSpan' Core.<$>
                  (x Core..:? "Duration") Core.<*> x Core..:? "StartTime"
