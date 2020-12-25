{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StartTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StartTimecode
  ( StartTimecode (..),

    -- * Smart constructor
    mkStartTimecode,

    -- * Lenses
    stTimecode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings to identify the start of the clip.
--
-- /See:/ 'mkStartTimecode' smart constructor.
newtype StartTimecode = StartTimecode'
  { -- | The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
    timecode :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartTimecode' value with any optional fields omitted.
mkStartTimecode ::
  StartTimecode
mkStartTimecode = StartTimecode' {timecode = Core.Nothing}

-- | The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
--
-- /Note:/ Consider using 'timecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTimecode :: Lens.Lens' StartTimecode (Core.Maybe Core.Text)
stTimecode = Lens.field @"timecode"
{-# DEPRECATED stTimecode "Use generic-lens or generic-optics with 'timecode' instead." #-}

instance Core.FromJSON StartTimecode where
  toJSON StartTimecode {..} =
    Core.object
      (Core.catMaybes [("timecode" Core..=) Core.<$> timecode])

instance Core.FromJSON StartTimecode where
  parseJSON =
    Core.withObject "StartTimecode" Core.$
      \x -> StartTimecode' Core.<$> (x Core..:? "timecode")
