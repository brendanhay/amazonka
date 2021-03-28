{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorPid
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.VideoSelectorPid
  ( VideoSelectorPid (..)
  -- * Smart constructor
  , mkVideoSelectorPid
  -- * Lenses
  , vspPid
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Video Selector Pid
--
-- /See:/ 'mkVideoSelectorPid' smart constructor.
newtype VideoSelectorPid = VideoSelectorPid'
  { pid :: Core.Maybe Core.Natural
    -- ^ Selects a specific PID from within a video source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VideoSelectorPid' value with any optional fields omitted.
mkVideoSelectorPid
    :: VideoSelectorPid
mkVideoSelectorPid = VideoSelectorPid'{pid = Core.Nothing}

-- | Selects a specific PID from within a video source.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspPid :: Lens.Lens' VideoSelectorPid (Core.Maybe Core.Natural)
vspPid = Lens.field @"pid"
{-# INLINEABLE vspPid #-}
{-# DEPRECATED pid "Use generic-lens or generic-optics with 'pid' instead"  #-}

instance Core.FromJSON VideoSelectorPid where
        toJSON VideoSelectorPid{..}
          = Core.object (Core.catMaybes [("pid" Core..=) Core.<$> pid])

instance Core.FromJSON VideoSelectorPid where
        parseJSON
          = Core.withObject "VideoSelectorPid" Core.$
              \ x -> VideoSelectorPid' Core.<$> (x Core..:? "pid")
