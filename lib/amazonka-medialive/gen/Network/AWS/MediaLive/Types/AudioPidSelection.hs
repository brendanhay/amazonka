{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioPidSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioPidSelection
  ( AudioPidSelection (..)
  -- * Smart constructor
  , mkAudioPidSelection
  -- * Lenses
  , apsPid
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Audio Pid Selection
--
-- /See:/ 'mkAudioPidSelection' smart constructor.
newtype AudioPidSelection = AudioPidSelection'
  { pid :: Core.Natural
    -- ^ Selects a specific PID from within a source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AudioPidSelection' value with any optional fields omitted.
mkAudioPidSelection
    :: Core.Natural -- ^ 'pid'
    -> AudioPidSelection
mkAudioPidSelection pid = AudioPidSelection'{pid}

-- | Selects a specific PID from within a source.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPid :: Lens.Lens' AudioPidSelection Core.Natural
apsPid = Lens.field @"pid"
{-# INLINEABLE apsPid #-}
{-# DEPRECATED pid "Use generic-lens or generic-optics with 'pid' instead"  #-}

instance Core.FromJSON AudioPidSelection where
        toJSON AudioPidSelection{..}
          = Core.object (Core.catMaybes [Core.Just ("pid" Core..= pid)])

instance Core.FromJSON AudioPidSelection where
        parseJSON
          = Core.withObject "AudioPidSelection" Core.$
              \ x -> AudioPidSelection' Core.<$> (x Core..: "pid")
