{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioTrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioTrack
  ( AudioTrack (..),

    -- * Smart constructor
    mkAudioTrack,

    -- * Lenses
    atTrack,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Audio Track
--
-- /See:/ 'mkAudioTrack' smart constructor.
newtype AudioTrack = AudioTrack'
  { -- | 1-based integer value that maps to a specific audio track
    track :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AudioTrack' value with any optional fields omitted.
mkAudioTrack ::
  -- | 'track'
  Core.Natural ->
  AudioTrack
mkAudioTrack track = AudioTrack' {track}

-- | 1-based integer value that maps to a specific audio track
--
-- /Note:/ Consider using 'track' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTrack :: Lens.Lens' AudioTrack Core.Natural
atTrack = Lens.field @"track"
{-# DEPRECATED atTrack "Use generic-lens or generic-optics with 'track' instead." #-}

instance Core.FromJSON AudioTrack where
  toJSON AudioTrack {..} =
    Core.object (Core.catMaybes [Core.Just ("track" Core..= track)])

instance Core.FromJSON AudioTrack where
  parseJSON =
    Core.withObject "AudioTrack" Core.$
      \x -> AudioTrack' Core.<$> (x Core..: "track")
