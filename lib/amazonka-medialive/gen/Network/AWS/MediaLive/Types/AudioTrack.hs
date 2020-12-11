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
import qualified Network.AWS.Prelude as Lude

-- | Audio Track
--
-- /See:/ 'mkAudioTrack' smart constructor.
newtype AudioTrack = AudioTrack' {track :: Lude.Natural}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioTrack' with the minimum fields required to make a request.
--
-- * 'track' - 1-based integer value that maps to a specific audio track
mkAudioTrack ::
  -- | 'track'
  Lude.Natural ->
  AudioTrack
mkAudioTrack pTrack_ = AudioTrack' {track = pTrack_}

-- | 1-based integer value that maps to a specific audio track
--
-- /Note:/ Consider using 'track' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTrack :: Lens.Lens' AudioTrack Lude.Natural
atTrack = Lens.lens (track :: AudioTrack -> Lude.Natural) (\s a -> s {track = a} :: AudioTrack)
{-# DEPRECATED atTrack "Use generic-lens or generic-optics with 'track' instead." #-}

instance Lude.FromJSON AudioTrack where
  parseJSON =
    Lude.withObject
      "AudioTrack"
      (\x -> AudioTrack' Lude.<$> (x Lude..: "track"))

instance Lude.ToJSON AudioTrack where
  toJSON AudioTrack' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("track" Lude..= track)])
