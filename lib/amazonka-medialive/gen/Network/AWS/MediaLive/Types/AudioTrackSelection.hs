{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioTrackSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioTrackSelection
  ( AudioTrackSelection (..),

    -- * Smart constructor
    mkAudioTrackSelection,

    -- * Lenses
    atsTracks,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioTrack
import qualified Network.AWS.Prelude as Lude

-- | Audio Track Selection
--
-- /See:/ 'mkAudioTrackSelection' smart constructor.
newtype AudioTrackSelection = AudioTrackSelection'
  { -- | Selects one or more unique audio tracks from within a source.
    tracks :: [AudioTrack]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioTrackSelection' with the minimum fields required to make a request.
--
-- * 'tracks' - Selects one or more unique audio tracks from within a source.
mkAudioTrackSelection ::
  AudioTrackSelection
mkAudioTrackSelection = AudioTrackSelection' {tracks = Lude.mempty}

-- | Selects one or more unique audio tracks from within a source.
--
-- /Note:/ Consider using 'tracks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsTracks :: Lens.Lens' AudioTrackSelection [AudioTrack]
atsTracks = Lens.lens (tracks :: AudioTrackSelection -> [AudioTrack]) (\s a -> s {tracks = a} :: AudioTrackSelection)
{-# DEPRECATED atsTracks "Use generic-lens or generic-optics with 'tracks' instead." #-}

instance Lude.FromJSON AudioTrackSelection where
  parseJSON =
    Lude.withObject
      "AudioTrackSelection"
      ( \x ->
          AudioTrackSelection'
            Lude.<$> (x Lude..:? "tracks" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AudioTrackSelection where
  toJSON AudioTrackSelection' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("tracks" Lude..= tracks)])
