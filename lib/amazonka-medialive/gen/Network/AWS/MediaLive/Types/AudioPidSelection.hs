{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioPidSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioPidSelection
  ( AudioPidSelection (..),

    -- * Smart constructor
    mkAudioPidSelection,

    -- * Lenses
    apsPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Audio Pid Selection
--
-- /See:/ 'mkAudioPidSelection' smart constructor.
newtype AudioPidSelection = AudioPidSelection'
  { -- | Selects a specific PID from within a source.
    pid :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioPidSelection' with the minimum fields required to make a request.
--
-- * 'pid' - Selects a specific PID from within a source.
mkAudioPidSelection ::
  -- | 'pid'
  Lude.Natural ->
  AudioPidSelection
mkAudioPidSelection pPid_ = AudioPidSelection' {pid = pPid_}

-- | Selects a specific PID from within a source.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPid :: Lens.Lens' AudioPidSelection Lude.Natural
apsPid = Lens.lens (pid :: AudioPidSelection -> Lude.Natural) (\s a -> s {pid = a} :: AudioPidSelection)
{-# DEPRECATED apsPid "Use generic-lens or generic-optics with 'pid' instead." #-}

instance Lude.FromJSON AudioPidSelection where
  parseJSON =
    Lude.withObject
      "AudioPidSelection"
      (\x -> AudioPidSelection' Lude.<$> (x Lude..: "pid"))

instance Lude.ToJSON AudioPidSelection where
  toJSON AudioPidSelection' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("pid" Lude..= pid)])
