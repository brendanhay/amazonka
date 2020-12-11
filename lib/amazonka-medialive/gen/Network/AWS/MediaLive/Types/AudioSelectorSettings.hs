-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioSelectorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioSelectorSettings
  ( AudioSelectorSettings (..),

    -- * Smart constructor
    mkAudioSelectorSettings,

    -- * Lenses
    assAudioLanguageSelection,
    assAudioTrackSelection,
    assAudioPidSelection,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioLanguageSelection
import Network.AWS.MediaLive.Types.AudioPidSelection
import Network.AWS.MediaLive.Types.AudioTrackSelection
import qualified Network.AWS.Prelude as Lude

-- | Audio Selector Settings
--
-- /See:/ 'mkAudioSelectorSettings' smart constructor.
data AudioSelectorSettings = AudioSelectorSettings'
  { audioLanguageSelection ::
      Lude.Maybe AudioLanguageSelection,
    audioTrackSelection ::
      Lude.Maybe AudioTrackSelection,
    audioPidSelection ::
      Lude.Maybe AudioPidSelection
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioSelectorSettings' with the minimum fields required to make a request.
--
-- * 'audioLanguageSelection' - Undocumented field.
-- * 'audioPidSelection' - Undocumented field.
-- * 'audioTrackSelection' - Undocumented field.
mkAudioSelectorSettings ::
  AudioSelectorSettings
mkAudioSelectorSettings =
  AudioSelectorSettings'
    { audioLanguageSelection = Lude.Nothing,
      audioTrackSelection = Lude.Nothing,
      audioPidSelection = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioLanguageSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAudioLanguageSelection :: Lens.Lens' AudioSelectorSettings (Lude.Maybe AudioLanguageSelection)
assAudioLanguageSelection = Lens.lens (audioLanguageSelection :: AudioSelectorSettings -> Lude.Maybe AudioLanguageSelection) (\s a -> s {audioLanguageSelection = a} :: AudioSelectorSettings)
{-# DEPRECATED assAudioLanguageSelection "Use generic-lens or generic-optics with 'audioLanguageSelection' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioTrackSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAudioTrackSelection :: Lens.Lens' AudioSelectorSettings (Lude.Maybe AudioTrackSelection)
assAudioTrackSelection = Lens.lens (audioTrackSelection :: AudioSelectorSettings -> Lude.Maybe AudioTrackSelection) (\s a -> s {audioTrackSelection = a} :: AudioSelectorSettings)
{-# DEPRECATED assAudioTrackSelection "Use generic-lens or generic-optics with 'audioTrackSelection' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioPidSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAudioPidSelection :: Lens.Lens' AudioSelectorSettings (Lude.Maybe AudioPidSelection)
assAudioPidSelection = Lens.lens (audioPidSelection :: AudioSelectorSettings -> Lude.Maybe AudioPidSelection) (\s a -> s {audioPidSelection = a} :: AudioSelectorSettings)
{-# DEPRECATED assAudioPidSelection "Use generic-lens or generic-optics with 'audioPidSelection' instead." #-}

instance Lude.FromJSON AudioSelectorSettings where
  parseJSON =
    Lude.withObject
      "AudioSelectorSettings"
      ( \x ->
          AudioSelectorSettings'
            Lude.<$> (x Lude..:? "audioLanguageSelection")
            Lude.<*> (x Lude..:? "audioTrackSelection")
            Lude.<*> (x Lude..:? "audioPidSelection")
      )

instance Lude.ToJSON AudioSelectorSettings where
  toJSON AudioSelectorSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("audioLanguageSelection" Lude..=)
              Lude.<$> audioLanguageSelection,
            ("audioTrackSelection" Lude..=) Lude.<$> audioTrackSelection,
            ("audioPidSelection" Lude..=) Lude.<$> audioPidSelection
          ]
      )
