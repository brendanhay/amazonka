{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioLanguageSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioLanguageSelection
  ( AudioLanguageSelection (..),

    -- * Smart constructor
    mkAudioLanguageSelection,

    -- * Lenses
    alsLanguageCode,
    alsLanguageSelectionPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy
import qualified Network.AWS.Prelude as Lude

-- | Audio Language Selection
--
-- /See:/ 'mkAudioLanguageSelection' smart constructor.
data AudioLanguageSelection = AudioLanguageSelection'
  { -- | Selects a specific three-letter language code from within an audio source.
    languageCode :: Lude.Text,
    -- | When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
    languageSelectionPolicy :: Lude.Maybe AudioLanguageSelectionPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioLanguageSelection' with the minimum fields required to make a request.
--
-- * 'languageCode' - Selects a specific three-letter language code from within an audio source.
-- * 'languageSelectionPolicy' - When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
mkAudioLanguageSelection ::
  -- | 'languageCode'
  Lude.Text ->
  AudioLanguageSelection
mkAudioLanguageSelection pLanguageCode_ =
  AudioLanguageSelection'
    { languageCode = pLanguageCode_,
      languageSelectionPolicy = Lude.Nothing
    }

-- | Selects a specific three-letter language code from within an audio source.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alsLanguageCode :: Lens.Lens' AudioLanguageSelection Lude.Text
alsLanguageCode = Lens.lens (languageCode :: AudioLanguageSelection -> Lude.Text) (\s a -> s {languageCode = a} :: AudioLanguageSelection)
{-# DEPRECATED alsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
--
-- /Note:/ Consider using 'languageSelectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alsLanguageSelectionPolicy :: Lens.Lens' AudioLanguageSelection (Lude.Maybe AudioLanguageSelectionPolicy)
alsLanguageSelectionPolicy = Lens.lens (languageSelectionPolicy :: AudioLanguageSelection -> Lude.Maybe AudioLanguageSelectionPolicy) (\s a -> s {languageSelectionPolicy = a} :: AudioLanguageSelection)
{-# DEPRECATED alsLanguageSelectionPolicy "Use generic-lens or generic-optics with 'languageSelectionPolicy' instead." #-}

instance Lude.FromJSON AudioLanguageSelection where
  parseJSON =
    Lude.withObject
      "AudioLanguageSelection"
      ( \x ->
          AudioLanguageSelection'
            Lude.<$> (x Lude..: "languageCode")
            Lude.<*> (x Lude..:? "languageSelectionPolicy")
      )

instance Lude.ToJSON AudioLanguageSelection where
  toJSON AudioLanguageSelection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("languageCode" Lude..= languageCode),
            ("languageSelectionPolicy" Lude..=)
              Lude.<$> languageSelectionPolicy
          ]
      )
