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
import qualified Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Language Selection
--
-- /See:/ 'mkAudioLanguageSelection' smart constructor.
data AudioLanguageSelection = AudioLanguageSelection'
  { -- | Selects a specific three-letter language code from within an audio source.
    languageCode :: Core.Text,
    -- | When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
    languageSelectionPolicy :: Core.Maybe Types.AudioLanguageSelectionPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioLanguageSelection' value with any optional fields omitted.
mkAudioLanguageSelection ::
  -- | 'languageCode'
  Core.Text ->
  AudioLanguageSelection
mkAudioLanguageSelection languageCode =
  AudioLanguageSelection'
    { languageCode,
      languageSelectionPolicy = Core.Nothing
    }

-- | Selects a specific three-letter language code from within an audio source.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alsLanguageCode :: Lens.Lens' AudioLanguageSelection Core.Text
alsLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED alsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
--
-- /Note:/ Consider using 'languageSelectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alsLanguageSelectionPolicy :: Lens.Lens' AudioLanguageSelection (Core.Maybe Types.AudioLanguageSelectionPolicy)
alsLanguageSelectionPolicy = Lens.field @"languageSelectionPolicy"
{-# DEPRECATED alsLanguageSelectionPolicy "Use generic-lens or generic-optics with 'languageSelectionPolicy' instead." #-}

instance Core.FromJSON AudioLanguageSelection where
  toJSON AudioLanguageSelection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("languageCode" Core..= languageCode),
            ("languageSelectionPolicy" Core..=)
              Core.<$> languageSelectionPolicy
          ]
      )

instance Core.FromJSON AudioLanguageSelection where
  parseJSON =
    Core.withObject "AudioLanguageSelection" Core.$
      \x ->
        AudioLanguageSelection'
          Core.<$> (x Core..: "languageCode")
          Core.<*> (x Core..:? "languageSelectionPolicy")
