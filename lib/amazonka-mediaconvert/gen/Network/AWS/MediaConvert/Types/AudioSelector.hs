{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioSelector
  ( AudioSelector (..),

    -- * Smart constructor
    mkAudioSelector,

    -- * Lenses
    asCustomLanguageCode,
    asDefaultSelection,
    asExternalAudioFileInput,
    asLanguageCode,
    asOffset,
    asPids,
    asProgramSelection,
    asRemixSettings,
    asSelectorType,
    asTracks,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AudioDefaultSelection as Types
import qualified Network.AWS.MediaConvert.Types.AudioSelectorType as Types
import qualified Network.AWS.MediaConvert.Types.LanguageCode as Types
import qualified Network.AWS.MediaConvert.Types.RemixSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Selector for Audio
--
-- /See:/ 'mkAudioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { -- | Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
    customLanguageCode :: Core.Maybe Core.Text,
    -- | Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
    defaultSelection :: Core.Maybe Types.AudioDefaultSelection,
    -- | Specifies audio data from an external file source.
    externalAudioFileInput :: Core.Maybe Core.Text,
    -- | Selects a specific language code from within an audio source.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | Specifies a time delta in milliseconds to offset the audio from the input video.
    offset :: Core.Maybe Core.Int,
    -- | Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
    pids :: Core.Maybe [Core.Natural],
    -- | Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
    programSelection :: Core.Maybe Core.Natural,
    -- | Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
    remixSettings :: Core.Maybe Types.RemixSettings,
    -- | Specifies the type of the audio selector.
    selectorType :: Core.Maybe Types.AudioSelectorType,
    -- | Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
    tracks :: Core.Maybe [Core.Natural]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioSelector' value with any optional fields omitted.
mkAudioSelector ::
  AudioSelector
mkAudioSelector =
  AudioSelector'
    { customLanguageCode = Core.Nothing,
      defaultSelection = Core.Nothing,
      externalAudioFileInput = Core.Nothing,
      languageCode = Core.Nothing,
      offset = Core.Nothing,
      pids = Core.Nothing,
      programSelection = Core.Nothing,
      remixSettings = Core.Nothing,
      selectorType = Core.Nothing,
      tracks = Core.Nothing
    }

-- | Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCustomLanguageCode :: Lens.Lens' AudioSelector (Core.Maybe Core.Text)
asCustomLanguageCode = Lens.field @"customLanguageCode"
{-# DEPRECATED asCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
--
-- /Note:/ Consider using 'defaultSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDefaultSelection :: Lens.Lens' AudioSelector (Core.Maybe Types.AudioDefaultSelection)
asDefaultSelection = Lens.field @"defaultSelection"
{-# DEPRECATED asDefaultSelection "Use generic-lens or generic-optics with 'defaultSelection' instead." #-}

-- | Specifies audio data from an external file source.
--
-- /Note:/ Consider using 'externalAudioFileInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asExternalAudioFileInput :: Lens.Lens' AudioSelector (Core.Maybe Core.Text)
asExternalAudioFileInput = Lens.field @"externalAudioFileInput"
{-# DEPRECATED asExternalAudioFileInput "Use generic-lens or generic-optics with 'externalAudioFileInput' instead." #-}

-- | Selects a specific language code from within an audio source.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLanguageCode :: Lens.Lens' AudioSelector (Core.Maybe Types.LanguageCode)
asLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED asLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Specifies a time delta in milliseconds to offset the audio from the input video.
--
-- /Note:/ Consider using 'offset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asOffset :: Lens.Lens' AudioSelector (Core.Maybe Core.Int)
asOffset = Lens.field @"offset"
{-# DEPRECATED asOffset "Use generic-lens or generic-optics with 'offset' instead." #-}

-- | Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
--
-- /Note:/ Consider using 'pids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asPids :: Lens.Lens' AudioSelector (Core.Maybe [Core.Natural])
asPids = Lens.field @"pids"
{-# DEPRECATED asPids "Use generic-lens or generic-optics with 'pids' instead." #-}

-- | Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
--
-- /Note:/ Consider using 'programSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asProgramSelection :: Lens.Lens' AudioSelector (Core.Maybe Core.Natural)
asProgramSelection = Lens.field @"programSelection"
{-# DEPRECATED asProgramSelection "Use generic-lens or generic-optics with 'programSelection' instead." #-}

-- | Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
--
-- /Note:/ Consider using 'remixSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRemixSettings :: Lens.Lens' AudioSelector (Core.Maybe Types.RemixSettings)
asRemixSettings = Lens.field @"remixSettings"
{-# DEPRECATED asRemixSettings "Use generic-lens or generic-optics with 'remixSettings' instead." #-}

-- | Specifies the type of the audio selector.
--
-- /Note:/ Consider using 'selectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSelectorType :: Lens.Lens' AudioSelector (Core.Maybe Types.AudioSelectorType)
asSelectorType = Lens.field @"selectorType"
{-# DEPRECATED asSelectorType "Use generic-lens or generic-optics with 'selectorType' instead." #-}

-- | Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
--
-- /Note:/ Consider using 'tracks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTracks :: Lens.Lens' AudioSelector (Core.Maybe [Core.Natural])
asTracks = Lens.field @"tracks"
{-# DEPRECATED asTracks "Use generic-lens or generic-optics with 'tracks' instead." #-}

instance Core.FromJSON AudioSelector where
  toJSON AudioSelector {..} =
    Core.object
      ( Core.catMaybes
          [ ("customLanguageCode" Core..=) Core.<$> customLanguageCode,
            ("defaultSelection" Core..=) Core.<$> defaultSelection,
            ("externalAudioFileInput" Core..=) Core.<$> externalAudioFileInput,
            ("languageCode" Core..=) Core.<$> languageCode,
            ("offset" Core..=) Core.<$> offset,
            ("pids" Core..=) Core.<$> pids,
            ("programSelection" Core..=) Core.<$> programSelection,
            ("remixSettings" Core..=) Core.<$> remixSettings,
            ("selectorType" Core..=) Core.<$> selectorType,
            ("tracks" Core..=) Core.<$> tracks
          ]
      )

instance Core.FromJSON AudioSelector where
  parseJSON =
    Core.withObject "AudioSelector" Core.$
      \x ->
        AudioSelector'
          Core.<$> (x Core..:? "customLanguageCode")
          Core.<*> (x Core..:? "defaultSelection")
          Core.<*> (x Core..:? "externalAudioFileInput")
          Core.<*> (x Core..:? "languageCode")
          Core.<*> (x Core..:? "offset")
          Core.<*> (x Core..:? "pids")
          Core.<*> (x Core..:? "programSelection")
          Core.<*> (x Core..:? "remixSettings")
          Core.<*> (x Core..:? "selectorType")
          Core.<*> (x Core..:? "tracks")
