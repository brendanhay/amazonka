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
    asTracks,
    asCustomLanguageCode,
    asProgramSelection,
    asLanguageCode,
    asOffset,
    asDefaultSelection,
    asPids,
    asSelectorType,
    asExternalAudioFileInput,
    asRemixSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioDefaultSelection
import Network.AWS.MediaConvert.Types.AudioSelectorType
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.MediaConvert.Types.RemixSettings
import qualified Network.AWS.Prelude as Lude

-- | Selector for Audio
--
-- /See:/ 'mkAudioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { -- | Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
    tracks :: Lude.Maybe [Lude.Natural],
    -- | Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
    customLanguageCode :: Lude.Maybe Lude.Text,
    -- | Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
    programSelection :: Lude.Maybe Lude.Natural,
    -- | Selects a specific language code from within an audio source.
    languageCode :: Lude.Maybe LanguageCode,
    -- | Specifies a time delta in milliseconds to offset the audio from the input video.
    offset :: Lude.Maybe Lude.Int,
    -- | Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
    defaultSelection :: Lude.Maybe AudioDefaultSelection,
    -- | Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
    pids :: Lude.Maybe [Lude.Natural],
    -- | Specifies the type of the audio selector.
    selectorType :: Lude.Maybe AudioSelectorType,
    -- | Specifies audio data from an external file source.
    externalAudioFileInput :: Lude.Maybe Lude.Text,
    -- | Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
    remixSettings :: Lude.Maybe RemixSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioSelector' with the minimum fields required to make a request.
--
-- * 'tracks' - Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
-- * 'customLanguageCode' - Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
-- * 'programSelection' - Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
-- * 'languageCode' - Selects a specific language code from within an audio source.
-- * 'offset' - Specifies a time delta in milliseconds to offset the audio from the input video.
-- * 'defaultSelection' - Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
-- * 'pids' - Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
-- * 'selectorType' - Specifies the type of the audio selector.
-- * 'externalAudioFileInput' - Specifies audio data from an external file source.
-- * 'remixSettings' - Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
mkAudioSelector ::
  AudioSelector
mkAudioSelector =
  AudioSelector'
    { tracks = Lude.Nothing,
      customLanguageCode = Lude.Nothing,
      programSelection = Lude.Nothing,
      languageCode = Lude.Nothing,
      offset = Lude.Nothing,
      defaultSelection = Lude.Nothing,
      pids = Lude.Nothing,
      selectorType = Lude.Nothing,
      externalAudioFileInput = Lude.Nothing,
      remixSettings = Lude.Nothing
    }

-- | Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
--
-- /Note:/ Consider using 'tracks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTracks :: Lens.Lens' AudioSelector (Lude.Maybe [Lude.Natural])
asTracks = Lens.lens (tracks :: AudioSelector -> Lude.Maybe [Lude.Natural]) (\s a -> s {tracks = a} :: AudioSelector)
{-# DEPRECATED asTracks "Use generic-lens or generic-optics with 'tracks' instead." #-}

-- | Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCustomLanguageCode :: Lens.Lens' AudioSelector (Lude.Maybe Lude.Text)
asCustomLanguageCode = Lens.lens (customLanguageCode :: AudioSelector -> Lude.Maybe Lude.Text) (\s a -> s {customLanguageCode = a} :: AudioSelector)
{-# DEPRECATED asCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
--
-- /Note:/ Consider using 'programSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asProgramSelection :: Lens.Lens' AudioSelector (Lude.Maybe Lude.Natural)
asProgramSelection = Lens.lens (programSelection :: AudioSelector -> Lude.Maybe Lude.Natural) (\s a -> s {programSelection = a} :: AudioSelector)
{-# DEPRECATED asProgramSelection "Use generic-lens or generic-optics with 'programSelection' instead." #-}

-- | Selects a specific language code from within an audio source.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLanguageCode :: Lens.Lens' AudioSelector (Lude.Maybe LanguageCode)
asLanguageCode = Lens.lens (languageCode :: AudioSelector -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: AudioSelector)
{-# DEPRECATED asLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Specifies a time delta in milliseconds to offset the audio from the input video.
--
-- /Note:/ Consider using 'offset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asOffset :: Lens.Lens' AudioSelector (Lude.Maybe Lude.Int)
asOffset = Lens.lens (offset :: AudioSelector -> Lude.Maybe Lude.Int) (\s a -> s {offset = a} :: AudioSelector)
{-# DEPRECATED asOffset "Use generic-lens or generic-optics with 'offset' instead." #-}

-- | Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
--
-- /Note:/ Consider using 'defaultSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDefaultSelection :: Lens.Lens' AudioSelector (Lude.Maybe AudioDefaultSelection)
asDefaultSelection = Lens.lens (defaultSelection :: AudioSelector -> Lude.Maybe AudioDefaultSelection) (\s a -> s {defaultSelection = a} :: AudioSelector)
{-# DEPRECATED asDefaultSelection "Use generic-lens or generic-optics with 'defaultSelection' instead." #-}

-- | Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
--
-- /Note:/ Consider using 'pids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asPids :: Lens.Lens' AudioSelector (Lude.Maybe [Lude.Natural])
asPids = Lens.lens (pids :: AudioSelector -> Lude.Maybe [Lude.Natural]) (\s a -> s {pids = a} :: AudioSelector)
{-# DEPRECATED asPids "Use generic-lens or generic-optics with 'pids' instead." #-}

-- | Specifies the type of the audio selector.
--
-- /Note:/ Consider using 'selectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSelectorType :: Lens.Lens' AudioSelector (Lude.Maybe AudioSelectorType)
asSelectorType = Lens.lens (selectorType :: AudioSelector -> Lude.Maybe AudioSelectorType) (\s a -> s {selectorType = a} :: AudioSelector)
{-# DEPRECATED asSelectorType "Use generic-lens or generic-optics with 'selectorType' instead." #-}

-- | Specifies audio data from an external file source.
--
-- /Note:/ Consider using 'externalAudioFileInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asExternalAudioFileInput :: Lens.Lens' AudioSelector (Lude.Maybe Lude.Text)
asExternalAudioFileInput = Lens.lens (externalAudioFileInput :: AudioSelector -> Lude.Maybe Lude.Text) (\s a -> s {externalAudioFileInput = a} :: AudioSelector)
{-# DEPRECATED asExternalAudioFileInput "Use generic-lens or generic-optics with 'externalAudioFileInput' instead." #-}

-- | Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
--
-- /Note:/ Consider using 'remixSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRemixSettings :: Lens.Lens' AudioSelector (Lude.Maybe RemixSettings)
asRemixSettings = Lens.lens (remixSettings :: AudioSelector -> Lude.Maybe RemixSettings) (\s a -> s {remixSettings = a} :: AudioSelector)
{-# DEPRECATED asRemixSettings "Use generic-lens or generic-optics with 'remixSettings' instead." #-}

instance Lude.FromJSON AudioSelector where
  parseJSON =
    Lude.withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            Lude.<$> (x Lude..:? "tracks" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "customLanguageCode")
            Lude.<*> (x Lude..:? "programSelection")
            Lude.<*> (x Lude..:? "languageCode")
            Lude.<*> (x Lude..:? "offset")
            Lude.<*> (x Lude..:? "defaultSelection")
            Lude.<*> (x Lude..:? "pids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "selectorType")
            Lude.<*> (x Lude..:? "externalAudioFileInput")
            Lude.<*> (x Lude..:? "remixSettings")
      )

instance Lude.ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tracks" Lude..=) Lude.<$> tracks,
            ("customLanguageCode" Lude..=) Lude.<$> customLanguageCode,
            ("programSelection" Lude..=) Lude.<$> programSelection,
            ("languageCode" Lude..=) Lude.<$> languageCode,
            ("offset" Lude..=) Lude.<$> offset,
            ("defaultSelection" Lude..=) Lude.<$> defaultSelection,
            ("pids" Lude..=) Lude.<$> pids,
            ("selectorType" Lude..=) Lude.<$> selectorType,
            ("externalAudioFileInput" Lude..=) Lude.<$> externalAudioFileInput,
            ("remixSettings" Lude..=) Lude.<$> remixSettings
          ]
      )
