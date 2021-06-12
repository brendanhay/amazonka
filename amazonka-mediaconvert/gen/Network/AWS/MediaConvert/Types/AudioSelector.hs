{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioSelector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioDefaultSelection
import Network.AWS.MediaConvert.Types.AudioSelectorType
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.MediaConvert.Types.RemixSettings

-- | Selector for Audio
--
-- /See:/ 'newAudioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { -- | Selects a specific language code from within an audio source.
    languageCode :: Core.Maybe LanguageCode,
    -- | Use this setting for input streams that contain Dolby E, to have the
    -- service extract specific program data from the track. To select multiple
    -- programs, create multiple selectors with the same Track and different
    -- Program numbers. In the console, this setting is visible when you set
    -- Selector type to Track. Choose the program number from the dropdown
    -- list. If you are sending a JSON file, provide the program ID, which is
    -- part of the audio metadata. If your input file has incorrect metadata,
    -- you can choose All channels instead of a program number to have the
    -- service ignore the program IDs and include all the programs in the
    -- track.
    programSelection :: Core.Maybe Core.Natural,
    -- | Selects a specific language code from within an audio source, using the
    -- ISO 639-2 or ISO 639-3 three-letter language code
    customLanguageCode :: Core.Maybe Core.Text,
    -- | Identify a track from the input audio to include in this selector by
    -- entering the track index number. To include several tracks in a single
    -- audio selector, specify multiple tracks as follows. Using the console,
    -- enter a comma-separated list. For examle, type \"1,2,3\" to include
    -- tracks 1 through 3. Specifying directly in your JSON job file, provide
    -- the track numbers in an array. For example, \"tracks\": [1,2,3].
    tracks :: Core.Maybe [Core.Natural],
    -- | Enable this setting on one audio selector to set it as the default for
    -- the job. The service uses this default for outputs where it can\'t find
    -- the specified input audio. If you don\'t set a default, those outputs
    -- have no audio.
    defaultSelection :: Core.Maybe AudioDefaultSelection,
    -- | Specifies the type of the audio selector.
    selectorType :: Core.Maybe AudioSelectorType,
    -- | Use these settings to reorder the audio channels of one input to match
    -- those of another input. This allows you to combine the two files into a
    -- single output, one after the other.
    remixSettings :: Core.Maybe RemixSettings,
    -- | Selects a specific PID from within an audio source (e.g. 257 selects PID
    -- 0x101).
    pids :: Core.Maybe [Core.Natural],
    -- | Specifies audio data from an external file source.
    externalAudioFileInput :: Core.Maybe Core.Text,
    -- | Specifies a time delta in milliseconds to offset the audio from the
    -- input video.
    offset :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AudioSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'audioSelector_languageCode' - Selects a specific language code from within an audio source.
--
-- 'programSelection', 'audioSelector_programSelection' - Use this setting for input streams that contain Dolby E, to have the
-- service extract specific program data from the track. To select multiple
-- programs, create multiple selectors with the same Track and different
-- Program numbers. In the console, this setting is visible when you set
-- Selector type to Track. Choose the program number from the dropdown
-- list. If you are sending a JSON file, provide the program ID, which is
-- part of the audio metadata. If your input file has incorrect metadata,
-- you can choose All channels instead of a program number to have the
-- service ignore the program IDs and include all the programs in the
-- track.
--
-- 'customLanguageCode', 'audioSelector_customLanguageCode' - Selects a specific language code from within an audio source, using the
-- ISO 639-2 or ISO 639-3 three-letter language code
--
-- 'tracks', 'audioSelector_tracks' - Identify a track from the input audio to include in this selector by
-- entering the track index number. To include several tracks in a single
-- audio selector, specify multiple tracks as follows. Using the console,
-- enter a comma-separated list. For examle, type \"1,2,3\" to include
-- tracks 1 through 3. Specifying directly in your JSON job file, provide
-- the track numbers in an array. For example, \"tracks\": [1,2,3].
--
-- 'defaultSelection', 'audioSelector_defaultSelection' - Enable this setting on one audio selector to set it as the default for
-- the job. The service uses this default for outputs where it can\'t find
-- the specified input audio. If you don\'t set a default, those outputs
-- have no audio.
--
-- 'selectorType', 'audioSelector_selectorType' - Specifies the type of the audio selector.
--
-- 'remixSettings', 'audioSelector_remixSettings' - Use these settings to reorder the audio channels of one input to match
-- those of another input. This allows you to combine the two files into a
-- single output, one after the other.
--
-- 'pids', 'audioSelector_pids' - Selects a specific PID from within an audio source (e.g. 257 selects PID
-- 0x101).
--
-- 'externalAudioFileInput', 'audioSelector_externalAudioFileInput' - Specifies audio data from an external file source.
--
-- 'offset', 'audioSelector_offset' - Specifies a time delta in milliseconds to offset the audio from the
-- input video.
newAudioSelector ::
  AudioSelector
newAudioSelector =
  AudioSelector'
    { languageCode = Core.Nothing,
      programSelection = Core.Nothing,
      customLanguageCode = Core.Nothing,
      tracks = Core.Nothing,
      defaultSelection = Core.Nothing,
      selectorType = Core.Nothing,
      remixSettings = Core.Nothing,
      pids = Core.Nothing,
      externalAudioFileInput = Core.Nothing,
      offset = Core.Nothing
    }

-- | Selects a specific language code from within an audio source.
audioSelector_languageCode :: Lens.Lens' AudioSelector (Core.Maybe LanguageCode)
audioSelector_languageCode = Lens.lens (\AudioSelector' {languageCode} -> languageCode) (\s@AudioSelector' {} a -> s {languageCode = a} :: AudioSelector)

-- | Use this setting for input streams that contain Dolby E, to have the
-- service extract specific program data from the track. To select multiple
-- programs, create multiple selectors with the same Track and different
-- Program numbers. In the console, this setting is visible when you set
-- Selector type to Track. Choose the program number from the dropdown
-- list. If you are sending a JSON file, provide the program ID, which is
-- part of the audio metadata. If your input file has incorrect metadata,
-- you can choose All channels instead of a program number to have the
-- service ignore the program IDs and include all the programs in the
-- track.
audioSelector_programSelection :: Lens.Lens' AudioSelector (Core.Maybe Core.Natural)
audioSelector_programSelection = Lens.lens (\AudioSelector' {programSelection} -> programSelection) (\s@AudioSelector' {} a -> s {programSelection = a} :: AudioSelector)

-- | Selects a specific language code from within an audio source, using the
-- ISO 639-2 or ISO 639-3 three-letter language code
audioSelector_customLanguageCode :: Lens.Lens' AudioSelector (Core.Maybe Core.Text)
audioSelector_customLanguageCode = Lens.lens (\AudioSelector' {customLanguageCode} -> customLanguageCode) (\s@AudioSelector' {} a -> s {customLanguageCode = a} :: AudioSelector)

-- | Identify a track from the input audio to include in this selector by
-- entering the track index number. To include several tracks in a single
-- audio selector, specify multiple tracks as follows. Using the console,
-- enter a comma-separated list. For examle, type \"1,2,3\" to include
-- tracks 1 through 3. Specifying directly in your JSON job file, provide
-- the track numbers in an array. For example, \"tracks\": [1,2,3].
audioSelector_tracks :: Lens.Lens' AudioSelector (Core.Maybe [Core.Natural])
audioSelector_tracks = Lens.lens (\AudioSelector' {tracks} -> tracks) (\s@AudioSelector' {} a -> s {tracks = a} :: AudioSelector) Core.. Lens.mapping Lens._Coerce

-- | Enable this setting on one audio selector to set it as the default for
-- the job. The service uses this default for outputs where it can\'t find
-- the specified input audio. If you don\'t set a default, those outputs
-- have no audio.
audioSelector_defaultSelection :: Lens.Lens' AudioSelector (Core.Maybe AudioDefaultSelection)
audioSelector_defaultSelection = Lens.lens (\AudioSelector' {defaultSelection} -> defaultSelection) (\s@AudioSelector' {} a -> s {defaultSelection = a} :: AudioSelector)

-- | Specifies the type of the audio selector.
audioSelector_selectorType :: Lens.Lens' AudioSelector (Core.Maybe AudioSelectorType)
audioSelector_selectorType = Lens.lens (\AudioSelector' {selectorType} -> selectorType) (\s@AudioSelector' {} a -> s {selectorType = a} :: AudioSelector)

-- | Use these settings to reorder the audio channels of one input to match
-- those of another input. This allows you to combine the two files into a
-- single output, one after the other.
audioSelector_remixSettings :: Lens.Lens' AudioSelector (Core.Maybe RemixSettings)
audioSelector_remixSettings = Lens.lens (\AudioSelector' {remixSettings} -> remixSettings) (\s@AudioSelector' {} a -> s {remixSettings = a} :: AudioSelector)

-- | Selects a specific PID from within an audio source (e.g. 257 selects PID
-- 0x101).
audioSelector_pids :: Lens.Lens' AudioSelector (Core.Maybe [Core.Natural])
audioSelector_pids = Lens.lens (\AudioSelector' {pids} -> pids) (\s@AudioSelector' {} a -> s {pids = a} :: AudioSelector) Core.. Lens.mapping Lens._Coerce

-- | Specifies audio data from an external file source.
audioSelector_externalAudioFileInput :: Lens.Lens' AudioSelector (Core.Maybe Core.Text)
audioSelector_externalAudioFileInput = Lens.lens (\AudioSelector' {externalAudioFileInput} -> externalAudioFileInput) (\s@AudioSelector' {} a -> s {externalAudioFileInput = a} :: AudioSelector)

-- | Specifies a time delta in milliseconds to offset the audio from the
-- input video.
audioSelector_offset :: Lens.Lens' AudioSelector (Core.Maybe Core.Int)
audioSelector_offset = Lens.lens (\AudioSelector' {offset} -> offset) (\s@AudioSelector' {} a -> s {offset = a} :: AudioSelector)

instance Core.FromJSON AudioSelector where
  parseJSON =
    Core.withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            Core.<$> (x Core..:? "languageCode")
            Core.<*> (x Core..:? "programSelection")
            Core.<*> (x Core..:? "customLanguageCode")
            Core.<*> (x Core..:? "tracks" Core..!= Core.mempty)
            Core.<*> (x Core..:? "defaultSelection")
            Core.<*> (x Core..:? "selectorType")
            Core.<*> (x Core..:? "remixSettings")
            Core.<*> (x Core..:? "pids" Core..!= Core.mempty)
            Core.<*> (x Core..:? "externalAudioFileInput")
            Core.<*> (x Core..:? "offset")
      )

instance Core.Hashable AudioSelector

instance Core.NFData AudioSelector

instance Core.ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("languageCode" Core..=) Core.<$> languageCode,
            ("programSelection" Core..=)
              Core.<$> programSelection,
            ("customLanguageCode" Core..=)
              Core.<$> customLanguageCode,
            ("tracks" Core..=) Core.<$> tracks,
            ("defaultSelection" Core..=)
              Core.<$> defaultSelection,
            ("selectorType" Core..=) Core.<$> selectorType,
            ("remixSettings" Core..=) Core.<$> remixSettings,
            ("pids" Core..=) Core.<$> pids,
            ("externalAudioFileInput" Core..=)
              Core.<$> externalAudioFileInput,
            ("offset" Core..=) Core.<$> offset
          ]
      )
