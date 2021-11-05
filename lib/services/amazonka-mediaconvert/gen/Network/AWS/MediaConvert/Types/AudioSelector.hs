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
-- Module      : Amazonka.MediaConvert.Types.AudioSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.AudioDefaultSelection
import Amazonka.MediaConvert.Types.AudioSelectorType
import Amazonka.MediaConvert.Types.HlsRenditionGroupSettings
import Amazonka.MediaConvert.Types.LanguageCode
import Amazonka.MediaConvert.Types.RemixSettings
import qualified Amazonka.Prelude as Prelude

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
--
-- /See:/ 'newAudioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { -- | Identify a track from the input audio to include in this selector by
    -- entering the track index number. To include several tracks in a single
    -- audio selector, specify multiple tracks as follows. Using the console,
    -- enter a comma-separated list. For examle, type \"1,2,3\" to include
    -- tracks 1 through 3. Specifying directly in your JSON job file, provide
    -- the track numbers in an array. For example, \"tracks\": [1,2,3].
    tracks :: Prelude.Maybe [Prelude.Natural],
    -- | Selects a specific language code from within an audio source, using the
    -- ISO 639-2 or ISO 639-3 three-letter language code
    customLanguageCode :: Prelude.Maybe Prelude.Text,
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
    programSelection :: Prelude.Maybe Prelude.Natural,
    -- | Selects a specific language code from within an audio source.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Specifies a time delta in milliseconds to offset the audio from the
    -- input video.
    offset :: Prelude.Maybe Prelude.Int,
    -- | Enable this setting on one audio selector to set it as the default for
    -- the job. The service uses this default for outputs where it can\'t find
    -- the specified input audio. If you don\'t set a default, those outputs
    -- have no audio.
    defaultSelection :: Prelude.Maybe AudioDefaultSelection,
    -- | Selects a specific PID from within an audio source (e.g. 257 selects PID
    -- 0x101).
    pids :: Prelude.Maybe [Prelude.Natural],
    -- | Settings specific to audio sources in an HLS alternate rendition group.
    -- Specify the properties (renditionGroupId, renditionName or
    -- renditionLanguageCode) to identify the unique audio track among the
    -- alternative rendition groups present in the HLS manifest. If no unique
    -- track is found, or multiple tracks match the properties provided, the
    -- job fails. If no properties in hlsRenditionGroupSettings are specified,
    -- the default audio track within the video segment is chosen. If there is
    -- no audio within video segment, the alternative audio with DEFAULT=YES is
    -- chosen instead.
    hlsRenditionGroupSettings :: Prelude.Maybe HlsRenditionGroupSettings,
    -- | Specifies the type of the audio selector.
    selectorType :: Prelude.Maybe AudioSelectorType,
    -- | Specifies audio data from an external file source.
    externalAudioFileInput :: Prelude.Maybe Prelude.Text,
    -- | Use these settings to reorder the audio channels of one input to match
    -- those of another input. This allows you to combine the two files into a
    -- single output, one after the other.
    remixSettings :: Prelude.Maybe RemixSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tracks', 'audioSelector_tracks' - Identify a track from the input audio to include in this selector by
-- entering the track index number. To include several tracks in a single
-- audio selector, specify multiple tracks as follows. Using the console,
-- enter a comma-separated list. For examle, type \"1,2,3\" to include
-- tracks 1 through 3. Specifying directly in your JSON job file, provide
-- the track numbers in an array. For example, \"tracks\": [1,2,3].
--
-- 'customLanguageCode', 'audioSelector_customLanguageCode' - Selects a specific language code from within an audio source, using the
-- ISO 639-2 or ISO 639-3 three-letter language code
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
-- 'languageCode', 'audioSelector_languageCode' - Selects a specific language code from within an audio source.
--
-- 'offset', 'audioSelector_offset' - Specifies a time delta in milliseconds to offset the audio from the
-- input video.
--
-- 'defaultSelection', 'audioSelector_defaultSelection' - Enable this setting on one audio selector to set it as the default for
-- the job. The service uses this default for outputs where it can\'t find
-- the specified input audio. If you don\'t set a default, those outputs
-- have no audio.
--
-- 'pids', 'audioSelector_pids' - Selects a specific PID from within an audio source (e.g. 257 selects PID
-- 0x101).
--
-- 'hlsRenditionGroupSettings', 'audioSelector_hlsRenditionGroupSettings' - Settings specific to audio sources in an HLS alternate rendition group.
-- Specify the properties (renditionGroupId, renditionName or
-- renditionLanguageCode) to identify the unique audio track among the
-- alternative rendition groups present in the HLS manifest. If no unique
-- track is found, or multiple tracks match the properties provided, the
-- job fails. If no properties in hlsRenditionGroupSettings are specified,
-- the default audio track within the video segment is chosen. If there is
-- no audio within video segment, the alternative audio with DEFAULT=YES is
-- chosen instead.
--
-- 'selectorType', 'audioSelector_selectorType' - Specifies the type of the audio selector.
--
-- 'externalAudioFileInput', 'audioSelector_externalAudioFileInput' - Specifies audio data from an external file source.
--
-- 'remixSettings', 'audioSelector_remixSettings' - Use these settings to reorder the audio channels of one input to match
-- those of another input. This allows you to combine the two files into a
-- single output, one after the other.
newAudioSelector ::
  AudioSelector
newAudioSelector =
  AudioSelector'
    { tracks = Prelude.Nothing,
      customLanguageCode = Prelude.Nothing,
      programSelection = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      offset = Prelude.Nothing,
      defaultSelection = Prelude.Nothing,
      pids = Prelude.Nothing,
      hlsRenditionGroupSettings = Prelude.Nothing,
      selectorType = Prelude.Nothing,
      externalAudioFileInput = Prelude.Nothing,
      remixSettings = Prelude.Nothing
    }

-- | Identify a track from the input audio to include in this selector by
-- entering the track index number. To include several tracks in a single
-- audio selector, specify multiple tracks as follows. Using the console,
-- enter a comma-separated list. For examle, type \"1,2,3\" to include
-- tracks 1 through 3. Specifying directly in your JSON job file, provide
-- the track numbers in an array. For example, \"tracks\": [1,2,3].
audioSelector_tracks :: Lens.Lens' AudioSelector (Prelude.Maybe [Prelude.Natural])
audioSelector_tracks = Lens.lens (\AudioSelector' {tracks} -> tracks) (\s@AudioSelector' {} a -> s {tracks = a} :: AudioSelector) Prelude.. Lens.mapping Lens.coerced

-- | Selects a specific language code from within an audio source, using the
-- ISO 639-2 or ISO 639-3 three-letter language code
audioSelector_customLanguageCode :: Lens.Lens' AudioSelector (Prelude.Maybe Prelude.Text)
audioSelector_customLanguageCode = Lens.lens (\AudioSelector' {customLanguageCode} -> customLanguageCode) (\s@AudioSelector' {} a -> s {customLanguageCode = a} :: AudioSelector)

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
audioSelector_programSelection :: Lens.Lens' AudioSelector (Prelude.Maybe Prelude.Natural)
audioSelector_programSelection = Lens.lens (\AudioSelector' {programSelection} -> programSelection) (\s@AudioSelector' {} a -> s {programSelection = a} :: AudioSelector)

-- | Selects a specific language code from within an audio source.
audioSelector_languageCode :: Lens.Lens' AudioSelector (Prelude.Maybe LanguageCode)
audioSelector_languageCode = Lens.lens (\AudioSelector' {languageCode} -> languageCode) (\s@AudioSelector' {} a -> s {languageCode = a} :: AudioSelector)

-- | Specifies a time delta in milliseconds to offset the audio from the
-- input video.
audioSelector_offset :: Lens.Lens' AudioSelector (Prelude.Maybe Prelude.Int)
audioSelector_offset = Lens.lens (\AudioSelector' {offset} -> offset) (\s@AudioSelector' {} a -> s {offset = a} :: AudioSelector)

-- | Enable this setting on one audio selector to set it as the default for
-- the job. The service uses this default for outputs where it can\'t find
-- the specified input audio. If you don\'t set a default, those outputs
-- have no audio.
audioSelector_defaultSelection :: Lens.Lens' AudioSelector (Prelude.Maybe AudioDefaultSelection)
audioSelector_defaultSelection = Lens.lens (\AudioSelector' {defaultSelection} -> defaultSelection) (\s@AudioSelector' {} a -> s {defaultSelection = a} :: AudioSelector)

-- | Selects a specific PID from within an audio source (e.g. 257 selects PID
-- 0x101).
audioSelector_pids :: Lens.Lens' AudioSelector (Prelude.Maybe [Prelude.Natural])
audioSelector_pids = Lens.lens (\AudioSelector' {pids} -> pids) (\s@AudioSelector' {} a -> s {pids = a} :: AudioSelector) Prelude.. Lens.mapping Lens.coerced

-- | Settings specific to audio sources in an HLS alternate rendition group.
-- Specify the properties (renditionGroupId, renditionName or
-- renditionLanguageCode) to identify the unique audio track among the
-- alternative rendition groups present in the HLS manifest. If no unique
-- track is found, or multiple tracks match the properties provided, the
-- job fails. If no properties in hlsRenditionGroupSettings are specified,
-- the default audio track within the video segment is chosen. If there is
-- no audio within video segment, the alternative audio with DEFAULT=YES is
-- chosen instead.
audioSelector_hlsRenditionGroupSettings :: Lens.Lens' AudioSelector (Prelude.Maybe HlsRenditionGroupSettings)
audioSelector_hlsRenditionGroupSettings = Lens.lens (\AudioSelector' {hlsRenditionGroupSettings} -> hlsRenditionGroupSettings) (\s@AudioSelector' {} a -> s {hlsRenditionGroupSettings = a} :: AudioSelector)

-- | Specifies the type of the audio selector.
audioSelector_selectorType :: Lens.Lens' AudioSelector (Prelude.Maybe AudioSelectorType)
audioSelector_selectorType = Lens.lens (\AudioSelector' {selectorType} -> selectorType) (\s@AudioSelector' {} a -> s {selectorType = a} :: AudioSelector)

-- | Specifies audio data from an external file source.
audioSelector_externalAudioFileInput :: Lens.Lens' AudioSelector (Prelude.Maybe Prelude.Text)
audioSelector_externalAudioFileInput = Lens.lens (\AudioSelector' {externalAudioFileInput} -> externalAudioFileInput) (\s@AudioSelector' {} a -> s {externalAudioFileInput = a} :: AudioSelector)

-- | Use these settings to reorder the audio channels of one input to match
-- those of another input. This allows you to combine the two files into a
-- single output, one after the other.
audioSelector_remixSettings :: Lens.Lens' AudioSelector (Prelude.Maybe RemixSettings)
audioSelector_remixSettings = Lens.lens (\AudioSelector' {remixSettings} -> remixSettings) (\s@AudioSelector' {} a -> s {remixSettings = a} :: AudioSelector)

instance Core.FromJSON AudioSelector where
  parseJSON =
    Core.withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            Prelude.<$> (x Core..:? "tracks" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "customLanguageCode")
            Prelude.<*> (x Core..:? "programSelection")
            Prelude.<*> (x Core..:? "languageCode")
            Prelude.<*> (x Core..:? "offset")
            Prelude.<*> (x Core..:? "defaultSelection")
            Prelude.<*> (x Core..:? "pids" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "hlsRenditionGroupSettings")
            Prelude.<*> (x Core..:? "selectorType")
            Prelude.<*> (x Core..:? "externalAudioFileInput")
            Prelude.<*> (x Core..:? "remixSettings")
      )

instance Prelude.Hashable AudioSelector

instance Prelude.NFData AudioSelector

instance Core.ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tracks" Core..=) Prelude.<$> tracks,
            ("customLanguageCode" Core..=)
              Prelude.<$> customLanguageCode,
            ("programSelection" Core..=)
              Prelude.<$> programSelection,
            ("languageCode" Core..=) Prelude.<$> languageCode,
            ("offset" Core..=) Prelude.<$> offset,
            ("defaultSelection" Core..=)
              Prelude.<$> defaultSelection,
            ("pids" Core..=) Prelude.<$> pids,
            ("hlsRenditionGroupSettings" Core..=)
              Prelude.<$> hlsRenditionGroupSettings,
            ("selectorType" Core..=) Prelude.<$> selectorType,
            ("externalAudioFileInput" Core..=)
              Prelude.<$> externalAudioFileInput,
            ("remixSettings" Core..=) Prelude.<$> remixSettings
          ]
      )
