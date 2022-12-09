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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AudioDefaultSelection
import Amazonka.MediaConvert.Types.AudioDurationCorrection
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
  { -- | Apply audio timing corrections to help synchronize audio and video in
    -- your output. To apply timing corrections, your input must meet the
    -- following requirements: * Container: MP4, or MOV, with an accurate
    -- time-to-sample (STTS) table. * Audio track: AAC. Choose from the
    -- following audio timing correction settings: * Disabled (Default): Apply
    -- no correction. * Auto: Recommended for most inputs. MediaConvert
    -- analyzes the audio timing in your input and determines which correction
    -- setting to use, if needed. * Track: Adjust the duration of each audio
    -- frame by a constant amount to align the audio track length with STTS
    -- duration. Track-level correction does not affect pitch, and is
    -- recommended for tonal audio content such as music. * Frame: Adjust the
    -- duration of each audio frame by a variable amount to align audio frames
    -- with STTS timestamps. No corrections are made to already-aligned frames.
    -- Frame-level correction may affect the pitch of corrected frames, and is
    -- recommended for atonal audio content such as speech or percussion.
    audioDurationCorrection :: Prelude.Maybe AudioDurationCorrection,
    -- | Selects a specific language code from within an audio source, using the
    -- ISO 639-2 or ISO 639-3 three-letter language code
    customLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | Enable this setting on one audio selector to set it as the default for
    -- the job. The service uses this default for outputs where it can\'t find
    -- the specified input audio. If you don\'t set a default, those outputs
    -- have no audio.
    defaultSelection :: Prelude.Maybe AudioDefaultSelection,
    -- | Specifies audio data from an external file source.
    externalAudioFileInput :: Prelude.Maybe Prelude.Text,
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
    -- | Selects a specific language code from within an audio source.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Specifies a time delta in milliseconds to offset the audio from the
    -- input video.
    offset :: Prelude.Maybe Prelude.Int,
    -- | Selects a specific PID from within an audio source (e.g. 257 selects PID
    -- 0x101).
    pids :: Prelude.Maybe [Prelude.Natural],
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
    -- | Use these settings to reorder the audio channels of one input to match
    -- those of another input. This allows you to combine the two files into a
    -- single output, one after the other.
    remixSettings :: Prelude.Maybe RemixSettings,
    -- | Specifies the type of the audio selector.
    selectorType :: Prelude.Maybe AudioSelectorType,
    -- | Identify a track from the input audio to include in this selector by
    -- entering the track index number. To include several tracks in a single
    -- audio selector, specify multiple tracks as follows. Using the console,
    -- enter a comma-separated list. For examle, type \"1,2,3\" to include
    -- tracks 1 through 3. Specifying directly in your JSON job file, provide
    -- the track numbers in an array. For example, \"tracks\": [1,2,3].
    tracks :: Prelude.Maybe [Prelude.Natural]
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
-- 'audioDurationCorrection', 'audioSelector_audioDurationCorrection' - Apply audio timing corrections to help synchronize audio and video in
-- your output. To apply timing corrections, your input must meet the
-- following requirements: * Container: MP4, or MOV, with an accurate
-- time-to-sample (STTS) table. * Audio track: AAC. Choose from the
-- following audio timing correction settings: * Disabled (Default): Apply
-- no correction. * Auto: Recommended for most inputs. MediaConvert
-- analyzes the audio timing in your input and determines which correction
-- setting to use, if needed. * Track: Adjust the duration of each audio
-- frame by a constant amount to align the audio track length with STTS
-- duration. Track-level correction does not affect pitch, and is
-- recommended for tonal audio content such as music. * Frame: Adjust the
-- duration of each audio frame by a variable amount to align audio frames
-- with STTS timestamps. No corrections are made to already-aligned frames.
-- Frame-level correction may affect the pitch of corrected frames, and is
-- recommended for atonal audio content such as speech or percussion.
--
-- 'customLanguageCode', 'audioSelector_customLanguageCode' - Selects a specific language code from within an audio source, using the
-- ISO 639-2 or ISO 639-3 three-letter language code
--
-- 'defaultSelection', 'audioSelector_defaultSelection' - Enable this setting on one audio selector to set it as the default for
-- the job. The service uses this default for outputs where it can\'t find
-- the specified input audio. If you don\'t set a default, those outputs
-- have no audio.
--
-- 'externalAudioFileInput', 'audioSelector_externalAudioFileInput' - Specifies audio data from an external file source.
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
-- 'languageCode', 'audioSelector_languageCode' - Selects a specific language code from within an audio source.
--
-- 'offset', 'audioSelector_offset' - Specifies a time delta in milliseconds to offset the audio from the
-- input video.
--
-- 'pids', 'audioSelector_pids' - Selects a specific PID from within an audio source (e.g. 257 selects PID
-- 0x101).
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
-- 'remixSettings', 'audioSelector_remixSettings' - Use these settings to reorder the audio channels of one input to match
-- those of another input. This allows you to combine the two files into a
-- single output, one after the other.
--
-- 'selectorType', 'audioSelector_selectorType' - Specifies the type of the audio selector.
--
-- 'tracks', 'audioSelector_tracks' - Identify a track from the input audio to include in this selector by
-- entering the track index number. To include several tracks in a single
-- audio selector, specify multiple tracks as follows. Using the console,
-- enter a comma-separated list. For examle, type \"1,2,3\" to include
-- tracks 1 through 3. Specifying directly in your JSON job file, provide
-- the track numbers in an array. For example, \"tracks\": [1,2,3].
newAudioSelector ::
  AudioSelector
newAudioSelector =
  AudioSelector'
    { audioDurationCorrection =
        Prelude.Nothing,
      customLanguageCode = Prelude.Nothing,
      defaultSelection = Prelude.Nothing,
      externalAudioFileInput = Prelude.Nothing,
      hlsRenditionGroupSettings = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      offset = Prelude.Nothing,
      pids = Prelude.Nothing,
      programSelection = Prelude.Nothing,
      remixSettings = Prelude.Nothing,
      selectorType = Prelude.Nothing,
      tracks = Prelude.Nothing
    }

-- | Apply audio timing corrections to help synchronize audio and video in
-- your output. To apply timing corrections, your input must meet the
-- following requirements: * Container: MP4, or MOV, with an accurate
-- time-to-sample (STTS) table. * Audio track: AAC. Choose from the
-- following audio timing correction settings: * Disabled (Default): Apply
-- no correction. * Auto: Recommended for most inputs. MediaConvert
-- analyzes the audio timing in your input and determines which correction
-- setting to use, if needed. * Track: Adjust the duration of each audio
-- frame by a constant amount to align the audio track length with STTS
-- duration. Track-level correction does not affect pitch, and is
-- recommended for tonal audio content such as music. * Frame: Adjust the
-- duration of each audio frame by a variable amount to align audio frames
-- with STTS timestamps. No corrections are made to already-aligned frames.
-- Frame-level correction may affect the pitch of corrected frames, and is
-- recommended for atonal audio content such as speech or percussion.
audioSelector_audioDurationCorrection :: Lens.Lens' AudioSelector (Prelude.Maybe AudioDurationCorrection)
audioSelector_audioDurationCorrection = Lens.lens (\AudioSelector' {audioDurationCorrection} -> audioDurationCorrection) (\s@AudioSelector' {} a -> s {audioDurationCorrection = a} :: AudioSelector)

-- | Selects a specific language code from within an audio source, using the
-- ISO 639-2 or ISO 639-3 three-letter language code
audioSelector_customLanguageCode :: Lens.Lens' AudioSelector (Prelude.Maybe Prelude.Text)
audioSelector_customLanguageCode = Lens.lens (\AudioSelector' {customLanguageCode} -> customLanguageCode) (\s@AudioSelector' {} a -> s {customLanguageCode = a} :: AudioSelector)

-- | Enable this setting on one audio selector to set it as the default for
-- the job. The service uses this default for outputs where it can\'t find
-- the specified input audio. If you don\'t set a default, those outputs
-- have no audio.
audioSelector_defaultSelection :: Lens.Lens' AudioSelector (Prelude.Maybe AudioDefaultSelection)
audioSelector_defaultSelection = Lens.lens (\AudioSelector' {defaultSelection} -> defaultSelection) (\s@AudioSelector' {} a -> s {defaultSelection = a} :: AudioSelector)

-- | Specifies audio data from an external file source.
audioSelector_externalAudioFileInput :: Lens.Lens' AudioSelector (Prelude.Maybe Prelude.Text)
audioSelector_externalAudioFileInput = Lens.lens (\AudioSelector' {externalAudioFileInput} -> externalAudioFileInput) (\s@AudioSelector' {} a -> s {externalAudioFileInput = a} :: AudioSelector)

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

-- | Selects a specific language code from within an audio source.
audioSelector_languageCode :: Lens.Lens' AudioSelector (Prelude.Maybe LanguageCode)
audioSelector_languageCode = Lens.lens (\AudioSelector' {languageCode} -> languageCode) (\s@AudioSelector' {} a -> s {languageCode = a} :: AudioSelector)

-- | Specifies a time delta in milliseconds to offset the audio from the
-- input video.
audioSelector_offset :: Lens.Lens' AudioSelector (Prelude.Maybe Prelude.Int)
audioSelector_offset = Lens.lens (\AudioSelector' {offset} -> offset) (\s@AudioSelector' {} a -> s {offset = a} :: AudioSelector)

-- | Selects a specific PID from within an audio source (e.g. 257 selects PID
-- 0x101).
audioSelector_pids :: Lens.Lens' AudioSelector (Prelude.Maybe [Prelude.Natural])
audioSelector_pids = Lens.lens (\AudioSelector' {pids} -> pids) (\s@AudioSelector' {} a -> s {pids = a} :: AudioSelector) Prelude.. Lens.mapping Lens.coerced

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

-- | Use these settings to reorder the audio channels of one input to match
-- those of another input. This allows you to combine the two files into a
-- single output, one after the other.
audioSelector_remixSettings :: Lens.Lens' AudioSelector (Prelude.Maybe RemixSettings)
audioSelector_remixSettings = Lens.lens (\AudioSelector' {remixSettings} -> remixSettings) (\s@AudioSelector' {} a -> s {remixSettings = a} :: AudioSelector)

-- | Specifies the type of the audio selector.
audioSelector_selectorType :: Lens.Lens' AudioSelector (Prelude.Maybe AudioSelectorType)
audioSelector_selectorType = Lens.lens (\AudioSelector' {selectorType} -> selectorType) (\s@AudioSelector' {} a -> s {selectorType = a} :: AudioSelector)

-- | Identify a track from the input audio to include in this selector by
-- entering the track index number. To include several tracks in a single
-- audio selector, specify multiple tracks as follows. Using the console,
-- enter a comma-separated list. For examle, type \"1,2,3\" to include
-- tracks 1 through 3. Specifying directly in your JSON job file, provide
-- the track numbers in an array. For example, \"tracks\": [1,2,3].
audioSelector_tracks :: Lens.Lens' AudioSelector (Prelude.Maybe [Prelude.Natural])
audioSelector_tracks = Lens.lens (\AudioSelector' {tracks} -> tracks) (\s@AudioSelector' {} a -> s {tracks = a} :: AudioSelector) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AudioSelector where
  parseJSON =
    Data.withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            Prelude.<$> (x Data..:? "audioDurationCorrection")
            Prelude.<*> (x Data..:? "customLanguageCode")
            Prelude.<*> (x Data..:? "defaultSelection")
            Prelude.<*> (x Data..:? "externalAudioFileInput")
            Prelude.<*> (x Data..:? "hlsRenditionGroupSettings")
            Prelude.<*> (x Data..:? "languageCode")
            Prelude.<*> (x Data..:? "offset")
            Prelude.<*> (x Data..:? "pids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "programSelection")
            Prelude.<*> (x Data..:? "remixSettings")
            Prelude.<*> (x Data..:? "selectorType")
            Prelude.<*> (x Data..:? "tracks" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AudioSelector where
  hashWithSalt _salt AudioSelector' {..} =
    _salt
      `Prelude.hashWithSalt` audioDurationCorrection
      `Prelude.hashWithSalt` customLanguageCode
      `Prelude.hashWithSalt` defaultSelection
      `Prelude.hashWithSalt` externalAudioFileInput
      `Prelude.hashWithSalt` hlsRenditionGroupSettings
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` pids
      `Prelude.hashWithSalt` programSelection
      `Prelude.hashWithSalt` remixSettings
      `Prelude.hashWithSalt` selectorType
      `Prelude.hashWithSalt` tracks

instance Prelude.NFData AudioSelector where
  rnf AudioSelector' {..} =
    Prelude.rnf audioDurationCorrection
      `Prelude.seq` Prelude.rnf customLanguageCode
      `Prelude.seq` Prelude.rnf defaultSelection
      `Prelude.seq` Prelude.rnf externalAudioFileInput
      `Prelude.seq` Prelude.rnf hlsRenditionGroupSettings
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf offset
      `Prelude.seq` Prelude.rnf pids
      `Prelude.seq` Prelude.rnf programSelection
      `Prelude.seq` Prelude.rnf remixSettings
      `Prelude.seq` Prelude.rnf selectorType
      `Prelude.seq` Prelude.rnf tracks

instance Data.ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioDurationCorrection" Data..=)
              Prelude.<$> audioDurationCorrection,
            ("customLanguageCode" Data..=)
              Prelude.<$> customLanguageCode,
            ("defaultSelection" Data..=)
              Prelude.<$> defaultSelection,
            ("externalAudioFileInput" Data..=)
              Prelude.<$> externalAudioFileInput,
            ("hlsRenditionGroupSettings" Data..=)
              Prelude.<$> hlsRenditionGroupSettings,
            ("languageCode" Data..=) Prelude.<$> languageCode,
            ("offset" Data..=) Prelude.<$> offset,
            ("pids" Data..=) Prelude.<$> pids,
            ("programSelection" Data..=)
              Prelude.<$> programSelection,
            ("remixSettings" Data..=) Prelude.<$> remixSettings,
            ("selectorType" Data..=) Prelude.<$> selectorType,
            ("tracks" Data..=) Prelude.<$> tracks
          ]
      )
