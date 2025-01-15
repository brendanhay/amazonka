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
-- Module      : Amazonka.MediaLive.Types.AudioDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AudioCodecSettings
import Amazonka.MediaLive.Types.AudioDescriptionAudioTypeControl
import Amazonka.MediaLive.Types.AudioDescriptionLanguageCodeControl
import Amazonka.MediaLive.Types.AudioNormalizationSettings
import Amazonka.MediaLive.Types.AudioType
import Amazonka.MediaLive.Types.AudioWatermarkSettings
import Amazonka.MediaLive.Types.RemixSettings
import qualified Amazonka.Prelude as Prelude

-- | Audio Description
--
-- /See:/ 'newAudioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { -- | Advanced audio normalization settings.
    audioNormalizationSettings :: Prelude.Maybe AudioNormalizationSettings,
    -- | Applies only if audioTypeControl is useConfigured. The values for
    -- audioType are defined in ISO-IEC 13818-1.
    audioType :: Prelude.Maybe AudioType,
    -- | Determines how audio type is determined. followInput: If the input
    -- contains an ISO 639 audioType, then that value is passed through to the
    -- output. If the input contains no ISO 639 audioType, the value in Audio
    -- Type is included in the output. useConfigured: The value in Audio Type
    -- is included in the output. Note that this field and audioType are both
    -- ignored if inputType is broadcasterMixedAd.
    audioTypeControl :: Prelude.Maybe AudioDescriptionAudioTypeControl,
    -- | Settings to configure one or more solutions that insert audio watermarks
    -- in the audio encode
    audioWatermarkingSettings :: Prelude.Maybe AudioWatermarkSettings,
    -- | Audio codec settings.
    codecSettings :: Prelude.Maybe AudioCodecSettings,
    -- | RFC 5646 language code representing the language of the audio output
    -- track. Only used if languageControlMode is useConfigured, or there is no
    -- ISO 639 language code specified in the input.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | Choosing followInput will cause the ISO 639 language code of the output
    -- to follow the ISO 639 language code of the input. The languageCode will
    -- be used when useConfigured is set, or when followInput is selected but
    -- there is no ISO 639 language code specified by the input.
    languageCodeControl :: Prelude.Maybe AudioDescriptionLanguageCodeControl,
    -- | Settings that control how input audio channels are remixed into the
    -- output audio channels.
    remixSettings :: Prelude.Maybe RemixSettings,
    -- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed
    -- by the player (eg. English, or Director Commentary).
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The name of the AudioSelector used as the source for this
    -- AudioDescription.
    audioSelectorName :: Prelude.Text,
    -- | The name of this AudioDescription. Outputs will use this name to
    -- uniquely identify this AudioDescription. Description names should be
    -- unique within this Live Event.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioNormalizationSettings', 'audioDescription_audioNormalizationSettings' - Advanced audio normalization settings.
--
-- 'audioType', 'audioDescription_audioType' - Applies only if audioTypeControl is useConfigured. The values for
-- audioType are defined in ISO-IEC 13818-1.
--
-- 'audioTypeControl', 'audioDescription_audioTypeControl' - Determines how audio type is determined. followInput: If the input
-- contains an ISO 639 audioType, then that value is passed through to the
-- output. If the input contains no ISO 639 audioType, the value in Audio
-- Type is included in the output. useConfigured: The value in Audio Type
-- is included in the output. Note that this field and audioType are both
-- ignored if inputType is broadcasterMixedAd.
--
-- 'audioWatermarkingSettings', 'audioDescription_audioWatermarkingSettings' - Settings to configure one or more solutions that insert audio watermarks
-- in the audio encode
--
-- 'codecSettings', 'audioDescription_codecSettings' - Audio codec settings.
--
-- 'languageCode', 'audioDescription_languageCode' - RFC 5646 language code representing the language of the audio output
-- track. Only used if languageControlMode is useConfigured, or there is no
-- ISO 639 language code specified in the input.
--
-- 'languageCodeControl', 'audioDescription_languageCodeControl' - Choosing followInput will cause the ISO 639 language code of the output
-- to follow the ISO 639 language code of the input. The languageCode will
-- be used when useConfigured is set, or when followInput is selected but
-- there is no ISO 639 language code specified by the input.
--
-- 'remixSettings', 'audioDescription_remixSettings' - Settings that control how input audio channels are remixed into the
-- output audio channels.
--
-- 'streamName', 'audioDescription_streamName' - Used for MS Smooth and Apple HLS outputs. Indicates the name displayed
-- by the player (eg. English, or Director Commentary).
--
-- 'audioSelectorName', 'audioDescription_audioSelectorName' - The name of the AudioSelector used as the source for this
-- AudioDescription.
--
-- 'name', 'audioDescription_name' - The name of this AudioDescription. Outputs will use this name to
-- uniquely identify this AudioDescription. Description names should be
-- unique within this Live Event.
newAudioDescription ::
  -- | 'audioSelectorName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  AudioDescription
newAudioDescription pAudioSelectorName_ pName_ =
  AudioDescription'
    { audioNormalizationSettings =
        Prelude.Nothing,
      audioType = Prelude.Nothing,
      audioTypeControl = Prelude.Nothing,
      audioWatermarkingSettings = Prelude.Nothing,
      codecSettings = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      languageCodeControl = Prelude.Nothing,
      remixSettings = Prelude.Nothing,
      streamName = Prelude.Nothing,
      audioSelectorName = pAudioSelectorName_,
      name = pName_
    }

-- | Advanced audio normalization settings.
audioDescription_audioNormalizationSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioNormalizationSettings)
audioDescription_audioNormalizationSettings = Lens.lens (\AudioDescription' {audioNormalizationSettings} -> audioNormalizationSettings) (\s@AudioDescription' {} a -> s {audioNormalizationSettings = a} :: AudioDescription)

-- | Applies only if audioTypeControl is useConfigured. The values for
-- audioType are defined in ISO-IEC 13818-1.
audioDescription_audioType :: Lens.Lens' AudioDescription (Prelude.Maybe AudioType)
audioDescription_audioType = Lens.lens (\AudioDescription' {audioType} -> audioType) (\s@AudioDescription' {} a -> s {audioType = a} :: AudioDescription)

-- | Determines how audio type is determined. followInput: If the input
-- contains an ISO 639 audioType, then that value is passed through to the
-- output. If the input contains no ISO 639 audioType, the value in Audio
-- Type is included in the output. useConfigured: The value in Audio Type
-- is included in the output. Note that this field and audioType are both
-- ignored if inputType is broadcasterMixedAd.
audioDescription_audioTypeControl :: Lens.Lens' AudioDescription (Prelude.Maybe AudioDescriptionAudioTypeControl)
audioDescription_audioTypeControl = Lens.lens (\AudioDescription' {audioTypeControl} -> audioTypeControl) (\s@AudioDescription' {} a -> s {audioTypeControl = a} :: AudioDescription)

-- | Settings to configure one or more solutions that insert audio watermarks
-- in the audio encode
audioDescription_audioWatermarkingSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioWatermarkSettings)
audioDescription_audioWatermarkingSettings = Lens.lens (\AudioDescription' {audioWatermarkingSettings} -> audioWatermarkingSettings) (\s@AudioDescription' {} a -> s {audioWatermarkingSettings = a} :: AudioDescription)

-- | Audio codec settings.
audioDescription_codecSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioCodecSettings)
audioDescription_codecSettings = Lens.lens (\AudioDescription' {codecSettings} -> codecSettings) (\s@AudioDescription' {} a -> s {codecSettings = a} :: AudioDescription)

-- | RFC 5646 language code representing the language of the audio output
-- track. Only used if languageControlMode is useConfigured, or there is no
-- ISO 639 language code specified in the input.
audioDescription_languageCode :: Lens.Lens' AudioDescription (Prelude.Maybe Prelude.Text)
audioDescription_languageCode = Lens.lens (\AudioDescription' {languageCode} -> languageCode) (\s@AudioDescription' {} a -> s {languageCode = a} :: AudioDescription)

-- | Choosing followInput will cause the ISO 639 language code of the output
-- to follow the ISO 639 language code of the input. The languageCode will
-- be used when useConfigured is set, or when followInput is selected but
-- there is no ISO 639 language code specified by the input.
audioDescription_languageCodeControl :: Lens.Lens' AudioDescription (Prelude.Maybe AudioDescriptionLanguageCodeControl)
audioDescription_languageCodeControl = Lens.lens (\AudioDescription' {languageCodeControl} -> languageCodeControl) (\s@AudioDescription' {} a -> s {languageCodeControl = a} :: AudioDescription)

-- | Settings that control how input audio channels are remixed into the
-- output audio channels.
audioDescription_remixSettings :: Lens.Lens' AudioDescription (Prelude.Maybe RemixSettings)
audioDescription_remixSettings = Lens.lens (\AudioDescription' {remixSettings} -> remixSettings) (\s@AudioDescription' {} a -> s {remixSettings = a} :: AudioDescription)

-- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed
-- by the player (eg. English, or Director Commentary).
audioDescription_streamName :: Lens.Lens' AudioDescription (Prelude.Maybe Prelude.Text)
audioDescription_streamName = Lens.lens (\AudioDescription' {streamName} -> streamName) (\s@AudioDescription' {} a -> s {streamName = a} :: AudioDescription)

-- | The name of the AudioSelector used as the source for this
-- AudioDescription.
audioDescription_audioSelectorName :: Lens.Lens' AudioDescription Prelude.Text
audioDescription_audioSelectorName = Lens.lens (\AudioDescription' {audioSelectorName} -> audioSelectorName) (\s@AudioDescription' {} a -> s {audioSelectorName = a} :: AudioDescription)

-- | The name of this AudioDescription. Outputs will use this name to
-- uniquely identify this AudioDescription. Description names should be
-- unique within this Live Event.
audioDescription_name :: Lens.Lens' AudioDescription Prelude.Text
audioDescription_name = Lens.lens (\AudioDescription' {name} -> name) (\s@AudioDescription' {} a -> s {name = a} :: AudioDescription)

instance Data.FromJSON AudioDescription where
  parseJSON =
    Data.withObject
      "AudioDescription"
      ( \x ->
          AudioDescription'
            Prelude.<$> (x Data..:? "audioNormalizationSettings")
            Prelude.<*> (x Data..:? "audioType")
            Prelude.<*> (x Data..:? "audioTypeControl")
            Prelude.<*> (x Data..:? "audioWatermarkingSettings")
            Prelude.<*> (x Data..:? "codecSettings")
            Prelude.<*> (x Data..:? "languageCode")
            Prelude.<*> (x Data..:? "languageCodeControl")
            Prelude.<*> (x Data..:? "remixSettings")
            Prelude.<*> (x Data..:? "streamName")
            Prelude.<*> (x Data..: "audioSelectorName")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable AudioDescription where
  hashWithSalt _salt AudioDescription' {..} =
    _salt
      `Prelude.hashWithSalt` audioNormalizationSettings
      `Prelude.hashWithSalt` audioType
      `Prelude.hashWithSalt` audioTypeControl
      `Prelude.hashWithSalt` audioWatermarkingSettings
      `Prelude.hashWithSalt` codecSettings
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` languageCodeControl
      `Prelude.hashWithSalt` remixSettings
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` audioSelectorName
      `Prelude.hashWithSalt` name

instance Prelude.NFData AudioDescription where
  rnf AudioDescription' {..} =
    Prelude.rnf audioNormalizationSettings `Prelude.seq`
      Prelude.rnf audioType `Prelude.seq`
        Prelude.rnf audioTypeControl `Prelude.seq`
          Prelude.rnf audioWatermarkingSettings `Prelude.seq`
            Prelude.rnf codecSettings `Prelude.seq`
              Prelude.rnf languageCode `Prelude.seq`
                Prelude.rnf languageCodeControl `Prelude.seq`
                  Prelude.rnf remixSettings `Prelude.seq`
                    Prelude.rnf streamName `Prelude.seq`
                      Prelude.rnf audioSelectorName `Prelude.seq`
                        Prelude.rnf name

instance Data.ToJSON AudioDescription where
  toJSON AudioDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioNormalizationSettings" Data..=)
              Prelude.<$> audioNormalizationSettings,
            ("audioType" Data..=) Prelude.<$> audioType,
            ("audioTypeControl" Data..=)
              Prelude.<$> audioTypeControl,
            ("audioWatermarkingSettings" Data..=)
              Prelude.<$> audioWatermarkingSettings,
            ("codecSettings" Data..=) Prelude.<$> codecSettings,
            ("languageCode" Data..=) Prelude.<$> languageCode,
            ("languageCodeControl" Data..=)
              Prelude.<$> languageCodeControl,
            ("remixSettings" Data..=) Prelude.<$> remixSettings,
            ("streamName" Data..=) Prelude.<$> streamName,
            Prelude.Just
              ("audioSelectorName" Data..= audioSelectorName),
            Prelude.Just ("name" Data..= name)
          ]
      )
