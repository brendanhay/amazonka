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
-- Module      : Network.AWS.MediaLive.Types.AudioDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioCodecSettings
import Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl
import Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
import Network.AWS.MediaLive.Types.AudioNormalizationSettings
import Network.AWS.MediaLive.Types.AudioType
import Network.AWS.MediaLive.Types.AudioWatermarkSettings
import Network.AWS.MediaLive.Types.RemixSettings
import qualified Network.AWS.Prelude as Prelude

-- | Audio Description
--
-- /See:/ 'newAudioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { -- | RFC 5646 language code representing the language of the audio output
    -- track. Only used if languageControlMode is useConfigured, or there is no
    -- ISO 639 language code specified in the input.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | Applies only if audioTypeControl is useConfigured. The values for
    -- audioType are defined in ISO-IEC 13818-1.
    audioType :: Prelude.Maybe AudioType,
    -- | Advanced audio normalization settings.
    audioNormalizationSettings :: Prelude.Maybe AudioNormalizationSettings,
    -- | Choosing followInput will cause the ISO 639 language code of the output
    -- to follow the ISO 639 language code of the input. The languageCode will
    -- be used when useConfigured is set, or when followInput is selected but
    -- there is no ISO 639 language code specified by the input.
    languageCodeControl :: Prelude.Maybe AudioDescriptionLanguageCodeControl,
    -- | Audio codec settings.
    codecSettings :: Prelude.Maybe AudioCodecSettings,
    -- | Settings to configure one or more solutions that insert audio watermarks
    -- in the audio encode
    audioWatermarkingSettings :: Prelude.Maybe AudioWatermarkSettings,
    -- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed
    -- by the player (eg. English, or Director Commentary).
    streamName :: Prelude.Maybe Prelude.Text,
    -- | Settings that control how input audio channels are remixed into the
    -- output audio channels.
    remixSettings :: Prelude.Maybe RemixSettings,
    -- | Determines how audio type is determined. followInput: If the input
    -- contains an ISO 639 audioType, then that value is passed through to the
    -- output. If the input contains no ISO 639 audioType, the value in Audio
    -- Type is included in the output. useConfigured: The value in Audio Type
    -- is included in the output. Note that this field and audioType are both
    -- ignored if inputType is broadcasterMixedAd.
    audioTypeControl :: Prelude.Maybe AudioDescriptionAudioTypeControl,
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
-- 'languageCode', 'audioDescription_languageCode' - RFC 5646 language code representing the language of the audio output
-- track. Only used if languageControlMode is useConfigured, or there is no
-- ISO 639 language code specified in the input.
--
-- 'audioType', 'audioDescription_audioType' - Applies only if audioTypeControl is useConfigured. The values for
-- audioType are defined in ISO-IEC 13818-1.
--
-- 'audioNormalizationSettings', 'audioDescription_audioNormalizationSettings' - Advanced audio normalization settings.
--
-- 'languageCodeControl', 'audioDescription_languageCodeControl' - Choosing followInput will cause the ISO 639 language code of the output
-- to follow the ISO 639 language code of the input. The languageCode will
-- be used when useConfigured is set, or when followInput is selected but
-- there is no ISO 639 language code specified by the input.
--
-- 'codecSettings', 'audioDescription_codecSettings' - Audio codec settings.
--
-- 'audioWatermarkingSettings', 'audioDescription_audioWatermarkingSettings' - Settings to configure one or more solutions that insert audio watermarks
-- in the audio encode
--
-- 'streamName', 'audioDescription_streamName' - Used for MS Smooth and Apple HLS outputs. Indicates the name displayed
-- by the player (eg. English, or Director Commentary).
--
-- 'remixSettings', 'audioDescription_remixSettings' - Settings that control how input audio channels are remixed into the
-- output audio channels.
--
-- 'audioTypeControl', 'audioDescription_audioTypeControl' - Determines how audio type is determined. followInput: If the input
-- contains an ISO 639 audioType, then that value is passed through to the
-- output. If the input contains no ISO 639 audioType, the value in Audio
-- Type is included in the output. useConfigured: The value in Audio Type
-- is included in the output. Note that this field and audioType are both
-- ignored if inputType is broadcasterMixedAd.
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
    { languageCode = Prelude.Nothing,
      audioType = Prelude.Nothing,
      audioNormalizationSettings = Prelude.Nothing,
      languageCodeControl = Prelude.Nothing,
      codecSettings = Prelude.Nothing,
      audioWatermarkingSettings = Prelude.Nothing,
      streamName = Prelude.Nothing,
      remixSettings = Prelude.Nothing,
      audioTypeControl = Prelude.Nothing,
      audioSelectorName = pAudioSelectorName_,
      name = pName_
    }

-- | RFC 5646 language code representing the language of the audio output
-- track. Only used if languageControlMode is useConfigured, or there is no
-- ISO 639 language code specified in the input.
audioDescription_languageCode :: Lens.Lens' AudioDescription (Prelude.Maybe Prelude.Text)
audioDescription_languageCode = Lens.lens (\AudioDescription' {languageCode} -> languageCode) (\s@AudioDescription' {} a -> s {languageCode = a} :: AudioDescription)

-- | Applies only if audioTypeControl is useConfigured. The values for
-- audioType are defined in ISO-IEC 13818-1.
audioDescription_audioType :: Lens.Lens' AudioDescription (Prelude.Maybe AudioType)
audioDescription_audioType = Lens.lens (\AudioDescription' {audioType} -> audioType) (\s@AudioDescription' {} a -> s {audioType = a} :: AudioDescription)

-- | Advanced audio normalization settings.
audioDescription_audioNormalizationSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioNormalizationSettings)
audioDescription_audioNormalizationSettings = Lens.lens (\AudioDescription' {audioNormalizationSettings} -> audioNormalizationSettings) (\s@AudioDescription' {} a -> s {audioNormalizationSettings = a} :: AudioDescription)

-- | Choosing followInput will cause the ISO 639 language code of the output
-- to follow the ISO 639 language code of the input. The languageCode will
-- be used when useConfigured is set, or when followInput is selected but
-- there is no ISO 639 language code specified by the input.
audioDescription_languageCodeControl :: Lens.Lens' AudioDescription (Prelude.Maybe AudioDescriptionLanguageCodeControl)
audioDescription_languageCodeControl = Lens.lens (\AudioDescription' {languageCodeControl} -> languageCodeControl) (\s@AudioDescription' {} a -> s {languageCodeControl = a} :: AudioDescription)

-- | Audio codec settings.
audioDescription_codecSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioCodecSettings)
audioDescription_codecSettings = Lens.lens (\AudioDescription' {codecSettings} -> codecSettings) (\s@AudioDescription' {} a -> s {codecSettings = a} :: AudioDescription)

-- | Settings to configure one or more solutions that insert audio watermarks
-- in the audio encode
audioDescription_audioWatermarkingSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioWatermarkSettings)
audioDescription_audioWatermarkingSettings = Lens.lens (\AudioDescription' {audioWatermarkingSettings} -> audioWatermarkingSettings) (\s@AudioDescription' {} a -> s {audioWatermarkingSettings = a} :: AudioDescription)

-- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed
-- by the player (eg. English, or Director Commentary).
audioDescription_streamName :: Lens.Lens' AudioDescription (Prelude.Maybe Prelude.Text)
audioDescription_streamName = Lens.lens (\AudioDescription' {streamName} -> streamName) (\s@AudioDescription' {} a -> s {streamName = a} :: AudioDescription)

-- | Settings that control how input audio channels are remixed into the
-- output audio channels.
audioDescription_remixSettings :: Lens.Lens' AudioDescription (Prelude.Maybe RemixSettings)
audioDescription_remixSettings = Lens.lens (\AudioDescription' {remixSettings} -> remixSettings) (\s@AudioDescription' {} a -> s {remixSettings = a} :: AudioDescription)

-- | Determines how audio type is determined. followInput: If the input
-- contains an ISO 639 audioType, then that value is passed through to the
-- output. If the input contains no ISO 639 audioType, the value in Audio
-- Type is included in the output. useConfigured: The value in Audio Type
-- is included in the output. Note that this field and audioType are both
-- ignored if inputType is broadcasterMixedAd.
audioDescription_audioTypeControl :: Lens.Lens' AudioDescription (Prelude.Maybe AudioDescriptionAudioTypeControl)
audioDescription_audioTypeControl = Lens.lens (\AudioDescription' {audioTypeControl} -> audioTypeControl) (\s@AudioDescription' {} a -> s {audioTypeControl = a} :: AudioDescription)

-- | The name of the AudioSelector used as the source for this
-- AudioDescription.
audioDescription_audioSelectorName :: Lens.Lens' AudioDescription Prelude.Text
audioDescription_audioSelectorName = Lens.lens (\AudioDescription' {audioSelectorName} -> audioSelectorName) (\s@AudioDescription' {} a -> s {audioSelectorName = a} :: AudioDescription)

-- | The name of this AudioDescription. Outputs will use this name to
-- uniquely identify this AudioDescription. Description names should be
-- unique within this Live Event.
audioDescription_name :: Lens.Lens' AudioDescription Prelude.Text
audioDescription_name = Lens.lens (\AudioDescription' {name} -> name) (\s@AudioDescription' {} a -> s {name = a} :: AudioDescription)

instance Core.FromJSON AudioDescription where
  parseJSON =
    Core.withObject
      "AudioDescription"
      ( \x ->
          AudioDescription'
            Prelude.<$> (x Core..:? "languageCode")
            Prelude.<*> (x Core..:? "audioType")
            Prelude.<*> (x Core..:? "audioNormalizationSettings")
            Prelude.<*> (x Core..:? "languageCodeControl")
            Prelude.<*> (x Core..:? "codecSettings")
            Prelude.<*> (x Core..:? "audioWatermarkingSettings")
            Prelude.<*> (x Core..:? "streamName")
            Prelude.<*> (x Core..:? "remixSettings")
            Prelude.<*> (x Core..:? "audioTypeControl")
            Prelude.<*> (x Core..: "audioSelectorName")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable AudioDescription

instance Prelude.NFData AudioDescription

instance Core.ToJSON AudioDescription where
  toJSON AudioDescription' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("languageCode" Core..=) Prelude.<$> languageCode,
            ("audioType" Core..=) Prelude.<$> audioType,
            ("audioNormalizationSettings" Core..=)
              Prelude.<$> audioNormalizationSettings,
            ("languageCodeControl" Core..=)
              Prelude.<$> languageCodeControl,
            ("codecSettings" Core..=) Prelude.<$> codecSettings,
            ("audioWatermarkingSettings" Core..=)
              Prelude.<$> audioWatermarkingSettings,
            ("streamName" Core..=) Prelude.<$> streamName,
            ("remixSettings" Core..=) Prelude.<$> remixSettings,
            ("audioTypeControl" Core..=)
              Prelude.<$> audioTypeControl,
            Prelude.Just
              ("audioSelectorName" Core..= audioSelectorName),
            Prelude.Just ("name" Core..= name)
          ]
      )
