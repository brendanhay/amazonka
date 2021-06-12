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
import Network.AWS.MediaLive.Types.RemixSettings

-- | Audio Description
--
-- /See:/ 'newAudioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { -- | RFC 5646 language code representing the language of the audio output
    -- track. Only used if languageControlMode is useConfigured, or there is no
    -- ISO 639 language code specified in the input.
    languageCode :: Core.Maybe Core.Text,
    -- | Applies only if audioTypeControl is useConfigured. The values for
    -- audioType are defined in ISO-IEC 13818-1.
    audioType :: Core.Maybe AudioType,
    -- | Audio codec settings.
    codecSettings :: Core.Maybe AudioCodecSettings,
    -- | Choosing followInput will cause the ISO 639 language code of the output
    -- to follow the ISO 639 language code of the input. The languageCode will
    -- be used when useConfigured is set, or when followInput is selected but
    -- there is no ISO 639 language code specified by the input.
    languageCodeControl :: Core.Maybe AudioDescriptionLanguageCodeControl,
    -- | Determines how audio type is determined. followInput: If the input
    -- contains an ISO 639 audioType, then that value is passed through to the
    -- output. If the input contains no ISO 639 audioType, the value in Audio
    -- Type is included in the output. useConfigured: The value in Audio Type
    -- is included in the output. Note that this field and audioType are both
    -- ignored if inputType is broadcasterMixedAd.
    audioTypeControl :: Core.Maybe AudioDescriptionAudioTypeControl,
    -- | Settings that control how input audio channels are remixed into the
    -- output audio channels.
    remixSettings :: Core.Maybe RemixSettings,
    -- | Advanced audio normalization settings.
    audioNormalizationSettings :: Core.Maybe AudioNormalizationSettings,
    -- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed
    -- by the player (eg. English, or Director Commentary).
    streamName :: Core.Maybe Core.Text,
    -- | The name of the AudioSelector used as the source for this
    -- AudioDescription.
    audioSelectorName :: Core.Text,
    -- | The name of this AudioDescription. Outputs will use this name to
    -- uniquely identify this AudioDescription. Description names should be
    -- unique within this Live Event.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'codecSettings', 'audioDescription_codecSettings' - Audio codec settings.
--
-- 'languageCodeControl', 'audioDescription_languageCodeControl' - Choosing followInput will cause the ISO 639 language code of the output
-- to follow the ISO 639 language code of the input. The languageCode will
-- be used when useConfigured is set, or when followInput is selected but
-- there is no ISO 639 language code specified by the input.
--
-- 'audioTypeControl', 'audioDescription_audioTypeControl' - Determines how audio type is determined. followInput: If the input
-- contains an ISO 639 audioType, then that value is passed through to the
-- output. If the input contains no ISO 639 audioType, the value in Audio
-- Type is included in the output. useConfigured: The value in Audio Type
-- is included in the output. Note that this field and audioType are both
-- ignored if inputType is broadcasterMixedAd.
--
-- 'remixSettings', 'audioDescription_remixSettings' - Settings that control how input audio channels are remixed into the
-- output audio channels.
--
-- 'audioNormalizationSettings', 'audioDescription_audioNormalizationSettings' - Advanced audio normalization settings.
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
  Core.Text ->
  -- | 'name'
  Core.Text ->
  AudioDescription
newAudioDescription pAudioSelectorName_ pName_ =
  AudioDescription'
    { languageCode = Core.Nothing,
      audioType = Core.Nothing,
      codecSettings = Core.Nothing,
      languageCodeControl = Core.Nothing,
      audioTypeControl = Core.Nothing,
      remixSettings = Core.Nothing,
      audioNormalizationSettings = Core.Nothing,
      streamName = Core.Nothing,
      audioSelectorName = pAudioSelectorName_,
      name = pName_
    }

-- | RFC 5646 language code representing the language of the audio output
-- track. Only used if languageControlMode is useConfigured, or there is no
-- ISO 639 language code specified in the input.
audioDescription_languageCode :: Lens.Lens' AudioDescription (Core.Maybe Core.Text)
audioDescription_languageCode = Lens.lens (\AudioDescription' {languageCode} -> languageCode) (\s@AudioDescription' {} a -> s {languageCode = a} :: AudioDescription)

-- | Applies only if audioTypeControl is useConfigured. The values for
-- audioType are defined in ISO-IEC 13818-1.
audioDescription_audioType :: Lens.Lens' AudioDescription (Core.Maybe AudioType)
audioDescription_audioType = Lens.lens (\AudioDescription' {audioType} -> audioType) (\s@AudioDescription' {} a -> s {audioType = a} :: AudioDescription)

-- | Audio codec settings.
audioDescription_codecSettings :: Lens.Lens' AudioDescription (Core.Maybe AudioCodecSettings)
audioDescription_codecSettings = Lens.lens (\AudioDescription' {codecSettings} -> codecSettings) (\s@AudioDescription' {} a -> s {codecSettings = a} :: AudioDescription)

-- | Choosing followInput will cause the ISO 639 language code of the output
-- to follow the ISO 639 language code of the input. The languageCode will
-- be used when useConfigured is set, or when followInput is selected but
-- there is no ISO 639 language code specified by the input.
audioDescription_languageCodeControl :: Lens.Lens' AudioDescription (Core.Maybe AudioDescriptionLanguageCodeControl)
audioDescription_languageCodeControl = Lens.lens (\AudioDescription' {languageCodeControl} -> languageCodeControl) (\s@AudioDescription' {} a -> s {languageCodeControl = a} :: AudioDescription)

-- | Determines how audio type is determined. followInput: If the input
-- contains an ISO 639 audioType, then that value is passed through to the
-- output. If the input contains no ISO 639 audioType, the value in Audio
-- Type is included in the output. useConfigured: The value in Audio Type
-- is included in the output. Note that this field and audioType are both
-- ignored if inputType is broadcasterMixedAd.
audioDescription_audioTypeControl :: Lens.Lens' AudioDescription (Core.Maybe AudioDescriptionAudioTypeControl)
audioDescription_audioTypeControl = Lens.lens (\AudioDescription' {audioTypeControl} -> audioTypeControl) (\s@AudioDescription' {} a -> s {audioTypeControl = a} :: AudioDescription)

-- | Settings that control how input audio channels are remixed into the
-- output audio channels.
audioDescription_remixSettings :: Lens.Lens' AudioDescription (Core.Maybe RemixSettings)
audioDescription_remixSettings = Lens.lens (\AudioDescription' {remixSettings} -> remixSettings) (\s@AudioDescription' {} a -> s {remixSettings = a} :: AudioDescription)

-- | Advanced audio normalization settings.
audioDescription_audioNormalizationSettings :: Lens.Lens' AudioDescription (Core.Maybe AudioNormalizationSettings)
audioDescription_audioNormalizationSettings = Lens.lens (\AudioDescription' {audioNormalizationSettings} -> audioNormalizationSettings) (\s@AudioDescription' {} a -> s {audioNormalizationSettings = a} :: AudioDescription)

-- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed
-- by the player (eg. English, or Director Commentary).
audioDescription_streamName :: Lens.Lens' AudioDescription (Core.Maybe Core.Text)
audioDescription_streamName = Lens.lens (\AudioDescription' {streamName} -> streamName) (\s@AudioDescription' {} a -> s {streamName = a} :: AudioDescription)

-- | The name of the AudioSelector used as the source for this
-- AudioDescription.
audioDescription_audioSelectorName :: Lens.Lens' AudioDescription Core.Text
audioDescription_audioSelectorName = Lens.lens (\AudioDescription' {audioSelectorName} -> audioSelectorName) (\s@AudioDescription' {} a -> s {audioSelectorName = a} :: AudioDescription)

-- | The name of this AudioDescription. Outputs will use this name to
-- uniquely identify this AudioDescription. Description names should be
-- unique within this Live Event.
audioDescription_name :: Lens.Lens' AudioDescription Core.Text
audioDescription_name = Lens.lens (\AudioDescription' {name} -> name) (\s@AudioDescription' {} a -> s {name = a} :: AudioDescription)

instance Core.FromJSON AudioDescription where
  parseJSON =
    Core.withObject
      "AudioDescription"
      ( \x ->
          AudioDescription'
            Core.<$> (x Core..:? "languageCode")
            Core.<*> (x Core..:? "audioType")
            Core.<*> (x Core..:? "codecSettings")
            Core.<*> (x Core..:? "languageCodeControl")
            Core.<*> (x Core..:? "audioTypeControl")
            Core.<*> (x Core..:? "remixSettings")
            Core.<*> (x Core..:? "audioNormalizationSettings")
            Core.<*> (x Core..:? "streamName")
            Core.<*> (x Core..: "audioSelectorName")
            Core.<*> (x Core..: "name")
      )

instance Core.Hashable AudioDescription

instance Core.NFData AudioDescription

instance Core.ToJSON AudioDescription where
  toJSON AudioDescription' {..} =
    Core.object
      ( Core.catMaybes
          [ ("languageCode" Core..=) Core.<$> languageCode,
            ("audioType" Core..=) Core.<$> audioType,
            ("codecSettings" Core..=) Core.<$> codecSettings,
            ("languageCodeControl" Core..=)
              Core.<$> languageCodeControl,
            ("audioTypeControl" Core..=)
              Core.<$> audioTypeControl,
            ("remixSettings" Core..=) Core.<$> remixSettings,
            ("audioNormalizationSettings" Core..=)
              Core.<$> audioNormalizationSettings,
            ("streamName" Core..=) Core.<$> streamName,
            Core.Just
              ("audioSelectorName" Core..= audioSelectorName),
            Core.Just ("name" Core..= name)
          ]
      )
