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
-- Module      : Amazonka.MediaConvert.Types.AudioDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AudioChannelTaggingSettings
import Amazonka.MediaConvert.Types.AudioCodecSettings
import Amazonka.MediaConvert.Types.AudioLanguageCodeControl
import Amazonka.MediaConvert.Types.AudioNormalizationSettings
import Amazonka.MediaConvert.Types.AudioTypeControl
import Amazonka.MediaConvert.Types.LanguageCode
import Amazonka.MediaConvert.Types.RemixSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings related to one audio tab on the MediaConvert console. In your
-- job JSON, an instance of AudioDescription is equivalent to one audio tab
-- in the console. Usually, one audio tab corresponds to one output audio
-- track. Depending on how you set up your input audio selectors and
-- whether you use audio selector groups, one audio tab can correspond to a
-- group of output audio tracks.
--
-- /See:/ 'newAudioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { -- | When you mimic a multi-channel audio layout with multiple mono-channel
    -- tracks, you can tag each channel layout manually. For example, you would
    -- tag the tracks that contain your left, right, and center audio with Left
    -- (L), Right (R), and Center (C), respectively. When you don\'t specify a
    -- value, MediaConvert labels your track as Center (C) by default. To use
    -- audio layout tagging, your output must be in a QuickTime (.mov)
    -- container; your audio codec must be AAC, WAV, or AIFF; and you must set
    -- up your audio track to have only one channel.
    audioChannelTaggingSettings :: Prelude.Maybe AudioChannelTaggingSettings,
    -- | Advanced audio normalization settings. Ignore these settings unless you
    -- need to comply with a loudness standard.
    audioNormalizationSettings :: Prelude.Maybe AudioNormalizationSettings,
    -- | Specifies which audio data to use from each input. In the simplest case,
    -- specify an \"Audio Selector\":#inputs-audio_selector by name based on
    -- its order within each input. For example if you specify \"Audio Selector
    -- 3\", then the third audio selector will be used from each input. If an
    -- input does not have an \"Audio Selector 3\", then the audio selector
    -- marked as \"default\" in that input will be used. If there is no audio
    -- selector marked as \"default\", silence will be inserted for the
    -- duration of that input. Alternatively, an \"Audio Selector
    -- Group\":#inputs-audio_selector_group name may be specified, with similar
    -- default\/silence behavior. If no audio_source_name is specified, then
    -- \"Audio Selector 1\" will be chosen automatically.
    audioSourceName :: Prelude.Maybe Prelude.Text,
    -- | Applies only if Follow Input Audio Type is unchecked (false). A number
    -- between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 =
    -- Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually
    -- Impaired Commentary, 4-255 = Reserved.
    audioType :: Prelude.Maybe Prelude.Natural,
    -- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type,
    -- then that value is passed through to the output. If the input contains
    -- no ISO 639 audio_type, the value in Audio Type is included in the
    -- output. Otherwise the value in Audio Type is included in the output.
    -- Note that this field and audioType are both ignored if
    -- audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
    audioTypeControl :: Prelude.Maybe AudioTypeControl,
    -- | Settings related to audio encoding. The settings in this group vary
    -- depending on the value that you choose for your audio codec.
    codecSettings :: Prelude.Maybe AudioCodecSettings,
    -- | Specify the language for this audio output track. The service puts this
    -- language code into your output audio track when you set Language code
    -- control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED).
    -- The service also uses your specified custom language code when you set
    -- Language code control (AudioLanguageCodeControl) to Follow input
    -- (FOLLOW_INPUT), but your input file doesn\'t specify a language code.
    -- For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For
    -- streaming outputs, you can also use any other code in the full RFC-5646
    -- specification. Streaming outputs are those that are in one of the
    -- following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth
    -- Streaming.
    customLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | Indicates the language of the audio output track. The ISO 639 language
    -- specified in the \'Language Code\' drop down will be used when \'Follow
    -- Input Language Code\' is not selected or when \'Follow Input Language
    -- Code\' is selected but there is no ISO 639 language code specified by
    -- the input.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Specify which source for language code takes precedence for this audio
    -- track. When you choose Follow input (FOLLOW_INPUT), the service uses the
    -- language code from the input track if it\'s present. If there\'s no
    -- languge code on the input track, the service uses the code that you
    -- specify in the setting Language code (languageCode or
    -- customLanguageCode). When you choose Use configured (USE_CONFIGURED),
    -- the service uses the language code that you specify.
    languageCodeControl :: Prelude.Maybe AudioLanguageCodeControl,
    -- | Advanced audio remixing settings.
    remixSettings :: Prelude.Maybe RemixSettings,
    -- | Specify a label for this output audio stream. For example, \"English\",
    -- \"Director commentary\", or \"track_2\". For streaming outputs,
    -- MediaConvert passes this information into destination manifests for
    -- display on the end-viewer\'s player device. For outputs in other output
    -- groups, the service ignores this setting.
    streamName :: Prelude.Maybe Prelude.Text
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
-- 'audioChannelTaggingSettings', 'audioDescription_audioChannelTaggingSettings' - When you mimic a multi-channel audio layout with multiple mono-channel
-- tracks, you can tag each channel layout manually. For example, you would
-- tag the tracks that contain your left, right, and center audio with Left
-- (L), Right (R), and Center (C), respectively. When you don\'t specify a
-- value, MediaConvert labels your track as Center (C) by default. To use
-- audio layout tagging, your output must be in a QuickTime (.mov)
-- container; your audio codec must be AAC, WAV, or AIFF; and you must set
-- up your audio track to have only one channel.
--
-- 'audioNormalizationSettings', 'audioDescription_audioNormalizationSettings' - Advanced audio normalization settings. Ignore these settings unless you
-- need to comply with a loudness standard.
--
-- 'audioSourceName', 'audioDescription_audioSourceName' - Specifies which audio data to use from each input. In the simplest case,
-- specify an \"Audio Selector\":#inputs-audio_selector by name based on
-- its order within each input. For example if you specify \"Audio Selector
-- 3\", then the third audio selector will be used from each input. If an
-- input does not have an \"Audio Selector 3\", then the audio selector
-- marked as \"default\" in that input will be used. If there is no audio
-- selector marked as \"default\", silence will be inserted for the
-- duration of that input. Alternatively, an \"Audio Selector
-- Group\":#inputs-audio_selector_group name may be specified, with similar
-- default\/silence behavior. If no audio_source_name is specified, then
-- \"Audio Selector 1\" will be chosen automatically.
--
-- 'audioType', 'audioDescription_audioType' - Applies only if Follow Input Audio Type is unchecked (false). A number
-- between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 =
-- Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually
-- Impaired Commentary, 4-255 = Reserved.
--
-- 'audioTypeControl', 'audioDescription_audioTypeControl' - When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type,
-- then that value is passed through to the output. If the input contains
-- no ISO 639 audio_type, the value in Audio Type is included in the
-- output. Otherwise the value in Audio Type is included in the output.
-- Note that this field and audioType are both ignored if
-- audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
--
-- 'codecSettings', 'audioDescription_codecSettings' - Settings related to audio encoding. The settings in this group vary
-- depending on the value that you choose for your audio codec.
--
-- 'customLanguageCode', 'audioDescription_customLanguageCode' - Specify the language for this audio output track. The service puts this
-- language code into your output audio track when you set Language code
-- control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED).
-- The service also uses your specified custom language code when you set
-- Language code control (AudioLanguageCodeControl) to Follow input
-- (FOLLOW_INPUT), but your input file doesn\'t specify a language code.
-- For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For
-- streaming outputs, you can also use any other code in the full RFC-5646
-- specification. Streaming outputs are those that are in one of the
-- following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth
-- Streaming.
--
-- 'languageCode', 'audioDescription_languageCode' - Indicates the language of the audio output track. The ISO 639 language
-- specified in the \'Language Code\' drop down will be used when \'Follow
-- Input Language Code\' is not selected or when \'Follow Input Language
-- Code\' is selected but there is no ISO 639 language code specified by
-- the input.
--
-- 'languageCodeControl', 'audioDescription_languageCodeControl' - Specify which source for language code takes precedence for this audio
-- track. When you choose Follow input (FOLLOW_INPUT), the service uses the
-- language code from the input track if it\'s present. If there\'s no
-- languge code on the input track, the service uses the code that you
-- specify in the setting Language code (languageCode or
-- customLanguageCode). When you choose Use configured (USE_CONFIGURED),
-- the service uses the language code that you specify.
--
-- 'remixSettings', 'audioDescription_remixSettings' - Advanced audio remixing settings.
--
-- 'streamName', 'audioDescription_streamName' - Specify a label for this output audio stream. For example, \"English\",
-- \"Director commentary\", or \"track_2\". For streaming outputs,
-- MediaConvert passes this information into destination manifests for
-- display on the end-viewer\'s player device. For outputs in other output
-- groups, the service ignores this setting.
newAudioDescription ::
  AudioDescription
newAudioDescription =
  AudioDescription'
    { audioChannelTaggingSettings =
        Prelude.Nothing,
      audioNormalizationSettings = Prelude.Nothing,
      audioSourceName = Prelude.Nothing,
      audioType = Prelude.Nothing,
      audioTypeControl = Prelude.Nothing,
      codecSettings = Prelude.Nothing,
      customLanguageCode = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      languageCodeControl = Prelude.Nothing,
      remixSettings = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | When you mimic a multi-channel audio layout with multiple mono-channel
-- tracks, you can tag each channel layout manually. For example, you would
-- tag the tracks that contain your left, right, and center audio with Left
-- (L), Right (R), and Center (C), respectively. When you don\'t specify a
-- value, MediaConvert labels your track as Center (C) by default. To use
-- audio layout tagging, your output must be in a QuickTime (.mov)
-- container; your audio codec must be AAC, WAV, or AIFF; and you must set
-- up your audio track to have only one channel.
audioDescription_audioChannelTaggingSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioChannelTaggingSettings)
audioDescription_audioChannelTaggingSettings = Lens.lens (\AudioDescription' {audioChannelTaggingSettings} -> audioChannelTaggingSettings) (\s@AudioDescription' {} a -> s {audioChannelTaggingSettings = a} :: AudioDescription)

-- | Advanced audio normalization settings. Ignore these settings unless you
-- need to comply with a loudness standard.
audioDescription_audioNormalizationSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioNormalizationSettings)
audioDescription_audioNormalizationSettings = Lens.lens (\AudioDescription' {audioNormalizationSettings} -> audioNormalizationSettings) (\s@AudioDescription' {} a -> s {audioNormalizationSettings = a} :: AudioDescription)

-- | Specifies which audio data to use from each input. In the simplest case,
-- specify an \"Audio Selector\":#inputs-audio_selector by name based on
-- its order within each input. For example if you specify \"Audio Selector
-- 3\", then the third audio selector will be used from each input. If an
-- input does not have an \"Audio Selector 3\", then the audio selector
-- marked as \"default\" in that input will be used. If there is no audio
-- selector marked as \"default\", silence will be inserted for the
-- duration of that input. Alternatively, an \"Audio Selector
-- Group\":#inputs-audio_selector_group name may be specified, with similar
-- default\/silence behavior. If no audio_source_name is specified, then
-- \"Audio Selector 1\" will be chosen automatically.
audioDescription_audioSourceName :: Lens.Lens' AudioDescription (Prelude.Maybe Prelude.Text)
audioDescription_audioSourceName = Lens.lens (\AudioDescription' {audioSourceName} -> audioSourceName) (\s@AudioDescription' {} a -> s {audioSourceName = a} :: AudioDescription)

-- | Applies only if Follow Input Audio Type is unchecked (false). A number
-- between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 =
-- Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually
-- Impaired Commentary, 4-255 = Reserved.
audioDescription_audioType :: Lens.Lens' AudioDescription (Prelude.Maybe Prelude.Natural)
audioDescription_audioType = Lens.lens (\AudioDescription' {audioType} -> audioType) (\s@AudioDescription' {} a -> s {audioType = a} :: AudioDescription)

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type,
-- then that value is passed through to the output. If the input contains
-- no ISO 639 audio_type, the value in Audio Type is included in the
-- output. Otherwise the value in Audio Type is included in the output.
-- Note that this field and audioType are both ignored if
-- audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
audioDescription_audioTypeControl :: Lens.Lens' AudioDescription (Prelude.Maybe AudioTypeControl)
audioDescription_audioTypeControl = Lens.lens (\AudioDescription' {audioTypeControl} -> audioTypeControl) (\s@AudioDescription' {} a -> s {audioTypeControl = a} :: AudioDescription)

-- | Settings related to audio encoding. The settings in this group vary
-- depending on the value that you choose for your audio codec.
audioDescription_codecSettings :: Lens.Lens' AudioDescription (Prelude.Maybe AudioCodecSettings)
audioDescription_codecSettings = Lens.lens (\AudioDescription' {codecSettings} -> codecSettings) (\s@AudioDescription' {} a -> s {codecSettings = a} :: AudioDescription)

-- | Specify the language for this audio output track. The service puts this
-- language code into your output audio track when you set Language code
-- control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED).
-- The service also uses your specified custom language code when you set
-- Language code control (AudioLanguageCodeControl) to Follow input
-- (FOLLOW_INPUT), but your input file doesn\'t specify a language code.
-- For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For
-- streaming outputs, you can also use any other code in the full RFC-5646
-- specification. Streaming outputs are those that are in one of the
-- following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth
-- Streaming.
audioDescription_customLanguageCode :: Lens.Lens' AudioDescription (Prelude.Maybe Prelude.Text)
audioDescription_customLanguageCode = Lens.lens (\AudioDescription' {customLanguageCode} -> customLanguageCode) (\s@AudioDescription' {} a -> s {customLanguageCode = a} :: AudioDescription)

-- | Indicates the language of the audio output track. The ISO 639 language
-- specified in the \'Language Code\' drop down will be used when \'Follow
-- Input Language Code\' is not selected or when \'Follow Input Language
-- Code\' is selected but there is no ISO 639 language code specified by
-- the input.
audioDescription_languageCode :: Lens.Lens' AudioDescription (Prelude.Maybe LanguageCode)
audioDescription_languageCode = Lens.lens (\AudioDescription' {languageCode} -> languageCode) (\s@AudioDescription' {} a -> s {languageCode = a} :: AudioDescription)

-- | Specify which source for language code takes precedence for this audio
-- track. When you choose Follow input (FOLLOW_INPUT), the service uses the
-- language code from the input track if it\'s present. If there\'s no
-- languge code on the input track, the service uses the code that you
-- specify in the setting Language code (languageCode or
-- customLanguageCode). When you choose Use configured (USE_CONFIGURED),
-- the service uses the language code that you specify.
audioDescription_languageCodeControl :: Lens.Lens' AudioDescription (Prelude.Maybe AudioLanguageCodeControl)
audioDescription_languageCodeControl = Lens.lens (\AudioDescription' {languageCodeControl} -> languageCodeControl) (\s@AudioDescription' {} a -> s {languageCodeControl = a} :: AudioDescription)

-- | Advanced audio remixing settings.
audioDescription_remixSettings :: Lens.Lens' AudioDescription (Prelude.Maybe RemixSettings)
audioDescription_remixSettings = Lens.lens (\AudioDescription' {remixSettings} -> remixSettings) (\s@AudioDescription' {} a -> s {remixSettings = a} :: AudioDescription)

-- | Specify a label for this output audio stream. For example, \"English\",
-- \"Director commentary\", or \"track_2\". For streaming outputs,
-- MediaConvert passes this information into destination manifests for
-- display on the end-viewer\'s player device. For outputs in other output
-- groups, the service ignores this setting.
audioDescription_streamName :: Lens.Lens' AudioDescription (Prelude.Maybe Prelude.Text)
audioDescription_streamName = Lens.lens (\AudioDescription' {streamName} -> streamName) (\s@AudioDescription' {} a -> s {streamName = a} :: AudioDescription)

instance Data.FromJSON AudioDescription where
  parseJSON =
    Data.withObject
      "AudioDescription"
      ( \x ->
          AudioDescription'
            Prelude.<$> (x Data..:? "audioChannelTaggingSettings")
            Prelude.<*> (x Data..:? "audioNormalizationSettings")
            Prelude.<*> (x Data..:? "audioSourceName")
            Prelude.<*> (x Data..:? "audioType")
            Prelude.<*> (x Data..:? "audioTypeControl")
            Prelude.<*> (x Data..:? "codecSettings")
            Prelude.<*> (x Data..:? "customLanguageCode")
            Prelude.<*> (x Data..:? "languageCode")
            Prelude.<*> (x Data..:? "languageCodeControl")
            Prelude.<*> (x Data..:? "remixSettings")
            Prelude.<*> (x Data..:? "streamName")
      )

instance Prelude.Hashable AudioDescription where
  hashWithSalt _salt AudioDescription' {..} =
    _salt
      `Prelude.hashWithSalt` audioChannelTaggingSettings
      `Prelude.hashWithSalt` audioNormalizationSettings
      `Prelude.hashWithSalt` audioSourceName
      `Prelude.hashWithSalt` audioType
      `Prelude.hashWithSalt` audioTypeControl
      `Prelude.hashWithSalt` codecSettings
      `Prelude.hashWithSalt` customLanguageCode
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` languageCodeControl
      `Prelude.hashWithSalt` remixSettings
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData AudioDescription where
  rnf AudioDescription' {..} =
    Prelude.rnf audioChannelTaggingSettings `Prelude.seq`
      Prelude.rnf audioNormalizationSettings `Prelude.seq`
        Prelude.rnf audioSourceName `Prelude.seq`
          Prelude.rnf audioType `Prelude.seq`
            Prelude.rnf audioTypeControl `Prelude.seq`
              Prelude.rnf codecSettings `Prelude.seq`
                Prelude.rnf customLanguageCode `Prelude.seq`
                  Prelude.rnf languageCode `Prelude.seq`
                    Prelude.rnf languageCodeControl `Prelude.seq`
                      Prelude.rnf remixSettings `Prelude.seq`
                        Prelude.rnf streamName

instance Data.ToJSON AudioDescription where
  toJSON AudioDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioChannelTaggingSettings" Data..=)
              Prelude.<$> audioChannelTaggingSettings,
            ("audioNormalizationSettings" Data..=)
              Prelude.<$> audioNormalizationSettings,
            ("audioSourceName" Data..=)
              Prelude.<$> audioSourceName,
            ("audioType" Data..=) Prelude.<$> audioType,
            ("audioTypeControl" Data..=)
              Prelude.<$> audioTypeControl,
            ("codecSettings" Data..=) Prelude.<$> codecSettings,
            ("customLanguageCode" Data..=)
              Prelude.<$> customLanguageCode,
            ("languageCode" Data..=) Prelude.<$> languageCode,
            ("languageCodeControl" Data..=)
              Prelude.<$> languageCodeControl,
            ("remixSettings" Data..=) Prelude.<$> remixSettings,
            ("streamName" Data..=) Prelude.<$> streamName
          ]
      )
