{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioDescription
  ( AudioDescription (..),

    -- * Smart constructor
    mkAudioDescription,

    -- * Lenses
    adAudioChannelTaggingSettings,
    adAudioNormalizationSettings,
    adAudioSourceName,
    adAudioType,
    adAudioTypeControl,
    adCodecSettings,
    adCustomLanguageCode,
    adLanguageCode,
    adLanguageCodeControl,
    adRemixSettings,
    adStreamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings as Types
import qualified Network.AWS.MediaConvert.Types.AudioCodecSettings as Types
import qualified Network.AWS.MediaConvert.Types.AudioLanguageCodeControl as Types
import qualified Network.AWS.MediaConvert.Types.AudioNormalizationSettings as Types
import qualified Network.AWS.MediaConvert.Types.AudioTypeControl as Types
import qualified Network.AWS.MediaConvert.Types.LanguageCode as Types
import qualified Network.AWS.MediaConvert.Types.RemixSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Description of audio output
--
-- /See:/ 'mkAudioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { -- | When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
    audioChannelTaggingSettings :: Core.Maybe Types.AudioChannelTaggingSettings,
    -- | Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
    audioNormalizationSettings :: Core.Maybe Types.AudioNormalizationSettings,
    -- | Specifies which audio data to use from each input. In the simplest case, specify an "Audio Selector":#inputs-audio_selector by name based on its order within each input. For example if you specify "Audio Selector 3", then the third audio selector will be used from each input. If an input does not have an "Audio Selector 3", then the audio selector marked as "default" in that input will be used. If there is no audio selector marked as "default", silence will be inserted for the duration of that input. Alternatively, an "Audio Selector Group":#inputs-audio_selector_group name may be specified, with similar default/silence behavior. If no audio_source_name is specified, then "Audio Selector 1" will be chosen automatically.
    audioSourceName :: Core.Maybe Core.Text,
    -- | Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
    audioType :: Core.Maybe Core.Natural,
    -- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
    audioTypeControl :: Core.Maybe Types.AudioTypeControl,
    -- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
    codecSettings :: Core.Maybe Types.AudioCodecSettings,
    -- | Specify the language for this audio output track. The service puts this language code into your output audio track when you set Language code control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED). The service also uses your specified custom language code when you set Language code control (AudioLanguageCodeControl) to Follow input (FOLLOW_INPUT), but your input file doesn't specify a language code. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
    customLanguageCode :: Core.Maybe Core.Text,
    -- | Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
    languageCodeControl :: Core.Maybe Types.AudioLanguageCodeControl,
    -- | Advanced audio remixing settings.
    remixSettings :: Core.Maybe Types.RemixSettings,
    -- | Specify a label for this output audio stream. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
    streamName :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioDescription' value with any optional fields omitted.
mkAudioDescription ::
  AudioDescription
mkAudioDescription =
  AudioDescription'
    { audioChannelTaggingSettings = Core.Nothing,
      audioNormalizationSettings = Core.Nothing,
      audioSourceName = Core.Nothing,
      audioType = Core.Nothing,
      audioTypeControl = Core.Nothing,
      codecSettings = Core.Nothing,
      customLanguageCode = Core.Nothing,
      languageCode = Core.Nothing,
      languageCodeControl = Core.Nothing,
      remixSettings = Core.Nothing,
      streamName = Core.Nothing
    }

-- | When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
--
-- /Note:/ Consider using 'audioChannelTaggingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioChannelTaggingSettings :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioChannelTaggingSettings)
adAudioChannelTaggingSettings = Lens.field @"audioChannelTaggingSettings"
{-# DEPRECATED adAudioChannelTaggingSettings "Use generic-lens or generic-optics with 'audioChannelTaggingSettings' instead." #-}

-- | Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
--
-- /Note:/ Consider using 'audioNormalizationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioNormalizationSettings :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioNormalizationSettings)
adAudioNormalizationSettings = Lens.field @"audioNormalizationSettings"
{-# DEPRECATED adAudioNormalizationSettings "Use generic-lens or generic-optics with 'audioNormalizationSettings' instead." #-}

-- | Specifies which audio data to use from each input. In the simplest case, specify an "Audio Selector":#inputs-audio_selector by name based on its order within each input. For example if you specify "Audio Selector 3", then the third audio selector will be used from each input. If an input does not have an "Audio Selector 3", then the audio selector marked as "default" in that input will be used. If there is no audio selector marked as "default", silence will be inserted for the duration of that input. Alternatively, an "Audio Selector Group":#inputs-audio_selector_group name may be specified, with similar default/silence behavior. If no audio_source_name is specified, then "Audio Selector 1" will be chosen automatically.
--
-- /Note:/ Consider using 'audioSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioSourceName :: Lens.Lens' AudioDescription (Core.Maybe Core.Text)
adAudioSourceName = Lens.field @"audioSourceName"
{-# DEPRECATED adAudioSourceName "Use generic-lens or generic-optics with 'audioSourceName' instead." #-}

-- | Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
--
-- /Note:/ Consider using 'audioType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioType :: Lens.Lens' AudioDescription (Core.Maybe Core.Natural)
adAudioType = Lens.field @"audioType"
{-# DEPRECATED adAudioType "Use generic-lens or generic-optics with 'audioType' instead." #-}

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
--
-- /Note:/ Consider using 'audioTypeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioTypeControl :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioTypeControl)
adAudioTypeControl = Lens.field @"audioTypeControl"
{-# DEPRECATED adAudioTypeControl "Use generic-lens or generic-optics with 'audioTypeControl' instead." #-}

-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
--
-- /Note:/ Consider using 'codecSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCodecSettings :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioCodecSettings)
adCodecSettings = Lens.field @"codecSettings"
{-# DEPRECATED adCodecSettings "Use generic-lens or generic-optics with 'codecSettings' instead." #-}

-- | Specify the language for this audio output track. The service puts this language code into your output audio track when you set Language code control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED). The service also uses your specified custom language code when you set Language code control (AudioLanguageCodeControl) to Follow input (FOLLOW_INPUT), but your input file doesn't specify a language code. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCustomLanguageCode :: Lens.Lens' AudioDescription (Core.Maybe Core.Text)
adCustomLanguageCode = Lens.field @"customLanguageCode"
{-# DEPRECATED adCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLanguageCode :: Lens.Lens' AudioDescription (Core.Maybe Types.LanguageCode)
adLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED adLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
--
-- /Note:/ Consider using 'languageCodeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLanguageCodeControl :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioLanguageCodeControl)
adLanguageCodeControl = Lens.field @"languageCodeControl"
{-# DEPRECATED adLanguageCodeControl "Use generic-lens or generic-optics with 'languageCodeControl' instead." #-}

-- | Advanced audio remixing settings.
--
-- /Note:/ Consider using 'remixSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRemixSettings :: Lens.Lens' AudioDescription (Core.Maybe Types.RemixSettings)
adRemixSettings = Lens.field @"remixSettings"
{-# DEPRECATED adRemixSettings "Use generic-lens or generic-optics with 'remixSettings' instead." #-}

-- | Specify a label for this output audio stream. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStreamName :: Lens.Lens' AudioDescription (Core.Maybe Core.Text)
adStreamName = Lens.field @"streamName"
{-# DEPRECATED adStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON AudioDescription where
  toJSON AudioDescription {..} =
    Core.object
      ( Core.catMaybes
          [ ("audioChannelTaggingSettings" Core..=)
              Core.<$> audioChannelTaggingSettings,
            ("audioNormalizationSettings" Core..=)
              Core.<$> audioNormalizationSettings,
            ("audioSourceName" Core..=) Core.<$> audioSourceName,
            ("audioType" Core..=) Core.<$> audioType,
            ("audioTypeControl" Core..=) Core.<$> audioTypeControl,
            ("codecSettings" Core..=) Core.<$> codecSettings,
            ("customLanguageCode" Core..=) Core.<$> customLanguageCode,
            ("languageCode" Core..=) Core.<$> languageCode,
            ("languageCodeControl" Core..=) Core.<$> languageCodeControl,
            ("remixSettings" Core..=) Core.<$> remixSettings,
            ("streamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.FromJSON AudioDescription where
  parseJSON =
    Core.withObject "AudioDescription" Core.$
      \x ->
        AudioDescription'
          Core.<$> (x Core..:? "audioChannelTaggingSettings")
          Core.<*> (x Core..:? "audioNormalizationSettings")
          Core.<*> (x Core..:? "audioSourceName")
          Core.<*> (x Core..:? "audioType")
          Core.<*> (x Core..:? "audioTypeControl")
          Core.<*> (x Core..:? "codecSettings")
          Core.<*> (x Core..:? "customLanguageCode")
          Core.<*> (x Core..:? "languageCode")
          Core.<*> (x Core..:? "languageCodeControl")
          Core.<*> (x Core..:? "remixSettings")
          Core.<*> (x Core..:? "streamName")
