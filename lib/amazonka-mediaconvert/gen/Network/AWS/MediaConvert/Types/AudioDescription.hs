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
    adAudioSourceName,
    adCustomLanguageCode,
    adLanguageCode,
    adAudioChannelTaggingSettings,
    adAudioType,
    adAudioNormalizationSettings,
    adLanguageCodeControl,
    adCodecSettings,
    adStreamName,
    adRemixSettings,
    adAudioTypeControl,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
import Network.AWS.MediaConvert.Types.AudioCodecSettings
import Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
import Network.AWS.MediaConvert.Types.AudioNormalizationSettings
import Network.AWS.MediaConvert.Types.AudioTypeControl
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.MediaConvert.Types.RemixSettings
import qualified Network.AWS.Prelude as Lude

-- | Description of audio output
--
-- /See:/ 'mkAudioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { audioSourceName ::
      Lude.Maybe Lude.Text,
    customLanguageCode :: Lude.Maybe Lude.Text,
    languageCode :: Lude.Maybe LanguageCode,
    audioChannelTaggingSettings ::
      Lude.Maybe AudioChannelTaggingSettings,
    audioType :: Lude.Maybe Lude.Natural,
    audioNormalizationSettings ::
      Lude.Maybe AudioNormalizationSettings,
    languageCodeControl ::
      Lude.Maybe AudioLanguageCodeControl,
    codecSettings :: Lude.Maybe AudioCodecSettings,
    streamName :: Lude.Maybe Lude.Text,
    remixSettings :: Lude.Maybe RemixSettings,
    audioTypeControl :: Lude.Maybe AudioTypeControl
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioDescription' with the minimum fields required to make a request.
--
-- * 'audioChannelTaggingSettings' - When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
-- * 'audioNormalizationSettings' - Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
-- * 'audioSourceName' - Specifies which audio data to use from each input. In the simplest case, specify an "Audio Selector":#inputs-audio_selector by name based on its order within each input. For example if you specify "Audio Selector 3", then the third audio selector will be used from each input. If an input does not have an "Audio Selector 3", then the audio selector marked as "default" in that input will be used. If there is no audio selector marked as "default", silence will be inserted for the duration of that input. Alternatively, an "Audio Selector Group":#inputs-audio_selector_group name may be specified, with similar default/silence behavior. If no audio_source_name is specified, then "Audio Selector 1" will be chosen automatically.
-- * 'audioType' - Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
-- * 'audioTypeControl' - When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
-- * 'codecSettings' - Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
-- * 'customLanguageCode' - Specify the language for this audio output track. The service puts this language code into your output audio track when you set Language code control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED). The service also uses your specified custom language code when you set Language code control (AudioLanguageCodeControl) to Follow input (FOLLOW_INPUT), but your input file doesn't specify a language code. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
-- * 'languageCode' - Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
-- * 'languageCodeControl' - Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
-- * 'remixSettings' - Advanced audio remixing settings.
-- * 'streamName' - Specify a label for this output audio stream. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
mkAudioDescription ::
  AudioDescription
mkAudioDescription =
  AudioDescription'
    { audioSourceName = Lude.Nothing,
      customLanguageCode = Lude.Nothing,
      languageCode = Lude.Nothing,
      audioChannelTaggingSettings = Lude.Nothing,
      audioType = Lude.Nothing,
      audioNormalizationSettings = Lude.Nothing,
      languageCodeControl = Lude.Nothing,
      codecSettings = Lude.Nothing,
      streamName = Lude.Nothing,
      remixSettings = Lude.Nothing,
      audioTypeControl = Lude.Nothing
    }

-- | Specifies which audio data to use from each input. In the simplest case, specify an "Audio Selector":#inputs-audio_selector by name based on its order within each input. For example if you specify "Audio Selector 3", then the third audio selector will be used from each input. If an input does not have an "Audio Selector 3", then the audio selector marked as "default" in that input will be used. If there is no audio selector marked as "default", silence will be inserted for the duration of that input. Alternatively, an "Audio Selector Group":#inputs-audio_selector_group name may be specified, with similar default/silence behavior. If no audio_source_name is specified, then "Audio Selector 1" will be chosen automatically.
--
-- /Note:/ Consider using 'audioSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioSourceName :: Lens.Lens' AudioDescription (Lude.Maybe Lude.Text)
adAudioSourceName = Lens.lens (audioSourceName :: AudioDescription -> Lude.Maybe Lude.Text) (\s a -> s {audioSourceName = a} :: AudioDescription)
{-# DEPRECATED adAudioSourceName "Use generic-lens or generic-optics with 'audioSourceName' instead." #-}

-- | Specify the language for this audio output track. The service puts this language code into your output audio track when you set Language code control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED). The service also uses your specified custom language code when you set Language code control (AudioLanguageCodeControl) to Follow input (FOLLOW_INPUT), but your input file doesn't specify a language code. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCustomLanguageCode :: Lens.Lens' AudioDescription (Lude.Maybe Lude.Text)
adCustomLanguageCode = Lens.lens (customLanguageCode :: AudioDescription -> Lude.Maybe Lude.Text) (\s a -> s {customLanguageCode = a} :: AudioDescription)
{-# DEPRECATED adCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLanguageCode :: Lens.Lens' AudioDescription (Lude.Maybe LanguageCode)
adLanguageCode = Lens.lens (languageCode :: AudioDescription -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: AudioDescription)
{-# DEPRECATED adLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
--
-- /Note:/ Consider using 'audioChannelTaggingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioChannelTaggingSettings :: Lens.Lens' AudioDescription (Lude.Maybe AudioChannelTaggingSettings)
adAudioChannelTaggingSettings = Lens.lens (audioChannelTaggingSettings :: AudioDescription -> Lude.Maybe AudioChannelTaggingSettings) (\s a -> s {audioChannelTaggingSettings = a} :: AudioDescription)
{-# DEPRECATED adAudioChannelTaggingSettings "Use generic-lens or generic-optics with 'audioChannelTaggingSettings' instead." #-}

-- | Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
--
-- /Note:/ Consider using 'audioType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioType :: Lens.Lens' AudioDescription (Lude.Maybe Lude.Natural)
adAudioType = Lens.lens (audioType :: AudioDescription -> Lude.Maybe Lude.Natural) (\s a -> s {audioType = a} :: AudioDescription)
{-# DEPRECATED adAudioType "Use generic-lens or generic-optics with 'audioType' instead." #-}

-- | Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
--
-- /Note:/ Consider using 'audioNormalizationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioNormalizationSettings :: Lens.Lens' AudioDescription (Lude.Maybe AudioNormalizationSettings)
adAudioNormalizationSettings = Lens.lens (audioNormalizationSettings :: AudioDescription -> Lude.Maybe AudioNormalizationSettings) (\s a -> s {audioNormalizationSettings = a} :: AudioDescription)
{-# DEPRECATED adAudioNormalizationSettings "Use generic-lens or generic-optics with 'audioNormalizationSettings' instead." #-}

-- | Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
--
-- /Note:/ Consider using 'languageCodeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLanguageCodeControl :: Lens.Lens' AudioDescription (Lude.Maybe AudioLanguageCodeControl)
adLanguageCodeControl = Lens.lens (languageCodeControl :: AudioDescription -> Lude.Maybe AudioLanguageCodeControl) (\s a -> s {languageCodeControl = a} :: AudioDescription)
{-# DEPRECATED adLanguageCodeControl "Use generic-lens or generic-optics with 'languageCodeControl' instead." #-}

-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
--
-- /Note:/ Consider using 'codecSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCodecSettings :: Lens.Lens' AudioDescription (Lude.Maybe AudioCodecSettings)
adCodecSettings = Lens.lens (codecSettings :: AudioDescription -> Lude.Maybe AudioCodecSettings) (\s a -> s {codecSettings = a} :: AudioDescription)
{-# DEPRECATED adCodecSettings "Use generic-lens or generic-optics with 'codecSettings' instead." #-}

-- | Specify a label for this output audio stream. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStreamName :: Lens.Lens' AudioDescription (Lude.Maybe Lude.Text)
adStreamName = Lens.lens (streamName :: AudioDescription -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: AudioDescription)
{-# DEPRECATED adStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | Advanced audio remixing settings.
--
-- /Note:/ Consider using 'remixSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRemixSettings :: Lens.Lens' AudioDescription (Lude.Maybe RemixSettings)
adRemixSettings = Lens.lens (remixSettings :: AudioDescription -> Lude.Maybe RemixSettings) (\s a -> s {remixSettings = a} :: AudioDescription)
{-# DEPRECATED adRemixSettings "Use generic-lens or generic-optics with 'remixSettings' instead." #-}

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
--
-- /Note:/ Consider using 'audioTypeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioTypeControl :: Lens.Lens' AudioDescription (Lude.Maybe AudioTypeControl)
adAudioTypeControl = Lens.lens (audioTypeControl :: AudioDescription -> Lude.Maybe AudioTypeControl) (\s a -> s {audioTypeControl = a} :: AudioDescription)
{-# DEPRECATED adAudioTypeControl "Use generic-lens or generic-optics with 'audioTypeControl' instead." #-}

instance Lude.FromJSON AudioDescription where
  parseJSON =
    Lude.withObject
      "AudioDescription"
      ( \x ->
          AudioDescription'
            Lude.<$> (x Lude..:? "audioSourceName")
            Lude.<*> (x Lude..:? "customLanguageCode")
            Lude.<*> (x Lude..:? "languageCode")
            Lude.<*> (x Lude..:? "audioChannelTaggingSettings")
            Lude.<*> (x Lude..:? "audioType")
            Lude.<*> (x Lude..:? "audioNormalizationSettings")
            Lude.<*> (x Lude..:? "languageCodeControl")
            Lude.<*> (x Lude..:? "codecSettings")
            Lude.<*> (x Lude..:? "streamName")
            Lude.<*> (x Lude..:? "remixSettings")
            Lude.<*> (x Lude..:? "audioTypeControl")
      )

instance Lude.ToJSON AudioDescription where
  toJSON AudioDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("audioSourceName" Lude..=) Lude.<$> audioSourceName,
            ("customLanguageCode" Lude..=) Lude.<$> customLanguageCode,
            ("languageCode" Lude..=) Lude.<$> languageCode,
            ("audioChannelTaggingSettings" Lude..=)
              Lude.<$> audioChannelTaggingSettings,
            ("audioType" Lude..=) Lude.<$> audioType,
            ("audioNormalizationSettings" Lude..=)
              Lude.<$> audioNormalizationSettings,
            ("languageCodeControl" Lude..=) Lude.<$> languageCodeControl,
            ("codecSettings" Lude..=) Lude.<$> codecSettings,
            ("streamName" Lude..=) Lude.<$> streamName,
            ("remixSettings" Lude..=) Lude.<$> remixSettings,
            ("audioTypeControl" Lude..=) Lude.<$> audioTypeControl
          ]
      )
