{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioDescription
  ( AudioDescription (..),

    -- * Smart constructor
    mkAudioDescription,

    -- * Lenses
    adLanguageCode,
    adAudioType,
    adAudioNormalizationSettings,
    adLanguageCodeControl,
    adCodecSettings,
    adStreamName,
    adRemixSettings,
    adAudioTypeControl,
    adAudioSelectorName,
    adName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioCodecSettings
import Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl
import Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
import Network.AWS.MediaLive.Types.AudioNormalizationSettings
import Network.AWS.MediaLive.Types.AudioType
import Network.AWS.MediaLive.Types.RemixSettings
import qualified Network.AWS.Prelude as Lude

-- | Audio Description
--
-- /See:/ 'mkAudioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { languageCode ::
      Lude.Maybe Lude.Text,
    audioType :: Lude.Maybe AudioType,
    audioNormalizationSettings ::
      Lude.Maybe AudioNormalizationSettings,
    languageCodeControl ::
      Lude.Maybe AudioDescriptionLanguageCodeControl,
    codecSettings :: Lude.Maybe AudioCodecSettings,
    streamName :: Lude.Maybe Lude.Text,
    remixSettings :: Lude.Maybe RemixSettings,
    audioTypeControl ::
      Lude.Maybe AudioDescriptionAudioTypeControl,
    audioSelectorName :: Lude.Text,
    name :: Lude.Text
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
-- * 'audioNormalizationSettings' - Advanced audio normalization settings.
-- * 'audioSelectorName' - The name of the AudioSelector used as the source for this AudioDescription.
-- * 'audioType' - Applies only if audioTypeControl is useConfigured. The values for audioType are defined in ISO-IEC 13818-1.
-- * 'audioTypeControl' - Determines how audio type is determined.
--
--   followInput: If the input contains an ISO 639 audioType, then that value is passed through to the output. If the input contains no ISO 639 audioType, the value in Audio Type is included in the output.
--   useConfigured: The value in Audio Type is included in the output.
-- Note that this field and audioType are both ignored if inputType is broadcasterMixedAd.
-- * 'codecSettings' - Audio codec settings.
-- * 'languageCode' - RFC 5646 language code representing the language of the audio output track. Only used if languageControlMode is useConfigured, or there is no ISO 639 language code specified in the input.
-- * 'languageCodeControl' - Choosing followInput will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The languageCode will be used when useConfigured is set, or when followInput is selected but there is no ISO 639 language code specified by the input.
-- * 'name' - The name of this AudioDescription. Outputs will use this name to uniquely identify this AudioDescription.  Description names should be unique within this Live Event.
-- * 'remixSettings' - Settings that control how input audio channels are remixed into the output audio channels.
-- * 'streamName' - Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary).
mkAudioDescription ::
  -- | 'audioSelectorName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  AudioDescription
mkAudioDescription pAudioSelectorName_ pName_ =
  AudioDescription'
    { languageCode = Lude.Nothing,
      audioType = Lude.Nothing,
      audioNormalizationSettings = Lude.Nothing,
      languageCodeControl = Lude.Nothing,
      codecSettings = Lude.Nothing,
      streamName = Lude.Nothing,
      remixSettings = Lude.Nothing,
      audioTypeControl = Lude.Nothing,
      audioSelectorName = pAudioSelectorName_,
      name = pName_
    }

-- | RFC 5646 language code representing the language of the audio output track. Only used if languageControlMode is useConfigured, or there is no ISO 639 language code specified in the input.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLanguageCode :: Lens.Lens' AudioDescription (Lude.Maybe Lude.Text)
adLanguageCode = Lens.lens (languageCode :: AudioDescription -> Lude.Maybe Lude.Text) (\s a -> s {languageCode = a} :: AudioDescription)
{-# DEPRECATED adLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Applies only if audioTypeControl is useConfigured. The values for audioType are defined in ISO-IEC 13818-1.
--
-- /Note:/ Consider using 'audioType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioType :: Lens.Lens' AudioDescription (Lude.Maybe AudioType)
adAudioType = Lens.lens (audioType :: AudioDescription -> Lude.Maybe AudioType) (\s a -> s {audioType = a} :: AudioDescription)
{-# DEPRECATED adAudioType "Use generic-lens or generic-optics with 'audioType' instead." #-}

-- | Advanced audio normalization settings.
--
-- /Note:/ Consider using 'audioNormalizationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioNormalizationSettings :: Lens.Lens' AudioDescription (Lude.Maybe AudioNormalizationSettings)
adAudioNormalizationSettings = Lens.lens (audioNormalizationSettings :: AudioDescription -> Lude.Maybe AudioNormalizationSettings) (\s a -> s {audioNormalizationSettings = a} :: AudioDescription)
{-# DEPRECATED adAudioNormalizationSettings "Use generic-lens or generic-optics with 'audioNormalizationSettings' instead." #-}

-- | Choosing followInput will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The languageCode will be used when useConfigured is set, or when followInput is selected but there is no ISO 639 language code specified by the input.
--
-- /Note:/ Consider using 'languageCodeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLanguageCodeControl :: Lens.Lens' AudioDescription (Lude.Maybe AudioDescriptionLanguageCodeControl)
adLanguageCodeControl = Lens.lens (languageCodeControl :: AudioDescription -> Lude.Maybe AudioDescriptionLanguageCodeControl) (\s a -> s {languageCodeControl = a} :: AudioDescription)
{-# DEPRECATED adLanguageCodeControl "Use generic-lens or generic-optics with 'languageCodeControl' instead." #-}

-- | Audio codec settings.
--
-- /Note:/ Consider using 'codecSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCodecSettings :: Lens.Lens' AudioDescription (Lude.Maybe AudioCodecSettings)
adCodecSettings = Lens.lens (codecSettings :: AudioDescription -> Lude.Maybe AudioCodecSettings) (\s a -> s {codecSettings = a} :: AudioDescription)
{-# DEPRECATED adCodecSettings "Use generic-lens or generic-optics with 'codecSettings' instead." #-}

-- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary).
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStreamName :: Lens.Lens' AudioDescription (Lude.Maybe Lude.Text)
adStreamName = Lens.lens (streamName :: AudioDescription -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: AudioDescription)
{-# DEPRECATED adStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | Settings that control how input audio channels are remixed into the output audio channels.
--
-- /Note:/ Consider using 'remixSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRemixSettings :: Lens.Lens' AudioDescription (Lude.Maybe RemixSettings)
adRemixSettings = Lens.lens (remixSettings :: AudioDescription -> Lude.Maybe RemixSettings) (\s a -> s {remixSettings = a} :: AudioDescription)
{-# DEPRECATED adRemixSettings "Use generic-lens or generic-optics with 'remixSettings' instead." #-}

-- | Determines how audio type is determined.
--
--   followInput: If the input contains an ISO 639 audioType, then that value is passed through to the output. If the input contains no ISO 639 audioType, the value in Audio Type is included in the output.
--   useConfigured: The value in Audio Type is included in the output.
-- Note that this field and audioType are both ignored if inputType is broadcasterMixedAd.
--
-- /Note:/ Consider using 'audioTypeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioTypeControl :: Lens.Lens' AudioDescription (Lude.Maybe AudioDescriptionAudioTypeControl)
adAudioTypeControl = Lens.lens (audioTypeControl :: AudioDescription -> Lude.Maybe AudioDescriptionAudioTypeControl) (\s a -> s {audioTypeControl = a} :: AudioDescription)
{-# DEPRECATED adAudioTypeControl "Use generic-lens or generic-optics with 'audioTypeControl' instead." #-}

-- | The name of the AudioSelector used as the source for this AudioDescription.
--
-- /Note:/ Consider using 'audioSelectorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioSelectorName :: Lens.Lens' AudioDescription Lude.Text
adAudioSelectorName = Lens.lens (audioSelectorName :: AudioDescription -> Lude.Text) (\s a -> s {audioSelectorName = a} :: AudioDescription)
{-# DEPRECATED adAudioSelectorName "Use generic-lens or generic-optics with 'audioSelectorName' instead." #-}

-- | The name of this AudioDescription. Outputs will use this name to uniquely identify this AudioDescription.  Description names should be unique within this Live Event.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adName :: Lens.Lens' AudioDescription Lude.Text
adName = Lens.lens (name :: AudioDescription -> Lude.Text) (\s a -> s {name = a} :: AudioDescription)
{-# DEPRECATED adName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AudioDescription where
  parseJSON =
    Lude.withObject
      "AudioDescription"
      ( \x ->
          AudioDescription'
            Lude.<$> (x Lude..:? "languageCode")
            Lude.<*> (x Lude..:? "audioType")
            Lude.<*> (x Lude..:? "audioNormalizationSettings")
            Lude.<*> (x Lude..:? "languageCodeControl")
            Lude.<*> (x Lude..:? "codecSettings")
            Lude.<*> (x Lude..:? "streamName")
            Lude.<*> (x Lude..:? "remixSettings")
            Lude.<*> (x Lude..:? "audioTypeControl")
            Lude.<*> (x Lude..: "audioSelectorName")
            Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON AudioDescription where
  toJSON AudioDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("languageCode" Lude..=) Lude.<$> languageCode,
            ("audioType" Lude..=) Lude.<$> audioType,
            ("audioNormalizationSettings" Lude..=)
              Lude.<$> audioNormalizationSettings,
            ("languageCodeControl" Lude..=) Lude.<$> languageCodeControl,
            ("codecSettings" Lude..=) Lude.<$> codecSettings,
            ("streamName" Lude..=) Lude.<$> streamName,
            ("remixSettings" Lude..=) Lude.<$> remixSettings,
            ("audioTypeControl" Lude..=) Lude.<$> audioTypeControl,
            Lude.Just ("audioSelectorName" Lude..= audioSelectorName),
            Lude.Just ("name" Lude..= name)
          ]
      )
