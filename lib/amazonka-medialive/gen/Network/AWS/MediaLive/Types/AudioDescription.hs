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
    adAudioSelectorName,
    adName,
    adAudioNormalizationSettings,
    adAudioType,
    adAudioTypeControl,
    adCodecSettings,
    adLanguageCode,
    adLanguageCodeControl,
    adRemixSettings,
    adStreamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioCodecSettings as Types
import qualified Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl as Types
import qualified Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl as Types
import qualified Network.AWS.MediaLive.Types.AudioNormalizationSettings as Types
import qualified Network.AWS.MediaLive.Types.AudioType as Types
import qualified Network.AWS.MediaLive.Types.RemixSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Description
--
-- /See:/ 'mkAudioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { -- | The name of the AudioSelector used as the source for this AudioDescription.
    audioSelectorName :: Core.Text,
    -- | The name of this AudioDescription. Outputs will use this name to uniquely identify this AudioDescription.  Description names should be unique within this Live Event.
    name :: Core.Text,
    -- | Advanced audio normalization settings.
    audioNormalizationSettings :: Core.Maybe Types.AudioNormalizationSettings,
    -- | Applies only if audioTypeControl is useConfigured. The values for audioType are defined in ISO-IEC 13818-1.
    audioType :: Core.Maybe Types.AudioType,
    -- | Determines how audio type is determined.
    --
    --   followInput: If the input contains an ISO 639 audioType, then that value is passed through to the output. If the input contains no ISO 639 audioType, the value in Audio Type is included in the output.
    --   useConfigured: The value in Audio Type is included in the output.
    -- Note that this field and audioType are both ignored if inputType is broadcasterMixedAd.
    audioTypeControl :: Core.Maybe Types.AudioDescriptionAudioTypeControl,
    -- | Audio codec settings.
    codecSettings :: Core.Maybe Types.AudioCodecSettings,
    -- | RFC 5646 language code representing the language of the audio output track. Only used if languageControlMode is useConfigured, or there is no ISO 639 language code specified in the input.
    languageCode :: Core.Maybe Core.Text,
    -- | Choosing followInput will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The languageCode will be used when useConfigured is set, or when followInput is selected but there is no ISO 639 language code specified by the input.
    languageCodeControl :: Core.Maybe Types.AudioDescriptionLanguageCodeControl,
    -- | Settings that control how input audio channels are remixed into the output audio channels.
    remixSettings :: Core.Maybe Types.RemixSettings,
    -- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary).
    streamName :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioDescription' value with any optional fields omitted.
mkAudioDescription ::
  -- | 'audioSelectorName'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  AudioDescription
mkAudioDescription audioSelectorName name =
  AudioDescription'
    { audioSelectorName,
      name,
      audioNormalizationSettings = Core.Nothing,
      audioType = Core.Nothing,
      audioTypeControl = Core.Nothing,
      codecSettings = Core.Nothing,
      languageCode = Core.Nothing,
      languageCodeControl = Core.Nothing,
      remixSettings = Core.Nothing,
      streamName = Core.Nothing
    }

-- | The name of the AudioSelector used as the source for this AudioDescription.
--
-- /Note:/ Consider using 'audioSelectorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioSelectorName :: Lens.Lens' AudioDescription Core.Text
adAudioSelectorName = Lens.field @"audioSelectorName"
{-# DEPRECATED adAudioSelectorName "Use generic-lens or generic-optics with 'audioSelectorName' instead." #-}

-- | The name of this AudioDescription. Outputs will use this name to uniquely identify this AudioDescription.  Description names should be unique within this Live Event.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adName :: Lens.Lens' AudioDescription Core.Text
adName = Lens.field @"name"
{-# DEPRECATED adName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Advanced audio normalization settings.
--
-- /Note:/ Consider using 'audioNormalizationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioNormalizationSettings :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioNormalizationSettings)
adAudioNormalizationSettings = Lens.field @"audioNormalizationSettings"
{-# DEPRECATED adAudioNormalizationSettings "Use generic-lens or generic-optics with 'audioNormalizationSettings' instead." #-}

-- | Applies only if audioTypeControl is useConfigured. The values for audioType are defined in ISO-IEC 13818-1.
--
-- /Note:/ Consider using 'audioType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioType :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioType)
adAudioType = Lens.field @"audioType"
{-# DEPRECATED adAudioType "Use generic-lens or generic-optics with 'audioType' instead." #-}

-- | Determines how audio type is determined.
--
--   followInput: If the input contains an ISO 639 audioType, then that value is passed through to the output. If the input contains no ISO 639 audioType, the value in Audio Type is included in the output.
--   useConfigured: The value in Audio Type is included in the output.
-- Note that this field and audioType are both ignored if inputType is broadcasterMixedAd.
--
-- /Note:/ Consider using 'audioTypeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAudioTypeControl :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioDescriptionAudioTypeControl)
adAudioTypeControl = Lens.field @"audioTypeControl"
{-# DEPRECATED adAudioTypeControl "Use generic-lens or generic-optics with 'audioTypeControl' instead." #-}

-- | Audio codec settings.
--
-- /Note:/ Consider using 'codecSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCodecSettings :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioCodecSettings)
adCodecSettings = Lens.field @"codecSettings"
{-# DEPRECATED adCodecSettings "Use generic-lens or generic-optics with 'codecSettings' instead." #-}

-- | RFC 5646 language code representing the language of the audio output track. Only used if languageControlMode is useConfigured, or there is no ISO 639 language code specified in the input.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLanguageCode :: Lens.Lens' AudioDescription (Core.Maybe Core.Text)
adLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED adLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Choosing followInput will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The languageCode will be used when useConfigured is set, or when followInput is selected but there is no ISO 639 language code specified by the input.
--
-- /Note:/ Consider using 'languageCodeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLanguageCodeControl :: Lens.Lens' AudioDescription (Core.Maybe Types.AudioDescriptionLanguageCodeControl)
adLanguageCodeControl = Lens.field @"languageCodeControl"
{-# DEPRECATED adLanguageCodeControl "Use generic-lens or generic-optics with 'languageCodeControl' instead." #-}

-- | Settings that control how input audio channels are remixed into the output audio channels.
--
-- /Note:/ Consider using 'remixSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRemixSettings :: Lens.Lens' AudioDescription (Core.Maybe Types.RemixSettings)
adRemixSettings = Lens.field @"remixSettings"
{-# DEPRECATED adRemixSettings "Use generic-lens or generic-optics with 'remixSettings' instead." #-}

-- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary).
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStreamName :: Lens.Lens' AudioDescription (Core.Maybe Core.Text)
adStreamName = Lens.field @"streamName"
{-# DEPRECATED adStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON AudioDescription where
  toJSON AudioDescription {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("audioSelectorName" Core..= audioSelectorName),
            Core.Just ("name" Core..= name),
            ("audioNormalizationSettings" Core..=)
              Core.<$> audioNormalizationSettings,
            ("audioType" Core..=) Core.<$> audioType,
            ("audioTypeControl" Core..=) Core.<$> audioTypeControl,
            ("codecSettings" Core..=) Core.<$> codecSettings,
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
          Core.<$> (x Core..: "audioSelectorName")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..:? "audioNormalizationSettings")
          Core.<*> (x Core..:? "audioType")
          Core.<*> (x Core..:? "audioTypeControl")
          Core.<*> (x Core..:? "codecSettings")
          Core.<*> (x Core..:? "languageCode")
          Core.<*> (x Core..:? "languageCodeControl")
          Core.<*> (x Core..:? "remixSettings")
          Core.<*> (x Core..:? "streamName")
