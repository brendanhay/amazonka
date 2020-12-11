-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PresetSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.PresetSettings
  ( PresetSettings (..),

    -- * Smart constructor
    mkPresetSettings,

    -- * Lenses
    psCaptionDescriptions,
    psVideoDescription,
    psContainerSettings,
    psAudioDescriptions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioDescription
import Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
import Network.AWS.MediaConvert.Types.ContainerSettings
import Network.AWS.MediaConvert.Types.VideoDescription
import qualified Network.AWS.Prelude as Lude

-- | Settings for preset
--
-- /See:/ 'mkPresetSettings' smart constructor.
data PresetSettings = PresetSettings'
  { captionDescriptions ::
      Lude.Maybe [CaptionDescriptionPreset],
    videoDescription :: Lude.Maybe VideoDescription,
    containerSettings :: Lude.Maybe ContainerSettings,
    audioDescriptions :: Lude.Maybe [AudioDescription]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PresetSettings' with the minimum fields required to make a request.
--
-- * 'audioDescriptions' - (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
-- * 'captionDescriptions' - Caption settings for this preset. There can be multiple caption settings in a single output.
-- * 'containerSettings' - Container specific settings.
-- * 'videoDescription' - (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec that you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
mkPresetSettings ::
  PresetSettings
mkPresetSettings =
  PresetSettings'
    { captionDescriptions = Lude.Nothing,
      videoDescription = Lude.Nothing,
      containerSettings = Lude.Nothing,
      audioDescriptions = Lude.Nothing
    }

-- | Caption settings for this preset. There can be multiple caption settings in a single output.
--
-- /Note:/ Consider using 'captionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCaptionDescriptions :: Lens.Lens' PresetSettings (Lude.Maybe [CaptionDescriptionPreset])
psCaptionDescriptions = Lens.lens (captionDescriptions :: PresetSettings -> Lude.Maybe [CaptionDescriptionPreset]) (\s a -> s {captionDescriptions = a} :: PresetSettings)
{-# DEPRECATED psCaptionDescriptions "Use generic-lens or generic-optics with 'captionDescriptions' instead." #-}

-- | (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec that you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
--
-- /Note:/ Consider using 'videoDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psVideoDescription :: Lens.Lens' PresetSettings (Lude.Maybe VideoDescription)
psVideoDescription = Lens.lens (videoDescription :: PresetSettings -> Lude.Maybe VideoDescription) (\s a -> s {videoDescription = a} :: PresetSettings)
{-# DEPRECATED psVideoDescription "Use generic-lens or generic-optics with 'videoDescription' instead." #-}

-- | Container specific settings.
--
-- /Note:/ Consider using 'containerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psContainerSettings :: Lens.Lens' PresetSettings (Lude.Maybe ContainerSettings)
psContainerSettings = Lens.lens (containerSettings :: PresetSettings -> Lude.Maybe ContainerSettings) (\s a -> s {containerSettings = a} :: PresetSettings)
{-# DEPRECATED psContainerSettings "Use generic-lens or generic-optics with 'containerSettings' instead." #-}

-- | (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
--
-- /Note:/ Consider using 'audioDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAudioDescriptions :: Lens.Lens' PresetSettings (Lude.Maybe [AudioDescription])
psAudioDescriptions = Lens.lens (audioDescriptions :: PresetSettings -> Lude.Maybe [AudioDescription]) (\s a -> s {audioDescriptions = a} :: PresetSettings)
{-# DEPRECATED psAudioDescriptions "Use generic-lens or generic-optics with 'audioDescriptions' instead." #-}

instance Lude.FromJSON PresetSettings where
  parseJSON =
    Lude.withObject
      "PresetSettings"
      ( \x ->
          PresetSettings'
            Lude.<$> (x Lude..:? "captionDescriptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "videoDescription")
            Lude.<*> (x Lude..:? "containerSettings")
            Lude.<*> (x Lude..:? "audioDescriptions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PresetSettings where
  toJSON PresetSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("captionDescriptions" Lude..=) Lude.<$> captionDescriptions,
            ("videoDescription" Lude..=) Lude.<$> videoDescription,
            ("containerSettings" Lude..=) Lude.<$> containerSettings,
            ("audioDescriptions" Lude..=) Lude.<$> audioDescriptions
          ]
      )
