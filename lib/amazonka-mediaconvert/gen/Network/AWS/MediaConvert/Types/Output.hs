-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Output
  ( Output (..),

    -- * Smart constructor
    mkOutput,

    -- * Lenses
    oCaptionDescriptions,
    oExtension,
    oVideoDescription,
    oContainerSettings,
    oOutputSettings,
    oPreset,
    oNameModifier,
    oAudioDescriptions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioDescription
import Network.AWS.MediaConvert.Types.CaptionDescription
import Network.AWS.MediaConvert.Types.ContainerSettings
import Network.AWS.MediaConvert.Types.OutputSettings
import Network.AWS.MediaConvert.Types.VideoDescription
import qualified Network.AWS.Prelude as Lude

-- | An output object describes the settings for a single output file or stream in an output group.
--
-- /See:/ 'mkOutput' smart constructor.
data Output = Output'
  { captionDescriptions ::
      Lude.Maybe [CaptionDescription],
    extension :: Lude.Maybe Lude.Text,
    videoDescription :: Lude.Maybe VideoDescription,
    containerSettings :: Lude.Maybe ContainerSettings,
    outputSettings :: Lude.Maybe OutputSettings,
    preset :: Lude.Maybe Lude.Text,
    nameModifier :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- * 'audioDescriptions' - (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
-- * 'captionDescriptions' - (CaptionDescriptions) contains groups of captions settings. For each output that has captions, include one instance of (CaptionDescriptions). (CaptionDescriptions) can contain multiple groups of captions settings.
-- * 'containerSettings' - Container specific settings.
-- * 'extension' - Use Extension (Extension) to specify the file extension for outputs in File output groups. If you do not specify a value, the service will use default extensions by container type as follows * MPEG-2 transport stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container, mp4 * WebM container, webm * No Container, the service will use codec extensions (e.g. AAC, H265, H265, AC3)
-- * 'nameModifier' - Use Name modifier (NameModifier) to have the service add a string to the end of each output filename. You specify the base filename as part of your destination URI. When you create multiple outputs in the same output group, Name modifier (NameModifier) is required. Name modifier also accepts format identifiers. For DASH ISO outputs, if you use the format identifiers $Number$ or $Time$ in one output, you must use them in the same way in all outputs of the output group.
-- * 'outputSettings' - Specific settings for this type of output.
-- * 'preset' - Use Preset (Preset) to specifiy a preset for your transcoding settings. Provide the system or custom preset name. You can specify either Preset (Preset) or Container settings (ContainerSettings), but not both.
-- * 'videoDescription' - (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec that you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
mkOutput ::
  Output
mkOutput =
  Output'
    { captionDescriptions = Lude.Nothing,
      extension = Lude.Nothing,
      videoDescription = Lude.Nothing,
      containerSettings = Lude.Nothing,
      outputSettings = Lude.Nothing,
      preset = Lude.Nothing,
      nameModifier = Lude.Nothing,
      audioDescriptions = Lude.Nothing
    }

-- | (CaptionDescriptions) contains groups of captions settings. For each output that has captions, include one instance of (CaptionDescriptions). (CaptionDescriptions) can contain multiple groups of captions settings.
--
-- /Note:/ Consider using 'captionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCaptionDescriptions :: Lens.Lens' Output (Lude.Maybe [CaptionDescription])
oCaptionDescriptions = Lens.lens (captionDescriptions :: Output -> Lude.Maybe [CaptionDescription]) (\s a -> s {captionDescriptions = a} :: Output)
{-# DEPRECATED oCaptionDescriptions "Use generic-lens or generic-optics with 'captionDescriptions' instead." #-}

-- | Use Extension (Extension) to specify the file extension for outputs in File output groups. If you do not specify a value, the service will use default extensions by container type as follows * MPEG-2 transport stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container, mp4 * WebM container, webm * No Container, the service will use codec extensions (e.g. AAC, H265, H265, AC3)
--
-- /Note:/ Consider using 'extension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oExtension :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oExtension = Lens.lens (extension :: Output -> Lude.Maybe Lude.Text) (\s a -> s {extension = a} :: Output)
{-# DEPRECATED oExtension "Use generic-lens or generic-optics with 'extension' instead." #-}

-- | (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec that you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
--
-- /Note:/ Consider using 'videoDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oVideoDescription :: Lens.Lens' Output (Lude.Maybe VideoDescription)
oVideoDescription = Lens.lens (videoDescription :: Output -> Lude.Maybe VideoDescription) (\s a -> s {videoDescription = a} :: Output)
{-# DEPRECATED oVideoDescription "Use generic-lens or generic-optics with 'videoDescription' instead." #-}

-- | Container specific settings.
--
-- /Note:/ Consider using 'containerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oContainerSettings :: Lens.Lens' Output (Lude.Maybe ContainerSettings)
oContainerSettings = Lens.lens (containerSettings :: Output -> Lude.Maybe ContainerSettings) (\s a -> s {containerSettings = a} :: Output)
{-# DEPRECATED oContainerSettings "Use generic-lens or generic-optics with 'containerSettings' instead." #-}

-- | Specific settings for this type of output.
--
-- /Note:/ Consider using 'outputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputSettings :: Lens.Lens' Output (Lude.Maybe OutputSettings)
oOutputSettings = Lens.lens (outputSettings :: Output -> Lude.Maybe OutputSettings) (\s a -> s {outputSettings = a} :: Output)
{-# DEPRECATED oOutputSettings "Use generic-lens or generic-optics with 'outputSettings' instead." #-}

-- | Use Preset (Preset) to specifiy a preset for your transcoding settings. Provide the system or custom preset name. You can specify either Preset (Preset) or Container settings (ContainerSettings), but not both.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPreset :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oPreset = Lens.lens (preset :: Output -> Lude.Maybe Lude.Text) (\s a -> s {preset = a} :: Output)
{-# DEPRECATED oPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | Use Name modifier (NameModifier) to have the service add a string to the end of each output filename. You specify the base filename as part of your destination URI. When you create multiple outputs in the same output group, Name modifier (NameModifier) is required. Name modifier also accepts format identifiers. For DASH ISO outputs, if you use the format identifiers $Number$ or $Time$ in one output, you must use them in the same way in all outputs of the output group.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oNameModifier :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oNameModifier = Lens.lens (nameModifier :: Output -> Lude.Maybe Lude.Text) (\s a -> s {nameModifier = a} :: Output)
{-# DEPRECATED oNameModifier "Use generic-lens or generic-optics with 'nameModifier' instead." #-}

-- | (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
--
-- /Note:/ Consider using 'audioDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAudioDescriptions :: Lens.Lens' Output (Lude.Maybe [AudioDescription])
oAudioDescriptions = Lens.lens (audioDescriptions :: Output -> Lude.Maybe [AudioDescription]) (\s a -> s {audioDescriptions = a} :: Output)
{-# DEPRECATED oAudioDescriptions "Use generic-lens or generic-optics with 'audioDescriptions' instead." #-}

instance Lude.FromJSON Output where
  parseJSON =
    Lude.withObject
      "Output"
      ( \x ->
          Output'
            Lude.<$> (x Lude..:? "captionDescriptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "extension")
            Lude.<*> (x Lude..:? "videoDescription")
            Lude.<*> (x Lude..:? "containerSettings")
            Lude.<*> (x Lude..:? "outputSettings")
            Lude.<*> (x Lude..:? "preset")
            Lude.<*> (x Lude..:? "nameModifier")
            Lude.<*> (x Lude..:? "audioDescriptions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Output where
  toJSON Output' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("captionDescriptions" Lude..=) Lude.<$> captionDescriptions,
            ("extension" Lude..=) Lude.<$> extension,
            ("videoDescription" Lude..=) Lude.<$> videoDescription,
            ("containerSettings" Lude..=) Lude.<$> containerSettings,
            ("outputSettings" Lude..=) Lude.<$> outputSettings,
            ("preset" Lude..=) Lude.<$> preset,
            ("nameModifier" Lude..=) Lude.<$> nameModifier,
            ("audioDescriptions" Lude..=) Lude.<$> audioDescriptions
          ]
      )
