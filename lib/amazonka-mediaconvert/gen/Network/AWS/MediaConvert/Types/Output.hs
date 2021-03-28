{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Output
  ( Output (..)
  -- * Smart constructor
  , mkOutput
  -- * Lenses
  , oAudioDescriptions
  , oCaptionDescriptions
  , oContainerSettings
  , oExtension
  , oNameModifier
  , oOutputSettings
  , oPreset
  , oVideoDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AudioDescription as Types
import qualified Network.AWS.MediaConvert.Types.CaptionDescription as Types
import qualified Network.AWS.MediaConvert.Types.ContainerSettings as Types
import qualified Network.AWS.MediaConvert.Types.OutputSettings as Types
import qualified Network.AWS.MediaConvert.Types.VideoDescription as Types
import qualified Network.AWS.Prelude as Core

-- | An output object describes the settings for a single output file or stream in an output group.
--
-- /See:/ 'mkOutput' smart constructor.
data Output = Output'
  { audioDescriptions :: Core.Maybe [Types.AudioDescription]
    -- ^ (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
  , captionDescriptions :: Core.Maybe [Types.CaptionDescription]
    -- ^ (CaptionDescriptions) contains groups of captions settings. For each output that has captions, include one instance of (CaptionDescriptions). (CaptionDescriptions) can contain multiple groups of captions settings.
  , containerSettings :: Core.Maybe Types.ContainerSettings
    -- ^ Container specific settings.
  , extension :: Core.Maybe Core.Text
    -- ^ Use Extension (Extension) to specify the file extension for outputs in File output groups. If you do not specify a value, the service will use default extensions by container type as follows * MPEG-2 transport stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container, mp4 * WebM container, webm * No Container, the service will use codec extensions (e.g. AAC, H265, H265, AC3)
  , nameModifier :: Core.Maybe Core.Text
    -- ^ Use Name modifier (NameModifier) to have the service add a string to the end of each output filename. You specify the base filename as part of your destination URI. When you create multiple outputs in the same output group, Name modifier (NameModifier) is required. Name modifier also accepts format identifiers. For DASH ISO outputs, if you use the format identifiers $Number$ or $Time$ in one output, you must use them in the same way in all outputs of the output group.
  , outputSettings :: Core.Maybe Types.OutputSettings
    -- ^ Specific settings for this type of output.
  , preset :: Core.Maybe Core.Text
    -- ^ Use Preset (Preset) to specifiy a preset for your transcoding settings. Provide the system or custom preset name. You can specify either Preset (Preset) or Container settings (ContainerSettings), but not both.
  , videoDescription :: Core.Maybe Types.VideoDescription
    -- ^ (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec that you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Output' value with any optional fields omitted.
mkOutput
    :: Output
mkOutput
  = Output'{audioDescriptions = Core.Nothing,
            captionDescriptions = Core.Nothing,
            containerSettings = Core.Nothing, extension = Core.Nothing,
            nameModifier = Core.Nothing, outputSettings = Core.Nothing,
            preset = Core.Nothing, videoDescription = Core.Nothing}

-- | (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
--
-- /Note:/ Consider using 'audioDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAudioDescriptions :: Lens.Lens' Output (Core.Maybe [Types.AudioDescription])
oAudioDescriptions = Lens.field @"audioDescriptions"
{-# INLINEABLE oAudioDescriptions #-}
{-# DEPRECATED audioDescriptions "Use generic-lens or generic-optics with 'audioDescriptions' instead"  #-}

-- | (CaptionDescriptions) contains groups of captions settings. For each output that has captions, include one instance of (CaptionDescriptions). (CaptionDescriptions) can contain multiple groups of captions settings.
--
-- /Note:/ Consider using 'captionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCaptionDescriptions :: Lens.Lens' Output (Core.Maybe [Types.CaptionDescription])
oCaptionDescriptions = Lens.field @"captionDescriptions"
{-# INLINEABLE oCaptionDescriptions #-}
{-# DEPRECATED captionDescriptions "Use generic-lens or generic-optics with 'captionDescriptions' instead"  #-}

-- | Container specific settings.
--
-- /Note:/ Consider using 'containerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oContainerSettings :: Lens.Lens' Output (Core.Maybe Types.ContainerSettings)
oContainerSettings = Lens.field @"containerSettings"
{-# INLINEABLE oContainerSettings #-}
{-# DEPRECATED containerSettings "Use generic-lens or generic-optics with 'containerSettings' instead"  #-}

-- | Use Extension (Extension) to specify the file extension for outputs in File output groups. If you do not specify a value, the service will use default extensions by container type as follows * MPEG-2 transport stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container, mp4 * WebM container, webm * No Container, the service will use codec extensions (e.g. AAC, H265, H265, AC3)
--
-- /Note:/ Consider using 'extension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oExtension :: Lens.Lens' Output (Core.Maybe Core.Text)
oExtension = Lens.field @"extension"
{-# INLINEABLE oExtension #-}
{-# DEPRECATED extension "Use generic-lens or generic-optics with 'extension' instead"  #-}

-- | Use Name modifier (NameModifier) to have the service add a string to the end of each output filename. You specify the base filename as part of your destination URI. When you create multiple outputs in the same output group, Name modifier (NameModifier) is required. Name modifier also accepts format identifiers. For DASH ISO outputs, if you use the format identifiers $Number$ or $Time$ in one output, you must use them in the same way in all outputs of the output group.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oNameModifier :: Lens.Lens' Output (Core.Maybe Core.Text)
oNameModifier = Lens.field @"nameModifier"
{-# INLINEABLE oNameModifier #-}
{-# DEPRECATED nameModifier "Use generic-lens or generic-optics with 'nameModifier' instead"  #-}

-- | Specific settings for this type of output.
--
-- /Note:/ Consider using 'outputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputSettings :: Lens.Lens' Output (Core.Maybe Types.OutputSettings)
oOutputSettings = Lens.field @"outputSettings"
{-# INLINEABLE oOutputSettings #-}
{-# DEPRECATED outputSettings "Use generic-lens or generic-optics with 'outputSettings' instead"  #-}

-- | Use Preset (Preset) to specifiy a preset for your transcoding settings. Provide the system or custom preset name. You can specify either Preset (Preset) or Container settings (ContainerSettings), but not both.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPreset :: Lens.Lens' Output (Core.Maybe Core.Text)
oPreset = Lens.field @"preset"
{-# INLINEABLE oPreset #-}
{-# DEPRECATED preset "Use generic-lens or generic-optics with 'preset' instead"  #-}

-- | (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec that you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
--
-- /Note:/ Consider using 'videoDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oVideoDescription :: Lens.Lens' Output (Core.Maybe Types.VideoDescription)
oVideoDescription = Lens.field @"videoDescription"
{-# INLINEABLE oVideoDescription #-}
{-# DEPRECATED videoDescription "Use generic-lens or generic-optics with 'videoDescription' instead"  #-}

instance Core.FromJSON Output where
        toJSON Output{..}
          = Core.object
              (Core.catMaybes
                 [("audioDescriptions" Core..=) Core.<$> audioDescriptions,
                  ("captionDescriptions" Core..=) Core.<$> captionDescriptions,
                  ("containerSettings" Core..=) Core.<$> containerSettings,
                  ("extension" Core..=) Core.<$> extension,
                  ("nameModifier" Core..=) Core.<$> nameModifier,
                  ("outputSettings" Core..=) Core.<$> outputSettings,
                  ("preset" Core..=) Core.<$> preset,
                  ("videoDescription" Core..=) Core.<$> videoDescription])

instance Core.FromJSON Output where
        parseJSON
          = Core.withObject "Output" Core.$
              \ x ->
                Output' Core.<$>
                  (x Core..:? "audioDescriptions") Core.<*>
                    x Core..:? "captionDescriptions"
                    Core.<*> x Core..:? "containerSettings"
                    Core.<*> x Core..:? "extension"
                    Core.<*> x Core..:? "nameModifier"
                    Core.<*> x Core..:? "outputSettings"
                    Core.<*> x Core..:? "preset"
                    Core.<*> x Core..:? "videoDescription"
