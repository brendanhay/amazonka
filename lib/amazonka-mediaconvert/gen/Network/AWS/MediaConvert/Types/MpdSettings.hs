{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MpdSettings
  ( MpdSettings (..)
  -- * Smart constructor
  , mkMpdSettings
  -- * Lenses
  , msfAccessibilityCaptionHints
  , msfAudioDuration
  , msfCaptionContainerType
  , msfScte35Esam
  , msfScte35Source
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints as Types
import qualified Network.AWS.MediaConvert.Types.MpdAudioDuration as Types
import qualified Network.AWS.MediaConvert.Types.MpdCaptionContainerType as Types
import qualified Network.AWS.MediaConvert.Types.MpdScte35Esam as Types
import qualified Network.AWS.MediaConvert.Types.MpdScte35Source as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for MP4 segments in DASH
--
-- /See:/ 'mkMpdSettings' smart constructor.
data MpdSettings = MpdSettings'
  { accessibilityCaptionHints :: Core.Maybe Types.MpdAccessibilityCaptionHints
    -- ^ <Accessibility>elements for embedded 608 captions. This markup isn't generally required, but some video players require it to discover and play embedded 608 captions. Keep the default value, Exclude (EXCLUDE), to leave these elements out. When you enable this setting, this is the markup that MediaConvert includes in your manifest: <Accessibility>
  , audioDuration :: Core.Maybe Types.MpdAudioDuration
    -- ^ Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
  , captionContainerType :: Core.Maybe Types.MpdCaptionContainerType
    -- ^ Use this setting only in DASH output groups that include sidecar TTML or IMSC captions.  You specify sidecar captions in a separate output from your audio and video. Choose Raw (RAW) for captions in a single XML file in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for captions in XML format contained within fragmented MP4 files. This set of fragmented MP4 files is separate from your video and audio fragmented MP4 files.
  , scte35Esam :: Core.Maybe Types.MpdScte35Esam
    -- ^ Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
  , scte35Source :: Core.Maybe Types.MpdScte35Source
    -- ^ Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MpdSettings' value with any optional fields omitted.
mkMpdSettings
    :: MpdSettings
mkMpdSettings
  = MpdSettings'{accessibilityCaptionHints = Core.Nothing,
                 audioDuration = Core.Nothing, captionContainerType = Core.Nothing,
                 scte35Esam = Core.Nothing, scte35Source = Core.Nothing}

-- | <Accessibility>elements for embedded 608 captions. This markup isn't generally required, but some video players require it to discover and play embedded 608 captions. Keep the default value, Exclude (EXCLUDE), to leave these elements out. When you enable this setting, this is the markup that MediaConvert includes in your manifest: <Accessibility>
--
-- /Note:/ Consider using 'accessibilityCaptionHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfAccessibilityCaptionHints :: Lens.Lens' MpdSettings (Core.Maybe Types.MpdAccessibilityCaptionHints)
msfAccessibilityCaptionHints = Lens.field @"accessibilityCaptionHints"
{-# INLINEABLE msfAccessibilityCaptionHints #-}
{-# DEPRECATED accessibilityCaptionHints "Use generic-lens or generic-optics with 'accessibilityCaptionHints' instead"  #-}

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfAudioDuration :: Lens.Lens' MpdSettings (Core.Maybe Types.MpdAudioDuration)
msfAudioDuration = Lens.field @"audioDuration"
{-# INLINEABLE msfAudioDuration #-}
{-# DEPRECATED audioDuration "Use generic-lens or generic-optics with 'audioDuration' instead"  #-}

-- | Use this setting only in DASH output groups that include sidecar TTML or IMSC captions.  You specify sidecar captions in a separate output from your audio and video. Choose Raw (RAW) for captions in a single XML file in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for captions in XML format contained within fragmented MP4 files. This set of fragmented MP4 files is separate from your video and audio fragmented MP4 files.
--
-- /Note:/ Consider using 'captionContainerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfCaptionContainerType :: Lens.Lens' MpdSettings (Core.Maybe Types.MpdCaptionContainerType)
msfCaptionContainerType = Lens.field @"captionContainerType"
{-# INLINEABLE msfCaptionContainerType #-}
{-# DEPRECATED captionContainerType "Use generic-lens or generic-optics with 'captionContainerType' instead"  #-}

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- /Note:/ Consider using 'scte35Esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfScte35Esam :: Lens.Lens' MpdSettings (Core.Maybe Types.MpdScte35Esam)
msfScte35Esam = Lens.field @"scte35Esam"
{-# INLINEABLE msfScte35Esam #-}
{-# DEPRECATED scte35Esam "Use generic-lens or generic-optics with 'scte35Esam' instead"  #-}

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
--
-- /Note:/ Consider using 'scte35Source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfScte35Source :: Lens.Lens' MpdSettings (Core.Maybe Types.MpdScte35Source)
msfScte35Source = Lens.field @"scte35Source"
{-# INLINEABLE msfScte35Source #-}
{-# DEPRECATED scte35Source "Use generic-lens or generic-optics with 'scte35Source' instead"  #-}

instance Core.FromJSON MpdSettings where
        toJSON MpdSettings{..}
          = Core.object
              (Core.catMaybes
                 [("accessibilityCaptionHints" Core..=) Core.<$>
                    accessibilityCaptionHints,
                  ("audioDuration" Core..=) Core.<$> audioDuration,
                  ("captionContainerType" Core..=) Core.<$> captionContainerType,
                  ("scte35Esam" Core..=) Core.<$> scte35Esam,
                  ("scte35Source" Core..=) Core.<$> scte35Source])

instance Core.FromJSON MpdSettings where
        parseJSON
          = Core.withObject "MpdSettings" Core.$
              \ x ->
                MpdSettings' Core.<$>
                  (x Core..:? "accessibilityCaptionHints") Core.<*>
                    x Core..:? "audioDuration"
                    Core.<*> x Core..:? "captionContainerType"
                    Core.<*> x Core..:? "scte35Esam"
                    Core.<*> x Core..:? "scte35Source"
