{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmfcSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmfcSettings
  ( CmfcSettings (..)
  -- * Smart constructor
  , mkCmfcSettings
  -- * Lenses
  , csAudioDuration
  , csScte35Esam
  , csScte35Source
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CmfcAudioDuration as Types
import qualified Network.AWS.MediaConvert.Types.CmfcScte35Esam as Types
import qualified Network.AWS.MediaConvert.Types.CmfcScte35Source as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for MP4 segments in CMAF
--
-- /See:/ 'mkCmfcSettings' smart constructor.
data CmfcSettings = CmfcSettings'
  { audioDuration :: Core.Maybe Types.CmfcAudioDuration
    -- ^ Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
  , scte35Esam :: Core.Maybe Types.CmfcScte35Esam
    -- ^ Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
  , scte35Source :: Core.Maybe Types.CmfcScte35Source
    -- ^ Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CmfcSettings' value with any optional fields omitted.
mkCmfcSettings
    :: CmfcSettings
mkCmfcSettings
  = CmfcSettings'{audioDuration = Core.Nothing,
                  scte35Esam = Core.Nothing, scte35Source = Core.Nothing}

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAudioDuration :: Lens.Lens' CmfcSettings (Core.Maybe Types.CmfcAudioDuration)
csAudioDuration = Lens.field @"audioDuration"
{-# INLINEABLE csAudioDuration #-}
{-# DEPRECATED audioDuration "Use generic-lens or generic-optics with 'audioDuration' instead"  #-}

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- /Note:/ Consider using 'scte35Esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csScte35Esam :: Lens.Lens' CmfcSettings (Core.Maybe Types.CmfcScte35Esam)
csScte35Esam = Lens.field @"scte35Esam"
{-# INLINEABLE csScte35Esam #-}
{-# DEPRECATED scte35Esam "Use generic-lens or generic-optics with 'scte35Esam' instead"  #-}

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
--
-- /Note:/ Consider using 'scte35Source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csScte35Source :: Lens.Lens' CmfcSettings (Core.Maybe Types.CmfcScte35Source)
csScte35Source = Lens.field @"scte35Source"
{-# INLINEABLE csScte35Source #-}
{-# DEPRECATED scte35Source "Use generic-lens or generic-optics with 'scte35Source' instead"  #-}

instance Core.FromJSON CmfcSettings where
        toJSON CmfcSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioDuration" Core..=) Core.<$> audioDuration,
                  ("scte35Esam" Core..=) Core.<$> scte35Esam,
                  ("scte35Source" Core..=) Core.<$> scte35Source])

instance Core.FromJSON CmfcSettings where
        parseJSON
          = Core.withObject "CmfcSettings" Core.$
              \ x ->
                CmfcSettings' Core.<$>
                  (x Core..:? "audioDuration") Core.<*> x Core..:? "scte35Esam"
                    Core.<*> x Core..:? "scte35Source"
