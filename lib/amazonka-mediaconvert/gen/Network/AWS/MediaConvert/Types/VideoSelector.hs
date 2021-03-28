{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.VideoSelector
  ( VideoSelector (..)
  -- * Smart constructor
  , mkVideoSelector
  -- * Lenses
  , vsAlphaBehavior
  , vsColorSpace
  , vsColorSpaceUsage
  , vsHdr10Metadata
  , vsPid
  , vsProgramNumber
  , vsRotate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AlphaBehavior as Types
import qualified Network.AWS.MediaConvert.Types.ColorSpace as Types
import qualified Network.AWS.MediaConvert.Types.ColorSpaceUsage as Types
import qualified Network.AWS.MediaConvert.Types.Hdr10Metadata as Types
import qualified Network.AWS.MediaConvert.Types.InputRotate as Types
import qualified Network.AWS.Prelude as Core

-- | Selector for video.
--
-- /See:/ 'mkVideoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { alphaBehavior :: Core.Maybe Types.AlphaBehavior
    -- ^ Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
  , colorSpace :: Core.Maybe Types.ColorSpace
    -- ^ If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
  , colorSpaceUsage :: Core.Maybe Types.ColorSpaceUsage
    -- ^ There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
  , hdr10Metadata :: Core.Maybe Types.Hdr10Metadata
    -- ^ Use these settings to provide HDR 10 metadata that is missing or inaccurate in your input video. Appropriate values vary depending on the input video and must be provided by a color grader. The color grader generates these values during the HDR 10 mastering process. The valid range for each of these settings is 0 to 50,000. Each increment represents 0.00002 in CIE1931 color coordinate. Related settings - When you specify these values, you must also set Color space (ColorSpace) to HDR 10 (HDR10). To specify whether the the values you specify here take precedence over the values in the metadata of your input file, set Color space usage (ColorSpaceUsage). To specify whether color metadata is included in an output, set Color metadata (ColorMetadata). For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
  , pid :: Core.Maybe Core.Natural
    -- ^ Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
  , programNumber :: Core.Maybe Core.Int
    -- ^ Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
  , rotate :: Core.Maybe Types.InputRotate
    -- ^ Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoSelector' value with any optional fields omitted.
mkVideoSelector
    :: VideoSelector
mkVideoSelector
  = VideoSelector'{alphaBehavior = Core.Nothing,
                   colorSpace = Core.Nothing, colorSpaceUsage = Core.Nothing,
                   hdr10Metadata = Core.Nothing, pid = Core.Nothing,
                   programNumber = Core.Nothing, rotate = Core.Nothing}

-- | Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
--
-- /Note:/ Consider using 'alphaBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsAlphaBehavior :: Lens.Lens' VideoSelector (Core.Maybe Types.AlphaBehavior)
vsAlphaBehavior = Lens.field @"alphaBehavior"
{-# INLINEABLE vsAlphaBehavior #-}
{-# DEPRECATED alphaBehavior "Use generic-lens or generic-optics with 'alphaBehavior' instead"  #-}

-- | If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
--
-- /Note:/ Consider using 'colorSpace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsColorSpace :: Lens.Lens' VideoSelector (Core.Maybe Types.ColorSpace)
vsColorSpace = Lens.field @"colorSpace"
{-# INLINEABLE vsColorSpace #-}
{-# DEPRECATED colorSpace "Use generic-lens or generic-optics with 'colorSpace' instead"  #-}

-- | There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
--
-- /Note:/ Consider using 'colorSpaceUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsColorSpaceUsage :: Lens.Lens' VideoSelector (Core.Maybe Types.ColorSpaceUsage)
vsColorSpaceUsage = Lens.field @"colorSpaceUsage"
{-# INLINEABLE vsColorSpaceUsage #-}
{-# DEPRECATED colorSpaceUsage "Use generic-lens or generic-optics with 'colorSpaceUsage' instead"  #-}

-- | Use these settings to provide HDR 10 metadata that is missing or inaccurate in your input video. Appropriate values vary depending on the input video and must be provided by a color grader. The color grader generates these values during the HDR 10 mastering process. The valid range for each of these settings is 0 to 50,000. Each increment represents 0.00002 in CIE1931 color coordinate. Related settings - When you specify these values, you must also set Color space (ColorSpace) to HDR 10 (HDR10). To specify whether the the values you specify here take precedence over the values in the metadata of your input file, set Color space usage (ColorSpaceUsage). To specify whether color metadata is included in an output, set Color metadata (ColorMetadata). For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
--
-- /Note:/ Consider using 'hdr10Metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsHdr10Metadata :: Lens.Lens' VideoSelector (Core.Maybe Types.Hdr10Metadata)
vsHdr10Metadata = Lens.field @"hdr10Metadata"
{-# INLINEABLE vsHdr10Metadata #-}
{-# DEPRECATED hdr10Metadata "Use generic-lens or generic-optics with 'hdr10Metadata' instead"  #-}

-- | Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsPid :: Lens.Lens' VideoSelector (Core.Maybe Core.Natural)
vsPid = Lens.field @"pid"
{-# INLINEABLE vsPid #-}
{-# DEPRECATED pid "Use generic-lens or generic-optics with 'pid' instead"  #-}

-- | Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsProgramNumber :: Lens.Lens' VideoSelector (Core.Maybe Core.Int)
vsProgramNumber = Lens.field @"programNumber"
{-# INLINEABLE vsProgramNumber #-}
{-# DEPRECATED programNumber "Use generic-lens or generic-optics with 'programNumber' instead"  #-}

-- | Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
--
-- /Note:/ Consider using 'rotate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsRotate :: Lens.Lens' VideoSelector (Core.Maybe Types.InputRotate)
vsRotate = Lens.field @"rotate"
{-# INLINEABLE vsRotate #-}
{-# DEPRECATED rotate "Use generic-lens or generic-optics with 'rotate' instead"  #-}

instance Core.FromJSON VideoSelector where
        toJSON VideoSelector{..}
          = Core.object
              (Core.catMaybes
                 [("alphaBehavior" Core..=) Core.<$> alphaBehavior,
                  ("colorSpace" Core..=) Core.<$> colorSpace,
                  ("colorSpaceUsage" Core..=) Core.<$> colorSpaceUsage,
                  ("hdr10Metadata" Core..=) Core.<$> hdr10Metadata,
                  ("pid" Core..=) Core.<$> pid,
                  ("programNumber" Core..=) Core.<$> programNumber,
                  ("rotate" Core..=) Core.<$> rotate])

instance Core.FromJSON VideoSelector where
        parseJSON
          = Core.withObject "VideoSelector" Core.$
              \ x ->
                VideoSelector' Core.<$>
                  (x Core..:? "alphaBehavior") Core.<*> x Core..:? "colorSpace"
                    Core.<*> x Core..:? "colorSpaceUsage"
                    Core.<*> x Core..:? "hdr10Metadata"
                    Core.<*> x Core..:? "pid"
                    Core.<*> x Core..:? "programNumber"
                    Core.<*> x Core..:? "rotate"
