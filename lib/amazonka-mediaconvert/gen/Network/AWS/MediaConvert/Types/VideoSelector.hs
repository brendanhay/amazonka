{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoSelector
  ( VideoSelector (..),

    -- * Smart constructor
    mkVideoSelector,

    -- * Lenses
    vsProgramNumber,
    vsAlphaBehavior,
    vsColorSpaceUsage,
    vsHdr10Metadata,
    vsPid,
    vsRotate,
    vsColorSpace,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AlphaBehavior
import Network.AWS.MediaConvert.Types.ColorSpace
import Network.AWS.MediaConvert.Types.ColorSpaceUsage
import Network.AWS.MediaConvert.Types.Hdr10Metadata
import Network.AWS.MediaConvert.Types.InputRotate
import qualified Network.AWS.Prelude as Lude

-- | Selector for video.
--
-- /See:/ 'mkVideoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { -- | Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
    programNumber :: Lude.Maybe Lude.Int,
    -- | Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
    alphaBehavior :: Lude.Maybe AlphaBehavior,
    -- | There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
    colorSpaceUsage :: Lude.Maybe ColorSpaceUsage,
    -- | Use these settings to provide HDR 10 metadata that is missing or inaccurate in your input video. Appropriate values vary depending on the input video and must be provided by a color grader. The color grader generates these values during the HDR 10 mastering process. The valid range for each of these settings is 0 to 50,000. Each increment represents 0.00002 in CIE1931 color coordinate. Related settings - When you specify these values, you must also set Color space (ColorSpace) to HDR 10 (HDR10). To specify whether the the values you specify here take precedence over the values in the metadata of your input file, set Color space usage (ColorSpaceUsage). To specify whether color metadata is included in an output, set Color metadata (ColorMetadata). For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
    hdr10Metadata :: Lude.Maybe Hdr10Metadata,
    -- | Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
    pid :: Lude.Maybe Lude.Natural,
    -- | Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
    rotate :: Lude.Maybe InputRotate,
    -- | If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
    colorSpace :: Lude.Maybe ColorSpace
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoSelector' with the minimum fields required to make a request.
--
-- * 'programNumber' - Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
-- * 'alphaBehavior' - Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
-- * 'colorSpaceUsage' - There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
-- * 'hdr10Metadata' - Use these settings to provide HDR 10 metadata that is missing or inaccurate in your input video. Appropriate values vary depending on the input video and must be provided by a color grader. The color grader generates these values during the HDR 10 mastering process. The valid range for each of these settings is 0 to 50,000. Each increment represents 0.00002 in CIE1931 color coordinate. Related settings - When you specify these values, you must also set Color space (ColorSpace) to HDR 10 (HDR10). To specify whether the the values you specify here take precedence over the values in the metadata of your input file, set Color space usage (ColorSpaceUsage). To specify whether color metadata is included in an output, set Color metadata (ColorMetadata). For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
-- * 'pid' - Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
-- * 'rotate' - Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
-- * 'colorSpace' - If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
mkVideoSelector ::
  VideoSelector
mkVideoSelector =
  VideoSelector'
    { programNumber = Lude.Nothing,
      alphaBehavior = Lude.Nothing,
      colorSpaceUsage = Lude.Nothing,
      hdr10Metadata = Lude.Nothing,
      pid = Lude.Nothing,
      rotate = Lude.Nothing,
      colorSpace = Lude.Nothing
    }

-- | Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsProgramNumber :: Lens.Lens' VideoSelector (Lude.Maybe Lude.Int)
vsProgramNumber = Lens.lens (programNumber :: VideoSelector -> Lude.Maybe Lude.Int) (\s a -> s {programNumber = a} :: VideoSelector)
{-# DEPRECATED vsProgramNumber "Use generic-lens or generic-optics with 'programNumber' instead." #-}

-- | Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
--
-- /Note:/ Consider using 'alphaBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsAlphaBehavior :: Lens.Lens' VideoSelector (Lude.Maybe AlphaBehavior)
vsAlphaBehavior = Lens.lens (alphaBehavior :: VideoSelector -> Lude.Maybe AlphaBehavior) (\s a -> s {alphaBehavior = a} :: VideoSelector)
{-# DEPRECATED vsAlphaBehavior "Use generic-lens or generic-optics with 'alphaBehavior' instead." #-}

-- | There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
--
-- /Note:/ Consider using 'colorSpaceUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsColorSpaceUsage :: Lens.Lens' VideoSelector (Lude.Maybe ColorSpaceUsage)
vsColorSpaceUsage = Lens.lens (colorSpaceUsage :: VideoSelector -> Lude.Maybe ColorSpaceUsage) (\s a -> s {colorSpaceUsage = a} :: VideoSelector)
{-# DEPRECATED vsColorSpaceUsage "Use generic-lens or generic-optics with 'colorSpaceUsage' instead." #-}

-- | Use these settings to provide HDR 10 metadata that is missing or inaccurate in your input video. Appropriate values vary depending on the input video and must be provided by a color grader. The color grader generates these values during the HDR 10 mastering process. The valid range for each of these settings is 0 to 50,000. Each increment represents 0.00002 in CIE1931 color coordinate. Related settings - When you specify these values, you must also set Color space (ColorSpace) to HDR 10 (HDR10). To specify whether the the values you specify here take precedence over the values in the metadata of your input file, set Color space usage (ColorSpaceUsage). To specify whether color metadata is included in an output, set Color metadata (ColorMetadata). For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
--
-- /Note:/ Consider using 'hdr10Metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsHdr10Metadata :: Lens.Lens' VideoSelector (Lude.Maybe Hdr10Metadata)
vsHdr10Metadata = Lens.lens (hdr10Metadata :: VideoSelector -> Lude.Maybe Hdr10Metadata) (\s a -> s {hdr10Metadata = a} :: VideoSelector)
{-# DEPRECATED vsHdr10Metadata "Use generic-lens or generic-optics with 'hdr10Metadata' instead." #-}

-- | Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsPid :: Lens.Lens' VideoSelector (Lude.Maybe Lude.Natural)
vsPid = Lens.lens (pid :: VideoSelector -> Lude.Maybe Lude.Natural) (\s a -> s {pid = a} :: VideoSelector)
{-# DEPRECATED vsPid "Use generic-lens or generic-optics with 'pid' instead." #-}

-- | Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
--
-- /Note:/ Consider using 'rotate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsRotate :: Lens.Lens' VideoSelector (Lude.Maybe InputRotate)
vsRotate = Lens.lens (rotate :: VideoSelector -> Lude.Maybe InputRotate) (\s a -> s {rotate = a} :: VideoSelector)
{-# DEPRECATED vsRotate "Use generic-lens or generic-optics with 'rotate' instead." #-}

-- | If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
--
-- /Note:/ Consider using 'colorSpace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsColorSpace :: Lens.Lens' VideoSelector (Lude.Maybe ColorSpace)
vsColorSpace = Lens.lens (colorSpace :: VideoSelector -> Lude.Maybe ColorSpace) (\s a -> s {colorSpace = a} :: VideoSelector)
{-# DEPRECATED vsColorSpace "Use generic-lens or generic-optics with 'colorSpace' instead." #-}

instance Lude.FromJSON VideoSelector where
  parseJSON =
    Lude.withObject
      "VideoSelector"
      ( \x ->
          VideoSelector'
            Lude.<$> (x Lude..:? "programNumber")
            Lude.<*> (x Lude..:? "alphaBehavior")
            Lude.<*> (x Lude..:? "colorSpaceUsage")
            Lude.<*> (x Lude..:? "hdr10Metadata")
            Lude.<*> (x Lude..:? "pid")
            Lude.<*> (x Lude..:? "rotate")
            Lude.<*> (x Lude..:? "colorSpace")
      )

instance Lude.ToJSON VideoSelector where
  toJSON VideoSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("programNumber" Lude..=) Lude.<$> programNumber,
            ("alphaBehavior" Lude..=) Lude.<$> alphaBehavior,
            ("colorSpaceUsage" Lude..=) Lude.<$> colorSpaceUsage,
            ("hdr10Metadata" Lude..=) Lude.<$> hdr10Metadata,
            ("pid" Lude..=) Lude.<$> pid,
            ("rotate" Lude..=) Lude.<$> rotate,
            ("colorSpace" Lude..=) Lude.<$> colorSpace
          ]
      )
