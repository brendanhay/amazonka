{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoSelector where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AlphaBehavior
import Network.AWS.MediaConvert.Types.ColorSpace
import Network.AWS.MediaConvert.Types.ColorSpaceUsage
import Network.AWS.MediaConvert.Types.Hdr10Metadata
import Network.AWS.MediaConvert.Types.InputRotate
import qualified Network.AWS.Prelude as Prelude

-- | Selector for video.
--
-- /See:/ 'newVideoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { -- | There are two sources for color metadata, the input file and the job
    -- input settings Color space (ColorSpace) and HDR master display
    -- information settings(Hdr10Metadata). The Color space usage setting
    -- determines which takes precedence. Choose Force (FORCE) to use color
    -- metadata from the input job settings. If you don\'t specify values for
    -- those settings, the service defaults to using metadata from your input.
    -- FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the
    -- source when it is present. If there\'s no color metadata in your input
    -- file, the service defaults to using values you specify in the input
    -- settings.
    colorSpaceUsage :: Prelude.Maybe ColorSpaceUsage,
    -- | Use these settings to provide HDR 10 metadata that is missing or
    -- inaccurate in your input video. Appropriate values vary depending on the
    -- input video and must be provided by a color grader. The color grader
    -- generates these values during the HDR 10 mastering process. The valid
    -- range for each of these settings is 0 to 50,000. Each increment
    -- represents 0.00002 in CIE1931 color coordinate. Related settings - When
    -- you specify these values, you must also set Color space (ColorSpace) to
    -- HDR 10 (HDR10). To specify whether the the values you specify here take
    -- precedence over the values in the metadata of your input file, set Color
    -- space usage (ColorSpaceUsage). To specify whether color metadata is
    -- included in an output, set Color metadata (ColorMetadata). For more
    -- information about MediaConvert HDR jobs, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
    hdr10Metadata :: Prelude.Maybe Hdr10Metadata,
    -- | Selects a specific program from within a multi-program transport stream.
    -- Note that Quad 4K is not currently supported.
    programNumber :: Prelude.Maybe Prelude.Int,
    -- | Use Rotate (InputRotate) to specify how the service rotates your video.
    -- You can choose automatic rotation or specify a rotation. You can specify
    -- a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video
    -- container is .mov or .mp4 and your input has rotation metadata, you can
    -- choose Automatic to have the service rotate your video according to the
    -- rotation specified in the metadata. The rotation must be within one
    -- degree of 90, 180, or 270 degrees. If the rotation metadata specifies
    -- any other rotation, the service will default to no rotation. By default,
    -- the service does no rotation, even if your input video has rotation
    -- metadata. The service doesn\'t pass through rotation metadata.
    rotate :: Prelude.Maybe InputRotate,
    -- | If your input video has accurate color space metadata, or if you don\'t
    -- know about color space, leave this set to the default value Follow
    -- (FOLLOW). The service will automatically detect your input color space.
    -- If your input video has metadata indicating the wrong color space,
    -- specify the accurate color space here. If your input video is HDR 10 and
    -- the SMPTE ST 2086 Mastering Display Color Volume static metadata isn\'t
    -- present in your video stream, or if that metadata is present but not
    -- accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct
    -- values in the input HDR 10 metadata (Hdr10Metadata) settings. For more
    -- information about MediaConvert HDR jobs, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
    colorSpace :: Prelude.Maybe ColorSpace,
    -- | Ignore this setting unless this input is a QuickTime animation with an
    -- alpha channel. Use this setting to create separate Key and Fill outputs.
    -- In each output, specify which part of the input MediaConvert uses. Leave
    -- this setting at the default value DISCARD to delete the alpha channel
    -- and preserve the video. Set it to REMAP_TO_LUMA to delete the video and
    -- map the alpha channel to the luma channel of your outputs.
    alphaBehavior :: Prelude.Maybe AlphaBehavior,
    -- | Use PID (Pid) to select specific video data from an input file. Specify
    -- this value as an integer; the system automatically converts it to the
    -- hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet
    -- identifier, is an identifier for a set of data in an MPEG-2 transport
    -- stream container.
    pid :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VideoSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colorSpaceUsage', 'videoSelector_colorSpaceUsage' - There are two sources for color metadata, the input file and the job
-- input settings Color space (ColorSpace) and HDR master display
-- information settings(Hdr10Metadata). The Color space usage setting
-- determines which takes precedence. Choose Force (FORCE) to use color
-- metadata from the input job settings. If you don\'t specify values for
-- those settings, the service defaults to using metadata from your input.
-- FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the
-- source when it is present. If there\'s no color metadata in your input
-- file, the service defaults to using values you specify in the input
-- settings.
--
-- 'hdr10Metadata', 'videoSelector_hdr10Metadata' - Use these settings to provide HDR 10 metadata that is missing or
-- inaccurate in your input video. Appropriate values vary depending on the
-- input video and must be provided by a color grader. The color grader
-- generates these values during the HDR 10 mastering process. The valid
-- range for each of these settings is 0 to 50,000. Each increment
-- represents 0.00002 in CIE1931 color coordinate. Related settings - When
-- you specify these values, you must also set Color space (ColorSpace) to
-- HDR 10 (HDR10). To specify whether the the values you specify here take
-- precedence over the values in the metadata of your input file, set Color
-- space usage (ColorSpaceUsage). To specify whether color metadata is
-- included in an output, set Color metadata (ColorMetadata). For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
--
-- 'programNumber', 'videoSelector_programNumber' - Selects a specific program from within a multi-program transport stream.
-- Note that Quad 4K is not currently supported.
--
-- 'rotate', 'videoSelector_rotate' - Use Rotate (InputRotate) to specify how the service rotates your video.
-- You can choose automatic rotation or specify a rotation. You can specify
-- a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video
-- container is .mov or .mp4 and your input has rotation metadata, you can
-- choose Automatic to have the service rotate your video according to the
-- rotation specified in the metadata. The rotation must be within one
-- degree of 90, 180, or 270 degrees. If the rotation metadata specifies
-- any other rotation, the service will default to no rotation. By default,
-- the service does no rotation, even if your input video has rotation
-- metadata. The service doesn\'t pass through rotation metadata.
--
-- 'colorSpace', 'videoSelector_colorSpace' - If your input video has accurate color space metadata, or if you don\'t
-- know about color space, leave this set to the default value Follow
-- (FOLLOW). The service will automatically detect your input color space.
-- If your input video has metadata indicating the wrong color space,
-- specify the accurate color space here. If your input video is HDR 10 and
-- the SMPTE ST 2086 Mastering Display Color Volume static metadata isn\'t
-- present in your video stream, or if that metadata is present but not
-- accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct
-- values in the input HDR 10 metadata (Hdr10Metadata) settings. For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
--
-- 'alphaBehavior', 'videoSelector_alphaBehavior' - Ignore this setting unless this input is a QuickTime animation with an
-- alpha channel. Use this setting to create separate Key and Fill outputs.
-- In each output, specify which part of the input MediaConvert uses. Leave
-- this setting at the default value DISCARD to delete the alpha channel
-- and preserve the video. Set it to REMAP_TO_LUMA to delete the video and
-- map the alpha channel to the luma channel of your outputs.
--
-- 'pid', 'videoSelector_pid' - Use PID (Pid) to select specific video data from an input file. Specify
-- this value as an integer; the system automatically converts it to the
-- hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet
-- identifier, is an identifier for a set of data in an MPEG-2 transport
-- stream container.
newVideoSelector ::
  VideoSelector
newVideoSelector =
  VideoSelector'
    { colorSpaceUsage = Prelude.Nothing,
      hdr10Metadata = Prelude.Nothing,
      programNumber = Prelude.Nothing,
      rotate = Prelude.Nothing,
      colorSpace = Prelude.Nothing,
      alphaBehavior = Prelude.Nothing,
      pid = Prelude.Nothing
    }

-- | There are two sources for color metadata, the input file and the job
-- input settings Color space (ColorSpace) and HDR master display
-- information settings(Hdr10Metadata). The Color space usage setting
-- determines which takes precedence. Choose Force (FORCE) to use color
-- metadata from the input job settings. If you don\'t specify values for
-- those settings, the service defaults to using metadata from your input.
-- FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the
-- source when it is present. If there\'s no color metadata in your input
-- file, the service defaults to using values you specify in the input
-- settings.
videoSelector_colorSpaceUsage :: Lens.Lens' VideoSelector (Prelude.Maybe ColorSpaceUsage)
videoSelector_colorSpaceUsage = Lens.lens (\VideoSelector' {colorSpaceUsage} -> colorSpaceUsage) (\s@VideoSelector' {} a -> s {colorSpaceUsage = a} :: VideoSelector)

-- | Use these settings to provide HDR 10 metadata that is missing or
-- inaccurate in your input video. Appropriate values vary depending on the
-- input video and must be provided by a color grader. The color grader
-- generates these values during the HDR 10 mastering process. The valid
-- range for each of these settings is 0 to 50,000. Each increment
-- represents 0.00002 in CIE1931 color coordinate. Related settings - When
-- you specify these values, you must also set Color space (ColorSpace) to
-- HDR 10 (HDR10). To specify whether the the values you specify here take
-- precedence over the values in the metadata of your input file, set Color
-- space usage (ColorSpaceUsage). To specify whether color metadata is
-- included in an output, set Color metadata (ColorMetadata). For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
videoSelector_hdr10Metadata :: Lens.Lens' VideoSelector (Prelude.Maybe Hdr10Metadata)
videoSelector_hdr10Metadata = Lens.lens (\VideoSelector' {hdr10Metadata} -> hdr10Metadata) (\s@VideoSelector' {} a -> s {hdr10Metadata = a} :: VideoSelector)

-- | Selects a specific program from within a multi-program transport stream.
-- Note that Quad 4K is not currently supported.
videoSelector_programNumber :: Lens.Lens' VideoSelector (Prelude.Maybe Prelude.Int)
videoSelector_programNumber = Lens.lens (\VideoSelector' {programNumber} -> programNumber) (\s@VideoSelector' {} a -> s {programNumber = a} :: VideoSelector)

-- | Use Rotate (InputRotate) to specify how the service rotates your video.
-- You can choose automatic rotation or specify a rotation. You can specify
-- a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video
-- container is .mov or .mp4 and your input has rotation metadata, you can
-- choose Automatic to have the service rotate your video according to the
-- rotation specified in the metadata. The rotation must be within one
-- degree of 90, 180, or 270 degrees. If the rotation metadata specifies
-- any other rotation, the service will default to no rotation. By default,
-- the service does no rotation, even if your input video has rotation
-- metadata. The service doesn\'t pass through rotation metadata.
videoSelector_rotate :: Lens.Lens' VideoSelector (Prelude.Maybe InputRotate)
videoSelector_rotate = Lens.lens (\VideoSelector' {rotate} -> rotate) (\s@VideoSelector' {} a -> s {rotate = a} :: VideoSelector)

-- | If your input video has accurate color space metadata, or if you don\'t
-- know about color space, leave this set to the default value Follow
-- (FOLLOW). The service will automatically detect your input color space.
-- If your input video has metadata indicating the wrong color space,
-- specify the accurate color space here. If your input video is HDR 10 and
-- the SMPTE ST 2086 Mastering Display Color Volume static metadata isn\'t
-- present in your video stream, or if that metadata is present but not
-- accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct
-- values in the input HDR 10 metadata (Hdr10Metadata) settings. For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
videoSelector_colorSpace :: Lens.Lens' VideoSelector (Prelude.Maybe ColorSpace)
videoSelector_colorSpace = Lens.lens (\VideoSelector' {colorSpace} -> colorSpace) (\s@VideoSelector' {} a -> s {colorSpace = a} :: VideoSelector)

-- | Ignore this setting unless this input is a QuickTime animation with an
-- alpha channel. Use this setting to create separate Key and Fill outputs.
-- In each output, specify which part of the input MediaConvert uses. Leave
-- this setting at the default value DISCARD to delete the alpha channel
-- and preserve the video. Set it to REMAP_TO_LUMA to delete the video and
-- map the alpha channel to the luma channel of your outputs.
videoSelector_alphaBehavior :: Lens.Lens' VideoSelector (Prelude.Maybe AlphaBehavior)
videoSelector_alphaBehavior = Lens.lens (\VideoSelector' {alphaBehavior} -> alphaBehavior) (\s@VideoSelector' {} a -> s {alphaBehavior = a} :: VideoSelector)

-- | Use PID (Pid) to select specific video data from an input file. Specify
-- this value as an integer; the system automatically converts it to the
-- hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet
-- identifier, is an identifier for a set of data in an MPEG-2 transport
-- stream container.
videoSelector_pid :: Lens.Lens' VideoSelector (Prelude.Maybe Prelude.Natural)
videoSelector_pid = Lens.lens (\VideoSelector' {pid} -> pid) (\s@VideoSelector' {} a -> s {pid = a} :: VideoSelector)

instance Prelude.FromJSON VideoSelector where
  parseJSON =
    Prelude.withObject
      "VideoSelector"
      ( \x ->
          VideoSelector'
            Prelude.<$> (x Prelude..:? "colorSpaceUsage")
            Prelude.<*> (x Prelude..:? "hdr10Metadata")
            Prelude.<*> (x Prelude..:? "programNumber")
            Prelude.<*> (x Prelude..:? "rotate")
            Prelude.<*> (x Prelude..:? "colorSpace")
            Prelude.<*> (x Prelude..:? "alphaBehavior")
            Prelude.<*> (x Prelude..:? "pid")
      )

instance Prelude.Hashable VideoSelector

instance Prelude.NFData VideoSelector

instance Prelude.ToJSON VideoSelector where
  toJSON VideoSelector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("colorSpaceUsage" Prelude..=)
              Prelude.<$> colorSpaceUsage,
            ("hdr10Metadata" Prelude..=)
              Prelude.<$> hdr10Metadata,
            ("programNumber" Prelude..=)
              Prelude.<$> programNumber,
            ("rotate" Prelude..=) Prelude.<$> rotate,
            ("colorSpace" Prelude..=) Prelude.<$> colorSpace,
            ("alphaBehavior" Prelude..=)
              Prelude.<$> alphaBehavior,
            ("pid" Prelude..=) Prelude.<$> pid
          ]
      )
