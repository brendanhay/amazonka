{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoSelector where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AlphaBehavior
import Network.AWS.MediaConvert.Types.ColorSpace
import Network.AWS.MediaConvert.Types.ColorSpaceUsage
import Network.AWS.MediaConvert.Types.Hdr10Metadata
import Network.AWS.MediaConvert.Types.InputRotate
import Network.AWS.Prelude

-- | Selector for video.
--
-- /See:/ 'videoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { _vsProgramNumber ::
      !(Maybe Int),
    _vsAlphaBehavior :: !(Maybe AlphaBehavior),
    _vsColorSpaceUsage :: !(Maybe ColorSpaceUsage),
    _vsHdr10Metadata :: !(Maybe Hdr10Metadata),
    _vsPid :: !(Maybe Nat),
    _vsRotate :: !(Maybe InputRotate),
    _vsColorSpace :: !(Maybe ColorSpace)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsProgramNumber' - Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
--
-- * 'vsAlphaBehavior' - Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
--
-- * 'vsColorSpaceUsage' - There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
--
-- * 'vsHdr10Metadata' - Use these settings to provide HDR 10 metadata that is missing or inaccurate in your input video. Appropriate values vary depending on the input video and must be provided by a color grader. The color grader generates these values during the HDR 10 mastering process. The valid range for each of these settings is 0 to 50,000. Each increment represents 0.00002 in CIE1931 color coordinate. Related settings - When you specify these values, you must also set Color space (ColorSpace) to HDR 10 (HDR10). To specify whether the the values you specify here take precedence over the values in the metadata of your input file, set Color space usage (ColorSpaceUsage). To specify whether color metadata is included in an output, set Color metadata (ColorMetadata). For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
--
-- * 'vsPid' - Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
--
-- * 'vsRotate' - Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
--
-- * 'vsColorSpace' - If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
videoSelector ::
  VideoSelector
videoSelector =
  VideoSelector'
    { _vsProgramNumber = Nothing,
      _vsAlphaBehavior = Nothing,
      _vsColorSpaceUsage = Nothing,
      _vsHdr10Metadata = Nothing,
      _vsPid = Nothing,
      _vsRotate = Nothing,
      _vsColorSpace = Nothing
    }

-- | Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
vsProgramNumber :: Lens' VideoSelector (Maybe Int)
vsProgramNumber = lens _vsProgramNumber (\s a -> s {_vsProgramNumber = a})

-- | Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
vsAlphaBehavior :: Lens' VideoSelector (Maybe AlphaBehavior)
vsAlphaBehavior = lens _vsAlphaBehavior (\s a -> s {_vsAlphaBehavior = a})

-- | There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
vsColorSpaceUsage :: Lens' VideoSelector (Maybe ColorSpaceUsage)
vsColorSpaceUsage = lens _vsColorSpaceUsage (\s a -> s {_vsColorSpaceUsage = a})

-- | Use these settings to provide HDR 10 metadata that is missing or inaccurate in your input video. Appropriate values vary depending on the input video and must be provided by a color grader. The color grader generates these values during the HDR 10 mastering process. The valid range for each of these settings is 0 to 50,000. Each increment represents 0.00002 in CIE1931 color coordinate. Related settings - When you specify these values, you must also set Color space (ColorSpace) to HDR 10 (HDR10). To specify whether the the values you specify here take precedence over the values in the metadata of your input file, set Color space usage (ColorSpaceUsage). To specify whether color metadata is included in an output, set Color metadata (ColorMetadata). For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
vsHdr10Metadata :: Lens' VideoSelector (Maybe Hdr10Metadata)
vsHdr10Metadata = lens _vsHdr10Metadata (\s a -> s {_vsHdr10Metadata = a})

-- | Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
vsPid :: Lens' VideoSelector (Maybe Natural)
vsPid = lens _vsPid (\s a -> s {_vsPid = a}) . mapping _Nat

-- | Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
vsRotate :: Lens' VideoSelector (Maybe InputRotate)
vsRotate = lens _vsRotate (\s a -> s {_vsRotate = a})

-- | If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
vsColorSpace :: Lens' VideoSelector (Maybe ColorSpace)
vsColorSpace = lens _vsColorSpace (\s a -> s {_vsColorSpace = a})

instance FromJSON VideoSelector where
  parseJSON =
    withObject
      "VideoSelector"
      ( \x ->
          VideoSelector'
            <$> (x .:? "programNumber")
            <*> (x .:? "alphaBehavior")
            <*> (x .:? "colorSpaceUsage")
            <*> (x .:? "hdr10Metadata")
            <*> (x .:? "pid")
            <*> (x .:? "rotate")
            <*> (x .:? "colorSpace")
      )

instance Hashable VideoSelector

instance NFData VideoSelector

instance ToJSON VideoSelector where
  toJSON VideoSelector' {..} =
    object
      ( catMaybes
          [ ("programNumber" .=) <$> _vsProgramNumber,
            ("alphaBehavior" .=) <$> _vsAlphaBehavior,
            ("colorSpaceUsage" .=) <$> _vsColorSpaceUsage,
            ("hdr10Metadata" .=) <$> _vsHdr10Metadata,
            ("pid" .=) <$> _vsPid,
            ("rotate" .=) <$> _vsRotate,
            ("colorSpace" .=) <$> _vsColorSpace
          ]
      )
