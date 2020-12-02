{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Vc3Class
import Network.AWS.MediaConvert.Types.Vc3FramerateControl
import Network.AWS.MediaConvert.Types.Vc3FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vc3InterlaceMode
import Network.AWS.MediaConvert.Types.Vc3SlowPal
import Network.AWS.MediaConvert.Types.Vc3Telecine
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VC3
--
-- /See:/ 'vc3Settings' smart constructor.
data Vc3Settings = Vc3Settings'
  { _vssSlowPal :: !(Maybe Vc3SlowPal),
    _vssTelecine :: !(Maybe Vc3Telecine),
    _vssInterlaceMode :: !(Maybe Vc3InterlaceMode),
    _vssFramerateDenominator :: !(Maybe Nat),
    _vssVc3Class :: !(Maybe Vc3Class),
    _vssFramerateConversionAlgorithm ::
      !(Maybe Vc3FramerateConversionAlgorithm),
    _vssFramerateControl :: !(Maybe Vc3FramerateControl),
    _vssFramerateNumerator :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Vc3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vssSlowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- * 'vssTelecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- * 'vssInterlaceMode' - Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
--
-- * 'vssFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'vssVc3Class' - Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
--
-- * 'vssFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'vssFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'vssFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
vc3Settings ::
  Vc3Settings
vc3Settings =
  Vc3Settings'
    { _vssSlowPal = Nothing,
      _vssTelecine = Nothing,
      _vssInterlaceMode = Nothing,
      _vssFramerateDenominator = Nothing,
      _vssVc3Class = Nothing,
      _vssFramerateConversionAlgorithm = Nothing,
      _vssFramerateControl = Nothing,
      _vssFramerateNumerator = Nothing
    }

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
vssSlowPal :: Lens' Vc3Settings (Maybe Vc3SlowPal)
vssSlowPal = lens _vssSlowPal (\s a -> s {_vssSlowPal = a})

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
vssTelecine :: Lens' Vc3Settings (Maybe Vc3Telecine)
vssTelecine = lens _vssTelecine (\s a -> s {_vssTelecine = a})

-- | Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
vssInterlaceMode :: Lens' Vc3Settings (Maybe Vc3InterlaceMode)
vssInterlaceMode = lens _vssInterlaceMode (\s a -> s {_vssInterlaceMode = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
vssFramerateDenominator :: Lens' Vc3Settings (Maybe Natural)
vssFramerateDenominator = lens _vssFramerateDenominator (\s a -> s {_vssFramerateDenominator = a}) . mapping _Nat

-- | Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
vssVc3Class :: Lens' Vc3Settings (Maybe Vc3Class)
vssVc3Class = lens _vssVc3Class (\s a -> s {_vssVc3Class = a})

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
vssFramerateConversionAlgorithm :: Lens' Vc3Settings (Maybe Vc3FramerateConversionAlgorithm)
vssFramerateConversionAlgorithm = lens _vssFramerateConversionAlgorithm (\s a -> s {_vssFramerateConversionAlgorithm = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
vssFramerateControl :: Lens' Vc3Settings (Maybe Vc3FramerateControl)
vssFramerateControl = lens _vssFramerateControl (\s a -> s {_vssFramerateControl = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
vssFramerateNumerator :: Lens' Vc3Settings (Maybe Natural)
vssFramerateNumerator = lens _vssFramerateNumerator (\s a -> s {_vssFramerateNumerator = a}) . mapping _Nat

instance FromJSON Vc3Settings where
  parseJSON =
    withObject
      "Vc3Settings"
      ( \x ->
          Vc3Settings'
            <$> (x .:? "slowPal")
            <*> (x .:? "telecine")
            <*> (x .:? "interlaceMode")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "vc3Class")
            <*> (x .:? "framerateConversionAlgorithm")
            <*> (x .:? "framerateControl")
            <*> (x .:? "framerateNumerator")
      )

instance Hashable Vc3Settings

instance NFData Vc3Settings

instance ToJSON Vc3Settings where
  toJSON Vc3Settings' {..} =
    object
      ( catMaybes
          [ ("slowPal" .=) <$> _vssSlowPal,
            ("telecine" .=) <$> _vssTelecine,
            ("interlaceMode" .=) <$> _vssInterlaceMode,
            ("framerateDenominator" .=) <$> _vssFramerateDenominator,
            ("vc3Class" .=) <$> _vssVc3Class,
            ("framerateConversionAlgorithm" .=)
              <$> _vssFramerateConversionAlgorithm,
            ("framerateControl" .=) <$> _vssFramerateControl,
            ("framerateNumerator" .=) <$> _vssFramerateNumerator
          ]
      )
