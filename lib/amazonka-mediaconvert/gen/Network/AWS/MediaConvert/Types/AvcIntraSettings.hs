{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AvcIntraClass
import Network.AWS.MediaConvert.Types.AvcIntraFramerateControl
import Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.AvcIntraInterlaceMode
import Network.AWS.MediaConvert.Types.AvcIntraSlowPal
import Network.AWS.MediaConvert.Types.AvcIntraTelecine
import Network.AWS.Prelude

-- | Required when you set your output video codec to AVC-Intra. For more information about the AVC-I settings, see the relevant specification. For detailed information about SD and HD in AVC-I, see https://ieeexplore.ieee.org/document/7290936.
--
-- /See:/ 'avcIntraSettings' smart constructor.
data AvcIntraSettings = AvcIntraSettings'
  { _aisSlowPal ::
      !(Maybe AvcIntraSlowPal),
    _aisTelecine :: !(Maybe AvcIntraTelecine),
    _aisInterlaceMode :: !(Maybe AvcIntraInterlaceMode),
    _aisAvcIntraClass :: !(Maybe AvcIntraClass),
    _aisFramerateDenominator :: !(Maybe Nat),
    _aisFramerateConversionAlgorithm ::
      !(Maybe AvcIntraFramerateConversionAlgorithm),
    _aisFramerateControl :: !(Maybe AvcIntraFramerateControl),
    _aisFramerateNumerator :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvcIntraSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aisSlowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- * 'aisTelecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- * 'aisInterlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- * 'aisAvcIntraClass' - Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
--
-- * 'aisFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'aisFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'aisFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'aisFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
avcIntraSettings ::
  AvcIntraSettings
avcIntraSettings =
  AvcIntraSettings'
    { _aisSlowPal = Nothing,
      _aisTelecine = Nothing,
      _aisInterlaceMode = Nothing,
      _aisAvcIntraClass = Nothing,
      _aisFramerateDenominator = Nothing,
      _aisFramerateConversionAlgorithm = Nothing,
      _aisFramerateControl = Nothing,
      _aisFramerateNumerator = Nothing
    }

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
aisSlowPal :: Lens' AvcIntraSettings (Maybe AvcIntraSlowPal)
aisSlowPal = lens _aisSlowPal (\s a -> s {_aisSlowPal = a})

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
aisTelecine :: Lens' AvcIntraSettings (Maybe AvcIntraTelecine)
aisTelecine = lens _aisTelecine (\s a -> s {_aisTelecine = a})

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
aisInterlaceMode :: Lens' AvcIntraSettings (Maybe AvcIntraInterlaceMode)
aisInterlaceMode = lens _aisInterlaceMode (\s a -> s {_aisInterlaceMode = a})

-- | Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
aisAvcIntraClass :: Lens' AvcIntraSettings (Maybe AvcIntraClass)
aisAvcIntraClass = lens _aisAvcIntraClass (\s a -> s {_aisAvcIntraClass = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
aisFramerateDenominator :: Lens' AvcIntraSettings (Maybe Natural)
aisFramerateDenominator = lens _aisFramerateDenominator (\s a -> s {_aisFramerateDenominator = a}) . mapping _Nat

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
aisFramerateConversionAlgorithm :: Lens' AvcIntraSettings (Maybe AvcIntraFramerateConversionAlgorithm)
aisFramerateConversionAlgorithm = lens _aisFramerateConversionAlgorithm (\s a -> s {_aisFramerateConversionAlgorithm = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
aisFramerateControl :: Lens' AvcIntraSettings (Maybe AvcIntraFramerateControl)
aisFramerateControl = lens _aisFramerateControl (\s a -> s {_aisFramerateControl = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
aisFramerateNumerator :: Lens' AvcIntraSettings (Maybe Natural)
aisFramerateNumerator = lens _aisFramerateNumerator (\s a -> s {_aisFramerateNumerator = a}) . mapping _Nat

instance FromJSON AvcIntraSettings where
  parseJSON =
    withObject
      "AvcIntraSettings"
      ( \x ->
          AvcIntraSettings'
            <$> (x .:? "slowPal")
            <*> (x .:? "telecine")
            <*> (x .:? "interlaceMode")
            <*> (x .:? "avcIntraClass")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "framerateConversionAlgorithm")
            <*> (x .:? "framerateControl")
            <*> (x .:? "framerateNumerator")
      )

instance Hashable AvcIntraSettings

instance NFData AvcIntraSettings

instance ToJSON AvcIntraSettings where
  toJSON AvcIntraSettings' {..} =
    object
      ( catMaybes
          [ ("slowPal" .=) <$> _aisSlowPal,
            ("telecine" .=) <$> _aisTelecine,
            ("interlaceMode" .=) <$> _aisInterlaceMode,
            ("avcIntraClass" .=) <$> _aisAvcIntraClass,
            ("framerateDenominator" .=) <$> _aisFramerateDenominator,
            ("framerateConversionAlgorithm" .=)
              <$> _aisFramerateConversionAlgorithm,
            ("framerateControl" .=) <$> _aisFramerateControl,
            ("framerateNumerator" .=) <$> _aisFramerateNumerator
          ]
      )
