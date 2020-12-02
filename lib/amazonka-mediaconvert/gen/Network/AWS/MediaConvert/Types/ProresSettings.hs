{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.ProresCodecProfile
import Network.AWS.MediaConvert.Types.ProresFramerateControl
import Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.ProresInterlaceMode
import Network.AWS.MediaConvert.Types.ProresParControl
import Network.AWS.MediaConvert.Types.ProresSlowPal
import Network.AWS.MediaConvert.Types.ProresTelecine
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
--
-- /See:/ 'proresSettings' smart constructor.
data ProresSettings = ProresSettings'
  { _psSlowPal ::
      !(Maybe ProresSlowPal),
    _psParNumerator :: !(Maybe Nat),
    _psTelecine :: !(Maybe ProresTelecine),
    _psInterlaceMode :: !(Maybe ProresInterlaceMode),
    _psParControl :: !(Maybe ProresParControl),
    _psCodecProfile :: !(Maybe ProresCodecProfile),
    _psFramerateDenominator :: !(Maybe Nat),
    _psFramerateConversionAlgorithm ::
      !(Maybe ProresFramerateConversionAlgorithm),
    _psFramerateControl :: !(Maybe ProresFramerateControl),
    _psFramerateNumerator :: !(Maybe Nat),
    _psParDenominator :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProresSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psSlowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- * 'psParNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- * 'psTelecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- * 'psInterlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- * 'psParControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- * 'psCodecProfile' - Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
--
-- * 'psFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'psFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'psFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'psFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'psParDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
proresSettings ::
  ProresSettings
proresSettings =
  ProresSettings'
    { _psSlowPal = Nothing,
      _psParNumerator = Nothing,
      _psTelecine = Nothing,
      _psInterlaceMode = Nothing,
      _psParControl = Nothing,
      _psCodecProfile = Nothing,
      _psFramerateDenominator = Nothing,
      _psFramerateConversionAlgorithm = Nothing,
      _psFramerateControl = Nothing,
      _psFramerateNumerator = Nothing,
      _psParDenominator = Nothing
    }

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
psSlowPal :: Lens' ProresSettings (Maybe ProresSlowPal)
psSlowPal = lens _psSlowPal (\s a -> s {_psSlowPal = a})

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
psParNumerator :: Lens' ProresSettings (Maybe Natural)
psParNumerator = lens _psParNumerator (\s a -> s {_psParNumerator = a}) . mapping _Nat

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
psTelecine :: Lens' ProresSettings (Maybe ProresTelecine)
psTelecine = lens _psTelecine (\s a -> s {_psTelecine = a})

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
psInterlaceMode :: Lens' ProresSettings (Maybe ProresInterlaceMode)
psInterlaceMode = lens _psInterlaceMode (\s a -> s {_psInterlaceMode = a})

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
psParControl :: Lens' ProresSettings (Maybe ProresParControl)
psParControl = lens _psParControl (\s a -> s {_psParControl = a})

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
psCodecProfile :: Lens' ProresSettings (Maybe ProresCodecProfile)
psCodecProfile = lens _psCodecProfile (\s a -> s {_psCodecProfile = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
psFramerateDenominator :: Lens' ProresSettings (Maybe Natural)
psFramerateDenominator = lens _psFramerateDenominator (\s a -> s {_psFramerateDenominator = a}) . mapping _Nat

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
psFramerateConversionAlgorithm :: Lens' ProresSettings (Maybe ProresFramerateConversionAlgorithm)
psFramerateConversionAlgorithm = lens _psFramerateConversionAlgorithm (\s a -> s {_psFramerateConversionAlgorithm = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
psFramerateControl :: Lens' ProresSettings (Maybe ProresFramerateControl)
psFramerateControl = lens _psFramerateControl (\s a -> s {_psFramerateControl = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
psFramerateNumerator :: Lens' ProresSettings (Maybe Natural)
psFramerateNumerator = lens _psFramerateNumerator (\s a -> s {_psFramerateNumerator = a}) . mapping _Nat

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
psParDenominator :: Lens' ProresSettings (Maybe Natural)
psParDenominator = lens _psParDenominator (\s a -> s {_psParDenominator = a}) . mapping _Nat

instance FromJSON ProresSettings where
  parseJSON =
    withObject
      "ProresSettings"
      ( \x ->
          ProresSettings'
            <$> (x .:? "slowPal")
            <*> (x .:? "parNumerator")
            <*> (x .:? "telecine")
            <*> (x .:? "interlaceMode")
            <*> (x .:? "parControl")
            <*> (x .:? "codecProfile")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "framerateConversionAlgorithm")
            <*> (x .:? "framerateControl")
            <*> (x .:? "framerateNumerator")
            <*> (x .:? "parDenominator")
      )

instance Hashable ProresSettings

instance NFData ProresSettings

instance ToJSON ProresSettings where
  toJSON ProresSettings' {..} =
    object
      ( catMaybes
          [ ("slowPal" .=) <$> _psSlowPal,
            ("parNumerator" .=) <$> _psParNumerator,
            ("telecine" .=) <$> _psTelecine,
            ("interlaceMode" .=) <$> _psInterlaceMode,
            ("parControl" .=) <$> _psParControl,
            ("codecProfile" .=) <$> _psCodecProfile,
            ("framerateDenominator" .=) <$> _psFramerateDenominator,
            ("framerateConversionAlgorithm" .=)
              <$> _psFramerateConversionAlgorithm,
            ("framerateControl" .=) <$> _psFramerateControl,
            ("framerateNumerator" .=) <$> _psFramerateNumerator,
            ("parDenominator" .=) <$> _psParDenominator
          ]
      )
