{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputTemplate where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AudioSelector
import Network.AWS.MediaConvert.Types.AudioSelectorGroup
import Network.AWS.MediaConvert.Types.CaptionSelector
import Network.AWS.MediaConvert.Types.ImageInserter
import Network.AWS.MediaConvert.Types.InputClipping
import Network.AWS.MediaConvert.Types.InputDeblockFilter
import Network.AWS.MediaConvert.Types.InputDenoiseFilter
import Network.AWS.MediaConvert.Types.InputFilterEnable
import Network.AWS.MediaConvert.Types.InputPsiControl
import Network.AWS.MediaConvert.Types.InputScanType
import Network.AWS.MediaConvert.Types.InputTimecodeSource
import Network.AWS.MediaConvert.Types.Rectangle
import Network.AWS.MediaConvert.Types.VideoSelector
import Network.AWS.Prelude

-- | Specified video input in a template.
--
-- /See:/ 'inputTemplate' smart constructor.
data InputTemplate = InputTemplate'
  { _itVideoSelector ::
      !(Maybe VideoSelector),
    _itProgramNumber :: !(Maybe Nat),
    _itAudioSelectorGroups ::
      !(Maybe (Map Text (AudioSelectorGroup))),
    _itTimecodeSource :: !(Maybe InputTimecodeSource),
    _itAudioSelectors :: !(Maybe (Map Text (AudioSelector))),
    _itDeblockFilter :: !(Maybe InputDeblockFilter),
    _itInputClippings :: !(Maybe [InputClipping]),
    _itCrop :: !(Maybe Rectangle),
    _itDenoiseFilter :: !(Maybe InputDenoiseFilter),
    _itImageInserter :: !(Maybe ImageInserter),
    _itFilterStrength :: !(Maybe Int),
    _itPsiControl :: !(Maybe InputPsiControl),
    _itCaptionSelectors :: !(Maybe (Map Text (CaptionSelector))),
    _itTimecodeStart :: !(Maybe Text),
    _itInputScanType :: !(Maybe InputScanType),
    _itPosition :: !(Maybe Rectangle),
    _itFilterEnable :: !(Maybe InputFilterEnable)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itVideoSelector' - Selector for video.
--
-- * 'itProgramNumber' - Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- * 'itAudioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- * 'itTimecodeSource' - Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- * 'itAudioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
--
-- * 'itDeblockFilter' - Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
--
-- * 'itInputClippings' - (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- * 'itCrop' - Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
--
-- * 'itDenoiseFilter' - Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
--
-- * 'itImageInserter' - Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
--
-- * 'itFilterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- * 'itPsiControl' - Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
--
-- * 'itCaptionSelectors' - Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
--
-- * 'itTimecodeStart' - Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- * 'itInputScanType' - When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
--
-- * 'itPosition' - Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
--
-- * 'itFilterEnable' - Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
inputTemplate ::
  InputTemplate
inputTemplate =
  InputTemplate'
    { _itVideoSelector = Nothing,
      _itProgramNumber = Nothing,
      _itAudioSelectorGroups = Nothing,
      _itTimecodeSource = Nothing,
      _itAudioSelectors = Nothing,
      _itDeblockFilter = Nothing,
      _itInputClippings = Nothing,
      _itCrop = Nothing,
      _itDenoiseFilter = Nothing,
      _itImageInserter = Nothing,
      _itFilterStrength = Nothing,
      _itPsiControl = Nothing,
      _itCaptionSelectors = Nothing,
      _itTimecodeStart = Nothing,
      _itInputScanType = Nothing,
      _itPosition = Nothing,
      _itFilterEnable = Nothing
    }

-- | Selector for video.
itVideoSelector :: Lens' InputTemplate (Maybe VideoSelector)
itVideoSelector = lens _itVideoSelector (\s a -> s {_itVideoSelector = a})

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
itProgramNumber :: Lens' InputTemplate (Maybe Natural)
itProgramNumber = lens _itProgramNumber (\s a -> s {_itProgramNumber = a}) . mapping _Nat

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
itAudioSelectorGroups :: Lens' InputTemplate (HashMap Text (AudioSelectorGroup))
itAudioSelectorGroups = lens _itAudioSelectorGroups (\s a -> s {_itAudioSelectorGroups = a}) . _Default . _Map

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
itTimecodeSource :: Lens' InputTemplate (Maybe InputTimecodeSource)
itTimecodeSource = lens _itTimecodeSource (\s a -> s {_itTimecodeSource = a})

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
itAudioSelectors :: Lens' InputTemplate (HashMap Text (AudioSelector))
itAudioSelectors = lens _itAudioSelectors (\s a -> s {_itAudioSelectors = a}) . _Default . _Map

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
itDeblockFilter :: Lens' InputTemplate (Maybe InputDeblockFilter)
itDeblockFilter = lens _itDeblockFilter (\s a -> s {_itDeblockFilter = a})

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
itInputClippings :: Lens' InputTemplate [InputClipping]
itInputClippings = lens _itInputClippings (\s a -> s {_itInputClippings = a}) . _Default . _Coerce

-- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
itCrop :: Lens' InputTemplate (Maybe Rectangle)
itCrop = lens _itCrop (\s a -> s {_itCrop = a})

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
itDenoiseFilter :: Lens' InputTemplate (Maybe InputDenoiseFilter)
itDenoiseFilter = lens _itDenoiseFilter (\s a -> s {_itDenoiseFilter = a})

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
itImageInserter :: Lens' InputTemplate (Maybe ImageInserter)
itImageInserter = lens _itImageInserter (\s a -> s {_itImageInserter = a})

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
itFilterStrength :: Lens' InputTemplate (Maybe Int)
itFilterStrength = lens _itFilterStrength (\s a -> s {_itFilterStrength = a})

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
itPsiControl :: Lens' InputTemplate (Maybe InputPsiControl)
itPsiControl = lens _itPsiControl (\s a -> s {_itPsiControl = a})

-- | Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
itCaptionSelectors :: Lens' InputTemplate (HashMap Text (CaptionSelector))
itCaptionSelectors = lens _itCaptionSelectors (\s a -> s {_itCaptionSelectors = a}) . _Default . _Map

-- | Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
itTimecodeStart :: Lens' InputTemplate (Maybe Text)
itTimecodeStart = lens _itTimecodeStart (\s a -> s {_itTimecodeStart = a})

-- | When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
itInputScanType :: Lens' InputTemplate (Maybe InputScanType)
itInputScanType = lens _itInputScanType (\s a -> s {_itInputScanType = a})

-- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
itPosition :: Lens' InputTemplate (Maybe Rectangle)
itPosition = lens _itPosition (\s a -> s {_itPosition = a})

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
itFilterEnable :: Lens' InputTemplate (Maybe InputFilterEnable)
itFilterEnable = lens _itFilterEnable (\s a -> s {_itFilterEnable = a})

instance FromJSON InputTemplate where
  parseJSON =
    withObject
      "InputTemplate"
      ( \x ->
          InputTemplate'
            <$> (x .:? "videoSelector")
            <*> (x .:? "programNumber")
            <*> (x .:? "audioSelectorGroups" .!= mempty)
            <*> (x .:? "timecodeSource")
            <*> (x .:? "audioSelectors" .!= mempty)
            <*> (x .:? "deblockFilter")
            <*> (x .:? "inputClippings" .!= mempty)
            <*> (x .:? "crop")
            <*> (x .:? "denoiseFilter")
            <*> (x .:? "imageInserter")
            <*> (x .:? "filterStrength")
            <*> (x .:? "psiControl")
            <*> (x .:? "captionSelectors" .!= mempty)
            <*> (x .:? "timecodeStart")
            <*> (x .:? "inputScanType")
            <*> (x .:? "position")
            <*> (x .:? "filterEnable")
      )

instance Hashable InputTemplate

instance NFData InputTemplate

instance ToJSON InputTemplate where
  toJSON InputTemplate' {..} =
    object
      ( catMaybes
          [ ("videoSelector" .=) <$> _itVideoSelector,
            ("programNumber" .=) <$> _itProgramNumber,
            ("audioSelectorGroups" .=) <$> _itAudioSelectorGroups,
            ("timecodeSource" .=) <$> _itTimecodeSource,
            ("audioSelectors" .=) <$> _itAudioSelectors,
            ("deblockFilter" .=) <$> _itDeblockFilter,
            ("inputClippings" .=) <$> _itInputClippings,
            ("crop" .=) <$> _itCrop,
            ("denoiseFilter" .=) <$> _itDenoiseFilter,
            ("imageInserter" .=) <$> _itImageInserter,
            ("filterStrength" .=) <$> _itFilterStrength,
            ("psiControl" .=) <$> _itPsiControl,
            ("captionSelectors" .=) <$> _itCaptionSelectors,
            ("timecodeStart" .=) <$> _itTimecodeStart,
            ("inputScanType" .=) <$> _itInputScanType,
            ("position" .=) <$> _itPosition,
            ("filterEnable" .=) <$> _itFilterEnable
          ]
      )
