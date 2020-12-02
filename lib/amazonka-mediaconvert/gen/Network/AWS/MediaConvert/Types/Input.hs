{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Input
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Input where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AudioSelector
import Network.AWS.MediaConvert.Types.AudioSelectorGroup
import Network.AWS.MediaConvert.Types.CaptionSelector
import Network.AWS.MediaConvert.Types.ImageInserter
import Network.AWS.MediaConvert.Types.InputClipping
import Network.AWS.MediaConvert.Types.InputDeblockFilter
import Network.AWS.MediaConvert.Types.InputDecryptionSettings
import Network.AWS.MediaConvert.Types.InputDenoiseFilter
import Network.AWS.MediaConvert.Types.InputFilterEnable
import Network.AWS.MediaConvert.Types.InputPsiControl
import Network.AWS.MediaConvert.Types.InputScanType
import Network.AWS.MediaConvert.Types.InputTimecodeSource
import Network.AWS.MediaConvert.Types.Rectangle
import Network.AWS.MediaConvert.Types.VideoSelector
import Network.AWS.Prelude

-- | Specifies media input
--
-- /See:/ 'input' smart constructor.
data Input = Input'
  { _iVideoSelector :: !(Maybe VideoSelector),
    _iSupplementalImps :: !(Maybe [Text]),
    _iProgramNumber :: !(Maybe Nat),
    _iAudioSelectorGroups :: !(Maybe (Map Text (AudioSelectorGroup))),
    _iTimecodeSource :: !(Maybe InputTimecodeSource),
    _iAudioSelectors :: !(Maybe (Map Text (AudioSelector))),
    _iDecryptionSettings :: !(Maybe InputDecryptionSettings),
    _iDeblockFilter :: !(Maybe InputDeblockFilter),
    _iInputClippings :: !(Maybe [InputClipping]),
    _iCrop :: !(Maybe Rectangle),
    _iDenoiseFilter :: !(Maybe InputDenoiseFilter),
    _iImageInserter :: !(Maybe ImageInserter),
    _iFilterStrength :: !(Maybe Int),
    _iPsiControl :: !(Maybe InputPsiControl),
    _iCaptionSelectors :: !(Maybe (Map Text (CaptionSelector))),
    _iFileInput :: !(Maybe Text),
    _iTimecodeStart :: !(Maybe Text),
    _iInputScanType :: !(Maybe InputScanType),
    _iPosition :: !(Maybe Rectangle),
    _iFilterEnable :: !(Maybe InputFilterEnable)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Input' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iVideoSelector' - Selector for video.
--
-- * 'iSupplementalImps' - Provide a list of any necessary supplemental IMPs. You need supplemental IMPs if the CPL that you're using for your input is in an incomplete IMP. Specify either the supplemental IMP directories with a trailing slash or the ASSETMAP.xml files. For example ["s3://bucket/ov/", "s3://bucket/vf2/ASSETMAP.xml"]. You don't need to specify the IMP that contains your input CPL, because the service automatically detects it.
--
-- * 'iProgramNumber' - Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- * 'iAudioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- * 'iTimecodeSource' - Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- * 'iAudioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
--
-- * 'iDecryptionSettings' - Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
--
-- * 'iDeblockFilter' - Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
--
-- * 'iInputClippings' - (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- * 'iCrop' - Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
--
-- * 'iDenoiseFilter' - Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
--
-- * 'iImageInserter' - Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
--
-- * 'iFilterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- * 'iPsiControl' - Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
--
-- * 'iCaptionSelectors' - Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
--
-- * 'iFileInput' - Specify the source file for your transcoding job. You can use multiple inputs in a single job. The service concatenates these inputs, in the order that you specify them in the job, to create the outputs. If your input format is IMF, specify your input by providing the path to your CPL. For example, "s3://bucket/vf/cpl.xml". If the CPL is in an incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps) to specify any supplemental IMPs that contain assets referenced by the CPL.
--
-- * 'iTimecodeStart' - Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- * 'iInputScanType' - When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
--
-- * 'iPosition' - Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
--
-- * 'iFilterEnable' - Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
input ::
  Input
input =
  Input'
    { _iVideoSelector = Nothing,
      _iSupplementalImps = Nothing,
      _iProgramNumber = Nothing,
      _iAudioSelectorGroups = Nothing,
      _iTimecodeSource = Nothing,
      _iAudioSelectors = Nothing,
      _iDecryptionSettings = Nothing,
      _iDeblockFilter = Nothing,
      _iInputClippings = Nothing,
      _iCrop = Nothing,
      _iDenoiseFilter = Nothing,
      _iImageInserter = Nothing,
      _iFilterStrength = Nothing,
      _iPsiControl = Nothing,
      _iCaptionSelectors = Nothing,
      _iFileInput = Nothing,
      _iTimecodeStart = Nothing,
      _iInputScanType = Nothing,
      _iPosition = Nothing,
      _iFilterEnable = Nothing
    }

-- | Selector for video.
iVideoSelector :: Lens' Input (Maybe VideoSelector)
iVideoSelector = lens _iVideoSelector (\s a -> s {_iVideoSelector = a})

-- | Provide a list of any necessary supplemental IMPs. You need supplemental IMPs if the CPL that you're using for your input is in an incomplete IMP. Specify either the supplemental IMP directories with a trailing slash or the ASSETMAP.xml files. For example ["s3://bucket/ov/", "s3://bucket/vf2/ASSETMAP.xml"]. You don't need to specify the IMP that contains your input CPL, because the service automatically detects it.
iSupplementalImps :: Lens' Input [Text]
iSupplementalImps = lens _iSupplementalImps (\s a -> s {_iSupplementalImps = a}) . _Default . _Coerce

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
iProgramNumber :: Lens' Input (Maybe Natural)
iProgramNumber = lens _iProgramNumber (\s a -> s {_iProgramNumber = a}) . mapping _Nat

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
iAudioSelectorGroups :: Lens' Input (HashMap Text (AudioSelectorGroup))
iAudioSelectorGroups = lens _iAudioSelectorGroups (\s a -> s {_iAudioSelectorGroups = a}) . _Default . _Map

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
iTimecodeSource :: Lens' Input (Maybe InputTimecodeSource)
iTimecodeSource = lens _iTimecodeSource (\s a -> s {_iTimecodeSource = a})

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
iAudioSelectors :: Lens' Input (HashMap Text (AudioSelector))
iAudioSelectors = lens _iAudioSelectors (\s a -> s {_iAudioSelectors = a}) . _Default . _Map

-- | Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
iDecryptionSettings :: Lens' Input (Maybe InputDecryptionSettings)
iDecryptionSettings = lens _iDecryptionSettings (\s a -> s {_iDecryptionSettings = a})

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
iDeblockFilter :: Lens' Input (Maybe InputDeblockFilter)
iDeblockFilter = lens _iDeblockFilter (\s a -> s {_iDeblockFilter = a})

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
iInputClippings :: Lens' Input [InputClipping]
iInputClippings = lens _iInputClippings (\s a -> s {_iInputClippings = a}) . _Default . _Coerce

-- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
iCrop :: Lens' Input (Maybe Rectangle)
iCrop = lens _iCrop (\s a -> s {_iCrop = a})

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
iDenoiseFilter :: Lens' Input (Maybe InputDenoiseFilter)
iDenoiseFilter = lens _iDenoiseFilter (\s a -> s {_iDenoiseFilter = a})

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
iImageInserter :: Lens' Input (Maybe ImageInserter)
iImageInserter = lens _iImageInserter (\s a -> s {_iImageInserter = a})

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
iFilterStrength :: Lens' Input (Maybe Int)
iFilterStrength = lens _iFilterStrength (\s a -> s {_iFilterStrength = a})

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
iPsiControl :: Lens' Input (Maybe InputPsiControl)
iPsiControl = lens _iPsiControl (\s a -> s {_iPsiControl = a})

-- | Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
iCaptionSelectors :: Lens' Input (HashMap Text (CaptionSelector))
iCaptionSelectors = lens _iCaptionSelectors (\s a -> s {_iCaptionSelectors = a}) . _Default . _Map

-- | Specify the source file for your transcoding job. You can use multiple inputs in a single job. The service concatenates these inputs, in the order that you specify them in the job, to create the outputs. If your input format is IMF, specify your input by providing the path to your CPL. For example, "s3://bucket/vf/cpl.xml". If the CPL is in an incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps) to specify any supplemental IMPs that contain assets referenced by the CPL.
iFileInput :: Lens' Input (Maybe Text)
iFileInput = lens _iFileInput (\s a -> s {_iFileInput = a})

-- | Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
iTimecodeStart :: Lens' Input (Maybe Text)
iTimecodeStart = lens _iTimecodeStart (\s a -> s {_iTimecodeStart = a})

-- | When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
iInputScanType :: Lens' Input (Maybe InputScanType)
iInputScanType = lens _iInputScanType (\s a -> s {_iInputScanType = a})

-- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
iPosition :: Lens' Input (Maybe Rectangle)
iPosition = lens _iPosition (\s a -> s {_iPosition = a})

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
iFilterEnable :: Lens' Input (Maybe InputFilterEnable)
iFilterEnable = lens _iFilterEnable (\s a -> s {_iFilterEnable = a})

instance FromJSON Input where
  parseJSON =
    withObject
      "Input"
      ( \x ->
          Input'
            <$> (x .:? "videoSelector")
            <*> (x .:? "supplementalImps" .!= mempty)
            <*> (x .:? "programNumber")
            <*> (x .:? "audioSelectorGroups" .!= mempty)
            <*> (x .:? "timecodeSource")
            <*> (x .:? "audioSelectors" .!= mempty)
            <*> (x .:? "decryptionSettings")
            <*> (x .:? "deblockFilter")
            <*> (x .:? "inputClippings" .!= mempty)
            <*> (x .:? "crop")
            <*> (x .:? "denoiseFilter")
            <*> (x .:? "imageInserter")
            <*> (x .:? "filterStrength")
            <*> (x .:? "psiControl")
            <*> (x .:? "captionSelectors" .!= mempty)
            <*> (x .:? "fileInput")
            <*> (x .:? "timecodeStart")
            <*> (x .:? "inputScanType")
            <*> (x .:? "position")
            <*> (x .:? "filterEnable")
      )

instance Hashable Input

instance NFData Input

instance ToJSON Input where
  toJSON Input' {..} =
    object
      ( catMaybes
          [ ("videoSelector" .=) <$> _iVideoSelector,
            ("supplementalImps" .=) <$> _iSupplementalImps,
            ("programNumber" .=) <$> _iProgramNumber,
            ("audioSelectorGroups" .=) <$> _iAudioSelectorGroups,
            ("timecodeSource" .=) <$> _iTimecodeSource,
            ("audioSelectors" .=) <$> _iAudioSelectors,
            ("decryptionSettings" .=) <$> _iDecryptionSettings,
            ("deblockFilter" .=) <$> _iDeblockFilter,
            ("inputClippings" .=) <$> _iInputClippings,
            ("crop" .=) <$> _iCrop,
            ("denoiseFilter" .=) <$> _iDenoiseFilter,
            ("imageInserter" .=) <$> _iImageInserter,
            ("filterStrength" .=) <$> _iFilterStrength,
            ("psiControl" .=) <$> _iPsiControl,
            ("captionSelectors" .=) <$> _iCaptionSelectors,
            ("fileInput" .=) <$> _iFileInput,
            ("timecodeStart" .=) <$> _iTimecodeStart,
            ("inputScanType" .=) <$> _iInputScanType,
            ("position" .=) <$> _iPosition,
            ("filterEnable" .=) <$> _iFilterEnable
          ]
      )
