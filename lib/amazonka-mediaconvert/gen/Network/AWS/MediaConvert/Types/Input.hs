{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Input
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Input
  ( Input (..),

    -- * Smart constructor
    mkInput,

    -- * Lenses
    iVideoSelector,
    iSupplementalImps,
    iProgramNumber,
    iAudioSelectorGroups,
    iTimecodeSource,
    iAudioSelectors,
    iDecryptionSettings,
    iDeblockFilter,
    iInputClippings,
    iCrop,
    iDenoiseFilter,
    iImageInserter,
    iFilterStrength,
    iPsiControl,
    iCaptionSelectors,
    iFileInput,
    iTimecodeStart,
    iInputScanType,
    iPosition,
    iFilterEnable,
  )
where

import qualified Network.AWS.Lens as Lens
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
import qualified Network.AWS.Prelude as Lude

-- | Specifies media input
--
-- /See:/ 'mkInput' smart constructor.
data Input = Input'
  { videoSelector :: Lude.Maybe VideoSelector,
    supplementalImps :: Lude.Maybe [Lude.Text],
    programNumber :: Lude.Maybe Lude.Natural,
    audioSelectorGroups ::
      Lude.Maybe (Lude.HashMap Lude.Text (AudioSelectorGroup)),
    timecodeSource :: Lude.Maybe InputTimecodeSource,
    audioSelectors ::
      Lude.Maybe (Lude.HashMap Lude.Text (AudioSelector)),
    decryptionSettings :: Lude.Maybe InputDecryptionSettings,
    deblockFilter :: Lude.Maybe InputDeblockFilter,
    inputClippings :: Lude.Maybe [InputClipping],
    crop :: Lude.Maybe Rectangle,
    denoiseFilter :: Lude.Maybe InputDenoiseFilter,
    imageInserter :: Lude.Maybe ImageInserter,
    filterStrength :: Lude.Maybe Lude.Int,
    psiControl :: Lude.Maybe InputPsiControl,
    captionSelectors ::
      Lude.Maybe (Lude.HashMap Lude.Text (CaptionSelector)),
    fileInput :: Lude.Maybe Lude.Text,
    timecodeStart :: Lude.Maybe Lude.Text,
    inputScanType :: Lude.Maybe InputScanType,
    position :: Lude.Maybe Rectangle,
    filterEnable :: Lude.Maybe InputFilterEnable
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Input' with the minimum fields required to make a request.
--
-- * 'audioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
-- * 'audioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
-- * 'captionSelectors' - Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
-- * 'crop' - Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
-- * 'deblockFilter' - Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
-- * 'decryptionSettings' - Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
-- * 'denoiseFilter' - Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
-- * 'fileInput' - Specify the source file for your transcoding job. You can use multiple inputs in a single job. The service concatenates these inputs, in the order that you specify them in the job, to create the outputs. If your input format is IMF, specify your input by providing the path to your CPL. For example, "s3://bucket/vf/cpl.xml". If the CPL is in an incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps) to specify any supplemental IMPs that contain assets referenced by the CPL.
-- * 'filterEnable' - Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
-- * 'filterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
-- * 'imageInserter' - Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
-- * 'inputClippings' - (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
-- * 'inputScanType' - When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
-- * 'position' - Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
-- * 'programNumber' - Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
-- * 'psiControl' - Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
-- * 'supplementalImps' - Provide a list of any necessary supplemental IMPs. You need supplemental IMPs if the CPL that you're using for your input is in an incomplete IMP. Specify either the supplemental IMP directories with a trailing slash or the ASSETMAP.xml files. For example ["s3://bucket/ov/", "s3://bucket/vf2/ASSETMAP.xml"]. You don't need to specify the IMP that contains your input CPL, because the service automatically detects it.
-- * 'timecodeSource' - Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
-- * 'timecodeStart' - Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
-- * 'videoSelector' - Selector for video.
mkInput ::
  Input
mkInput =
  Input'
    { videoSelector = Lude.Nothing,
      supplementalImps = Lude.Nothing,
      programNumber = Lude.Nothing,
      audioSelectorGroups = Lude.Nothing,
      timecodeSource = Lude.Nothing,
      audioSelectors = Lude.Nothing,
      decryptionSettings = Lude.Nothing,
      deblockFilter = Lude.Nothing,
      inputClippings = Lude.Nothing,
      crop = Lude.Nothing,
      denoiseFilter = Lude.Nothing,
      imageInserter = Lude.Nothing,
      filterStrength = Lude.Nothing,
      psiControl = Lude.Nothing,
      captionSelectors = Lude.Nothing,
      fileInput = Lude.Nothing,
      timecodeStart = Lude.Nothing,
      inputScanType = Lude.Nothing,
      position = Lude.Nothing,
      filterEnable = Lude.Nothing
    }

-- | Selector for video.
--
-- /Note:/ Consider using 'videoSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVideoSelector :: Lens.Lens' Input (Lude.Maybe VideoSelector)
iVideoSelector = Lens.lens (videoSelector :: Input -> Lude.Maybe VideoSelector) (\s a -> s {videoSelector = a} :: Input)
{-# DEPRECATED iVideoSelector "Use generic-lens or generic-optics with 'videoSelector' instead." #-}

-- | Provide a list of any necessary supplemental IMPs. You need supplemental IMPs if the CPL that you're using for your input is in an incomplete IMP. Specify either the supplemental IMP directories with a trailing slash or the ASSETMAP.xml files. For example ["s3://bucket/ov/", "s3://bucket/vf2/ASSETMAP.xml"]. You don't need to specify the IMP that contains your input CPL, because the service automatically detects it.
--
-- /Note:/ Consider using 'supplementalImps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSupplementalImps :: Lens.Lens' Input (Lude.Maybe [Lude.Text])
iSupplementalImps = Lens.lens (supplementalImps :: Input -> Lude.Maybe [Lude.Text]) (\s a -> s {supplementalImps = a} :: Input)
{-# DEPRECATED iSupplementalImps "Use generic-lens or generic-optics with 'supplementalImps' instead." #-}

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iProgramNumber :: Lens.Lens' Input (Lude.Maybe Lude.Natural)
iProgramNumber = Lens.lens (programNumber :: Input -> Lude.Maybe Lude.Natural) (\s a -> s {programNumber = a} :: Input)
{-# DEPRECATED iProgramNumber "Use generic-lens or generic-optics with 'programNumber' instead." #-}

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- /Note:/ Consider using 'audioSelectorGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAudioSelectorGroups :: Lens.Lens' Input (Lude.Maybe (Lude.HashMap Lude.Text (AudioSelectorGroup)))
iAudioSelectorGroups = Lens.lens (audioSelectorGroups :: Input -> Lude.Maybe (Lude.HashMap Lude.Text (AudioSelectorGroup))) (\s a -> s {audioSelectorGroups = a} :: Input)
{-# DEPRECATED iAudioSelectorGroups "Use generic-lens or generic-optics with 'audioSelectorGroups' instead." #-}

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- /Note:/ Consider using 'timecodeSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTimecodeSource :: Lens.Lens' Input (Lude.Maybe InputTimecodeSource)
iTimecodeSource = Lens.lens (timecodeSource :: Input -> Lude.Maybe InputTimecodeSource) (\s a -> s {timecodeSource = a} :: Input)
{-# DEPRECATED iTimecodeSource "Use generic-lens or generic-optics with 'timecodeSource' instead." #-}

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
--
-- /Note:/ Consider using 'audioSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAudioSelectors :: Lens.Lens' Input (Lude.Maybe (Lude.HashMap Lude.Text (AudioSelector)))
iAudioSelectors = Lens.lens (audioSelectors :: Input -> Lude.Maybe (Lude.HashMap Lude.Text (AudioSelector))) (\s a -> s {audioSelectors = a} :: Input)
{-# DEPRECATED iAudioSelectors "Use generic-lens or generic-optics with 'audioSelectors' instead." #-}

-- | Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
--
-- /Note:/ Consider using 'decryptionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDecryptionSettings :: Lens.Lens' Input (Lude.Maybe InputDecryptionSettings)
iDecryptionSettings = Lens.lens (decryptionSettings :: Input -> Lude.Maybe InputDecryptionSettings) (\s a -> s {decryptionSettings = a} :: Input)
{-# DEPRECATED iDecryptionSettings "Use generic-lens or generic-optics with 'decryptionSettings' instead." #-}

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
--
-- /Note:/ Consider using 'deblockFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDeblockFilter :: Lens.Lens' Input (Lude.Maybe InputDeblockFilter)
iDeblockFilter = Lens.lens (deblockFilter :: Input -> Lude.Maybe InputDeblockFilter) (\s a -> s {deblockFilter = a} :: Input)
{-# DEPRECATED iDeblockFilter "Use generic-lens or generic-optics with 'deblockFilter' instead." #-}

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- /Note:/ Consider using 'inputClippings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputClippings :: Lens.Lens' Input (Lude.Maybe [InputClipping])
iInputClippings = Lens.lens (inputClippings :: Input -> Lude.Maybe [InputClipping]) (\s a -> s {inputClippings = a} :: Input)
{-# DEPRECATED iInputClippings "Use generic-lens or generic-optics with 'inputClippings' instead." #-}

-- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
--
-- /Note:/ Consider using 'crop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCrop :: Lens.Lens' Input (Lude.Maybe Rectangle)
iCrop = Lens.lens (crop :: Input -> Lude.Maybe Rectangle) (\s a -> s {crop = a} :: Input)
{-# DEPRECATED iCrop "Use generic-lens or generic-optics with 'crop' instead." #-}

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
--
-- /Note:/ Consider using 'denoiseFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDenoiseFilter :: Lens.Lens' Input (Lude.Maybe InputDenoiseFilter)
iDenoiseFilter = Lens.lens (denoiseFilter :: Input -> Lude.Maybe InputDenoiseFilter) (\s a -> s {denoiseFilter = a} :: Input)
{-# DEPRECATED iDenoiseFilter "Use generic-lens or generic-optics with 'denoiseFilter' instead." #-}

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'imageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageInserter :: Lens.Lens' Input (Lude.Maybe ImageInserter)
iImageInserter = Lens.lens (imageInserter :: Input -> Lude.Maybe ImageInserter) (\s a -> s {imageInserter = a} :: Input)
{-# DEPRECATED iImageInserter "Use generic-lens or generic-optics with 'imageInserter' instead." #-}

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- /Note:/ Consider using 'filterStrength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFilterStrength :: Lens.Lens' Input (Lude.Maybe Lude.Int)
iFilterStrength = Lens.lens (filterStrength :: Input -> Lude.Maybe Lude.Int) (\s a -> s {filterStrength = a} :: Input)
{-# DEPRECATED iFilterStrength "Use generic-lens or generic-optics with 'filterStrength' instead." #-}

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
--
-- /Note:/ Consider using 'psiControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPsiControl :: Lens.Lens' Input (Lude.Maybe InputPsiControl)
iPsiControl = Lens.lens (psiControl :: Input -> Lude.Maybe InputPsiControl) (\s a -> s {psiControl = a} :: Input)
{-# DEPRECATED iPsiControl "Use generic-lens or generic-optics with 'psiControl' instead." #-}

-- | Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
--
-- /Note:/ Consider using 'captionSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCaptionSelectors :: Lens.Lens' Input (Lude.Maybe (Lude.HashMap Lude.Text (CaptionSelector)))
iCaptionSelectors = Lens.lens (captionSelectors :: Input -> Lude.Maybe (Lude.HashMap Lude.Text (CaptionSelector))) (\s a -> s {captionSelectors = a} :: Input)
{-# DEPRECATED iCaptionSelectors "Use generic-lens or generic-optics with 'captionSelectors' instead." #-}

-- | Specify the source file for your transcoding job. You can use multiple inputs in a single job. The service concatenates these inputs, in the order that you specify them in the job, to create the outputs. If your input format is IMF, specify your input by providing the path to your CPL. For example, "s3://bucket/vf/cpl.xml". If the CPL is in an incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps) to specify any supplemental IMPs that contain assets referenced by the CPL.
--
-- /Note:/ Consider using 'fileInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFileInput :: Lens.Lens' Input (Lude.Maybe Lude.Text)
iFileInput = Lens.lens (fileInput :: Input -> Lude.Maybe Lude.Text) (\s a -> s {fileInput = a} :: Input)
{-# DEPRECATED iFileInput "Use generic-lens or generic-optics with 'fileInput' instead." #-}

-- | Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- /Note:/ Consider using 'timecodeStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTimecodeStart :: Lens.Lens' Input (Lude.Maybe Lude.Text)
iTimecodeStart = Lens.lens (timecodeStart :: Input -> Lude.Maybe Lude.Text) (\s a -> s {timecodeStart = a} :: Input)
{-# DEPRECATED iTimecodeStart "Use generic-lens or generic-optics with 'timecodeStart' instead." #-}

-- | When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
--
-- /Note:/ Consider using 'inputScanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputScanType :: Lens.Lens' Input (Lude.Maybe InputScanType)
iInputScanType = Lens.lens (inputScanType :: Input -> Lude.Maybe InputScanType) (\s a -> s {inputScanType = a} :: Input)
{-# DEPRECATED iInputScanType "Use generic-lens or generic-optics with 'inputScanType' instead." #-}

-- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPosition :: Lens.Lens' Input (Lude.Maybe Rectangle)
iPosition = Lens.lens (position :: Input -> Lude.Maybe Rectangle) (\s a -> s {position = a} :: Input)
{-# DEPRECATED iPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
--
-- /Note:/ Consider using 'filterEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFilterEnable :: Lens.Lens' Input (Lude.Maybe InputFilterEnable)
iFilterEnable = Lens.lens (filterEnable :: Input -> Lude.Maybe InputFilterEnable) (\s a -> s {filterEnable = a} :: Input)
{-# DEPRECATED iFilterEnable "Use generic-lens or generic-optics with 'filterEnable' instead." #-}

instance Lude.FromJSON Input where
  parseJSON =
    Lude.withObject
      "Input"
      ( \x ->
          Input'
            Lude.<$> (x Lude..:? "videoSelector")
            Lude.<*> (x Lude..:? "supplementalImps" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "programNumber")
            Lude.<*> (x Lude..:? "audioSelectorGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "timecodeSource")
            Lude.<*> (x Lude..:? "audioSelectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "decryptionSettings")
            Lude.<*> (x Lude..:? "deblockFilter")
            Lude.<*> (x Lude..:? "inputClippings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "crop")
            Lude.<*> (x Lude..:? "denoiseFilter")
            Lude.<*> (x Lude..:? "imageInserter")
            Lude.<*> (x Lude..:? "filterStrength")
            Lude.<*> (x Lude..:? "psiControl")
            Lude.<*> (x Lude..:? "captionSelectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "fileInput")
            Lude.<*> (x Lude..:? "timecodeStart")
            Lude.<*> (x Lude..:? "inputScanType")
            Lude.<*> (x Lude..:? "position")
            Lude.<*> (x Lude..:? "filterEnable")
      )

instance Lude.ToJSON Input where
  toJSON Input' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("videoSelector" Lude..=) Lude.<$> videoSelector,
            ("supplementalImps" Lude..=) Lude.<$> supplementalImps,
            ("programNumber" Lude..=) Lude.<$> programNumber,
            ("audioSelectorGroups" Lude..=) Lude.<$> audioSelectorGroups,
            ("timecodeSource" Lude..=) Lude.<$> timecodeSource,
            ("audioSelectors" Lude..=) Lude.<$> audioSelectors,
            ("decryptionSettings" Lude..=) Lude.<$> decryptionSettings,
            ("deblockFilter" Lude..=) Lude.<$> deblockFilter,
            ("inputClippings" Lude..=) Lude.<$> inputClippings,
            ("crop" Lude..=) Lude.<$> crop,
            ("denoiseFilter" Lude..=) Lude.<$> denoiseFilter,
            ("imageInserter" Lude..=) Lude.<$> imageInserter,
            ("filterStrength" Lude..=) Lude.<$> filterStrength,
            ("psiControl" Lude..=) Lude.<$> psiControl,
            ("captionSelectors" Lude..=) Lude.<$> captionSelectors,
            ("fileInput" Lude..=) Lude.<$> fileInput,
            ("timecodeStart" Lude..=) Lude.<$> timecodeStart,
            ("inputScanType" Lude..=) Lude.<$> inputScanType,
            ("position" Lude..=) Lude.<$> position,
            ("filterEnable" Lude..=) Lude.<$> filterEnable
          ]
      )
