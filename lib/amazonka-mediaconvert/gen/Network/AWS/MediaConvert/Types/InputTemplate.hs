-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputTemplate
  ( InputTemplate (..),

    -- * Smart constructor
    mkInputTemplate,

    -- * Lenses
    itVideoSelector,
    itProgramNumber,
    itAudioSelectorGroups,
    itTimecodeSource,
    itAudioSelectors,
    itDeblockFilter,
    itInputClippings,
    itCrop,
    itDenoiseFilter,
    itImageInserter,
    itFilterStrength,
    itPsiControl,
    itCaptionSelectors,
    itTimecodeStart,
    itInputScanType,
    itPosition,
    itFilterEnable,
  )
where

import qualified Network.AWS.Lens as Lens
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
import qualified Network.AWS.Prelude as Lude

-- | Specified video input in a template.
--
-- /See:/ 'mkInputTemplate' smart constructor.
data InputTemplate = InputTemplate'
  { videoSelector ::
      Lude.Maybe VideoSelector,
    programNumber :: Lude.Maybe Lude.Natural,
    audioSelectorGroups ::
      Lude.Maybe (Lude.HashMap Lude.Text (AudioSelectorGroup)),
    timecodeSource :: Lude.Maybe InputTimecodeSource,
    audioSelectors ::
      Lude.Maybe (Lude.HashMap Lude.Text (AudioSelector)),
    deblockFilter :: Lude.Maybe InputDeblockFilter,
    inputClippings :: Lude.Maybe [InputClipping],
    crop :: Lude.Maybe Rectangle,
    denoiseFilter :: Lude.Maybe InputDenoiseFilter,
    imageInserter :: Lude.Maybe ImageInserter,
    filterStrength :: Lude.Maybe Lude.Int,
    psiControl :: Lude.Maybe InputPsiControl,
    captionSelectors ::
      Lude.Maybe (Lude.HashMap Lude.Text (CaptionSelector)),
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

-- | Creates a value of 'InputTemplate' with the minimum fields required to make a request.
--
-- * 'audioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
-- * 'audioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
-- * 'captionSelectors' - Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
-- * 'crop' - Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
-- * 'deblockFilter' - Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
-- * 'denoiseFilter' - Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
-- * 'filterEnable' - Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
-- * 'filterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
-- * 'imageInserter' - Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
-- * 'inputClippings' - (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
-- * 'inputScanType' - When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
-- * 'position' - Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
-- * 'programNumber' - Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
-- * 'psiControl' - Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
-- * 'timecodeSource' - Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
-- * 'timecodeStart' - Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
-- * 'videoSelector' - Selector for video.
mkInputTemplate ::
  InputTemplate
mkInputTemplate =
  InputTemplate'
    { videoSelector = Lude.Nothing,
      programNumber = Lude.Nothing,
      audioSelectorGroups = Lude.Nothing,
      timecodeSource = Lude.Nothing,
      audioSelectors = Lude.Nothing,
      deblockFilter = Lude.Nothing,
      inputClippings = Lude.Nothing,
      crop = Lude.Nothing,
      denoiseFilter = Lude.Nothing,
      imageInserter = Lude.Nothing,
      filterStrength = Lude.Nothing,
      psiControl = Lude.Nothing,
      captionSelectors = Lude.Nothing,
      timecodeStart = Lude.Nothing,
      inputScanType = Lude.Nothing,
      position = Lude.Nothing,
      filterEnable = Lude.Nothing
    }

-- | Selector for video.
--
-- /Note:/ Consider using 'videoSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itVideoSelector :: Lens.Lens' InputTemplate (Lude.Maybe VideoSelector)
itVideoSelector = Lens.lens (videoSelector :: InputTemplate -> Lude.Maybe VideoSelector) (\s a -> s {videoSelector = a} :: InputTemplate)
{-# DEPRECATED itVideoSelector "Use generic-lens or generic-optics with 'videoSelector' instead." #-}

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itProgramNumber :: Lens.Lens' InputTemplate (Lude.Maybe Lude.Natural)
itProgramNumber = Lens.lens (programNumber :: InputTemplate -> Lude.Maybe Lude.Natural) (\s a -> s {programNumber = a} :: InputTemplate)
{-# DEPRECATED itProgramNumber "Use generic-lens or generic-optics with 'programNumber' instead." #-}

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- /Note:/ Consider using 'audioSelectorGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itAudioSelectorGroups :: Lens.Lens' InputTemplate (Lude.Maybe (Lude.HashMap Lude.Text (AudioSelectorGroup)))
itAudioSelectorGroups = Lens.lens (audioSelectorGroups :: InputTemplate -> Lude.Maybe (Lude.HashMap Lude.Text (AudioSelectorGroup))) (\s a -> s {audioSelectorGroups = a} :: InputTemplate)
{-# DEPRECATED itAudioSelectorGroups "Use generic-lens or generic-optics with 'audioSelectorGroups' instead." #-}

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- /Note:/ Consider using 'timecodeSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTimecodeSource :: Lens.Lens' InputTemplate (Lude.Maybe InputTimecodeSource)
itTimecodeSource = Lens.lens (timecodeSource :: InputTemplate -> Lude.Maybe InputTimecodeSource) (\s a -> s {timecodeSource = a} :: InputTemplate)
{-# DEPRECATED itTimecodeSource "Use generic-lens or generic-optics with 'timecodeSource' instead." #-}

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
--
-- /Note:/ Consider using 'audioSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itAudioSelectors :: Lens.Lens' InputTemplate (Lude.Maybe (Lude.HashMap Lude.Text (AudioSelector)))
itAudioSelectors = Lens.lens (audioSelectors :: InputTemplate -> Lude.Maybe (Lude.HashMap Lude.Text (AudioSelector))) (\s a -> s {audioSelectors = a} :: InputTemplate)
{-# DEPRECATED itAudioSelectors "Use generic-lens or generic-optics with 'audioSelectors' instead." #-}

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
--
-- /Note:/ Consider using 'deblockFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itDeblockFilter :: Lens.Lens' InputTemplate (Lude.Maybe InputDeblockFilter)
itDeblockFilter = Lens.lens (deblockFilter :: InputTemplate -> Lude.Maybe InputDeblockFilter) (\s a -> s {deblockFilter = a} :: InputTemplate)
{-# DEPRECATED itDeblockFilter "Use generic-lens or generic-optics with 'deblockFilter' instead." #-}

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- /Note:/ Consider using 'inputClippings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInputClippings :: Lens.Lens' InputTemplate (Lude.Maybe [InputClipping])
itInputClippings = Lens.lens (inputClippings :: InputTemplate -> Lude.Maybe [InputClipping]) (\s a -> s {inputClippings = a} :: InputTemplate)
{-# DEPRECATED itInputClippings "Use generic-lens or generic-optics with 'inputClippings' instead." #-}

-- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
--
-- /Note:/ Consider using 'crop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itCrop :: Lens.Lens' InputTemplate (Lude.Maybe Rectangle)
itCrop = Lens.lens (crop :: InputTemplate -> Lude.Maybe Rectangle) (\s a -> s {crop = a} :: InputTemplate)
{-# DEPRECATED itCrop "Use generic-lens or generic-optics with 'crop' instead." #-}

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
--
-- /Note:/ Consider using 'denoiseFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itDenoiseFilter :: Lens.Lens' InputTemplate (Lude.Maybe InputDenoiseFilter)
itDenoiseFilter = Lens.lens (denoiseFilter :: InputTemplate -> Lude.Maybe InputDenoiseFilter) (\s a -> s {denoiseFilter = a} :: InputTemplate)
{-# DEPRECATED itDenoiseFilter "Use generic-lens or generic-optics with 'denoiseFilter' instead." #-}

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'imageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImageInserter :: Lens.Lens' InputTemplate (Lude.Maybe ImageInserter)
itImageInserter = Lens.lens (imageInserter :: InputTemplate -> Lude.Maybe ImageInserter) (\s a -> s {imageInserter = a} :: InputTemplate)
{-# DEPRECATED itImageInserter "Use generic-lens or generic-optics with 'imageInserter' instead." #-}

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- /Note:/ Consider using 'filterStrength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itFilterStrength :: Lens.Lens' InputTemplate (Lude.Maybe Lude.Int)
itFilterStrength = Lens.lens (filterStrength :: InputTemplate -> Lude.Maybe Lude.Int) (\s a -> s {filterStrength = a} :: InputTemplate)
{-# DEPRECATED itFilterStrength "Use generic-lens or generic-optics with 'filterStrength' instead." #-}

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
--
-- /Note:/ Consider using 'psiControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itPsiControl :: Lens.Lens' InputTemplate (Lude.Maybe InputPsiControl)
itPsiControl = Lens.lens (psiControl :: InputTemplate -> Lude.Maybe InputPsiControl) (\s a -> s {psiControl = a} :: InputTemplate)
{-# DEPRECATED itPsiControl "Use generic-lens or generic-optics with 'psiControl' instead." #-}

-- | Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
--
-- /Note:/ Consider using 'captionSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itCaptionSelectors :: Lens.Lens' InputTemplate (Lude.Maybe (Lude.HashMap Lude.Text (CaptionSelector)))
itCaptionSelectors = Lens.lens (captionSelectors :: InputTemplate -> Lude.Maybe (Lude.HashMap Lude.Text (CaptionSelector))) (\s a -> s {captionSelectors = a} :: InputTemplate)
{-# DEPRECATED itCaptionSelectors "Use generic-lens or generic-optics with 'captionSelectors' instead." #-}

-- | Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- /Note:/ Consider using 'timecodeStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTimecodeStart :: Lens.Lens' InputTemplate (Lude.Maybe Lude.Text)
itTimecodeStart = Lens.lens (timecodeStart :: InputTemplate -> Lude.Maybe Lude.Text) (\s a -> s {timecodeStart = a} :: InputTemplate)
{-# DEPRECATED itTimecodeStart "Use generic-lens or generic-optics with 'timecodeStart' instead." #-}

-- | When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
--
-- /Note:/ Consider using 'inputScanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInputScanType :: Lens.Lens' InputTemplate (Lude.Maybe InputScanType)
itInputScanType = Lens.lens (inputScanType :: InputTemplate -> Lude.Maybe InputScanType) (\s a -> s {inputScanType = a} :: InputTemplate)
{-# DEPRECATED itInputScanType "Use generic-lens or generic-optics with 'inputScanType' instead." #-}

-- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itPosition :: Lens.Lens' InputTemplate (Lude.Maybe Rectangle)
itPosition = Lens.lens (position :: InputTemplate -> Lude.Maybe Rectangle) (\s a -> s {position = a} :: InputTemplate)
{-# DEPRECATED itPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
--
-- /Note:/ Consider using 'filterEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itFilterEnable :: Lens.Lens' InputTemplate (Lude.Maybe InputFilterEnable)
itFilterEnable = Lens.lens (filterEnable :: InputTemplate -> Lude.Maybe InputFilterEnable) (\s a -> s {filterEnable = a} :: InputTemplate)
{-# DEPRECATED itFilterEnable "Use generic-lens or generic-optics with 'filterEnable' instead." #-}

instance Lude.FromJSON InputTemplate where
  parseJSON =
    Lude.withObject
      "InputTemplate"
      ( \x ->
          InputTemplate'
            Lude.<$> (x Lude..:? "videoSelector")
            Lude.<*> (x Lude..:? "programNumber")
            Lude.<*> (x Lude..:? "audioSelectorGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "timecodeSource")
            Lude.<*> (x Lude..:? "audioSelectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "deblockFilter")
            Lude.<*> (x Lude..:? "inputClippings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "crop")
            Lude.<*> (x Lude..:? "denoiseFilter")
            Lude.<*> (x Lude..:? "imageInserter")
            Lude.<*> (x Lude..:? "filterStrength")
            Lude.<*> (x Lude..:? "psiControl")
            Lude.<*> (x Lude..:? "captionSelectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "timecodeStart")
            Lude.<*> (x Lude..:? "inputScanType")
            Lude.<*> (x Lude..:? "position")
            Lude.<*> (x Lude..:? "filterEnable")
      )

instance Lude.ToJSON InputTemplate where
  toJSON InputTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("videoSelector" Lude..=) Lude.<$> videoSelector,
            ("programNumber" Lude..=) Lude.<$> programNumber,
            ("audioSelectorGroups" Lude..=) Lude.<$> audioSelectorGroups,
            ("timecodeSource" Lude..=) Lude.<$> timecodeSource,
            ("audioSelectors" Lude..=) Lude.<$> audioSelectors,
            ("deblockFilter" Lude..=) Lude.<$> deblockFilter,
            ("inputClippings" Lude..=) Lude.<$> inputClippings,
            ("crop" Lude..=) Lude.<$> crop,
            ("denoiseFilter" Lude..=) Lude.<$> denoiseFilter,
            ("imageInserter" Lude..=) Lude.<$> imageInserter,
            ("filterStrength" Lude..=) Lude.<$> filterStrength,
            ("psiControl" Lude..=) Lude.<$> psiControl,
            ("captionSelectors" Lude..=) Lude.<$> captionSelectors,
            ("timecodeStart" Lude..=) Lude.<$> timecodeStart,
            ("inputScanType" Lude..=) Lude.<$> inputScanType,
            ("position" Lude..=) Lude.<$> position,
            ("filterEnable" Lude..=) Lude.<$> filterEnable
          ]
      )
