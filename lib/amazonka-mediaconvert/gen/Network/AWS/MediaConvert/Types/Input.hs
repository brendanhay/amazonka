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
    iAudioSelectorGroups,
    iAudioSelectors,
    iCaptionSelectors,
    iCrop,
    iDeblockFilter,
    iDecryptionSettings,
    iDenoiseFilter,
    iFileInput,
    iFilterEnable,
    iFilterStrength,
    iImageInserter,
    iInputClippings,
    iInputScanType,
    iPosition,
    iProgramNumber,
    iPsiControl,
    iSupplementalImps,
    iTimecodeSource,
    iTimecodeStart,
    iVideoSelector,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AudioSelector as Types
import qualified Network.AWS.MediaConvert.Types.AudioSelectorGroup as Types
import qualified Network.AWS.MediaConvert.Types.CaptionSelector as Types
import qualified Network.AWS.MediaConvert.Types.ImageInserter as Types
import qualified Network.AWS.MediaConvert.Types.InputClipping as Types
import qualified Network.AWS.MediaConvert.Types.InputDeblockFilter as Types
import qualified Network.AWS.MediaConvert.Types.InputDecryptionSettings as Types
import qualified Network.AWS.MediaConvert.Types.InputDenoiseFilter as Types
import qualified Network.AWS.MediaConvert.Types.InputFilterEnable as Types
import qualified Network.AWS.MediaConvert.Types.InputPsiControl as Types
import qualified Network.AWS.MediaConvert.Types.InputScanType as Types
import qualified Network.AWS.MediaConvert.Types.InputTimecodeSource as Types
import qualified Network.AWS.MediaConvert.Types.Rectangle as Types
import qualified Network.AWS.MediaConvert.Types.VideoSelector as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies media input
--
-- /See:/ 'mkInput' smart constructor.
data Input = Input'
  { -- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
    audioSelectorGroups :: Core.Maybe (Core.HashMap Core.Text Types.AudioSelectorGroup),
    -- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
    audioSelectors :: Core.Maybe (Core.HashMap Core.Text Types.AudioSelector),
    -- | Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
    captionSelectors :: Core.Maybe (Core.HashMap Core.Text Types.CaptionSelector),
    -- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
    crop :: Core.Maybe Types.Rectangle,
    -- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
    deblockFilter :: Core.Maybe Types.InputDeblockFilter,
    -- | Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
    decryptionSettings :: Core.Maybe Types.InputDecryptionSettings,
    -- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
    denoiseFilter :: Core.Maybe Types.InputDenoiseFilter,
    -- | Specify the source file for your transcoding job. You can use multiple inputs in a single job. The service concatenates these inputs, in the order that you specify them in the job, to create the outputs. If your input format is IMF, specify your input by providing the path to your CPL. For example, "s3://bucket/vf/cpl.xml". If the CPL is in an incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps) to specify any supplemental IMPs that contain assets referenced by the CPL.
    fileInput :: Core.Maybe Core.Text,
    -- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
    filterEnable :: Core.Maybe Types.InputFilterEnable,
    -- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
    filterStrength :: Core.Maybe Core.Int,
    -- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
    imageInserter :: Core.Maybe Types.ImageInserter,
    -- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
    inputClippings :: Core.Maybe [Types.InputClipping],
    -- | When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
    inputScanType :: Core.Maybe Types.InputScanType,
    -- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
    position :: Core.Maybe Types.Rectangle,
    -- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
    programNumber :: Core.Maybe Core.Natural,
    -- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
    psiControl :: Core.Maybe Types.InputPsiControl,
    -- | Provide a list of any necessary supplemental IMPs. You need supplemental IMPs if the CPL that you're using for your input is in an incomplete IMP. Specify either the supplemental IMP directories with a trailing slash or the ASSETMAP.xml files. For example ["s3://bucket/ov/", "s3://bucket/vf2/ASSETMAP.xml"]. You don't need to specify the IMP that contains your input CPL, because the service automatically detects it.
    supplementalImps :: Core.Maybe [Core.Text],
    -- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
    timecodeSource :: Core.Maybe Types.InputTimecodeSource,
    -- | Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
    timecodeStart :: Core.Maybe Core.Text,
    -- | Selector for video.
    videoSelector :: Core.Maybe Types.VideoSelector
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Input' value with any optional fields omitted.
mkInput ::
  Input
mkInput =
  Input'
    { audioSelectorGroups = Core.Nothing,
      audioSelectors = Core.Nothing,
      captionSelectors = Core.Nothing,
      crop = Core.Nothing,
      deblockFilter = Core.Nothing,
      decryptionSettings = Core.Nothing,
      denoiseFilter = Core.Nothing,
      fileInput = Core.Nothing,
      filterEnable = Core.Nothing,
      filterStrength = Core.Nothing,
      imageInserter = Core.Nothing,
      inputClippings = Core.Nothing,
      inputScanType = Core.Nothing,
      position = Core.Nothing,
      programNumber = Core.Nothing,
      psiControl = Core.Nothing,
      supplementalImps = Core.Nothing,
      timecodeSource = Core.Nothing,
      timecodeStart = Core.Nothing,
      videoSelector = Core.Nothing
    }

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- /Note:/ Consider using 'audioSelectorGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAudioSelectorGroups :: Lens.Lens' Input (Core.Maybe (Core.HashMap Core.Text Types.AudioSelectorGroup))
iAudioSelectorGroups = Lens.field @"audioSelectorGroups"
{-# DEPRECATED iAudioSelectorGroups "Use generic-lens or generic-optics with 'audioSelectorGroups' instead." #-}

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
--
-- /Note:/ Consider using 'audioSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAudioSelectors :: Lens.Lens' Input (Core.Maybe (Core.HashMap Core.Text Types.AudioSelector))
iAudioSelectors = Lens.field @"audioSelectors"
{-# DEPRECATED iAudioSelectors "Use generic-lens or generic-optics with 'audioSelectors' instead." #-}

-- | Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
--
-- /Note:/ Consider using 'captionSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCaptionSelectors :: Lens.Lens' Input (Core.Maybe (Core.HashMap Core.Text Types.CaptionSelector))
iCaptionSelectors = Lens.field @"captionSelectors"
{-# DEPRECATED iCaptionSelectors "Use generic-lens or generic-optics with 'captionSelectors' instead." #-}

-- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
--
-- /Note:/ Consider using 'crop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCrop :: Lens.Lens' Input (Core.Maybe Types.Rectangle)
iCrop = Lens.field @"crop"
{-# DEPRECATED iCrop "Use generic-lens or generic-optics with 'crop' instead." #-}

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
--
-- /Note:/ Consider using 'deblockFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDeblockFilter :: Lens.Lens' Input (Core.Maybe Types.InputDeblockFilter)
iDeblockFilter = Lens.field @"deblockFilter"
{-# DEPRECATED iDeblockFilter "Use generic-lens or generic-optics with 'deblockFilter' instead." #-}

-- | Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
--
-- /Note:/ Consider using 'decryptionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDecryptionSettings :: Lens.Lens' Input (Core.Maybe Types.InputDecryptionSettings)
iDecryptionSettings = Lens.field @"decryptionSettings"
{-# DEPRECATED iDecryptionSettings "Use generic-lens or generic-optics with 'decryptionSettings' instead." #-}

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
--
-- /Note:/ Consider using 'denoiseFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDenoiseFilter :: Lens.Lens' Input (Core.Maybe Types.InputDenoiseFilter)
iDenoiseFilter = Lens.field @"denoiseFilter"
{-# DEPRECATED iDenoiseFilter "Use generic-lens or generic-optics with 'denoiseFilter' instead." #-}

-- | Specify the source file for your transcoding job. You can use multiple inputs in a single job. The service concatenates these inputs, in the order that you specify them in the job, to create the outputs. If your input format is IMF, specify your input by providing the path to your CPL. For example, "s3://bucket/vf/cpl.xml". If the CPL is in an incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps) to specify any supplemental IMPs that contain assets referenced by the CPL.
--
-- /Note:/ Consider using 'fileInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFileInput :: Lens.Lens' Input (Core.Maybe Core.Text)
iFileInput = Lens.field @"fileInput"
{-# DEPRECATED iFileInput "Use generic-lens or generic-optics with 'fileInput' instead." #-}

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
--
-- /Note:/ Consider using 'filterEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFilterEnable :: Lens.Lens' Input (Core.Maybe Types.InputFilterEnable)
iFilterEnable = Lens.field @"filterEnable"
{-# DEPRECATED iFilterEnable "Use generic-lens or generic-optics with 'filterEnable' instead." #-}

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- /Note:/ Consider using 'filterStrength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFilterStrength :: Lens.Lens' Input (Core.Maybe Core.Int)
iFilterStrength = Lens.field @"filterStrength"
{-# DEPRECATED iFilterStrength "Use generic-lens or generic-optics with 'filterStrength' instead." #-}

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'imageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageInserter :: Lens.Lens' Input (Core.Maybe Types.ImageInserter)
iImageInserter = Lens.field @"imageInserter"
{-# DEPRECATED iImageInserter "Use generic-lens or generic-optics with 'imageInserter' instead." #-}

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- /Note:/ Consider using 'inputClippings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputClippings :: Lens.Lens' Input (Core.Maybe [Types.InputClipping])
iInputClippings = Lens.field @"inputClippings"
{-# DEPRECATED iInputClippings "Use generic-lens or generic-optics with 'inputClippings' instead." #-}

-- | When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
--
-- /Note:/ Consider using 'inputScanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputScanType :: Lens.Lens' Input (Core.Maybe Types.InputScanType)
iInputScanType = Lens.field @"inputScanType"
{-# DEPRECATED iInputScanType "Use generic-lens or generic-optics with 'inputScanType' instead." #-}

-- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPosition :: Lens.Lens' Input (Core.Maybe Types.Rectangle)
iPosition = Lens.field @"position"
{-# DEPRECATED iPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iProgramNumber :: Lens.Lens' Input (Core.Maybe Core.Natural)
iProgramNumber = Lens.field @"programNumber"
{-# DEPRECATED iProgramNumber "Use generic-lens or generic-optics with 'programNumber' instead." #-}

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
--
-- /Note:/ Consider using 'psiControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPsiControl :: Lens.Lens' Input (Core.Maybe Types.InputPsiControl)
iPsiControl = Lens.field @"psiControl"
{-# DEPRECATED iPsiControl "Use generic-lens or generic-optics with 'psiControl' instead." #-}

-- | Provide a list of any necessary supplemental IMPs. You need supplemental IMPs if the CPL that you're using for your input is in an incomplete IMP. Specify either the supplemental IMP directories with a trailing slash or the ASSETMAP.xml files. For example ["s3://bucket/ov/", "s3://bucket/vf2/ASSETMAP.xml"]. You don't need to specify the IMP that contains your input CPL, because the service automatically detects it.
--
-- /Note:/ Consider using 'supplementalImps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSupplementalImps :: Lens.Lens' Input (Core.Maybe [Core.Text])
iSupplementalImps = Lens.field @"supplementalImps"
{-# DEPRECATED iSupplementalImps "Use generic-lens or generic-optics with 'supplementalImps' instead." #-}

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- /Note:/ Consider using 'timecodeSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTimecodeSource :: Lens.Lens' Input (Core.Maybe Types.InputTimecodeSource)
iTimecodeSource = Lens.field @"timecodeSource"
{-# DEPRECATED iTimecodeSource "Use generic-lens or generic-optics with 'timecodeSource' instead." #-}

-- | Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- /Note:/ Consider using 'timecodeStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTimecodeStart :: Lens.Lens' Input (Core.Maybe Core.Text)
iTimecodeStart = Lens.field @"timecodeStart"
{-# DEPRECATED iTimecodeStart "Use generic-lens or generic-optics with 'timecodeStart' instead." #-}

-- | Selector for video.
--
-- /Note:/ Consider using 'videoSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVideoSelector :: Lens.Lens' Input (Core.Maybe Types.VideoSelector)
iVideoSelector = Lens.field @"videoSelector"
{-# DEPRECATED iVideoSelector "Use generic-lens or generic-optics with 'videoSelector' instead." #-}

instance Core.FromJSON Input where
  toJSON Input {..} =
    Core.object
      ( Core.catMaybes
          [ ("audioSelectorGroups" Core..=) Core.<$> audioSelectorGroups,
            ("audioSelectors" Core..=) Core.<$> audioSelectors,
            ("captionSelectors" Core..=) Core.<$> captionSelectors,
            ("crop" Core..=) Core.<$> crop,
            ("deblockFilter" Core..=) Core.<$> deblockFilter,
            ("decryptionSettings" Core..=) Core.<$> decryptionSettings,
            ("denoiseFilter" Core..=) Core.<$> denoiseFilter,
            ("fileInput" Core..=) Core.<$> fileInput,
            ("filterEnable" Core..=) Core.<$> filterEnable,
            ("filterStrength" Core..=) Core.<$> filterStrength,
            ("imageInserter" Core..=) Core.<$> imageInserter,
            ("inputClippings" Core..=) Core.<$> inputClippings,
            ("inputScanType" Core..=) Core.<$> inputScanType,
            ("position" Core..=) Core.<$> position,
            ("programNumber" Core..=) Core.<$> programNumber,
            ("psiControl" Core..=) Core.<$> psiControl,
            ("supplementalImps" Core..=) Core.<$> supplementalImps,
            ("timecodeSource" Core..=) Core.<$> timecodeSource,
            ("timecodeStart" Core..=) Core.<$> timecodeStart,
            ("videoSelector" Core..=) Core.<$> videoSelector
          ]
      )

instance Core.FromJSON Input where
  parseJSON =
    Core.withObject "Input" Core.$
      \x ->
        Input'
          Core.<$> (x Core..:? "audioSelectorGroups")
          Core.<*> (x Core..:? "audioSelectors")
          Core.<*> (x Core..:? "captionSelectors")
          Core.<*> (x Core..:? "crop")
          Core.<*> (x Core..:? "deblockFilter")
          Core.<*> (x Core..:? "decryptionSettings")
          Core.<*> (x Core..:? "denoiseFilter")
          Core.<*> (x Core..:? "fileInput")
          Core.<*> (x Core..:? "filterEnable")
          Core.<*> (x Core..:? "filterStrength")
          Core.<*> (x Core..:? "imageInserter")
          Core.<*> (x Core..:? "inputClippings")
          Core.<*> (x Core..:? "inputScanType")
          Core.<*> (x Core..:? "position")
          Core.<*> (x Core..:? "programNumber")
          Core.<*> (x Core..:? "psiControl")
          Core.<*> (x Core..:? "supplementalImps")
          Core.<*> (x Core..:? "timecodeSource")
          Core.<*> (x Core..:? "timecodeStart")
          Core.<*> (x Core..:? "videoSelector")
