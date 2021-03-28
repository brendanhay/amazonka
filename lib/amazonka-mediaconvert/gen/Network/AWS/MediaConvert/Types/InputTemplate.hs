{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputTemplate
  ( InputTemplate (..)
  -- * Smart constructor
  , mkInputTemplate
  -- * Lenses
  , itAudioSelectorGroups
  , itAudioSelectors
  , itCaptionSelectors
  , itCrop
  , itDeblockFilter
  , itDenoiseFilter
  , itFilterEnable
  , itFilterStrength
  , itImageInserter
  , itInputClippings
  , itInputScanType
  , itPosition
  , itProgramNumber
  , itPsiControl
  , itTimecodeSource
  , itTimecodeStart
  , itVideoSelector
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AudioSelector as Types
import qualified Network.AWS.MediaConvert.Types.AudioSelectorGroup as Types
import qualified Network.AWS.MediaConvert.Types.CaptionSelector as Types
import qualified Network.AWS.MediaConvert.Types.ImageInserter as Types
import qualified Network.AWS.MediaConvert.Types.InputClipping as Types
import qualified Network.AWS.MediaConvert.Types.InputDeblockFilter as Types
import qualified Network.AWS.MediaConvert.Types.InputDenoiseFilter as Types
import qualified Network.AWS.MediaConvert.Types.InputFilterEnable as Types
import qualified Network.AWS.MediaConvert.Types.InputPsiControl as Types
import qualified Network.AWS.MediaConvert.Types.InputScanType as Types
import qualified Network.AWS.MediaConvert.Types.InputTimecodeSource as Types
import qualified Network.AWS.MediaConvert.Types.Rectangle as Types
import qualified Network.AWS.MediaConvert.Types.VideoSelector as Types
import qualified Network.AWS.Prelude as Core

-- | Specified video input in a template.
--
-- /See:/ 'mkInputTemplate' smart constructor.
data InputTemplate = InputTemplate'
  { audioSelectorGroups :: Core.Maybe (Core.HashMap Core.Text Types.AudioSelectorGroup)
    -- ^ Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
  , audioSelectors :: Core.Maybe (Core.HashMap Core.Text Types.AudioSelector)
    -- ^ Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
  , captionSelectors :: Core.Maybe (Core.HashMap Core.Text Types.CaptionSelector)
    -- ^ Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
  , crop :: Core.Maybe Types.Rectangle
    -- ^ Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
  , deblockFilter :: Core.Maybe Types.InputDeblockFilter
    -- ^ Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
  , denoiseFilter :: Core.Maybe Types.InputDenoiseFilter
    -- ^ Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
  , filterEnable :: Core.Maybe Types.InputFilterEnable
    -- ^ Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
  , filterStrength :: Core.Maybe Core.Int
    -- ^ Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
  , imageInserter :: Core.Maybe Types.ImageInserter
    -- ^ Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
  , inputClippings :: Core.Maybe [Types.InputClipping]
    -- ^ (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
  , inputScanType :: Core.Maybe Types.InputScanType
    -- ^ When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
  , position :: Core.Maybe Types.Rectangle
    -- ^ Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
  , programNumber :: Core.Maybe Core.Natural
    -- ^ Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
  , psiControl :: Core.Maybe Types.InputPsiControl
    -- ^ Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
  , timecodeSource :: Core.Maybe Types.InputTimecodeSource
    -- ^ Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
  , timecodeStart :: Core.Maybe Core.Text
    -- ^ Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
  , videoSelector :: Core.Maybe Types.VideoSelector
    -- ^ Selector for video.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputTemplate' value with any optional fields omitted.
mkInputTemplate
    :: InputTemplate
mkInputTemplate
  = InputTemplate'{audioSelectorGroups = Core.Nothing,
                   audioSelectors = Core.Nothing, captionSelectors = Core.Nothing,
                   crop = Core.Nothing, deblockFilter = Core.Nothing,
                   denoiseFilter = Core.Nothing, filterEnable = Core.Nothing,
                   filterStrength = Core.Nothing, imageInserter = Core.Nothing,
                   inputClippings = Core.Nothing, inputScanType = Core.Nothing,
                   position = Core.Nothing, programNumber = Core.Nothing,
                   psiControl = Core.Nothing, timecodeSource = Core.Nothing,
                   timecodeStart = Core.Nothing, videoSelector = Core.Nothing}

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- /Note:/ Consider using 'audioSelectorGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itAudioSelectorGroups :: Lens.Lens' InputTemplate (Core.Maybe (Core.HashMap Core.Text Types.AudioSelectorGroup))
itAudioSelectorGroups = Lens.field @"audioSelectorGroups"
{-# INLINEABLE itAudioSelectorGroups #-}
{-# DEPRECATED audioSelectorGroups "Use generic-lens or generic-optics with 'audioSelectorGroups' instead"  #-}

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use multiple Audio selectors per input.
--
-- /Note:/ Consider using 'audioSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itAudioSelectors :: Lens.Lens' InputTemplate (Core.Maybe (Core.HashMap Core.Text Types.AudioSelector))
itAudioSelectors = Lens.field @"audioSelectors"
{-# INLINEABLE itAudioSelectors #-}
{-# DEPRECATED audioSelectors "Use generic-lens or generic-optics with 'audioSelectors' instead"  #-}

-- | Use captions selectors to specify the captions data from your input that you use in your outputs. You can use up to 20 captions selectors per input.
--
-- /Note:/ Consider using 'captionSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itCaptionSelectors :: Lens.Lens' InputTemplate (Core.Maybe (Core.HashMap Core.Text Types.CaptionSelector))
itCaptionSelectors = Lens.field @"captionSelectors"
{-# INLINEABLE itCaptionSelectors #-}
{-# DEPRECATED captionSelectors "Use generic-lens or generic-optics with 'captionSelectors' instead"  #-}

-- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame. If you specify a value here, it will override any value that you specify in the output setting Cropping selection (crop).
--
-- /Note:/ Consider using 'crop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itCrop :: Lens.Lens' InputTemplate (Core.Maybe Types.Rectangle)
itCrop = Lens.field @"crop"
{-# INLINEABLE itCrop #-}
{-# DEPRECATED crop "Use generic-lens or generic-optics with 'crop' instead"  #-}

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
--
-- /Note:/ Consider using 'deblockFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itDeblockFilter :: Lens.Lens' InputTemplate (Core.Maybe Types.InputDeblockFilter)
itDeblockFilter = Lens.field @"deblockFilter"
{-# INLINEABLE itDeblockFilter #-}
{-# DEPRECATED deblockFilter "Use generic-lens or generic-optics with 'deblockFilter' instead"  #-}

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
--
-- /Note:/ Consider using 'denoiseFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itDenoiseFilter :: Lens.Lens' InputTemplate (Core.Maybe Types.InputDenoiseFilter)
itDenoiseFilter = Lens.field @"denoiseFilter"
{-# INLINEABLE itDenoiseFilter #-}
{-# DEPRECATED denoiseFilter "Use generic-lens or generic-optics with 'denoiseFilter' instead"  #-}

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
--
-- /Note:/ Consider using 'filterEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itFilterEnable :: Lens.Lens' InputTemplate (Core.Maybe Types.InputFilterEnable)
itFilterEnable = Lens.field @"filterEnable"
{-# INLINEABLE itFilterEnable #-}
{-# DEPRECATED filterEnable "Use generic-lens or generic-optics with 'filterEnable' instead"  #-}

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- /Note:/ Consider using 'filterStrength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itFilterStrength :: Lens.Lens' InputTemplate (Core.Maybe Core.Int)
itFilterStrength = Lens.field @"filterStrength"
{-# INLINEABLE itFilterStrength #-}
{-# DEPRECATED filterStrength "Use generic-lens or generic-optics with 'filterStrength' instead"  #-}

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'imageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImageInserter :: Lens.Lens' InputTemplate (Core.Maybe Types.ImageInserter)
itImageInserter = Lens.field @"imageInserter"
{-# INLINEABLE itImageInserter #-}
{-# DEPRECATED imageInserter "Use generic-lens or generic-optics with 'imageInserter' instead"  #-}

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- /Note:/ Consider using 'inputClippings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInputClippings :: Lens.Lens' InputTemplate (Core.Maybe [Types.InputClipping])
itInputClippings = Lens.field @"inputClippings"
{-# INLINEABLE itInputClippings #-}
{-# DEPRECATED inputClippings "Use generic-lens or generic-optics with 'inputClippings' instead"  #-}

-- | When you have a progressive segmented frame (PsF) input, use this setting to flag the input as PsF. MediaConvert doesn't automatically detect PsF. Therefore, flagging your input as PsF results in better preservation of video quality when you do deinterlacing and frame rate conversion. If you don't specify, the default value is Auto (AUTO). Auto is the correct setting for all inputs that are not PsF. Don't set this value to PsF when your input is interlaced. Doing so creates horizontal interlacing artifacts.
--
-- /Note:/ Consider using 'inputScanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInputScanType :: Lens.Lens' InputTemplate (Core.Maybe Types.InputScanType)
itInputScanType = Lens.field @"inputScanType"
{-# INLINEABLE itInputScanType #-}
{-# DEPRECATED inputScanType "Use generic-lens or generic-optics with 'inputScanType' instead"  #-}

-- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black. If you specify a value here, it will override any value that you specify in the output setting Selection placement (position). If you specify a value here, this will override any AFD values in your input, even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If you specify a value here, this will ignore anything that you specify for the setting Scaling Behavior (scalingBehavior).
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itPosition :: Lens.Lens' InputTemplate (Core.Maybe Types.Rectangle)
itPosition = Lens.field @"position"
{-# INLINEABLE itPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itProgramNumber :: Lens.Lens' InputTemplate (Core.Maybe Core.Natural)
itProgramNumber = Lens.field @"programNumber"
{-# INLINEABLE itProgramNumber #-}
{-# DEPRECATED programNumber "Use generic-lens or generic-optics with 'programNumber' instead"  #-}

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
--
-- /Note:/ Consider using 'psiControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itPsiControl :: Lens.Lens' InputTemplate (Core.Maybe Types.InputPsiControl)
itPsiControl = Lens.field @"psiControl"
{-# INLINEABLE itPsiControl #-}
{-# DEPRECATED psiControl "Use generic-lens or generic-optics with 'psiControl' instead"  #-}

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- /Note:/ Consider using 'timecodeSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTimecodeSource :: Lens.Lens' InputTemplate (Core.Maybe Types.InputTimecodeSource)
itTimecodeSource = Lens.field @"timecodeSource"
{-# INLINEABLE itTimecodeSource #-}
{-# DEPRECATED timecodeSource "Use generic-lens or generic-optics with 'timecodeSource' instead"  #-}

-- | Specify the timecode that you want the service to use for this input's initial frame. To use this setting, you must set the Timecode source setting, located under the input settings (InputTimecodeSource), to Specified start (SPECIFIEDSTART). For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
--
-- /Note:/ Consider using 'timecodeStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTimecodeStart :: Lens.Lens' InputTemplate (Core.Maybe Core.Text)
itTimecodeStart = Lens.field @"timecodeStart"
{-# INLINEABLE itTimecodeStart #-}
{-# DEPRECATED timecodeStart "Use generic-lens or generic-optics with 'timecodeStart' instead"  #-}

-- | Selector for video.
--
-- /Note:/ Consider using 'videoSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itVideoSelector :: Lens.Lens' InputTemplate (Core.Maybe Types.VideoSelector)
itVideoSelector = Lens.field @"videoSelector"
{-# INLINEABLE itVideoSelector #-}
{-# DEPRECATED videoSelector "Use generic-lens or generic-optics with 'videoSelector' instead"  #-}

instance Core.FromJSON InputTemplate where
        toJSON InputTemplate{..}
          = Core.object
              (Core.catMaybes
                 [("audioSelectorGroups" Core..=) Core.<$> audioSelectorGroups,
                  ("audioSelectors" Core..=) Core.<$> audioSelectors,
                  ("captionSelectors" Core..=) Core.<$> captionSelectors,
                  ("crop" Core..=) Core.<$> crop,
                  ("deblockFilter" Core..=) Core.<$> deblockFilter,
                  ("denoiseFilter" Core..=) Core.<$> denoiseFilter,
                  ("filterEnable" Core..=) Core.<$> filterEnable,
                  ("filterStrength" Core..=) Core.<$> filterStrength,
                  ("imageInserter" Core..=) Core.<$> imageInserter,
                  ("inputClippings" Core..=) Core.<$> inputClippings,
                  ("inputScanType" Core..=) Core.<$> inputScanType,
                  ("position" Core..=) Core.<$> position,
                  ("programNumber" Core..=) Core.<$> programNumber,
                  ("psiControl" Core..=) Core.<$> psiControl,
                  ("timecodeSource" Core..=) Core.<$> timecodeSource,
                  ("timecodeStart" Core..=) Core.<$> timecodeStart,
                  ("videoSelector" Core..=) Core.<$> videoSelector])

instance Core.FromJSON InputTemplate where
        parseJSON
          = Core.withObject "InputTemplate" Core.$
              \ x ->
                InputTemplate' Core.<$>
                  (x Core..:? "audioSelectorGroups") Core.<*>
                    x Core..:? "audioSelectors"
                    Core.<*> x Core..:? "captionSelectors"
                    Core.<*> x Core..:? "crop"
                    Core.<*> x Core..:? "deblockFilter"
                    Core.<*> x Core..:? "denoiseFilter"
                    Core.<*> x Core..:? "filterEnable"
                    Core.<*> x Core..:? "filterStrength"
                    Core.<*> x Core..:? "imageInserter"
                    Core.<*> x Core..:? "inputClippings"
                    Core.<*> x Core..:? "inputScanType"
                    Core.<*> x Core..:? "position"
                    Core.<*> x Core..:? "programNumber"
                    Core.<*> x Core..:? "psiControl"
                    Core.<*> x Core..:? "timecodeSource"
                    Core.<*> x Core..:? "timecodeStart"
                    Core.<*> x Core..:? "videoSelector"
