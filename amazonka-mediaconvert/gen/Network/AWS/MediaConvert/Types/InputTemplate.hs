{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputTemplate where

import qualified Network.AWS.Core as Core
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
import qualified Network.AWS.Prelude as Prelude

-- | Specified video input in a template.
--
-- /See:/ 'newInputTemplate' smart constructor.
data InputTemplate = InputTemplate'
  { -- | Enable the image inserter feature to include a graphic overlay on your
    -- video. Enable or disable this feature for each input individually. This
    -- setting is disabled by default.
    imageInserter :: Prelude.Maybe ImageInserter,
    -- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
    -- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
    -- uncompressed video inputs.
    denoiseFilter :: Prelude.Maybe InputDenoiseFilter,
    -- | When you have a progressive segmented frame (PsF) input, use this
    -- setting to flag the input as PsF. MediaConvert doesn\'t automatically
    -- detect PsF. Therefore, flagging your input as PsF results in better
    -- preservation of video quality when you do deinterlacing and frame rate
    -- conversion. If you don\'t specify, the default value is Auto (AUTO).
    -- Auto is the correct setting for all inputs that are not PsF. Don\'t set
    -- this value to PsF when your input is interlaced. Doing so creates
    -- horizontal interlacing artifacts.
    inputScanType :: Prelude.Maybe InputScanType,
    -- | (InputClippings) contains sets of start and end times that together
    -- specify a portion of the input to be used in the outputs. If you provide
    -- only a start time, the clip will be the entire input from that point to
    -- the end. If you provide only an end time, it will be the entire input up
    -- to that point. When you specify more than one input clip, the
    -- transcoding service creates the job outputs by stringing the clips
    -- together in the order you specify them.
    inputClippings :: Prelude.Maybe [InputClipping],
    -- | Specify the timecode that you want the service to use for this input\'s
    -- initial frame. To use this setting, you must set the Timecode source
    -- setting, located under the input settings (InputTimecodeSource), to
    -- Specified start (SPECIFIEDSTART). For more information about timecodes,
    -- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
    timecodeStart :: Prelude.Maybe Prelude.Text,
    -- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
    -- from the input that you will use in your outputs. You can use multiple
    -- Audio selectors per input.
    audioSelectors :: Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelector),
    -- | Use Filter strength (FilterStrength) to adjust the magnitude the input
    -- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
    -- 0.
    filterStrength :: Prelude.Maybe Prelude.Int,
    -- | Set PSI control (InputPsiControl) for transport stream inputs to specify
    -- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
    -- audio and video. * Use PSI - Scan only PSI data.
    psiControl :: Prelude.Maybe InputPsiControl,
    -- | Use Program (programNumber) to select a specific program from within a
    -- multi-program transport stream. Note that Quad 4K is not currently
    -- supported. Default is the first program within the transport stream. If
    -- the program you specify doesn\'t exist, the transcoding service will use
    -- this default.
    programNumber :: Prelude.Maybe Prelude.Natural,
    -- | Specifies set of audio selectors within an input to combine. An input
    -- may have multiple audio selector groups. See \"Audio Selector
    -- Group\":#inputs-audio_selector_group for more information.
    audioSelectorGroups :: Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelectorGroup),
    -- | Selector for video.
    videoSelector :: Prelude.Maybe VideoSelector,
    -- | Specify how the transcoding service applies the denoise and deblock
    -- filters. You must also enable the filters separately, with Denoise
    -- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
    -- transcoding service determines whether to apply filtering, depending on
    -- input type and quality. * Disable - The input is not filtered. This is
    -- true even if you use the API to enable them in (InputDeblockFilter) and
    -- (InputDeblockFilter). * Force - The input is filtered regardless of
    -- input type.
    filterEnable :: Prelude.Maybe InputFilterEnable,
    -- | Use Selection placement (position) to define the video area in your
    -- output frame. The area outside of the rectangle that you specify here is
    -- black. If you specify a value here, it will override any value that you
    -- specify in the output setting Selection placement (position). If you
    -- specify a value here, this will override any AFD values in your input,
    -- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
    -- you specify a value here, this will ignore anything that you specify for
    -- the setting Scaling Behavior (scalingBehavior).
    position :: Prelude.Maybe Rectangle,
    -- | Use Cropping selection (crop) to specify the video area that the service
    -- will include in the output video frame. If you specify a value here, it
    -- will override any value that you specify in the output setting Cropping
    -- selection (crop).
    crop :: Prelude.Maybe Rectangle,
    -- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
    -- output. Default is disabled. Only manually controllable for MPEG2 and
    -- uncompressed video inputs.
    deblockFilter :: Prelude.Maybe InputDeblockFilter,
    -- | Use captions selectors to specify the captions data from your input that
    -- you use in your outputs. You can use up to 20 captions selectors per
    -- input.
    captionSelectors :: Prelude.Maybe (Prelude.HashMap Prelude.Text CaptionSelector),
    -- | Use this Timecode source setting, located under the input settings
    -- (InputTimecodeSource), to specify how the service counts input video
    -- frames. This input frame count affects only the behavior of features
    -- that apply to a single input at a time, such as input clipping and
    -- synchronizing some captions formats. Choose Embedded (EMBEDDED) to use
    -- the timecodes in your input video. Choose Start at zero (ZEROBASED) to
    -- start the first frame at zero. Choose Specified start (SPECIFIEDSTART)
    -- to start the first frame at the timecode that you specify in the setting
    -- Start timecode (timecodeStart). If you don\'t specify a value for
    -- Timecode source, the service will use Embedded by default. For more
    -- information about timecodes, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
    timecodeSource :: Prelude.Maybe InputTimecodeSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageInserter', 'inputTemplate_imageInserter' - Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input individually. This
-- setting is disabled by default.
--
-- 'denoiseFilter', 'inputTemplate_denoiseFilter' - Enable Denoise (InputDenoiseFilter) to filter noise from the input.
-- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
-- uncompressed video inputs.
--
-- 'inputScanType', 'inputTemplate_inputScanType' - When you have a progressive segmented frame (PsF) input, use this
-- setting to flag the input as PsF. MediaConvert doesn\'t automatically
-- detect PsF. Therefore, flagging your input as PsF results in better
-- preservation of video quality when you do deinterlacing and frame rate
-- conversion. If you don\'t specify, the default value is Auto (AUTO).
-- Auto is the correct setting for all inputs that are not PsF. Don\'t set
-- this value to PsF when your input is interlaced. Doing so creates
-- horizontal interlacing artifacts.
--
-- 'inputClippings', 'inputTemplate_inputClippings' - (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
--
-- 'timecodeStart', 'inputTemplate_timecodeStart' - Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
--
-- 'audioSelectors', 'inputTemplate_audioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
--
-- 'filterStrength', 'inputTemplate_filterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input
-- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
-- 0.
--
-- 'psiControl', 'inputTemplate_psiControl' - Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
--
-- 'programNumber', 'inputTemplate_programNumber' - Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
--
-- 'audioSelectorGroups', 'inputTemplate_audioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input
-- may have multiple audio selector groups. See \"Audio Selector
-- Group\":#inputs-audio_selector_group for more information.
--
-- 'videoSelector', 'inputTemplate_videoSelector' - Selector for video.
--
-- 'filterEnable', 'inputTemplate_filterEnable' - Specify how the transcoding service applies the denoise and deblock
-- filters. You must also enable the filters separately, with Denoise
-- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
-- transcoding service determines whether to apply filtering, depending on
-- input type and quality. * Disable - The input is not filtered. This is
-- true even if you use the API to enable them in (InputDeblockFilter) and
-- (InputDeblockFilter). * Force - The input is filtered regardless of
-- input type.
--
-- 'position', 'inputTemplate_position' - Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black. If you specify a value here, it will override any value that you
-- specify in the output setting Selection placement (position). If you
-- specify a value here, this will override any AFD values in your input,
-- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
-- you specify a value here, this will ignore anything that you specify for
-- the setting Scaling Behavior (scalingBehavior).
--
-- 'crop', 'inputTemplate_crop' - Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
--
-- 'deblockFilter', 'inputTemplate_deblockFilter' - Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
--
-- 'captionSelectors', 'inputTemplate_captionSelectors' - Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
--
-- 'timecodeSource', 'inputTemplate_timecodeSource' - Use this Timecode source setting, located under the input settings
-- (InputTimecodeSource), to specify how the service counts input video
-- frames. This input frame count affects only the behavior of features
-- that apply to a single input at a time, such as input clipping and
-- synchronizing some captions formats. Choose Embedded (EMBEDDED) to use
-- the timecodes in your input video. Choose Start at zero (ZEROBASED) to
-- start the first frame at zero. Choose Specified start (SPECIFIEDSTART)
-- to start the first frame at the timecode that you specify in the setting
-- Start timecode (timecodeStart). If you don\'t specify a value for
-- Timecode source, the service will use Embedded by default. For more
-- information about timecodes, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
newInputTemplate ::
  InputTemplate
newInputTemplate =
  InputTemplate'
    { imageInserter = Prelude.Nothing,
      denoiseFilter = Prelude.Nothing,
      inputScanType = Prelude.Nothing,
      inputClippings = Prelude.Nothing,
      timecodeStart = Prelude.Nothing,
      audioSelectors = Prelude.Nothing,
      filterStrength = Prelude.Nothing,
      psiControl = Prelude.Nothing,
      programNumber = Prelude.Nothing,
      audioSelectorGroups = Prelude.Nothing,
      videoSelector = Prelude.Nothing,
      filterEnable = Prelude.Nothing,
      position = Prelude.Nothing,
      crop = Prelude.Nothing,
      deblockFilter = Prelude.Nothing,
      captionSelectors = Prelude.Nothing,
      timecodeSource = Prelude.Nothing
    }

-- | Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input individually. This
-- setting is disabled by default.
inputTemplate_imageInserter :: Lens.Lens' InputTemplate (Prelude.Maybe ImageInserter)
inputTemplate_imageInserter = Lens.lens (\InputTemplate' {imageInserter} -> imageInserter) (\s@InputTemplate' {} a -> s {imageInserter = a} :: InputTemplate)

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
-- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
-- uncompressed video inputs.
inputTemplate_denoiseFilter :: Lens.Lens' InputTemplate (Prelude.Maybe InputDenoiseFilter)
inputTemplate_denoiseFilter = Lens.lens (\InputTemplate' {denoiseFilter} -> denoiseFilter) (\s@InputTemplate' {} a -> s {denoiseFilter = a} :: InputTemplate)

-- | When you have a progressive segmented frame (PsF) input, use this
-- setting to flag the input as PsF. MediaConvert doesn\'t automatically
-- detect PsF. Therefore, flagging your input as PsF results in better
-- preservation of video quality when you do deinterlacing and frame rate
-- conversion. If you don\'t specify, the default value is Auto (AUTO).
-- Auto is the correct setting for all inputs that are not PsF. Don\'t set
-- this value to PsF when your input is interlaced. Doing so creates
-- horizontal interlacing artifacts.
inputTemplate_inputScanType :: Lens.Lens' InputTemplate (Prelude.Maybe InputScanType)
inputTemplate_inputScanType = Lens.lens (\InputTemplate' {inputScanType} -> inputScanType) (\s@InputTemplate' {} a -> s {inputScanType = a} :: InputTemplate)

-- | (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
inputTemplate_inputClippings :: Lens.Lens' InputTemplate (Prelude.Maybe [InputClipping])
inputTemplate_inputClippings = Lens.lens (\InputTemplate' {inputClippings} -> inputClippings) (\s@InputTemplate' {} a -> s {inputClippings = a} :: InputTemplate) Prelude.. Lens.mapping Lens._Coerce

-- | Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
inputTemplate_timecodeStart :: Lens.Lens' InputTemplate (Prelude.Maybe Prelude.Text)
inputTemplate_timecodeStart = Lens.lens (\InputTemplate' {timecodeStart} -> timecodeStart) (\s@InputTemplate' {} a -> s {timecodeStart = a} :: InputTemplate)

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
inputTemplate_audioSelectors :: Lens.Lens' InputTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelector))
inputTemplate_audioSelectors = Lens.lens (\InputTemplate' {audioSelectors} -> audioSelectors) (\s@InputTemplate' {} a -> s {audioSelectors = a} :: InputTemplate) Prelude.. Lens.mapping Lens._Coerce

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input
-- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
-- 0.
inputTemplate_filterStrength :: Lens.Lens' InputTemplate (Prelude.Maybe Prelude.Int)
inputTemplate_filterStrength = Lens.lens (\InputTemplate' {filterStrength} -> filterStrength) (\s@InputTemplate' {} a -> s {filterStrength = a} :: InputTemplate)

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
inputTemplate_psiControl :: Lens.Lens' InputTemplate (Prelude.Maybe InputPsiControl)
inputTemplate_psiControl = Lens.lens (\InputTemplate' {psiControl} -> psiControl) (\s@InputTemplate' {} a -> s {psiControl = a} :: InputTemplate)

-- | Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
inputTemplate_programNumber :: Lens.Lens' InputTemplate (Prelude.Maybe Prelude.Natural)
inputTemplate_programNumber = Lens.lens (\InputTemplate' {programNumber} -> programNumber) (\s@InputTemplate' {} a -> s {programNumber = a} :: InputTemplate)

-- | Specifies set of audio selectors within an input to combine. An input
-- may have multiple audio selector groups. See \"Audio Selector
-- Group\":#inputs-audio_selector_group for more information.
inputTemplate_audioSelectorGroups :: Lens.Lens' InputTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelectorGroup))
inputTemplate_audioSelectorGroups = Lens.lens (\InputTemplate' {audioSelectorGroups} -> audioSelectorGroups) (\s@InputTemplate' {} a -> s {audioSelectorGroups = a} :: InputTemplate) Prelude.. Lens.mapping Lens._Coerce

-- | Selector for video.
inputTemplate_videoSelector :: Lens.Lens' InputTemplate (Prelude.Maybe VideoSelector)
inputTemplate_videoSelector = Lens.lens (\InputTemplate' {videoSelector} -> videoSelector) (\s@InputTemplate' {} a -> s {videoSelector = a} :: InputTemplate)

-- | Specify how the transcoding service applies the denoise and deblock
-- filters. You must also enable the filters separately, with Denoise
-- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
-- transcoding service determines whether to apply filtering, depending on
-- input type and quality. * Disable - The input is not filtered. This is
-- true even if you use the API to enable them in (InputDeblockFilter) and
-- (InputDeblockFilter). * Force - The input is filtered regardless of
-- input type.
inputTemplate_filterEnable :: Lens.Lens' InputTemplate (Prelude.Maybe InputFilterEnable)
inputTemplate_filterEnable = Lens.lens (\InputTemplate' {filterEnable} -> filterEnable) (\s@InputTemplate' {} a -> s {filterEnable = a} :: InputTemplate)

-- | Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black. If you specify a value here, it will override any value that you
-- specify in the output setting Selection placement (position). If you
-- specify a value here, this will override any AFD values in your input,
-- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
-- you specify a value here, this will ignore anything that you specify for
-- the setting Scaling Behavior (scalingBehavior).
inputTemplate_position :: Lens.Lens' InputTemplate (Prelude.Maybe Rectangle)
inputTemplate_position = Lens.lens (\InputTemplate' {position} -> position) (\s@InputTemplate' {} a -> s {position = a} :: InputTemplate)

-- | Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
inputTemplate_crop :: Lens.Lens' InputTemplate (Prelude.Maybe Rectangle)
inputTemplate_crop = Lens.lens (\InputTemplate' {crop} -> crop) (\s@InputTemplate' {} a -> s {crop = a} :: InputTemplate)

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
inputTemplate_deblockFilter :: Lens.Lens' InputTemplate (Prelude.Maybe InputDeblockFilter)
inputTemplate_deblockFilter = Lens.lens (\InputTemplate' {deblockFilter} -> deblockFilter) (\s@InputTemplate' {} a -> s {deblockFilter = a} :: InputTemplate)

-- | Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
inputTemplate_captionSelectors :: Lens.Lens' InputTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text CaptionSelector))
inputTemplate_captionSelectors = Lens.lens (\InputTemplate' {captionSelectors} -> captionSelectors) (\s@InputTemplate' {} a -> s {captionSelectors = a} :: InputTemplate) Prelude.. Lens.mapping Lens._Coerce

-- | Use this Timecode source setting, located under the input settings
-- (InputTimecodeSource), to specify how the service counts input video
-- frames. This input frame count affects only the behavior of features
-- that apply to a single input at a time, such as input clipping and
-- synchronizing some captions formats. Choose Embedded (EMBEDDED) to use
-- the timecodes in your input video. Choose Start at zero (ZEROBASED) to
-- start the first frame at zero. Choose Specified start (SPECIFIEDSTART)
-- to start the first frame at the timecode that you specify in the setting
-- Start timecode (timecodeStart). If you don\'t specify a value for
-- Timecode source, the service will use Embedded by default. For more
-- information about timecodes, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
inputTemplate_timecodeSource :: Lens.Lens' InputTemplate (Prelude.Maybe InputTimecodeSource)
inputTemplate_timecodeSource = Lens.lens (\InputTemplate' {timecodeSource} -> timecodeSource) (\s@InputTemplate' {} a -> s {timecodeSource = a} :: InputTemplate)

instance Core.FromJSON InputTemplate where
  parseJSON =
    Core.withObject
      "InputTemplate"
      ( \x ->
          InputTemplate'
            Prelude.<$> (x Core..:? "imageInserter")
            Prelude.<*> (x Core..:? "denoiseFilter")
            Prelude.<*> (x Core..:? "inputScanType")
            Prelude.<*> (x Core..:? "inputClippings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "timecodeStart")
            Prelude.<*> (x Core..:? "audioSelectors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "filterStrength")
            Prelude.<*> (x Core..:? "psiControl")
            Prelude.<*> (x Core..:? "programNumber")
            Prelude.<*> ( x Core..:? "audioSelectorGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "videoSelector")
            Prelude.<*> (x Core..:? "filterEnable")
            Prelude.<*> (x Core..:? "position")
            Prelude.<*> (x Core..:? "crop")
            Prelude.<*> (x Core..:? "deblockFilter")
            Prelude.<*> ( x Core..:? "captionSelectors"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "timecodeSource")
      )

instance Prelude.Hashable InputTemplate

instance Prelude.NFData InputTemplate

instance Core.ToJSON InputTemplate where
  toJSON InputTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("imageInserter" Core..=) Prelude.<$> imageInserter,
            ("denoiseFilter" Core..=) Prelude.<$> denoiseFilter,
            ("inputScanType" Core..=) Prelude.<$> inputScanType,
            ("inputClippings" Core..=)
              Prelude.<$> inputClippings,
            ("timecodeStart" Core..=) Prelude.<$> timecodeStart,
            ("audioSelectors" Core..=)
              Prelude.<$> audioSelectors,
            ("filterStrength" Core..=)
              Prelude.<$> filterStrength,
            ("psiControl" Core..=) Prelude.<$> psiControl,
            ("programNumber" Core..=) Prelude.<$> programNumber,
            ("audioSelectorGroups" Core..=)
              Prelude.<$> audioSelectorGroups,
            ("videoSelector" Core..=) Prelude.<$> videoSelector,
            ("filterEnable" Core..=) Prelude.<$> filterEnable,
            ("position" Core..=) Prelude.<$> position,
            ("crop" Core..=) Prelude.<$> crop,
            ("deblockFilter" Core..=) Prelude.<$> deblockFilter,
            ("captionSelectors" Core..=)
              Prelude.<$> captionSelectors,
            ("timecodeSource" Core..=)
              Prelude.<$> timecodeSource
          ]
      )
