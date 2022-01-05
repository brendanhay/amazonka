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
-- Module      : Amazonka.MediaConvert.Types.InputTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.InputTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.AudioSelector
import Amazonka.MediaConvert.Types.AudioSelectorGroup
import Amazonka.MediaConvert.Types.CaptionSelector
import Amazonka.MediaConvert.Types.ImageInserter
import Amazonka.MediaConvert.Types.InputClipping
import Amazonka.MediaConvert.Types.InputDeblockFilter
import Amazonka.MediaConvert.Types.InputDenoiseFilter
import Amazonka.MediaConvert.Types.InputFilterEnable
import Amazonka.MediaConvert.Types.InputPsiControl
import Amazonka.MediaConvert.Types.InputScanType
import Amazonka.MediaConvert.Types.InputTimecodeSource
import Amazonka.MediaConvert.Types.Rectangle
import Amazonka.MediaConvert.Types.VideoSelector
import qualified Amazonka.Prelude as Prelude

-- | Specified video input in a template.
--
-- /See:/ 'newInputTemplate' smart constructor.
data InputTemplate = InputTemplate'
  { -- | Input video selectors contain the video settings for the input. Each of
    -- your inputs can have up to one video selector.
    videoSelector :: Prelude.Maybe VideoSelector,
    -- | Use Program (programNumber) to select a specific program from within a
    -- multi-program transport stream. Note that Quad 4K is not currently
    -- supported. Default is the first program within the transport stream. If
    -- the program you specify doesn\'t exist, the transcoding service will use
    -- this default.
    programNumber :: Prelude.Maybe Prelude.Natural,
    -- | Use audio selector groups to combine multiple sidecar audio inputs so
    -- that you can assign them to a single output audio tab
    -- (AudioDescription). Note that, if you\'re working with embedded audio,
    -- it\'s simpler to assign multiple input tracks into a single audio
    -- selector rather than use an audio selector group.
    audioSelectorGroups :: Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelectorGroup),
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
    timecodeSource :: Prelude.Maybe InputTimecodeSource,
    -- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
    -- from the input that you will use in your outputs. You can use multiple
    -- Audio selectors per input.
    audioSelectors :: Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelector),
    -- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
    -- output. Default is disabled. Only manually controllable for MPEG2 and
    -- uncompressed video inputs.
    deblockFilter :: Prelude.Maybe InputDeblockFilter,
    -- | (InputClippings) contains sets of start and end times that together
    -- specify a portion of the input to be used in the outputs. If you provide
    -- only a start time, the clip will be the entire input from that point to
    -- the end. If you provide only an end time, it will be the entire input up
    -- to that point. When you specify more than one input clip, the
    -- transcoding service creates the job outputs by stringing the clips
    -- together in the order you specify them.
    inputClippings :: Prelude.Maybe [InputClipping],
    -- | Use Cropping selection (crop) to specify the video area that the service
    -- will include in the output video frame. If you specify a value here, it
    -- will override any value that you specify in the output setting Cropping
    -- selection (crop).
    crop :: Prelude.Maybe Rectangle,
    -- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
    -- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
    -- uncompressed video inputs.
    denoiseFilter :: Prelude.Maybe InputDenoiseFilter,
    -- | Enable the image inserter feature to include a graphic overlay on your
    -- video. Enable or disable this feature for each input individually. This
    -- setting is disabled by default.
    imageInserter :: Prelude.Maybe ImageInserter,
    -- | Use Filter strength (FilterStrength) to adjust the magnitude the input
    -- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
    -- 0.
    filterStrength :: Prelude.Maybe Prelude.Int,
    -- | Set PSI control (InputPsiControl) for transport stream inputs to specify
    -- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
    -- audio and video. * Use PSI - Scan only PSI data.
    psiControl :: Prelude.Maybe InputPsiControl,
    -- | Use captions selectors to specify the captions data from your input that
    -- you use in your outputs. You can use up to 20 captions selectors per
    -- input.
    captionSelectors :: Prelude.Maybe (Prelude.HashMap Prelude.Text CaptionSelector),
    -- | Specify the timecode that you want the service to use for this input\'s
    -- initial frame. To use this setting, you must set the Timecode source
    -- setting, located under the input settings (InputTimecodeSource), to
    -- Specified start (SPECIFIEDSTART). For more information about timecodes,
    -- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
    timecodeStart :: Prelude.Maybe Prelude.Text,
    -- | When you have a progressive segmented frame (PsF) input, use this
    -- setting to flag the input as PsF. MediaConvert doesn\'t automatically
    -- detect PsF. Therefore, flagging your input as PsF results in better
    -- preservation of video quality when you do deinterlacing and frame rate
    -- conversion. If you don\'t specify, the default value is Auto (AUTO).
    -- Auto is the correct setting for all inputs that are not PsF. Don\'t set
    -- this value to PsF when your input is interlaced. Doing so creates
    -- horizontal interlacing artifacts.
    inputScanType :: Prelude.Maybe InputScanType,
    -- | Use Selection placement (position) to define the video area in your
    -- output frame. The area outside of the rectangle that you specify here is
    -- black. If you specify a value here, it will override any value that you
    -- specify in the output setting Selection placement (position). If you
    -- specify a value here, this will override any AFD values in your input,
    -- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
    -- you specify a value here, this will ignore anything that you specify for
    -- the setting Scaling Behavior (scalingBehavior).
    position :: Prelude.Maybe Rectangle,
    -- | Specify how the transcoding service applies the denoise and deblock
    -- filters. You must also enable the filters separately, with Denoise
    -- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
    -- transcoding service determines whether to apply filtering, depending on
    -- input type and quality. * Disable - The input is not filtered. This is
    -- true even if you use the API to enable them in (InputDeblockFilter) and
    -- (InputDeblockFilter). * Force - The input is filtered regardless of
    -- input type.
    filterEnable :: Prelude.Maybe InputFilterEnable
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
-- 'videoSelector', 'inputTemplate_videoSelector' - Input video selectors contain the video settings for the input. Each of
-- your inputs can have up to one video selector.
--
-- 'programNumber', 'inputTemplate_programNumber' - Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
--
-- 'audioSelectorGroups', 'inputTemplate_audioSelectorGroups' - Use audio selector groups to combine multiple sidecar audio inputs so
-- that you can assign them to a single output audio tab
-- (AudioDescription). Note that, if you\'re working with embedded audio,
-- it\'s simpler to assign multiple input tracks into a single audio
-- selector rather than use an audio selector group.
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
--
-- 'audioSelectors', 'inputTemplate_audioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
--
-- 'deblockFilter', 'inputTemplate_deblockFilter' - Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
--
-- 'inputClippings', 'inputTemplate_inputClippings' - (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
--
-- 'crop', 'inputTemplate_crop' - Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
--
-- 'denoiseFilter', 'inputTemplate_denoiseFilter' - Enable Denoise (InputDenoiseFilter) to filter noise from the input.
-- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
-- uncompressed video inputs.
--
-- 'imageInserter', 'inputTemplate_imageInserter' - Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input individually. This
-- setting is disabled by default.
--
-- 'filterStrength', 'inputTemplate_filterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input
-- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
-- 0.
--
-- 'psiControl', 'inputTemplate_psiControl' - Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
--
-- 'captionSelectors', 'inputTemplate_captionSelectors' - Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
--
-- 'timecodeStart', 'inputTemplate_timecodeStart' - Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
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
-- 'position', 'inputTemplate_position' - Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black. If you specify a value here, it will override any value that you
-- specify in the output setting Selection placement (position). If you
-- specify a value here, this will override any AFD values in your input,
-- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
-- you specify a value here, this will ignore anything that you specify for
-- the setting Scaling Behavior (scalingBehavior).
--
-- 'filterEnable', 'inputTemplate_filterEnable' - Specify how the transcoding service applies the denoise and deblock
-- filters. You must also enable the filters separately, with Denoise
-- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
-- transcoding service determines whether to apply filtering, depending on
-- input type and quality. * Disable - The input is not filtered. This is
-- true even if you use the API to enable them in (InputDeblockFilter) and
-- (InputDeblockFilter). * Force - The input is filtered regardless of
-- input type.
newInputTemplate ::
  InputTemplate
newInputTemplate =
  InputTemplate'
    { videoSelector = Prelude.Nothing,
      programNumber = Prelude.Nothing,
      audioSelectorGroups = Prelude.Nothing,
      timecodeSource = Prelude.Nothing,
      audioSelectors = Prelude.Nothing,
      deblockFilter = Prelude.Nothing,
      inputClippings = Prelude.Nothing,
      crop = Prelude.Nothing,
      denoiseFilter = Prelude.Nothing,
      imageInserter = Prelude.Nothing,
      filterStrength = Prelude.Nothing,
      psiControl = Prelude.Nothing,
      captionSelectors = Prelude.Nothing,
      timecodeStart = Prelude.Nothing,
      inputScanType = Prelude.Nothing,
      position = Prelude.Nothing,
      filterEnable = Prelude.Nothing
    }

-- | Input video selectors contain the video settings for the input. Each of
-- your inputs can have up to one video selector.
inputTemplate_videoSelector :: Lens.Lens' InputTemplate (Prelude.Maybe VideoSelector)
inputTemplate_videoSelector = Lens.lens (\InputTemplate' {videoSelector} -> videoSelector) (\s@InputTemplate' {} a -> s {videoSelector = a} :: InputTemplate)

-- | Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
inputTemplate_programNumber :: Lens.Lens' InputTemplate (Prelude.Maybe Prelude.Natural)
inputTemplate_programNumber = Lens.lens (\InputTemplate' {programNumber} -> programNumber) (\s@InputTemplate' {} a -> s {programNumber = a} :: InputTemplate)

-- | Use audio selector groups to combine multiple sidecar audio inputs so
-- that you can assign them to a single output audio tab
-- (AudioDescription). Note that, if you\'re working with embedded audio,
-- it\'s simpler to assign multiple input tracks into a single audio
-- selector rather than use an audio selector group.
inputTemplate_audioSelectorGroups :: Lens.Lens' InputTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelectorGroup))
inputTemplate_audioSelectorGroups = Lens.lens (\InputTemplate' {audioSelectorGroups} -> audioSelectorGroups) (\s@InputTemplate' {} a -> s {audioSelectorGroups = a} :: InputTemplate) Prelude.. Lens.mapping Lens.coerced

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

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
inputTemplate_audioSelectors :: Lens.Lens' InputTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelector))
inputTemplate_audioSelectors = Lens.lens (\InputTemplate' {audioSelectors} -> audioSelectors) (\s@InputTemplate' {} a -> s {audioSelectors = a} :: InputTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
inputTemplate_deblockFilter :: Lens.Lens' InputTemplate (Prelude.Maybe InputDeblockFilter)
inputTemplate_deblockFilter = Lens.lens (\InputTemplate' {deblockFilter} -> deblockFilter) (\s@InputTemplate' {} a -> s {deblockFilter = a} :: InputTemplate)

-- | (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
inputTemplate_inputClippings :: Lens.Lens' InputTemplate (Prelude.Maybe [InputClipping])
inputTemplate_inputClippings = Lens.lens (\InputTemplate' {inputClippings} -> inputClippings) (\s@InputTemplate' {} a -> s {inputClippings = a} :: InputTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
inputTemplate_crop :: Lens.Lens' InputTemplate (Prelude.Maybe Rectangle)
inputTemplate_crop = Lens.lens (\InputTemplate' {crop} -> crop) (\s@InputTemplate' {} a -> s {crop = a} :: InputTemplate)

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
-- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
-- uncompressed video inputs.
inputTemplate_denoiseFilter :: Lens.Lens' InputTemplate (Prelude.Maybe InputDenoiseFilter)
inputTemplate_denoiseFilter = Lens.lens (\InputTemplate' {denoiseFilter} -> denoiseFilter) (\s@InputTemplate' {} a -> s {denoiseFilter = a} :: InputTemplate)

-- | Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input individually. This
-- setting is disabled by default.
inputTemplate_imageInserter :: Lens.Lens' InputTemplate (Prelude.Maybe ImageInserter)
inputTemplate_imageInserter = Lens.lens (\InputTemplate' {imageInserter} -> imageInserter) (\s@InputTemplate' {} a -> s {imageInserter = a} :: InputTemplate)

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

-- | Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
inputTemplate_captionSelectors :: Lens.Lens' InputTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text CaptionSelector))
inputTemplate_captionSelectors = Lens.lens (\InputTemplate' {captionSelectors} -> captionSelectors) (\s@InputTemplate' {} a -> s {captionSelectors = a} :: InputTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
inputTemplate_timecodeStart :: Lens.Lens' InputTemplate (Prelude.Maybe Prelude.Text)
inputTemplate_timecodeStart = Lens.lens (\InputTemplate' {timecodeStart} -> timecodeStart) (\s@InputTemplate' {} a -> s {timecodeStart = a} :: InputTemplate)

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

instance Core.FromJSON InputTemplate where
  parseJSON =
    Core.withObject
      "InputTemplate"
      ( \x ->
          InputTemplate'
            Prelude.<$> (x Core..:? "videoSelector")
            Prelude.<*> (x Core..:? "programNumber")
            Prelude.<*> ( x Core..:? "audioSelectorGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "timecodeSource")
            Prelude.<*> (x Core..:? "audioSelectors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "deblockFilter")
            Prelude.<*> (x Core..:? "inputClippings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "crop")
            Prelude.<*> (x Core..:? "denoiseFilter")
            Prelude.<*> (x Core..:? "imageInserter")
            Prelude.<*> (x Core..:? "filterStrength")
            Prelude.<*> (x Core..:? "psiControl")
            Prelude.<*> ( x Core..:? "captionSelectors"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "timecodeStart")
            Prelude.<*> (x Core..:? "inputScanType")
            Prelude.<*> (x Core..:? "position")
            Prelude.<*> (x Core..:? "filterEnable")
      )

instance Prelude.Hashable InputTemplate where
  hashWithSalt _salt InputTemplate' {..} =
    _salt `Prelude.hashWithSalt` videoSelector
      `Prelude.hashWithSalt` programNumber
      `Prelude.hashWithSalt` audioSelectorGroups
      `Prelude.hashWithSalt` timecodeSource
      `Prelude.hashWithSalt` audioSelectors
      `Prelude.hashWithSalt` deblockFilter
      `Prelude.hashWithSalt` inputClippings
      `Prelude.hashWithSalt` crop
      `Prelude.hashWithSalt` denoiseFilter
      `Prelude.hashWithSalt` imageInserter
      `Prelude.hashWithSalt` filterStrength
      `Prelude.hashWithSalt` psiControl
      `Prelude.hashWithSalt` captionSelectors
      `Prelude.hashWithSalt` timecodeStart
      `Prelude.hashWithSalt` inputScanType
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` filterEnable

instance Prelude.NFData InputTemplate where
  rnf InputTemplate' {..} =
    Prelude.rnf videoSelector
      `Prelude.seq` Prelude.rnf programNumber
      `Prelude.seq` Prelude.rnf audioSelectorGroups
      `Prelude.seq` Prelude.rnf timecodeSource
      `Prelude.seq` Prelude.rnf audioSelectors
      `Prelude.seq` Prelude.rnf deblockFilter
      `Prelude.seq` Prelude.rnf inputClippings
      `Prelude.seq` Prelude.rnf crop
      `Prelude.seq` Prelude.rnf denoiseFilter
      `Prelude.seq` Prelude.rnf imageInserter
      `Prelude.seq` Prelude.rnf filterStrength
      `Prelude.seq` Prelude.rnf psiControl
      `Prelude.seq` Prelude.rnf captionSelectors
      `Prelude.seq` Prelude.rnf timecodeStart
      `Prelude.seq` Prelude.rnf inputScanType
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf filterEnable

instance Core.ToJSON InputTemplate where
  toJSON InputTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("videoSelector" Core..=) Prelude.<$> videoSelector,
            ("programNumber" Core..=) Prelude.<$> programNumber,
            ("audioSelectorGroups" Core..=)
              Prelude.<$> audioSelectorGroups,
            ("timecodeSource" Core..=)
              Prelude.<$> timecodeSource,
            ("audioSelectors" Core..=)
              Prelude.<$> audioSelectors,
            ("deblockFilter" Core..=) Prelude.<$> deblockFilter,
            ("inputClippings" Core..=)
              Prelude.<$> inputClippings,
            ("crop" Core..=) Prelude.<$> crop,
            ("denoiseFilter" Core..=) Prelude.<$> denoiseFilter,
            ("imageInserter" Core..=) Prelude.<$> imageInserter,
            ("filterStrength" Core..=)
              Prelude.<$> filterStrength,
            ("psiControl" Core..=) Prelude.<$> psiControl,
            ("captionSelectors" Core..=)
              Prelude.<$> captionSelectors,
            ("timecodeStart" Core..=) Prelude.<$> timecodeStart,
            ("inputScanType" Core..=) Prelude.<$> inputScanType,
            ("position" Core..=) Prelude.<$> position,
            ("filterEnable" Core..=) Prelude.<$> filterEnable
          ]
      )
