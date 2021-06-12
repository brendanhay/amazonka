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
-- Module      : Network.AWS.MediaConvert.Types.Input
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Input where

import qualified Network.AWS.Core as Core
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

-- | Specifies media input
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | Enable the image inserter feature to include a graphic overlay on your
    -- video. Enable or disable this feature for each input individually. This
    -- setting is disabled by default.
    imageInserter :: Core.Maybe ImageInserter,
    -- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
    -- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
    -- uncompressed video inputs.
    denoiseFilter :: Core.Maybe InputDenoiseFilter,
    -- | Provide a list of any necessary supplemental IMPs. You need supplemental
    -- IMPs if the CPL that you\'re using for your input is in an incomplete
    -- IMP. Specify either the supplemental IMP directories with a trailing
    -- slash or the ASSETMAP.xml files. For example [\"s3:\/\/bucket\/ov\/\",
    -- \"s3:\/\/bucket\/vf2\/ASSETMAP.xml\"]. You don\'t need to specify the
    -- IMP that contains your input CPL, because the service automatically
    -- detects it.
    supplementalImps :: Core.Maybe [Core.Text],
    -- | When you have a progressive segmented frame (PsF) input, use this
    -- setting to flag the input as PsF. MediaConvert doesn\'t automatically
    -- detect PsF. Therefore, flagging your input as PsF results in better
    -- preservation of video quality when you do deinterlacing and frame rate
    -- conversion. If you don\'t specify, the default value is Auto (AUTO).
    -- Auto is the correct setting for all inputs that are not PsF. Don\'t set
    -- this value to PsF when your input is interlaced. Doing so creates
    -- horizontal interlacing artifacts.
    inputScanType :: Core.Maybe InputScanType,
    -- | (InputClippings) contains sets of start and end times that together
    -- specify a portion of the input to be used in the outputs. If you provide
    -- only a start time, the clip will be the entire input from that point to
    -- the end. If you provide only an end time, it will be the entire input up
    -- to that point. When you specify more than one input clip, the
    -- transcoding service creates the job outputs by stringing the clips
    -- together in the order you specify them.
    inputClippings :: Core.Maybe [InputClipping],
    -- | Specify the source file for your transcoding job. You can use multiple
    -- inputs in a single job. The service concatenates these inputs, in the
    -- order that you specify them in the job, to create the outputs. If your
    -- input format is IMF, specify your input by providing the path to your
    -- CPL. For example, \"s3:\/\/bucket\/vf\/cpl.xml\". If the CPL is in an
    -- incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps)
    -- to specify any supplemental IMPs that contain assets referenced by the
    -- CPL.
    fileInput :: Core.Maybe Core.Text,
    -- | Specify the timecode that you want the service to use for this input\'s
    -- initial frame. To use this setting, you must set the Timecode source
    -- setting, located under the input settings (InputTimecodeSource), to
    -- Specified start (SPECIFIEDSTART). For more information about timecodes,
    -- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
    timecodeStart :: Core.Maybe Core.Text,
    -- | Settings for decrypting any input files that you encrypt before you
    -- upload them to Amazon S3. MediaConvert can decrypt files only when you
    -- use AWS Key Management Service (KMS) to encrypt the data key that you
    -- use to encrypt your content.
    decryptionSettings :: Core.Maybe InputDecryptionSettings,
    -- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
    -- from the input that you will use in your outputs. You can use multiple
    -- Audio selectors per input.
    audioSelectors :: Core.Maybe (Core.HashMap Core.Text AudioSelector),
    -- | Use Filter strength (FilterStrength) to adjust the magnitude the input
    -- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
    -- 0.
    filterStrength :: Core.Maybe Core.Int,
    -- | Set PSI control (InputPsiControl) for transport stream inputs to specify
    -- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
    -- audio and video. * Use PSI - Scan only PSI data.
    psiControl :: Core.Maybe InputPsiControl,
    -- | Use Program (programNumber) to select a specific program from within a
    -- multi-program transport stream. Note that Quad 4K is not currently
    -- supported. Default is the first program within the transport stream. If
    -- the program you specify doesn\'t exist, the transcoding service will use
    -- this default.
    programNumber :: Core.Maybe Core.Natural,
    -- | Specifies set of audio selectors within an input to combine. An input
    -- may have multiple audio selector groups. See \"Audio Selector
    -- Group\":#inputs-audio_selector_group for more information.
    audioSelectorGroups :: Core.Maybe (Core.HashMap Core.Text AudioSelectorGroup),
    -- | Selector for video.
    videoSelector :: Core.Maybe VideoSelector,
    -- | Specify how the transcoding service applies the denoise and deblock
    -- filters. You must also enable the filters separately, with Denoise
    -- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
    -- transcoding service determines whether to apply filtering, depending on
    -- input type and quality. * Disable - The input is not filtered. This is
    -- true even if you use the API to enable them in (InputDeblockFilter) and
    -- (InputDeblockFilter). * Force - The input is filtered regardless of
    -- input type.
    filterEnable :: Core.Maybe InputFilterEnable,
    -- | Use Selection placement (position) to define the video area in your
    -- output frame. The area outside of the rectangle that you specify here is
    -- black. If you specify a value here, it will override any value that you
    -- specify in the output setting Selection placement (position). If you
    -- specify a value here, this will override any AFD values in your input,
    -- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
    -- you specify a value here, this will ignore anything that you specify for
    -- the setting Scaling Behavior (scalingBehavior).
    position :: Core.Maybe Rectangle,
    -- | Use Cropping selection (crop) to specify the video area that the service
    -- will include in the output video frame. If you specify a value here, it
    -- will override any value that you specify in the output setting Cropping
    -- selection (crop).
    crop :: Core.Maybe Rectangle,
    -- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
    -- output. Default is disabled. Only manually controllable for MPEG2 and
    -- uncompressed video inputs.
    deblockFilter :: Core.Maybe InputDeblockFilter,
    -- | Use captions selectors to specify the captions data from your input that
    -- you use in your outputs. You can use up to 20 captions selectors per
    -- input.
    captionSelectors :: Core.Maybe (Core.HashMap Core.Text CaptionSelector),
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
    timecodeSource :: Core.Maybe InputTimecodeSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageInserter', 'input_imageInserter' - Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input individually. This
-- setting is disabled by default.
--
-- 'denoiseFilter', 'input_denoiseFilter' - Enable Denoise (InputDenoiseFilter) to filter noise from the input.
-- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
-- uncompressed video inputs.
--
-- 'supplementalImps', 'input_supplementalImps' - Provide a list of any necessary supplemental IMPs. You need supplemental
-- IMPs if the CPL that you\'re using for your input is in an incomplete
-- IMP. Specify either the supplemental IMP directories with a trailing
-- slash or the ASSETMAP.xml files. For example [\"s3:\/\/bucket\/ov\/\",
-- \"s3:\/\/bucket\/vf2\/ASSETMAP.xml\"]. You don\'t need to specify the
-- IMP that contains your input CPL, because the service automatically
-- detects it.
--
-- 'inputScanType', 'input_inputScanType' - When you have a progressive segmented frame (PsF) input, use this
-- setting to flag the input as PsF. MediaConvert doesn\'t automatically
-- detect PsF. Therefore, flagging your input as PsF results in better
-- preservation of video quality when you do deinterlacing and frame rate
-- conversion. If you don\'t specify, the default value is Auto (AUTO).
-- Auto is the correct setting for all inputs that are not PsF. Don\'t set
-- this value to PsF when your input is interlaced. Doing so creates
-- horizontal interlacing artifacts.
--
-- 'inputClippings', 'input_inputClippings' - (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
--
-- 'fileInput', 'input_fileInput' - Specify the source file for your transcoding job. You can use multiple
-- inputs in a single job. The service concatenates these inputs, in the
-- order that you specify them in the job, to create the outputs. If your
-- input format is IMF, specify your input by providing the path to your
-- CPL. For example, \"s3:\/\/bucket\/vf\/cpl.xml\". If the CPL is in an
-- incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps)
-- to specify any supplemental IMPs that contain assets referenced by the
-- CPL.
--
-- 'timecodeStart', 'input_timecodeStart' - Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
--
-- 'decryptionSettings', 'input_decryptionSettings' - Settings for decrypting any input files that you encrypt before you
-- upload them to Amazon S3. MediaConvert can decrypt files only when you
-- use AWS Key Management Service (KMS) to encrypt the data key that you
-- use to encrypt your content.
--
-- 'audioSelectors', 'input_audioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
--
-- 'filterStrength', 'input_filterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input
-- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
-- 0.
--
-- 'psiControl', 'input_psiControl' - Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
--
-- 'programNumber', 'input_programNumber' - Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
--
-- 'audioSelectorGroups', 'input_audioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input
-- may have multiple audio selector groups. See \"Audio Selector
-- Group\":#inputs-audio_selector_group for more information.
--
-- 'videoSelector', 'input_videoSelector' - Selector for video.
--
-- 'filterEnable', 'input_filterEnable' - Specify how the transcoding service applies the denoise and deblock
-- filters. You must also enable the filters separately, with Denoise
-- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
-- transcoding service determines whether to apply filtering, depending on
-- input type and quality. * Disable - The input is not filtered. This is
-- true even if you use the API to enable them in (InputDeblockFilter) and
-- (InputDeblockFilter). * Force - The input is filtered regardless of
-- input type.
--
-- 'position', 'input_position' - Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black. If you specify a value here, it will override any value that you
-- specify in the output setting Selection placement (position). If you
-- specify a value here, this will override any AFD values in your input,
-- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
-- you specify a value here, this will ignore anything that you specify for
-- the setting Scaling Behavior (scalingBehavior).
--
-- 'crop', 'input_crop' - Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
--
-- 'deblockFilter', 'input_deblockFilter' - Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
--
-- 'captionSelectors', 'input_captionSelectors' - Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
--
-- 'timecodeSource', 'input_timecodeSource' - Use this Timecode source setting, located under the input settings
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
newInput ::
  Input
newInput =
  Input'
    { imageInserter = Core.Nothing,
      denoiseFilter = Core.Nothing,
      supplementalImps = Core.Nothing,
      inputScanType = Core.Nothing,
      inputClippings = Core.Nothing,
      fileInput = Core.Nothing,
      timecodeStart = Core.Nothing,
      decryptionSettings = Core.Nothing,
      audioSelectors = Core.Nothing,
      filterStrength = Core.Nothing,
      psiControl = Core.Nothing,
      programNumber = Core.Nothing,
      audioSelectorGroups = Core.Nothing,
      videoSelector = Core.Nothing,
      filterEnable = Core.Nothing,
      position = Core.Nothing,
      crop = Core.Nothing,
      deblockFilter = Core.Nothing,
      captionSelectors = Core.Nothing,
      timecodeSource = Core.Nothing
    }

-- | Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input individually. This
-- setting is disabled by default.
input_imageInserter :: Lens.Lens' Input (Core.Maybe ImageInserter)
input_imageInserter = Lens.lens (\Input' {imageInserter} -> imageInserter) (\s@Input' {} a -> s {imageInserter = a} :: Input)

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
-- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
-- uncompressed video inputs.
input_denoiseFilter :: Lens.Lens' Input (Core.Maybe InputDenoiseFilter)
input_denoiseFilter = Lens.lens (\Input' {denoiseFilter} -> denoiseFilter) (\s@Input' {} a -> s {denoiseFilter = a} :: Input)

-- | Provide a list of any necessary supplemental IMPs. You need supplemental
-- IMPs if the CPL that you\'re using for your input is in an incomplete
-- IMP. Specify either the supplemental IMP directories with a trailing
-- slash or the ASSETMAP.xml files. For example [\"s3:\/\/bucket\/ov\/\",
-- \"s3:\/\/bucket\/vf2\/ASSETMAP.xml\"]. You don\'t need to specify the
-- IMP that contains your input CPL, because the service automatically
-- detects it.
input_supplementalImps :: Lens.Lens' Input (Core.Maybe [Core.Text])
input_supplementalImps = Lens.lens (\Input' {supplementalImps} -> supplementalImps) (\s@Input' {} a -> s {supplementalImps = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | When you have a progressive segmented frame (PsF) input, use this
-- setting to flag the input as PsF. MediaConvert doesn\'t automatically
-- detect PsF. Therefore, flagging your input as PsF results in better
-- preservation of video quality when you do deinterlacing and frame rate
-- conversion. If you don\'t specify, the default value is Auto (AUTO).
-- Auto is the correct setting for all inputs that are not PsF. Don\'t set
-- this value to PsF when your input is interlaced. Doing so creates
-- horizontal interlacing artifacts.
input_inputScanType :: Lens.Lens' Input (Core.Maybe InputScanType)
input_inputScanType = Lens.lens (\Input' {inputScanType} -> inputScanType) (\s@Input' {} a -> s {inputScanType = a} :: Input)

-- | (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
input_inputClippings :: Lens.Lens' Input (Core.Maybe [InputClipping])
input_inputClippings = Lens.lens (\Input' {inputClippings} -> inputClippings) (\s@Input' {} a -> s {inputClippings = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | Specify the source file for your transcoding job. You can use multiple
-- inputs in a single job. The service concatenates these inputs, in the
-- order that you specify them in the job, to create the outputs. If your
-- input format is IMF, specify your input by providing the path to your
-- CPL. For example, \"s3:\/\/bucket\/vf\/cpl.xml\". If the CPL is in an
-- incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps)
-- to specify any supplemental IMPs that contain assets referenced by the
-- CPL.
input_fileInput :: Lens.Lens' Input (Core.Maybe Core.Text)
input_fileInput = Lens.lens (\Input' {fileInput} -> fileInput) (\s@Input' {} a -> s {fileInput = a} :: Input)

-- | Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
input_timecodeStart :: Lens.Lens' Input (Core.Maybe Core.Text)
input_timecodeStart = Lens.lens (\Input' {timecodeStart} -> timecodeStart) (\s@Input' {} a -> s {timecodeStart = a} :: Input)

-- | Settings for decrypting any input files that you encrypt before you
-- upload them to Amazon S3. MediaConvert can decrypt files only when you
-- use AWS Key Management Service (KMS) to encrypt the data key that you
-- use to encrypt your content.
input_decryptionSettings :: Lens.Lens' Input (Core.Maybe InputDecryptionSettings)
input_decryptionSettings = Lens.lens (\Input' {decryptionSettings} -> decryptionSettings) (\s@Input' {} a -> s {decryptionSettings = a} :: Input)

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
input_audioSelectors :: Lens.Lens' Input (Core.Maybe (Core.HashMap Core.Text AudioSelector))
input_audioSelectors = Lens.lens (\Input' {audioSelectors} -> audioSelectors) (\s@Input' {} a -> s {audioSelectors = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input
-- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
-- 0.
input_filterStrength :: Lens.Lens' Input (Core.Maybe Core.Int)
input_filterStrength = Lens.lens (\Input' {filterStrength} -> filterStrength) (\s@Input' {} a -> s {filterStrength = a} :: Input)

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
input_psiControl :: Lens.Lens' Input (Core.Maybe InputPsiControl)
input_psiControl = Lens.lens (\Input' {psiControl} -> psiControl) (\s@Input' {} a -> s {psiControl = a} :: Input)

-- | Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
input_programNumber :: Lens.Lens' Input (Core.Maybe Core.Natural)
input_programNumber = Lens.lens (\Input' {programNumber} -> programNumber) (\s@Input' {} a -> s {programNumber = a} :: Input)

-- | Specifies set of audio selectors within an input to combine. An input
-- may have multiple audio selector groups. See \"Audio Selector
-- Group\":#inputs-audio_selector_group for more information.
input_audioSelectorGroups :: Lens.Lens' Input (Core.Maybe (Core.HashMap Core.Text AudioSelectorGroup))
input_audioSelectorGroups = Lens.lens (\Input' {audioSelectorGroups} -> audioSelectorGroups) (\s@Input' {} a -> s {audioSelectorGroups = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | Selector for video.
input_videoSelector :: Lens.Lens' Input (Core.Maybe VideoSelector)
input_videoSelector = Lens.lens (\Input' {videoSelector} -> videoSelector) (\s@Input' {} a -> s {videoSelector = a} :: Input)

-- | Specify how the transcoding service applies the denoise and deblock
-- filters. You must also enable the filters separately, with Denoise
-- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
-- transcoding service determines whether to apply filtering, depending on
-- input type and quality. * Disable - The input is not filtered. This is
-- true even if you use the API to enable them in (InputDeblockFilter) and
-- (InputDeblockFilter). * Force - The input is filtered regardless of
-- input type.
input_filterEnable :: Lens.Lens' Input (Core.Maybe InputFilterEnable)
input_filterEnable = Lens.lens (\Input' {filterEnable} -> filterEnable) (\s@Input' {} a -> s {filterEnable = a} :: Input)

-- | Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black. If you specify a value here, it will override any value that you
-- specify in the output setting Selection placement (position). If you
-- specify a value here, this will override any AFD values in your input,
-- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
-- you specify a value here, this will ignore anything that you specify for
-- the setting Scaling Behavior (scalingBehavior).
input_position :: Lens.Lens' Input (Core.Maybe Rectangle)
input_position = Lens.lens (\Input' {position} -> position) (\s@Input' {} a -> s {position = a} :: Input)

-- | Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
input_crop :: Lens.Lens' Input (Core.Maybe Rectangle)
input_crop = Lens.lens (\Input' {crop} -> crop) (\s@Input' {} a -> s {crop = a} :: Input)

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
input_deblockFilter :: Lens.Lens' Input (Core.Maybe InputDeblockFilter)
input_deblockFilter = Lens.lens (\Input' {deblockFilter} -> deblockFilter) (\s@Input' {} a -> s {deblockFilter = a} :: Input)

-- | Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
input_captionSelectors :: Lens.Lens' Input (Core.Maybe (Core.HashMap Core.Text CaptionSelector))
input_captionSelectors = Lens.lens (\Input' {captionSelectors} -> captionSelectors) (\s@Input' {} a -> s {captionSelectors = a} :: Input) Core.. Lens.mapping Lens._Coerce

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
input_timecodeSource :: Lens.Lens' Input (Core.Maybe InputTimecodeSource)
input_timecodeSource = Lens.lens (\Input' {timecodeSource} -> timecodeSource) (\s@Input' {} a -> s {timecodeSource = a} :: Input)

instance Core.FromJSON Input where
  parseJSON =
    Core.withObject
      "Input"
      ( \x ->
          Input'
            Core.<$> (x Core..:? "imageInserter")
            Core.<*> (x Core..:? "denoiseFilter")
            Core.<*> (x Core..:? "supplementalImps" Core..!= Core.mempty)
            Core.<*> (x Core..:? "inputScanType")
            Core.<*> (x Core..:? "inputClippings" Core..!= Core.mempty)
            Core.<*> (x Core..:? "fileInput")
            Core.<*> (x Core..:? "timecodeStart")
            Core.<*> (x Core..:? "decryptionSettings")
            Core.<*> (x Core..:? "audioSelectors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "filterStrength")
            Core.<*> (x Core..:? "psiControl")
            Core.<*> (x Core..:? "programNumber")
            Core.<*> ( x Core..:? "audioSelectorGroups"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "videoSelector")
            Core.<*> (x Core..:? "filterEnable")
            Core.<*> (x Core..:? "position")
            Core.<*> (x Core..:? "crop")
            Core.<*> (x Core..:? "deblockFilter")
            Core.<*> (x Core..:? "captionSelectors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "timecodeSource")
      )

instance Core.Hashable Input

instance Core.NFData Input

instance Core.ToJSON Input where
  toJSON Input' {..} =
    Core.object
      ( Core.catMaybes
          [ ("imageInserter" Core..=) Core.<$> imageInserter,
            ("denoiseFilter" Core..=) Core.<$> denoiseFilter,
            ("supplementalImps" Core..=)
              Core.<$> supplementalImps,
            ("inputScanType" Core..=) Core.<$> inputScanType,
            ("inputClippings" Core..=) Core.<$> inputClippings,
            ("fileInput" Core..=) Core.<$> fileInput,
            ("timecodeStart" Core..=) Core.<$> timecodeStart,
            ("decryptionSettings" Core..=)
              Core.<$> decryptionSettings,
            ("audioSelectors" Core..=) Core.<$> audioSelectors,
            ("filterStrength" Core..=) Core.<$> filterStrength,
            ("psiControl" Core..=) Core.<$> psiControl,
            ("programNumber" Core..=) Core.<$> programNumber,
            ("audioSelectorGroups" Core..=)
              Core.<$> audioSelectorGroups,
            ("videoSelector" Core..=) Core.<$> videoSelector,
            ("filterEnable" Core..=) Core.<$> filterEnable,
            ("position" Core..=) Core.<$> position,
            ("crop" Core..=) Core.<$> crop,
            ("deblockFilter" Core..=) Core.<$> deblockFilter,
            ("captionSelectors" Core..=)
              Core.<$> captionSelectors,
            ("timecodeSource" Core..=) Core.<$> timecodeSource
          ]
      )
