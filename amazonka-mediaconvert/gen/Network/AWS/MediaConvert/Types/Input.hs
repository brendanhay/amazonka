{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Prelude as Prelude

-- | Specifies media input
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | Enable the image inserter feature to include a graphic overlay on your
    -- video. Enable or disable this feature for each input individually. This
    -- setting is disabled by default.
    imageInserter :: Prelude.Maybe ImageInserter,
    -- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
    -- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
    -- uncompressed video inputs.
    denoiseFilter :: Prelude.Maybe InputDenoiseFilter,
    -- | Provide a list of any necessary supplemental IMPs. You need supplemental
    -- IMPs if the CPL that you\'re using for your input is in an incomplete
    -- IMP. Specify either the supplemental IMP directories with a trailing
    -- slash or the ASSETMAP.xml files. For example [\"s3:\/\/bucket\/ov\/\",
    -- \"s3:\/\/bucket\/vf2\/ASSETMAP.xml\"]. You don\'t need to specify the
    -- IMP that contains your input CPL, because the service automatically
    -- detects it.
    supplementalImps :: Prelude.Maybe [Prelude.Text],
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
    -- | Specify the source file for your transcoding job. You can use multiple
    -- inputs in a single job. The service concatenates these inputs, in the
    -- order that you specify them in the job, to create the outputs. If your
    -- input format is IMF, specify your input by providing the path to your
    -- CPL. For example, \"s3:\/\/bucket\/vf\/cpl.xml\". If the CPL is in an
    -- incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps)
    -- to specify any supplemental IMPs that contain assets referenced by the
    -- CPL.
    fileInput :: Prelude.Maybe Prelude.Text,
    -- | Specify the timecode that you want the service to use for this input\'s
    -- initial frame. To use this setting, you must set the Timecode source
    -- setting, located under the input settings (InputTimecodeSource), to
    -- Specified start (SPECIFIEDSTART). For more information about timecodes,
    -- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
    timecodeStart :: Prelude.Maybe Prelude.Text,
    -- | Settings for decrypting any input files that you encrypt before you
    -- upload them to Amazon S3. MediaConvert can decrypt files only when you
    -- use AWS Key Management Service (KMS) to encrypt the data key that you
    -- use to encrypt your content.
    decryptionSettings :: Prelude.Maybe InputDecryptionSettings,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { imageInserter = Prelude.Nothing,
      denoiseFilter = Prelude.Nothing,
      supplementalImps = Prelude.Nothing,
      inputScanType = Prelude.Nothing,
      inputClippings = Prelude.Nothing,
      fileInput = Prelude.Nothing,
      timecodeStart = Prelude.Nothing,
      decryptionSettings = Prelude.Nothing,
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
input_imageInserter :: Lens.Lens' Input (Prelude.Maybe ImageInserter)
input_imageInserter = Lens.lens (\Input' {imageInserter} -> imageInserter) (\s@Input' {} a -> s {imageInserter = a} :: Input)

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
-- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
-- uncompressed video inputs.
input_denoiseFilter :: Lens.Lens' Input (Prelude.Maybe InputDenoiseFilter)
input_denoiseFilter = Lens.lens (\Input' {denoiseFilter} -> denoiseFilter) (\s@Input' {} a -> s {denoiseFilter = a} :: Input)

-- | Provide a list of any necessary supplemental IMPs. You need supplemental
-- IMPs if the CPL that you\'re using for your input is in an incomplete
-- IMP. Specify either the supplemental IMP directories with a trailing
-- slash or the ASSETMAP.xml files. For example [\"s3:\/\/bucket\/ov\/\",
-- \"s3:\/\/bucket\/vf2\/ASSETMAP.xml\"]. You don\'t need to specify the
-- IMP that contains your input CPL, because the service automatically
-- detects it.
input_supplementalImps :: Lens.Lens' Input (Prelude.Maybe [Prelude.Text])
input_supplementalImps = Lens.lens (\Input' {supplementalImps} -> supplementalImps) (\s@Input' {} a -> s {supplementalImps = a} :: Input) Prelude.. Lens.mapping Prelude._Coerce

-- | When you have a progressive segmented frame (PsF) input, use this
-- setting to flag the input as PsF. MediaConvert doesn\'t automatically
-- detect PsF. Therefore, flagging your input as PsF results in better
-- preservation of video quality when you do deinterlacing and frame rate
-- conversion. If you don\'t specify, the default value is Auto (AUTO).
-- Auto is the correct setting for all inputs that are not PsF. Don\'t set
-- this value to PsF when your input is interlaced. Doing so creates
-- horizontal interlacing artifacts.
input_inputScanType :: Lens.Lens' Input (Prelude.Maybe InputScanType)
input_inputScanType = Lens.lens (\Input' {inputScanType} -> inputScanType) (\s@Input' {} a -> s {inputScanType = a} :: Input)

-- | (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
input_inputClippings :: Lens.Lens' Input (Prelude.Maybe [InputClipping])
input_inputClippings = Lens.lens (\Input' {inputClippings} -> inputClippings) (\s@Input' {} a -> s {inputClippings = a} :: Input) Prelude.. Lens.mapping Prelude._Coerce

-- | Specify the source file for your transcoding job. You can use multiple
-- inputs in a single job. The service concatenates these inputs, in the
-- order that you specify them in the job, to create the outputs. If your
-- input format is IMF, specify your input by providing the path to your
-- CPL. For example, \"s3:\/\/bucket\/vf\/cpl.xml\". If the CPL is in an
-- incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps)
-- to specify any supplemental IMPs that contain assets referenced by the
-- CPL.
input_fileInput :: Lens.Lens' Input (Prelude.Maybe Prelude.Text)
input_fileInput = Lens.lens (\Input' {fileInput} -> fileInput) (\s@Input' {} a -> s {fileInput = a} :: Input)

-- | Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
input_timecodeStart :: Lens.Lens' Input (Prelude.Maybe Prelude.Text)
input_timecodeStart = Lens.lens (\Input' {timecodeStart} -> timecodeStart) (\s@Input' {} a -> s {timecodeStart = a} :: Input)

-- | Settings for decrypting any input files that you encrypt before you
-- upload them to Amazon S3. MediaConvert can decrypt files only when you
-- use AWS Key Management Service (KMS) to encrypt the data key that you
-- use to encrypt your content.
input_decryptionSettings :: Lens.Lens' Input (Prelude.Maybe InputDecryptionSettings)
input_decryptionSettings = Lens.lens (\Input' {decryptionSettings} -> decryptionSettings) (\s@Input' {} a -> s {decryptionSettings = a} :: Input)

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
input_audioSelectors :: Lens.Lens' Input (Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelector))
input_audioSelectors = Lens.lens (\Input' {audioSelectors} -> audioSelectors) (\s@Input' {} a -> s {audioSelectors = a} :: Input) Prelude.. Lens.mapping Prelude._Coerce

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input
-- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
-- 0.
input_filterStrength :: Lens.Lens' Input (Prelude.Maybe Prelude.Int)
input_filterStrength = Lens.lens (\Input' {filterStrength} -> filterStrength) (\s@Input' {} a -> s {filterStrength = a} :: Input)

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
input_psiControl :: Lens.Lens' Input (Prelude.Maybe InputPsiControl)
input_psiControl = Lens.lens (\Input' {psiControl} -> psiControl) (\s@Input' {} a -> s {psiControl = a} :: Input)

-- | Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
input_programNumber :: Lens.Lens' Input (Prelude.Maybe Prelude.Natural)
input_programNumber = Lens.lens (\Input' {programNumber} -> programNumber) (\s@Input' {} a -> s {programNumber = a} :: Input)

-- | Specifies set of audio selectors within an input to combine. An input
-- may have multiple audio selector groups. See \"Audio Selector
-- Group\":#inputs-audio_selector_group for more information.
input_audioSelectorGroups :: Lens.Lens' Input (Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelectorGroup))
input_audioSelectorGroups = Lens.lens (\Input' {audioSelectorGroups} -> audioSelectorGroups) (\s@Input' {} a -> s {audioSelectorGroups = a} :: Input) Prelude.. Lens.mapping Prelude._Coerce

-- | Selector for video.
input_videoSelector :: Lens.Lens' Input (Prelude.Maybe VideoSelector)
input_videoSelector = Lens.lens (\Input' {videoSelector} -> videoSelector) (\s@Input' {} a -> s {videoSelector = a} :: Input)

-- | Specify how the transcoding service applies the denoise and deblock
-- filters. You must also enable the filters separately, with Denoise
-- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
-- transcoding service determines whether to apply filtering, depending on
-- input type and quality. * Disable - The input is not filtered. This is
-- true even if you use the API to enable them in (InputDeblockFilter) and
-- (InputDeblockFilter). * Force - The input is filtered regardless of
-- input type.
input_filterEnable :: Lens.Lens' Input (Prelude.Maybe InputFilterEnable)
input_filterEnable = Lens.lens (\Input' {filterEnable} -> filterEnable) (\s@Input' {} a -> s {filterEnable = a} :: Input)

-- | Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black. If you specify a value here, it will override any value that you
-- specify in the output setting Selection placement (position). If you
-- specify a value here, this will override any AFD values in your input,
-- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
-- you specify a value here, this will ignore anything that you specify for
-- the setting Scaling Behavior (scalingBehavior).
input_position :: Lens.Lens' Input (Prelude.Maybe Rectangle)
input_position = Lens.lens (\Input' {position} -> position) (\s@Input' {} a -> s {position = a} :: Input)

-- | Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
input_crop :: Lens.Lens' Input (Prelude.Maybe Rectangle)
input_crop = Lens.lens (\Input' {crop} -> crop) (\s@Input' {} a -> s {crop = a} :: Input)

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
input_deblockFilter :: Lens.Lens' Input (Prelude.Maybe InputDeblockFilter)
input_deblockFilter = Lens.lens (\Input' {deblockFilter} -> deblockFilter) (\s@Input' {} a -> s {deblockFilter = a} :: Input)

-- | Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
input_captionSelectors :: Lens.Lens' Input (Prelude.Maybe (Prelude.HashMap Prelude.Text CaptionSelector))
input_captionSelectors = Lens.lens (\Input' {captionSelectors} -> captionSelectors) (\s@Input' {} a -> s {captionSelectors = a} :: Input) Prelude.. Lens.mapping Prelude._Coerce

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
input_timecodeSource :: Lens.Lens' Input (Prelude.Maybe InputTimecodeSource)
input_timecodeSource = Lens.lens (\Input' {timecodeSource} -> timecodeSource) (\s@Input' {} a -> s {timecodeSource = a} :: Input)

instance Prelude.FromJSON Input where
  parseJSON =
    Prelude.withObject
      "Input"
      ( \x ->
          Input'
            Prelude.<$> (x Prelude..:? "imageInserter")
            Prelude.<*> (x Prelude..:? "denoiseFilter")
            Prelude.<*> ( x Prelude..:? "supplementalImps"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "inputScanType")
            Prelude.<*> ( x Prelude..:? "inputClippings"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "fileInput")
            Prelude.<*> (x Prelude..:? "timecodeStart")
            Prelude.<*> (x Prelude..:? "decryptionSettings")
            Prelude.<*> ( x Prelude..:? "audioSelectors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "filterStrength")
            Prelude.<*> (x Prelude..:? "psiControl")
            Prelude.<*> (x Prelude..:? "programNumber")
            Prelude.<*> ( x Prelude..:? "audioSelectorGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "videoSelector")
            Prelude.<*> (x Prelude..:? "filterEnable")
            Prelude.<*> (x Prelude..:? "position")
            Prelude.<*> (x Prelude..:? "crop")
            Prelude.<*> (x Prelude..:? "deblockFilter")
            Prelude.<*> ( x Prelude..:? "captionSelectors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "timecodeSource")
      )

instance Prelude.Hashable Input

instance Prelude.NFData Input

instance Prelude.ToJSON Input where
  toJSON Input' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("imageInserter" Prelude..=)
              Prelude.<$> imageInserter,
            ("denoiseFilter" Prelude..=)
              Prelude.<$> denoiseFilter,
            ("supplementalImps" Prelude..=)
              Prelude.<$> supplementalImps,
            ("inputScanType" Prelude..=)
              Prelude.<$> inputScanType,
            ("inputClippings" Prelude..=)
              Prelude.<$> inputClippings,
            ("fileInput" Prelude..=) Prelude.<$> fileInput,
            ("timecodeStart" Prelude..=)
              Prelude.<$> timecodeStart,
            ("decryptionSettings" Prelude..=)
              Prelude.<$> decryptionSettings,
            ("audioSelectors" Prelude..=)
              Prelude.<$> audioSelectors,
            ("filterStrength" Prelude..=)
              Prelude.<$> filterStrength,
            ("psiControl" Prelude..=) Prelude.<$> psiControl,
            ("programNumber" Prelude..=)
              Prelude.<$> programNumber,
            ("audioSelectorGroups" Prelude..=)
              Prelude.<$> audioSelectorGroups,
            ("videoSelector" Prelude..=)
              Prelude.<$> videoSelector,
            ("filterEnable" Prelude..=) Prelude.<$> filterEnable,
            ("position" Prelude..=) Prelude.<$> position,
            ("crop" Prelude..=) Prelude.<$> crop,
            ("deblockFilter" Prelude..=)
              Prelude.<$> deblockFilter,
            ("captionSelectors" Prelude..=)
              Prelude.<$> captionSelectors,
            ("timecodeSource" Prelude..=)
              Prelude.<$> timecodeSource
          ]
      )
