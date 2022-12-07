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
-- Module      : Amazonka.MediaConvert.Types.Input
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Input where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AudioSelector
import Amazonka.MediaConvert.Types.AudioSelectorGroup
import Amazonka.MediaConvert.Types.CaptionSelector
import Amazonka.MediaConvert.Types.ImageInserter
import Amazonka.MediaConvert.Types.InputClipping
import Amazonka.MediaConvert.Types.InputDeblockFilter
import Amazonka.MediaConvert.Types.InputDecryptionSettings
import Amazonka.MediaConvert.Types.InputDenoiseFilter
import Amazonka.MediaConvert.Types.InputFilterEnable
import Amazonka.MediaConvert.Types.InputPsiControl
import Amazonka.MediaConvert.Types.InputScanType
import Amazonka.MediaConvert.Types.InputTimecodeSource
import Amazonka.MediaConvert.Types.InputVideoGenerator
import Amazonka.MediaConvert.Types.Rectangle
import Amazonka.MediaConvert.Types.VideoSelector
import qualified Amazonka.Prelude as Prelude

-- | Use inputs to define the source files used in your transcoding job. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/specify-input-settings.html.
-- You can use multiple video inputs to do input stitching. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/assembling-multiple-inputs-and-input-clips.html
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
    -- output. Default is disabled. Only manually controllable for MPEG2 and
    -- uncompressed video inputs.
    deblockFilter :: Prelude.Maybe InputDeblockFilter,
    -- | Set PSI control (InputPsiControl) for transport stream inputs to specify
    -- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
    -- audio and video. * Use PSI - Scan only PSI data.
    psiControl :: Prelude.Maybe InputPsiControl,
    -- | Use captions selectors to specify the captions data from your input that
    -- you use in your outputs. You can use up to 20 captions selectors per
    -- input.
    captionSelectors :: Prelude.Maybe (Prelude.HashMap Prelude.Text CaptionSelector),
    -- | Use audio selector groups to combine multiple sidecar audio inputs so
    -- that you can assign them to a single output audio tab
    -- (AudioDescription). Note that, if you\'re working with embedded audio,
    -- it\'s simpler to assign multiple input tracks into a single audio
    -- selector rather than use an audio selector group.
    audioSelectorGroups :: Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelectorGroup),
    -- | Use Filter strength (FilterStrength) to adjust the magnitude the input
    -- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
    -- 0.
    filterStrength :: Prelude.Maybe Prelude.Int,
    -- | Specify the timecode that you want the service to use for this input\'s
    -- initial frame. To use this setting, you must set the Timecode source
    -- setting, located under the input settings (InputTimecodeSource), to
    -- Specified start (SPECIFIEDSTART). For more information about timecodes,
    -- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
    timecodeStart :: Prelude.Maybe Prelude.Text,
    -- | Use this setting only when your video source has Dolby Vision studio
    -- mastering metadata that is carried in a separate XML file. Specify the
    -- Amazon S3 location for the metadata XML file. MediaConvert uses this
    -- file to provide global and frame-level metadata for Dolby Vision
    -- preprocessing. When you specify a file here and your input also has
    -- interleaved global and frame level metadata, MediaConvert ignores the
    -- interleaved metadata and uses only the the metadata from this external
    -- XML file. Note that your IAM service role must grant MediaConvert read
    -- permissions to this file. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html.
    dolbyVisionMetadataXml :: Prelude.Maybe Prelude.Text,
    -- | (InputClippings) contains sets of start and end times that together
    -- specify a portion of the input to be used in the outputs. If you provide
    -- only a start time, the clip will be the entire input from that point to
    -- the end. If you provide only an end time, it will be the entire input up
    -- to that point. When you specify more than one input clip, the
    -- transcoding service creates the job outputs by stringing the clips
    -- together in the order you specify them.
    inputClippings :: Prelude.Maybe [InputClipping],
    -- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
    -- from the input that you will use in your outputs. You can use multiple
    -- Audio selectors per input.
    audioSelectors :: Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelector),
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
    -- | When you include Video generator, MediaConvert creates a video input
    -- with black frames. Use this setting if you do not have a video input or
    -- if you want to add black video frames before, or after, other inputs.
    -- You can specify Video generator, or you can specify an Input file, but
    -- you cannot specify both. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/video-generator.html
    videoGenerator :: Prelude.Maybe InputVideoGenerator,
    -- | Use Program (programNumber) to select a specific program from within a
    -- multi-program transport stream. Note that Quad 4K is not currently
    -- supported. Default is the first program within the transport stream. If
    -- the program you specify doesn\'t exist, the transcoding service will use
    -- this default.
    programNumber :: Prelude.Maybe Prelude.Natural,
    -- | Use Cropping selection (crop) to specify the video area that the service
    -- will include in the output video frame. If you specify a value here, it
    -- will override any value that you specify in the output setting Cropping
    -- selection (crop).
    crop :: Prelude.Maybe Rectangle,
    -- | Provide a list of any necessary supplemental IMPs. You need supplemental
    -- IMPs if the CPL that you\'re using for your input is in an incomplete
    -- IMP. Specify either the supplemental IMP directories with a trailing
    -- slash or the ASSETMAP.xml files. For example [\"s3:\/\/bucket\/ov\/\",
    -- \"s3:\/\/bucket\/vf2\/ASSETMAP.xml\"]. You don\'t need to specify the
    -- IMP that contains your input CPL, because the service automatically
    -- detects it.
    supplementalImps :: Prelude.Maybe [Prelude.Text],
    -- | Specify how the transcoding service applies the denoise and deblock
    -- filters. You must also enable the filters separately, with Denoise
    -- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
    -- transcoding service determines whether to apply filtering, depending on
    -- input type and quality. * Disable - The input is not filtered. This is
    -- true even if you use the API to enable them in (InputDeblockFilter) and
    -- (InputDeblockFilter). * Force - The input is filtered regardless of
    -- input type.
    filterEnable :: Prelude.Maybe InputFilterEnable,
    -- | Enable the image inserter feature to include a graphic overlay on your
    -- video. Enable or disable this feature for each input individually. This
    -- setting is disabled by default.
    imageInserter :: Prelude.Maybe ImageInserter,
    -- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.
    -- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
    -- uncompressed video inputs.
    denoiseFilter :: Prelude.Maybe InputDenoiseFilter,
    -- | Use Selection placement (position) to define the video area in your
    -- output frame. The area outside of the rectangle that you specify here is
    -- black. If you specify a value here, it will override any value that you
    -- specify in the output setting Selection placement (position). If you
    -- specify a value here, this will override any AFD values in your input,
    -- even if you set Respond to AFD (RespondToAfd) to Respond (RESPOND). If
    -- you specify a value here, this will ignore anything that you specify for
    -- the setting Scaling Behavior (scalingBehavior).
    position :: Prelude.Maybe Rectangle,
    -- | When you have a progressive segmented frame (PsF) input, use this
    -- setting to flag the input as PsF. MediaConvert doesn\'t automatically
    -- detect PsF. Therefore, flagging your input as PsF results in better
    -- preservation of video quality when you do deinterlacing and frame rate
    -- conversion. If you don\'t specify, the default value is Auto (AUTO).
    -- Auto is the correct setting for all inputs that are not PsF. Don\'t set
    -- this value to PsF when your input is interlaced. Doing so creates
    -- horizontal interlacing artifacts.
    inputScanType :: Prelude.Maybe InputScanType,
    -- | Settings for decrypting any input files that you encrypt before you
    -- upload them to Amazon S3. MediaConvert can decrypt files only when you
    -- use AWS Key Management Service (KMS) to encrypt the data key that you
    -- use to encrypt your content.
    decryptionSettings :: Prelude.Maybe InputDecryptionSettings,
    -- | Specify the source file for your transcoding job. You can use multiple
    -- inputs in a single job. The service concatenates these inputs, in the
    -- order that you specify them in the job, to create the outputs. If your
    -- input format is IMF, specify your input by providing the path to your
    -- CPL. For example, \"s3:\/\/bucket\/vf\/cpl.xml\". If the CPL is in an
    -- incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps)
    -- to specify any supplemental IMPs that contain assets referenced by the
    -- CPL.
    fileInput :: Prelude.Maybe Prelude.Text,
    -- | Input video selectors contain the video settings for the input. Each of
    -- your inputs can have up to one video selector.
    videoSelector :: Prelude.Maybe VideoSelector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deblockFilter', 'input_deblockFilter' - Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
--
-- 'psiControl', 'input_psiControl' - Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
--
-- 'captionSelectors', 'input_captionSelectors' - Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
--
-- 'audioSelectorGroups', 'input_audioSelectorGroups' - Use audio selector groups to combine multiple sidecar audio inputs so
-- that you can assign them to a single output audio tab
-- (AudioDescription). Note that, if you\'re working with embedded audio,
-- it\'s simpler to assign multiple input tracks into a single audio
-- selector rather than use an audio selector group.
--
-- 'filterStrength', 'input_filterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input
-- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
-- 0.
--
-- 'timecodeStart', 'input_timecodeStart' - Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
--
-- 'dolbyVisionMetadataXml', 'input_dolbyVisionMetadataXml' - Use this setting only when your video source has Dolby Vision studio
-- mastering metadata that is carried in a separate XML file. Specify the
-- Amazon S3 location for the metadata XML file. MediaConvert uses this
-- file to provide global and frame-level metadata for Dolby Vision
-- preprocessing. When you specify a file here and your input also has
-- interleaved global and frame level metadata, MediaConvert ignores the
-- interleaved metadata and uses only the the metadata from this external
-- XML file. Note that your IAM service role must grant MediaConvert read
-- permissions to this file. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html.
--
-- 'inputClippings', 'input_inputClippings' - (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
--
-- 'audioSelectors', 'input_audioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
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
--
-- 'videoGenerator', 'input_videoGenerator' - When you include Video generator, MediaConvert creates a video input
-- with black frames. Use this setting if you do not have a video input or
-- if you want to add black video frames before, or after, other inputs.
-- You can specify Video generator, or you can specify an Input file, but
-- you cannot specify both. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/video-generator.html
--
-- 'programNumber', 'input_programNumber' - Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
--
-- 'crop', 'input_crop' - Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
--
-- 'supplementalImps', 'input_supplementalImps' - Provide a list of any necessary supplemental IMPs. You need supplemental
-- IMPs if the CPL that you\'re using for your input is in an incomplete
-- IMP. Specify either the supplemental IMP directories with a trailing
-- slash or the ASSETMAP.xml files. For example [\"s3:\/\/bucket\/ov\/\",
-- \"s3:\/\/bucket\/vf2\/ASSETMAP.xml\"]. You don\'t need to specify the
-- IMP that contains your input CPL, because the service automatically
-- detects it.
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
-- 'imageInserter', 'input_imageInserter' - Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input individually. This
-- setting is disabled by default.
--
-- 'denoiseFilter', 'input_denoiseFilter' - Enable Denoise (InputDenoiseFilter) to filter noise from the input.
-- Default is disabled. Only applicable to MPEG2, H.264, H.265, and
-- uncompressed video inputs.
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
-- 'inputScanType', 'input_inputScanType' - When you have a progressive segmented frame (PsF) input, use this
-- setting to flag the input as PsF. MediaConvert doesn\'t automatically
-- detect PsF. Therefore, flagging your input as PsF results in better
-- preservation of video quality when you do deinterlacing and frame rate
-- conversion. If you don\'t specify, the default value is Auto (AUTO).
-- Auto is the correct setting for all inputs that are not PsF. Don\'t set
-- this value to PsF when your input is interlaced. Doing so creates
-- horizontal interlacing artifacts.
--
-- 'decryptionSettings', 'input_decryptionSettings' - Settings for decrypting any input files that you encrypt before you
-- upload them to Amazon S3. MediaConvert can decrypt files only when you
-- use AWS Key Management Service (KMS) to encrypt the data key that you
-- use to encrypt your content.
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
-- 'videoSelector', 'input_videoSelector' - Input video selectors contain the video settings for the input. Each of
-- your inputs can have up to one video selector.
newInput ::
  Input
newInput =
  Input'
    { deblockFilter = Prelude.Nothing,
      psiControl = Prelude.Nothing,
      captionSelectors = Prelude.Nothing,
      audioSelectorGroups = Prelude.Nothing,
      filterStrength = Prelude.Nothing,
      timecodeStart = Prelude.Nothing,
      dolbyVisionMetadataXml = Prelude.Nothing,
      inputClippings = Prelude.Nothing,
      audioSelectors = Prelude.Nothing,
      timecodeSource = Prelude.Nothing,
      videoGenerator = Prelude.Nothing,
      programNumber = Prelude.Nothing,
      crop = Prelude.Nothing,
      supplementalImps = Prelude.Nothing,
      filterEnable = Prelude.Nothing,
      imageInserter = Prelude.Nothing,
      denoiseFilter = Prelude.Nothing,
      position = Prelude.Nothing,
      inputScanType = Prelude.Nothing,
      decryptionSettings = Prelude.Nothing,
      fileInput = Prelude.Nothing,
      videoSelector = Prelude.Nothing
    }

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the
-- output. Default is disabled. Only manually controllable for MPEG2 and
-- uncompressed video inputs.
input_deblockFilter :: Lens.Lens' Input (Prelude.Maybe InputDeblockFilter)
input_deblockFilter = Lens.lens (\Input' {deblockFilter} -> deblockFilter) (\s@Input' {} a -> s {deblockFilter = a} :: Input)

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
input_psiControl :: Lens.Lens' Input (Prelude.Maybe InputPsiControl)
input_psiControl = Lens.lens (\Input' {psiControl} -> psiControl) (\s@Input' {} a -> s {psiControl = a} :: Input)

-- | Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
input_captionSelectors :: Lens.Lens' Input (Prelude.Maybe (Prelude.HashMap Prelude.Text CaptionSelector))
input_captionSelectors = Lens.lens (\Input' {captionSelectors} -> captionSelectors) (\s@Input' {} a -> s {captionSelectors = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | Use audio selector groups to combine multiple sidecar audio inputs so
-- that you can assign them to a single output audio tab
-- (AudioDescription). Note that, if you\'re working with embedded audio,
-- it\'s simpler to assign multiple input tracks into a single audio
-- selector rather than use an audio selector group.
input_audioSelectorGroups :: Lens.Lens' Input (Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelectorGroup))
input_audioSelectorGroups = Lens.lens (\Input' {audioSelectorGroups} -> audioSelectorGroups) (\s@Input' {} a -> s {audioSelectorGroups = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input
-- filter settings (Deblock and Denoise). The range is -5 to 5. Default is
-- 0.
input_filterStrength :: Lens.Lens' Input (Prelude.Maybe Prelude.Int)
input_filterStrength = Lens.lens (\Input' {filterStrength} -> filterStrength) (\s@Input' {} a -> s {filterStrength = a} :: Input)

-- | Specify the timecode that you want the service to use for this input\'s
-- initial frame. To use this setting, you must set the Timecode source
-- setting, located under the input settings (InputTimecodeSource), to
-- Specified start (SPECIFIEDSTART). For more information about timecodes,
-- see https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
input_timecodeStart :: Lens.Lens' Input (Prelude.Maybe Prelude.Text)
input_timecodeStart = Lens.lens (\Input' {timecodeStart} -> timecodeStart) (\s@Input' {} a -> s {timecodeStart = a} :: Input)

-- | Use this setting only when your video source has Dolby Vision studio
-- mastering metadata that is carried in a separate XML file. Specify the
-- Amazon S3 location for the metadata XML file. MediaConvert uses this
-- file to provide global and frame-level metadata for Dolby Vision
-- preprocessing. When you specify a file here and your input also has
-- interleaved global and frame level metadata, MediaConvert ignores the
-- interleaved metadata and uses only the the metadata from this external
-- XML file. Note that your IAM service role must grant MediaConvert read
-- permissions to this file. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html.
input_dolbyVisionMetadataXml :: Lens.Lens' Input (Prelude.Maybe Prelude.Text)
input_dolbyVisionMetadataXml = Lens.lens (\Input' {dolbyVisionMetadataXml} -> dolbyVisionMetadataXml) (\s@Input' {} a -> s {dolbyVisionMetadataXml = a} :: Input)

-- | (InputClippings) contains sets of start and end times that together
-- specify a portion of the input to be used in the outputs. If you provide
-- only a start time, the clip will be the entire input from that point to
-- the end. If you provide only an end time, it will be the entire input up
-- to that point. When you specify more than one input clip, the
-- transcoding service creates the job outputs by stringing the clips
-- together in the order you specify them.
input_inputClippings :: Lens.Lens' Input (Prelude.Maybe [InputClipping])
input_inputClippings = Lens.lens (\Input' {inputClippings} -> inputClippings) (\s@Input' {} a -> s {inputClippings = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks
-- from the input that you will use in your outputs. You can use multiple
-- Audio selectors per input.
input_audioSelectors :: Lens.Lens' Input (Prelude.Maybe (Prelude.HashMap Prelude.Text AudioSelector))
input_audioSelectors = Lens.lens (\Input' {audioSelectors} -> audioSelectors) (\s@Input' {} a -> s {audioSelectors = a} :: Input) Prelude.. Lens.mapping Lens.coerced

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

-- | When you include Video generator, MediaConvert creates a video input
-- with black frames. Use this setting if you do not have a video input or
-- if you want to add black video frames before, or after, other inputs.
-- You can specify Video generator, or you can specify an Input file, but
-- you cannot specify both. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/video-generator.html
input_videoGenerator :: Lens.Lens' Input (Prelude.Maybe InputVideoGenerator)
input_videoGenerator = Lens.lens (\Input' {videoGenerator} -> videoGenerator) (\s@Input' {} a -> s {videoGenerator = a} :: Input)

-- | Use Program (programNumber) to select a specific program from within a
-- multi-program transport stream. Note that Quad 4K is not currently
-- supported. Default is the first program within the transport stream. If
-- the program you specify doesn\'t exist, the transcoding service will use
-- this default.
input_programNumber :: Lens.Lens' Input (Prelude.Maybe Prelude.Natural)
input_programNumber = Lens.lens (\Input' {programNumber} -> programNumber) (\s@Input' {} a -> s {programNumber = a} :: Input)

-- | Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame. If you specify a value here, it
-- will override any value that you specify in the output setting Cropping
-- selection (crop).
input_crop :: Lens.Lens' Input (Prelude.Maybe Rectangle)
input_crop = Lens.lens (\Input' {crop} -> crop) (\s@Input' {} a -> s {crop = a} :: Input)

-- | Provide a list of any necessary supplemental IMPs. You need supplemental
-- IMPs if the CPL that you\'re using for your input is in an incomplete
-- IMP. Specify either the supplemental IMP directories with a trailing
-- slash or the ASSETMAP.xml files. For example [\"s3:\/\/bucket\/ov\/\",
-- \"s3:\/\/bucket\/vf2\/ASSETMAP.xml\"]. You don\'t need to specify the
-- IMP that contains your input CPL, because the service automatically
-- detects it.
input_supplementalImps :: Lens.Lens' Input (Prelude.Maybe [Prelude.Text])
input_supplementalImps = Lens.lens (\Input' {supplementalImps} -> supplementalImps) (\s@Input' {} a -> s {supplementalImps = a} :: Input) Prelude.. Lens.mapping Lens.coerced

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

-- | Settings for decrypting any input files that you encrypt before you
-- upload them to Amazon S3. MediaConvert can decrypt files only when you
-- use AWS Key Management Service (KMS) to encrypt the data key that you
-- use to encrypt your content.
input_decryptionSettings :: Lens.Lens' Input (Prelude.Maybe InputDecryptionSettings)
input_decryptionSettings = Lens.lens (\Input' {decryptionSettings} -> decryptionSettings) (\s@Input' {} a -> s {decryptionSettings = a} :: Input)

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

-- | Input video selectors contain the video settings for the input. Each of
-- your inputs can have up to one video selector.
input_videoSelector :: Lens.Lens' Input (Prelude.Maybe VideoSelector)
input_videoSelector = Lens.lens (\Input' {videoSelector} -> videoSelector) (\s@Input' {} a -> s {videoSelector = a} :: Input)

instance Data.FromJSON Input where
  parseJSON =
    Data.withObject
      "Input"
      ( \x ->
          Input'
            Prelude.<$> (x Data..:? "deblockFilter")
            Prelude.<*> (x Data..:? "psiControl")
            Prelude.<*> ( x Data..:? "captionSelectors"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "audioSelectorGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "filterStrength")
            Prelude.<*> (x Data..:? "timecodeStart")
            Prelude.<*> (x Data..:? "dolbyVisionMetadataXml")
            Prelude.<*> (x Data..:? "inputClippings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "audioSelectors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timecodeSource")
            Prelude.<*> (x Data..:? "videoGenerator")
            Prelude.<*> (x Data..:? "programNumber")
            Prelude.<*> (x Data..:? "crop")
            Prelude.<*> ( x Data..:? "supplementalImps"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "filterEnable")
            Prelude.<*> (x Data..:? "imageInserter")
            Prelude.<*> (x Data..:? "denoiseFilter")
            Prelude.<*> (x Data..:? "position")
            Prelude.<*> (x Data..:? "inputScanType")
            Prelude.<*> (x Data..:? "decryptionSettings")
            Prelude.<*> (x Data..:? "fileInput")
            Prelude.<*> (x Data..:? "videoSelector")
      )

instance Prelude.Hashable Input where
  hashWithSalt _salt Input' {..} =
    _salt `Prelude.hashWithSalt` deblockFilter
      `Prelude.hashWithSalt` psiControl
      `Prelude.hashWithSalt` captionSelectors
      `Prelude.hashWithSalt` audioSelectorGroups
      `Prelude.hashWithSalt` filterStrength
      `Prelude.hashWithSalt` timecodeStart
      `Prelude.hashWithSalt` dolbyVisionMetadataXml
      `Prelude.hashWithSalt` inputClippings
      `Prelude.hashWithSalt` audioSelectors
      `Prelude.hashWithSalt` timecodeSource
      `Prelude.hashWithSalt` videoGenerator
      `Prelude.hashWithSalt` programNumber
      `Prelude.hashWithSalt` crop
      `Prelude.hashWithSalt` supplementalImps
      `Prelude.hashWithSalt` filterEnable
      `Prelude.hashWithSalt` imageInserter
      `Prelude.hashWithSalt` denoiseFilter
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` inputScanType
      `Prelude.hashWithSalt` decryptionSettings
      `Prelude.hashWithSalt` fileInput
      `Prelude.hashWithSalt` videoSelector

instance Prelude.NFData Input where
  rnf Input' {..} =
    Prelude.rnf deblockFilter
      `Prelude.seq` Prelude.rnf psiControl
      `Prelude.seq` Prelude.rnf captionSelectors
      `Prelude.seq` Prelude.rnf audioSelectorGroups
      `Prelude.seq` Prelude.rnf filterStrength
      `Prelude.seq` Prelude.rnf timecodeStart
      `Prelude.seq` Prelude.rnf dolbyVisionMetadataXml
      `Prelude.seq` Prelude.rnf inputClippings
      `Prelude.seq` Prelude.rnf audioSelectors
      `Prelude.seq` Prelude.rnf timecodeSource
      `Prelude.seq` Prelude.rnf videoGenerator
      `Prelude.seq` Prelude.rnf programNumber
      `Prelude.seq` Prelude.rnf crop
      `Prelude.seq` Prelude.rnf supplementalImps
      `Prelude.seq` Prelude.rnf filterEnable
      `Prelude.seq` Prelude.rnf imageInserter
      `Prelude.seq` Prelude.rnf denoiseFilter
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf inputScanType
      `Prelude.seq` Prelude.rnf decryptionSettings
      `Prelude.seq` Prelude.rnf fileInput
      `Prelude.seq` Prelude.rnf videoSelector

instance Data.ToJSON Input where
  toJSON Input' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deblockFilter" Data..=) Prelude.<$> deblockFilter,
            ("psiControl" Data..=) Prelude.<$> psiControl,
            ("captionSelectors" Data..=)
              Prelude.<$> captionSelectors,
            ("audioSelectorGroups" Data..=)
              Prelude.<$> audioSelectorGroups,
            ("filterStrength" Data..=)
              Prelude.<$> filterStrength,
            ("timecodeStart" Data..=) Prelude.<$> timecodeStart,
            ("dolbyVisionMetadataXml" Data..=)
              Prelude.<$> dolbyVisionMetadataXml,
            ("inputClippings" Data..=)
              Prelude.<$> inputClippings,
            ("audioSelectors" Data..=)
              Prelude.<$> audioSelectors,
            ("timecodeSource" Data..=)
              Prelude.<$> timecodeSource,
            ("videoGenerator" Data..=)
              Prelude.<$> videoGenerator,
            ("programNumber" Data..=) Prelude.<$> programNumber,
            ("crop" Data..=) Prelude.<$> crop,
            ("supplementalImps" Data..=)
              Prelude.<$> supplementalImps,
            ("filterEnable" Data..=) Prelude.<$> filterEnable,
            ("imageInserter" Data..=) Prelude.<$> imageInserter,
            ("denoiseFilter" Data..=) Prelude.<$> denoiseFilter,
            ("position" Data..=) Prelude.<$> position,
            ("inputScanType" Data..=) Prelude.<$> inputScanType,
            ("decryptionSettings" Data..=)
              Prelude.<$> decryptionSettings,
            ("fileInput" Data..=) Prelude.<$> fileInput,
            ("videoSelector" Data..=) Prelude.<$> videoSelector
          ]
      )
