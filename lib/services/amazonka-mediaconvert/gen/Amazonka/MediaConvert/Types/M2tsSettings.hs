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
-- Module      : Amazonka.MediaConvert.Types.M2tsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.M2tsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.DvbNitSettings
import Amazonka.MediaConvert.Types.DvbSdtSettings
import Amazonka.MediaConvert.Types.DvbTdtSettings
import Amazonka.MediaConvert.Types.M2tsAudioBufferModel
import Amazonka.MediaConvert.Types.M2tsAudioDuration
import Amazonka.MediaConvert.Types.M2tsBufferModel
import Amazonka.MediaConvert.Types.M2tsDataPtsControl
import Amazonka.MediaConvert.Types.M2tsEbpAudioInterval
import Amazonka.MediaConvert.Types.M2tsEbpPlacement
import Amazonka.MediaConvert.Types.M2tsEsRateInPes
import Amazonka.MediaConvert.Types.M2tsForceTsVideoEbpOrder
import Amazonka.MediaConvert.Types.M2tsNielsenId3
import Amazonka.MediaConvert.Types.M2tsPcrControl
import Amazonka.MediaConvert.Types.M2tsRateMode
import Amazonka.MediaConvert.Types.M2tsScte35Esam
import Amazonka.MediaConvert.Types.M2tsScte35Source
import Amazonka.MediaConvert.Types.M2tsSegmentationMarkers
import Amazonka.MediaConvert.Types.M2tsSegmentationStyle
import qualified Amazonka.Prelude as Prelude

-- | MPEG-2 TS container settings. These apply to outputs in a File output
-- group when the output\'s container (ContainerType) is MPEG-2 Transport
-- Stream (M2TS). In these assets, data is organized by the program map
-- table (PMT). Each transport stream program contains subsets of data,
-- including audio, video, and metadata. Each of these subsets of data has
-- a numerical label called a packet identifier (PID). Each transport
-- stream program corresponds to one MediaConvert output. The PMT lists the
-- types of data in a program along with their PID. Downstream systems and
-- players use the program map table to look up the PID for each type of
-- data it accesses and then uses the PIDs to locate specific data within
-- the asset.
--
-- /See:/ 'newM2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'
  { -- | Specify the packet identifier (PID) for the program map table (PMT)
    -- itself. Default is 480.
    pmtPid :: Prelude.Maybe Prelude.Natural,
    -- | Specify the packet identifier (PID) of the elementary video stream in
    -- the transport stream.
    videoPid :: Prelude.Maybe Prelude.Natural,
    -- | Controls what buffer model to use for accurate interleaving. If set to
    -- MULTIPLEX, use multiplex buffer model. If set to NONE, this can lead to
    -- lower latency, but low-memory devices may not be able to play back the
    -- stream without interruptions.
    bufferModel :: Prelude.Maybe M2tsBufferModel,
    -- | Use Program number (programNumber) to specify the program number used in
    -- the program map table (PMT) for this output. Default is 1. Program
    -- numbers and program map tables are parts of MPEG-2 transport stream
    -- containers, used for organizing data.
    programNumber :: Prelude.Maybe Prelude.Natural,
    -- | Specify the packet identifier (PID) of the SCTE-35 stream in the
    -- transport stream.
    scte35Pid :: Prelude.Maybe Prelude.Natural,
    -- | When set, enforces that Encoder Boundary Points do not come within the
    -- specified time interval of each other by looking ahead at input video.
    -- If another EBP is going to come in within the specified time interval,
    -- the current EBP is not emitted, and the segment is \"stretched\" to the
    -- next marker. The lookahead value does not add latency to the system. The
    -- Live Event must be configured elsewhere to create sufficient latency to
    -- make the lookahead accurate.
    minEbpInterval :: Prelude.Maybe Prelude.Natural,
    -- | Specify the ID for the transport stream itself in the program map table
    -- for this output. Transport stream IDs and program map tables are parts
    -- of MPEG-2 transport stream containers, used for organizing data.
    transportStreamId :: Prelude.Maybe Prelude.Natural,
    -- | Specify the maximum time, in milliseconds, between Program Clock
    -- References (PCRs) inserted into the transport stream.
    maxPcrInterval :: Prelude.Maybe Prelude.Natural,
    -- | The length, in seconds, of each fragment. Only used with EBP markers.
    fragmentTime :: Prelude.Maybe Prelude.Double,
    -- | Specify the packet identifier (PID) of the private metadata stream.
    -- Default is 503.
    privateMetadataPid :: Prelude.Maybe Prelude.Natural,
    -- | Include this in your job settings to put SCTE-35 markers in your HLS and
    -- transport stream outputs at the insertion points that you specify in an
    -- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
    scte35Esam :: Prelude.Maybe M2tsScte35Esam,
    -- | Specify this setting only when your output will be consumed by a
    -- downstream repackaging workflow that is sensitive to very small duration
    -- differences between video and audio. For this situation, choose Match
    -- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
    -- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
    -- choose Match video duration, MediaConvert pads the output audio streams
    -- with silence or trims them to ensure that the total duration of each
    -- audio stream is at least as long as the total duration of the video
    -- stream. After padding or trimming, the audio stream duration is no more
    -- than one frame longer than the video stream. MediaConvert applies audio
    -- padding or trimming only to the end of the last segment of the output.
    -- For unsegmented outputs, MediaConvert adds padding only to the end of
    -- the file. When you keep the default value, any minor discrepancies
    -- between audio and video duration will depend on your output audio codec.
    audioDuration :: Prelude.Maybe M2tsAudioDuration,
    -- | Specify the number of milliseconds between instances of the program map
    -- table (PMT) in the output transport stream.
    pmtInterval :: Prelude.Maybe Prelude.Natural,
    -- | Use these settings to insert a DVB Service Description Table (SDT) in
    -- the transport stream of this output. When you work directly in your JSON
    -- job specification, include this object only when your job has a
    -- transport stream output and the container settings contain the object
    -- M2tsSettings.
    dvbSdtSettings :: Prelude.Maybe DvbSdtSettings,
    -- | Value in bits per second of extra null packets to insert into the
    -- transport stream. This can be used if a downstream encryption system
    -- requires periodic null packets.
    nullPacketBitrate :: Prelude.Maybe Prelude.Double,
    -- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
    audioBufferModel :: Prelude.Maybe M2tsAudioBufferModel,
    -- | Specify the packet identifier (PID) for timed metadata in this output.
    -- Default is 502.
    timedMetadataPid :: Prelude.Maybe Prelude.Natural,
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Prelude.Maybe Prelude.Natural,
    -- | Specify the packet identifier (PID) for the program clock reference
    -- (PCR) in this output. If you do not specify a value, the service will
    -- use the value for Video PID (VideoPid).
    pcrPid :: Prelude.Maybe Prelude.Natural,
    -- | Inserts segmentation markers at each segmentation_time period.
    -- rai_segstart sets the Random Access Indicator bit in the adaptation
    -- field. rai_adapt sets the RAI bit and adds the current timecode in the
    -- private data bytes. psi_segstart inserts PAT and PMT tables at the start
    -- of segments. ebp adds Encoder Boundary Point information to the
    -- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
    -- ebp_legacy adds Encoder Boundary Point information to the adaptation
    -- field using a legacy proprietary format.
    segmentationMarkers :: Prelude.Maybe M2tsSegmentationMarkers,
    -- | Specify the packet identifiers (PIDs) for DVB subtitle data included in
    -- this output. Specify multiple PIDs as a JSON array. Default is the range
    -- 460-479.
    dvbSubPids :: Prelude.Maybe [Prelude.Natural],
    -- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
    -- if you want SCTE-35 markers that appear in your input to also appear in
    -- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
    -- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
    -- (NONE). Also provide the ESAM XML as a string in the setting Signal
    -- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
    -- the property scte35Esam).
    scte35Source :: Prelude.Maybe M2tsScte35Source,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    patInterval :: Prelude.Maybe Prelude.Natural,
    -- | Keep the default value (DEFAULT) unless you know that your audio EBP
    -- markers are incorrectly appearing before your video EBP markers. To
    -- correct this problem, set this value to Force (FORCE).
    forceTsVideoEbpOrder :: Prelude.Maybe M2tsForceTsVideoEbpOrder,
    -- | Controls whether to include the ES Rate field in the PES header.
    esRateInPes :: Prelude.Maybe M2tsEsRateInPes,
    -- | Specify the output bitrate of the transport stream in bits per second.
    -- Setting to 0 lets the muxer automatically determine the appropriate
    -- bitrate. Other common values are 3750000, 7500000, and 15000000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the packet identifiers (PIDs) for any elementary audio streams
    -- you include in this output. Specify multiple PIDs as a JSON array.
    -- Default is the range 482-492.
    audioPids :: Prelude.Maybe [Prelude.Natural],
    -- | Specify the packet identifier (PID) for DVB teletext data you include in
    -- this output. Default is 499.
    dvbTeletextPid :: Prelude.Maybe Prelude.Natural,
    -- | If INSERT, Nielsen inaudible tones for media tracking will be detected
    -- in the input audio and an equivalent ID3 tag will be inserted in the
    -- output.
    nielsenId3 :: Prelude.Maybe M2tsNielsenId3,
    -- | If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
    -- packets with Presentation Timestamp (PTS) values greater than or equal
    -- to the first video packet PTS (MediaConvert drops captions and data
    -- packets with lesser PTS values). Keep the default value (AUTO) to allow
    -- all PTS values.
    dataPTSControl :: Prelude.Maybe M2tsDataPtsControl,
    -- | Specify the length, in seconds, of each segment. Required unless markers
    -- is set to _none_.
    segmentationTime :: Prelude.Maybe Prelude.Double,
    -- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
    -- to partitions 3 and 4. The interval between these additional markers
    -- will be fixed, and will be slightly shorter than the video EBP marker
    -- interval. When set to VIDEO_INTERVAL, these additional markers will not
    -- be inserted. Only applicable when EBP segmentation markers are is
    -- selected (segmentationMarkers is EBP or EBP_LEGACY).
    ebpAudioInterval :: Prelude.Maybe M2tsEbpAudioInterval,
    -- | Use these settings to insert a DVB Network Information Table (NIT) in
    -- the transport stream of this output. When you work directly in your JSON
    -- job specification, include this object only when your job has a
    -- transport stream output and the container settings contain the object
    -- M2tsSettings.
    dvbNitSettings :: Prelude.Maybe DvbNitSettings,
    -- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is
    -- inserted for every Packetized Elementary Stream (PES) header. This is
    -- effective only when the PCR PID is the same as the video or audio
    -- elementary stream.
    pcrControl :: Prelude.Maybe M2tsPcrControl,
    -- | Selects which PIDs to place EBP markers on. They can either be placed
    -- only on the video PID, or on both the video PID and all audio PIDs. Only
    -- applicable when EBP segmentation markers are is selected
    -- (segmentationMarkers is EBP or EBP_LEGACY).
    ebpPlacement :: Prelude.Maybe M2tsEbpPlacement,
    -- | When set to CBR, inserts null packets into transport stream to fill
    -- specified bitrate. When set to VBR, the bitrate setting acts as the
    -- maximum bitrate, but the output will not be padded up to that bitrate.
    rateMode :: Prelude.Maybe M2tsRateMode,
    -- | The segmentation style parameter controls how segmentation markers are
    -- inserted into the transport stream. With avails, it is possible that
    -- segments may be truncated, which can influence where future segmentation
    -- markers are inserted. When a segmentation style of \"reset_cadence\" is
    -- selected and a segment is truncated due to an avail, we will reset the
    -- segmentation cadence. This means the subsequent segment will have a
    -- duration of of $segmentation_time seconds. When a segmentation style of
    -- \"maintain_cadence\" is selected and a segment is truncated due to an
    -- avail, we will not reset the segmentation cadence. This means the
    -- subsequent segment will likely be truncated as well. However, all
    -- segments after that will have a duration of $segmentation_time seconds.
    -- Note that EBP lookahead is a slight exception to this rule.
    segmentationStyle :: Prelude.Maybe M2tsSegmentationStyle,
    -- | Use these settings to insert a DVB Time and Date Table (TDT) in the
    -- transport stream of this output. When you work directly in your JSON job
    -- specification, include this object only when your job has a transport
    -- stream output and the container settings contain the object
    -- M2tsSettings.
    dvbTdtSettings :: Prelude.Maybe DvbTdtSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'M2tsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pmtPid', 'm2tsSettings_pmtPid' - Specify the packet identifier (PID) for the program map table (PMT)
-- itself. Default is 480.
--
-- 'videoPid', 'm2tsSettings_videoPid' - Specify the packet identifier (PID) of the elementary video stream in
-- the transport stream.
--
-- 'bufferModel', 'm2tsSettings_bufferModel' - Controls what buffer model to use for accurate interleaving. If set to
-- MULTIPLEX, use multiplex buffer model. If set to NONE, this can lead to
-- lower latency, but low-memory devices may not be able to play back the
-- stream without interruptions.
--
-- 'programNumber', 'm2tsSettings_programNumber' - Use Program number (programNumber) to specify the program number used in
-- the program map table (PMT) for this output. Default is 1. Program
-- numbers and program map tables are parts of MPEG-2 transport stream
-- containers, used for organizing data.
--
-- 'scte35Pid', 'm2tsSettings_scte35Pid' - Specify the packet identifier (PID) of the SCTE-35 stream in the
-- transport stream.
--
-- 'minEbpInterval', 'm2tsSettings_minEbpInterval' - When set, enforces that Encoder Boundary Points do not come within the
-- specified time interval of each other by looking ahead at input video.
-- If another EBP is going to come in within the specified time interval,
-- the current EBP is not emitted, and the segment is \"stretched\" to the
-- next marker. The lookahead value does not add latency to the system. The
-- Live Event must be configured elsewhere to create sufficient latency to
-- make the lookahead accurate.
--
-- 'transportStreamId', 'm2tsSettings_transportStreamId' - Specify the ID for the transport stream itself in the program map table
-- for this output. Transport stream IDs and program map tables are parts
-- of MPEG-2 transport stream containers, used for organizing data.
--
-- 'maxPcrInterval', 'm2tsSettings_maxPcrInterval' - Specify the maximum time, in milliseconds, between Program Clock
-- References (PCRs) inserted into the transport stream.
--
-- 'fragmentTime', 'm2tsSettings_fragmentTime' - The length, in seconds, of each fragment. Only used with EBP markers.
--
-- 'privateMetadataPid', 'm2tsSettings_privateMetadataPid' - Specify the packet identifier (PID) of the private metadata stream.
-- Default is 503.
--
-- 'scte35Esam', 'm2tsSettings_scte35Esam' - Include this in your job settings to put SCTE-35 markers in your HLS and
-- transport stream outputs at the insertion points that you specify in an
-- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- 'audioDuration', 'm2tsSettings_audioDuration' - Specify this setting only when your output will be consumed by a
-- downstream repackaging workflow that is sensitive to very small duration
-- differences between video and audio. For this situation, choose Match
-- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
-- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
-- choose Match video duration, MediaConvert pads the output audio streams
-- with silence or trims them to ensure that the total duration of each
-- audio stream is at least as long as the total duration of the video
-- stream. After padding or trimming, the audio stream duration is no more
-- than one frame longer than the video stream. MediaConvert applies audio
-- padding or trimming only to the end of the last segment of the output.
-- For unsegmented outputs, MediaConvert adds padding only to the end of
-- the file. When you keep the default value, any minor discrepancies
-- between audio and video duration will depend on your output audio codec.
--
-- 'pmtInterval', 'm2tsSettings_pmtInterval' - Specify the number of milliseconds between instances of the program map
-- table (PMT) in the output transport stream.
--
-- 'dvbSdtSettings', 'm2tsSettings_dvbSdtSettings' - Use these settings to insert a DVB Service Description Table (SDT) in
-- the transport stream of this output. When you work directly in your JSON
-- job specification, include this object only when your job has a
-- transport stream output and the container settings contain the object
-- M2tsSettings.
--
-- 'nullPacketBitrate', 'm2tsSettings_nullPacketBitrate' - Value in bits per second of extra null packets to insert into the
-- transport stream. This can be used if a downstream encryption system
-- requires periodic null packets.
--
-- 'audioBufferModel', 'm2tsSettings_audioBufferModel' - Selects between the DVB and ATSC buffer models for Dolby Digital audio.
--
-- 'timedMetadataPid', 'm2tsSettings_timedMetadataPid' - Specify the packet identifier (PID) for timed metadata in this output.
-- Default is 502.
--
-- 'audioFramesPerPes', 'm2tsSettings_audioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- 'pcrPid', 'm2tsSettings_pcrPid' - Specify the packet identifier (PID) for the program clock reference
-- (PCR) in this output. If you do not specify a value, the service will
-- use the value for Video PID (VideoPid).
--
-- 'segmentationMarkers', 'm2tsSettings_segmentationMarkers' - Inserts segmentation markers at each segmentation_time period.
-- rai_segstart sets the Random Access Indicator bit in the adaptation
-- field. rai_adapt sets the RAI bit and adds the current timecode in the
-- private data bytes. psi_segstart inserts PAT and PMT tables at the start
-- of segments. ebp adds Encoder Boundary Point information to the
-- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
-- ebp_legacy adds Encoder Boundary Point information to the adaptation
-- field using a legacy proprietary format.
--
-- 'dvbSubPids', 'm2tsSettings_dvbSubPids' - Specify the packet identifiers (PIDs) for DVB subtitle data included in
-- this output. Specify multiple PIDs as a JSON array. Default is the range
-- 460-479.
--
-- 'scte35Source', 'm2tsSettings_scte35Source' - For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE). Also provide the ESAM XML as a string in the setting Signal
-- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
-- the property scte35Esam).
--
-- 'patInterval', 'm2tsSettings_patInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'forceTsVideoEbpOrder', 'm2tsSettings_forceTsVideoEbpOrder' - Keep the default value (DEFAULT) unless you know that your audio EBP
-- markers are incorrectly appearing before your video EBP markers. To
-- correct this problem, set this value to Force (FORCE).
--
-- 'esRateInPes', 'm2tsSettings_esRateInPes' - Controls whether to include the ES Rate field in the PES header.
--
-- 'bitrate', 'm2tsSettings_bitrate' - Specify the output bitrate of the transport stream in bits per second.
-- Setting to 0 lets the muxer automatically determine the appropriate
-- bitrate. Other common values are 3750000, 7500000, and 15000000.
--
-- 'audioPids', 'm2tsSettings_audioPids' - Specify the packet identifiers (PIDs) for any elementary audio streams
-- you include in this output. Specify multiple PIDs as a JSON array.
-- Default is the range 482-492.
--
-- 'dvbTeletextPid', 'm2tsSettings_dvbTeletextPid' - Specify the packet identifier (PID) for DVB teletext data you include in
-- this output. Default is 499.
--
-- 'nielsenId3', 'm2tsSettings_nielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
--
-- 'dataPTSControl', 'm2tsSettings_dataPTSControl' - If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
-- packets with Presentation Timestamp (PTS) values greater than or equal
-- to the first video packet PTS (MediaConvert drops captions and data
-- packets with lesser PTS values). Keep the default value (AUTO) to allow
-- all PTS values.
--
-- 'segmentationTime', 'm2tsSettings_segmentationTime' - Specify the length, in seconds, of each segment. Required unless markers
-- is set to _none_.
--
-- 'ebpAudioInterval', 'm2tsSettings_ebpAudioInterval' - When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. When set to VIDEO_INTERVAL, these additional markers will not
-- be inserted. Only applicable when EBP segmentation markers are is
-- selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- 'dvbNitSettings', 'm2tsSettings_dvbNitSettings' - Use these settings to insert a DVB Network Information Table (NIT) in
-- the transport stream of this output. When you work directly in your JSON
-- job specification, include this object only when your job has a
-- transport stream output and the container settings contain the object
-- M2tsSettings.
--
-- 'pcrControl', 'm2tsSettings_pcrControl' - When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This is
-- effective only when the PCR PID is the same as the video or audio
-- elementary stream.
--
-- 'ebpPlacement', 'm2tsSettings_ebpPlacement' - Selects which PIDs to place EBP markers on. They can either be placed
-- only on the video PID, or on both the video PID and all audio PIDs. Only
-- applicable when EBP segmentation markers are is selected
-- (segmentationMarkers is EBP or EBP_LEGACY).
--
-- 'rateMode', 'm2tsSettings_rateMode' - When set to CBR, inserts null packets into transport stream to fill
-- specified bitrate. When set to VBR, the bitrate setting acts as the
-- maximum bitrate, but the output will not be padded up to that bitrate.
--
-- 'segmentationStyle', 'm2tsSettings_segmentationStyle' - The segmentation style parameter controls how segmentation markers are
-- inserted into the transport stream. With avails, it is possible that
-- segments may be truncated, which can influence where future segmentation
-- markers are inserted. When a segmentation style of \"reset_cadence\" is
-- selected and a segment is truncated due to an avail, we will reset the
-- segmentation cadence. This means the subsequent segment will have a
-- duration of of $segmentation_time seconds. When a segmentation style of
-- \"maintain_cadence\" is selected and a segment is truncated due to an
-- avail, we will not reset the segmentation cadence. This means the
-- subsequent segment will likely be truncated as well. However, all
-- segments after that will have a duration of $segmentation_time seconds.
-- Note that EBP lookahead is a slight exception to this rule.
--
-- 'dvbTdtSettings', 'm2tsSettings_dvbTdtSettings' - Use these settings to insert a DVB Time and Date Table (TDT) in the
-- transport stream of this output. When you work directly in your JSON job
-- specification, include this object only when your job has a transport
-- stream output and the container settings contain the object
-- M2tsSettings.
newM2tsSettings ::
  M2tsSettings
newM2tsSettings =
  M2tsSettings'
    { pmtPid = Prelude.Nothing,
      videoPid = Prelude.Nothing,
      bufferModel = Prelude.Nothing,
      programNumber = Prelude.Nothing,
      scte35Pid = Prelude.Nothing,
      minEbpInterval = Prelude.Nothing,
      transportStreamId = Prelude.Nothing,
      maxPcrInterval = Prelude.Nothing,
      fragmentTime = Prelude.Nothing,
      privateMetadataPid = Prelude.Nothing,
      scte35Esam = Prelude.Nothing,
      audioDuration = Prelude.Nothing,
      pmtInterval = Prelude.Nothing,
      dvbSdtSettings = Prelude.Nothing,
      nullPacketBitrate = Prelude.Nothing,
      audioBufferModel = Prelude.Nothing,
      timedMetadataPid = Prelude.Nothing,
      audioFramesPerPes = Prelude.Nothing,
      pcrPid = Prelude.Nothing,
      segmentationMarkers = Prelude.Nothing,
      dvbSubPids = Prelude.Nothing,
      scte35Source = Prelude.Nothing,
      patInterval = Prelude.Nothing,
      forceTsVideoEbpOrder = Prelude.Nothing,
      esRateInPes = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      audioPids = Prelude.Nothing,
      dvbTeletextPid = Prelude.Nothing,
      nielsenId3 = Prelude.Nothing,
      dataPTSControl = Prelude.Nothing,
      segmentationTime = Prelude.Nothing,
      ebpAudioInterval = Prelude.Nothing,
      dvbNitSettings = Prelude.Nothing,
      pcrControl = Prelude.Nothing,
      ebpPlacement = Prelude.Nothing,
      rateMode = Prelude.Nothing,
      segmentationStyle = Prelude.Nothing,
      dvbTdtSettings = Prelude.Nothing
    }

-- | Specify the packet identifier (PID) for the program map table (PMT)
-- itself. Default is 480.
m2tsSettings_pmtPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_pmtPid = Lens.lens (\M2tsSettings' {pmtPid} -> pmtPid) (\s@M2tsSettings' {} a -> s {pmtPid = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) of the elementary video stream in
-- the transport stream.
m2tsSettings_videoPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_videoPid = Lens.lens (\M2tsSettings' {videoPid} -> videoPid) (\s@M2tsSettings' {} a -> s {videoPid = a} :: M2tsSettings)

-- | Controls what buffer model to use for accurate interleaving. If set to
-- MULTIPLEX, use multiplex buffer model. If set to NONE, this can lead to
-- lower latency, but low-memory devices may not be able to play back the
-- stream without interruptions.
m2tsSettings_bufferModel :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsBufferModel)
m2tsSettings_bufferModel = Lens.lens (\M2tsSettings' {bufferModel} -> bufferModel) (\s@M2tsSettings' {} a -> s {bufferModel = a} :: M2tsSettings)

-- | Use Program number (programNumber) to specify the program number used in
-- the program map table (PMT) for this output. Default is 1. Program
-- numbers and program map tables are parts of MPEG-2 transport stream
-- containers, used for organizing data.
m2tsSettings_programNumber :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_programNumber = Lens.lens (\M2tsSettings' {programNumber} -> programNumber) (\s@M2tsSettings' {} a -> s {programNumber = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) of the SCTE-35 stream in the
-- transport stream.
m2tsSettings_scte35Pid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_scte35Pid = Lens.lens (\M2tsSettings' {scte35Pid} -> scte35Pid) (\s@M2tsSettings' {} a -> s {scte35Pid = a} :: M2tsSettings)

-- | When set, enforces that Encoder Boundary Points do not come within the
-- specified time interval of each other by looking ahead at input video.
-- If another EBP is going to come in within the specified time interval,
-- the current EBP is not emitted, and the segment is \"stretched\" to the
-- next marker. The lookahead value does not add latency to the system. The
-- Live Event must be configured elsewhere to create sufficient latency to
-- make the lookahead accurate.
m2tsSettings_minEbpInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_minEbpInterval = Lens.lens (\M2tsSettings' {minEbpInterval} -> minEbpInterval) (\s@M2tsSettings' {} a -> s {minEbpInterval = a} :: M2tsSettings)

-- | Specify the ID for the transport stream itself in the program map table
-- for this output. Transport stream IDs and program map tables are parts
-- of MPEG-2 transport stream containers, used for organizing data.
m2tsSettings_transportStreamId :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_transportStreamId = Lens.lens (\M2tsSettings' {transportStreamId} -> transportStreamId) (\s@M2tsSettings' {} a -> s {transportStreamId = a} :: M2tsSettings)

-- | Specify the maximum time, in milliseconds, between Program Clock
-- References (PCRs) inserted into the transport stream.
m2tsSettings_maxPcrInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_maxPcrInterval = Lens.lens (\M2tsSettings' {maxPcrInterval} -> maxPcrInterval) (\s@M2tsSettings' {} a -> s {maxPcrInterval = a} :: M2tsSettings)

-- | The length, in seconds, of each fragment. Only used with EBP markers.
m2tsSettings_fragmentTime :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_fragmentTime = Lens.lens (\M2tsSettings' {fragmentTime} -> fragmentTime) (\s@M2tsSettings' {} a -> s {fragmentTime = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) of the private metadata stream.
-- Default is 503.
m2tsSettings_privateMetadataPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_privateMetadataPid = Lens.lens (\M2tsSettings' {privateMetadataPid} -> privateMetadataPid) (\s@M2tsSettings' {} a -> s {privateMetadataPid = a} :: M2tsSettings)

-- | Include this in your job settings to put SCTE-35 markers in your HLS and
-- transport stream outputs at the insertion points that you specify in an
-- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
m2tsSettings_scte35Esam :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsScte35Esam)
m2tsSettings_scte35Esam = Lens.lens (\M2tsSettings' {scte35Esam} -> scte35Esam) (\s@M2tsSettings' {} a -> s {scte35Esam = a} :: M2tsSettings)

-- | Specify this setting only when your output will be consumed by a
-- downstream repackaging workflow that is sensitive to very small duration
-- differences between video and audio. For this situation, choose Match
-- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
-- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
-- choose Match video duration, MediaConvert pads the output audio streams
-- with silence or trims them to ensure that the total duration of each
-- audio stream is at least as long as the total duration of the video
-- stream. After padding or trimming, the audio stream duration is no more
-- than one frame longer than the video stream. MediaConvert applies audio
-- padding or trimming only to the end of the last segment of the output.
-- For unsegmented outputs, MediaConvert adds padding only to the end of
-- the file. When you keep the default value, any minor discrepancies
-- between audio and video duration will depend on your output audio codec.
m2tsSettings_audioDuration :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsAudioDuration)
m2tsSettings_audioDuration = Lens.lens (\M2tsSettings' {audioDuration} -> audioDuration) (\s@M2tsSettings' {} a -> s {audioDuration = a} :: M2tsSettings)

-- | Specify the number of milliseconds between instances of the program map
-- table (PMT) in the output transport stream.
m2tsSettings_pmtInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_pmtInterval = Lens.lens (\M2tsSettings' {pmtInterval} -> pmtInterval) (\s@M2tsSettings' {} a -> s {pmtInterval = a} :: M2tsSettings)

-- | Use these settings to insert a DVB Service Description Table (SDT) in
-- the transport stream of this output. When you work directly in your JSON
-- job specification, include this object only when your job has a
-- transport stream output and the container settings contain the object
-- M2tsSettings.
m2tsSettings_dvbSdtSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbSdtSettings)
m2tsSettings_dvbSdtSettings = Lens.lens (\M2tsSettings' {dvbSdtSettings} -> dvbSdtSettings) (\s@M2tsSettings' {} a -> s {dvbSdtSettings = a} :: M2tsSettings)

-- | Value in bits per second of extra null packets to insert into the
-- transport stream. This can be used if a downstream encryption system
-- requires periodic null packets.
m2tsSettings_nullPacketBitrate :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_nullPacketBitrate = Lens.lens (\M2tsSettings' {nullPacketBitrate} -> nullPacketBitrate) (\s@M2tsSettings' {} a -> s {nullPacketBitrate = a} :: M2tsSettings)

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
m2tsSettings_audioBufferModel :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsAudioBufferModel)
m2tsSettings_audioBufferModel = Lens.lens (\M2tsSettings' {audioBufferModel} -> audioBufferModel) (\s@M2tsSettings' {} a -> s {audioBufferModel = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) for timed metadata in this output.
-- Default is 502.
m2tsSettings_timedMetadataPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_timedMetadataPid = Lens.lens (\M2tsSettings' {timedMetadataPid} -> timedMetadataPid) (\s@M2tsSettings' {} a -> s {timedMetadataPid = a} :: M2tsSettings)

-- | The number of audio frames to insert for each PES packet.
m2tsSettings_audioFramesPerPes :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_audioFramesPerPes = Lens.lens (\M2tsSettings' {audioFramesPerPes} -> audioFramesPerPes) (\s@M2tsSettings' {} a -> s {audioFramesPerPes = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) for the program clock reference
-- (PCR) in this output. If you do not specify a value, the service will
-- use the value for Video PID (VideoPid).
m2tsSettings_pcrPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_pcrPid = Lens.lens (\M2tsSettings' {pcrPid} -> pcrPid) (\s@M2tsSettings' {} a -> s {pcrPid = a} :: M2tsSettings)

-- | Inserts segmentation markers at each segmentation_time period.
-- rai_segstart sets the Random Access Indicator bit in the adaptation
-- field. rai_adapt sets the RAI bit and adds the current timecode in the
-- private data bytes. psi_segstart inserts PAT and PMT tables at the start
-- of segments. ebp adds Encoder Boundary Point information to the
-- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
-- ebp_legacy adds Encoder Boundary Point information to the adaptation
-- field using a legacy proprietary format.
m2tsSettings_segmentationMarkers :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsSegmentationMarkers)
m2tsSettings_segmentationMarkers = Lens.lens (\M2tsSettings' {segmentationMarkers} -> segmentationMarkers) (\s@M2tsSettings' {} a -> s {segmentationMarkers = a} :: M2tsSettings)

-- | Specify the packet identifiers (PIDs) for DVB subtitle data included in
-- this output. Specify multiple PIDs as a JSON array. Default is the range
-- 460-479.
m2tsSettings_dvbSubPids :: Lens.Lens' M2tsSettings (Prelude.Maybe [Prelude.Natural])
m2tsSettings_dvbSubPids = Lens.lens (\M2tsSettings' {dvbSubPids} -> dvbSubPids) (\s@M2tsSettings' {} a -> s {dvbSubPids = a} :: M2tsSettings) Prelude.. Lens.mapping Lens.coerced

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE). Also provide the ESAM XML as a string in the setting Signal
-- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
-- the property scte35Esam).
m2tsSettings_scte35Source :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsScte35Source)
m2tsSettings_scte35Source = Lens.lens (\M2tsSettings' {scte35Source} -> scte35Source) (\s@M2tsSettings' {} a -> s {scte35Source = a} :: M2tsSettings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
m2tsSettings_patInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_patInterval = Lens.lens (\M2tsSettings' {patInterval} -> patInterval) (\s@M2tsSettings' {} a -> s {patInterval = a} :: M2tsSettings)

-- | Keep the default value (DEFAULT) unless you know that your audio EBP
-- markers are incorrectly appearing before your video EBP markers. To
-- correct this problem, set this value to Force (FORCE).
m2tsSettings_forceTsVideoEbpOrder :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsForceTsVideoEbpOrder)
m2tsSettings_forceTsVideoEbpOrder = Lens.lens (\M2tsSettings' {forceTsVideoEbpOrder} -> forceTsVideoEbpOrder) (\s@M2tsSettings' {} a -> s {forceTsVideoEbpOrder = a} :: M2tsSettings)

-- | Controls whether to include the ES Rate field in the PES header.
m2tsSettings_esRateInPes :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsEsRateInPes)
m2tsSettings_esRateInPes = Lens.lens (\M2tsSettings' {esRateInPes} -> esRateInPes) (\s@M2tsSettings' {} a -> s {esRateInPes = a} :: M2tsSettings)

-- | Specify the output bitrate of the transport stream in bits per second.
-- Setting to 0 lets the muxer automatically determine the appropriate
-- bitrate. Other common values are 3750000, 7500000, and 15000000.
m2tsSettings_bitrate :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_bitrate = Lens.lens (\M2tsSettings' {bitrate} -> bitrate) (\s@M2tsSettings' {} a -> s {bitrate = a} :: M2tsSettings)

-- | Specify the packet identifiers (PIDs) for any elementary audio streams
-- you include in this output. Specify multiple PIDs as a JSON array.
-- Default is the range 482-492.
m2tsSettings_audioPids :: Lens.Lens' M2tsSettings (Prelude.Maybe [Prelude.Natural])
m2tsSettings_audioPids = Lens.lens (\M2tsSettings' {audioPids} -> audioPids) (\s@M2tsSettings' {} a -> s {audioPids = a} :: M2tsSettings) Prelude.. Lens.mapping Lens.coerced

-- | Specify the packet identifier (PID) for DVB teletext data you include in
-- this output. Default is 499.
m2tsSettings_dvbTeletextPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_dvbTeletextPid = Lens.lens (\M2tsSettings' {dvbTeletextPid} -> dvbTeletextPid) (\s@M2tsSettings' {} a -> s {dvbTeletextPid = a} :: M2tsSettings)

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
m2tsSettings_nielsenId3 :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsNielsenId3)
m2tsSettings_nielsenId3 = Lens.lens (\M2tsSettings' {nielsenId3} -> nielsenId3) (\s@M2tsSettings' {} a -> s {nielsenId3 = a} :: M2tsSettings)

-- | If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
-- packets with Presentation Timestamp (PTS) values greater than or equal
-- to the first video packet PTS (MediaConvert drops captions and data
-- packets with lesser PTS values). Keep the default value (AUTO) to allow
-- all PTS values.
m2tsSettings_dataPTSControl :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsDataPtsControl)
m2tsSettings_dataPTSControl = Lens.lens (\M2tsSettings' {dataPTSControl} -> dataPTSControl) (\s@M2tsSettings' {} a -> s {dataPTSControl = a} :: M2tsSettings)

-- | Specify the length, in seconds, of each segment. Required unless markers
-- is set to _none_.
m2tsSettings_segmentationTime :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_segmentationTime = Lens.lens (\M2tsSettings' {segmentationTime} -> segmentationTime) (\s@M2tsSettings' {} a -> s {segmentationTime = a} :: M2tsSettings)

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. When set to VIDEO_INTERVAL, these additional markers will not
-- be inserted. Only applicable when EBP segmentation markers are is
-- selected (segmentationMarkers is EBP or EBP_LEGACY).
m2tsSettings_ebpAudioInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsEbpAudioInterval)
m2tsSettings_ebpAudioInterval = Lens.lens (\M2tsSettings' {ebpAudioInterval} -> ebpAudioInterval) (\s@M2tsSettings' {} a -> s {ebpAudioInterval = a} :: M2tsSettings)

-- | Use these settings to insert a DVB Network Information Table (NIT) in
-- the transport stream of this output. When you work directly in your JSON
-- job specification, include this object only when your job has a
-- transport stream output and the container settings contain the object
-- M2tsSettings.
m2tsSettings_dvbNitSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbNitSettings)
m2tsSettings_dvbNitSettings = Lens.lens (\M2tsSettings' {dvbNitSettings} -> dvbNitSettings) (\s@M2tsSettings' {} a -> s {dvbNitSettings = a} :: M2tsSettings)

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This is
-- effective only when the PCR PID is the same as the video or audio
-- elementary stream.
m2tsSettings_pcrControl :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsPcrControl)
m2tsSettings_pcrControl = Lens.lens (\M2tsSettings' {pcrControl} -> pcrControl) (\s@M2tsSettings' {} a -> s {pcrControl = a} :: M2tsSettings)

-- | Selects which PIDs to place EBP markers on. They can either be placed
-- only on the video PID, or on both the video PID and all audio PIDs. Only
-- applicable when EBP segmentation markers are is selected
-- (segmentationMarkers is EBP or EBP_LEGACY).
m2tsSettings_ebpPlacement :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsEbpPlacement)
m2tsSettings_ebpPlacement = Lens.lens (\M2tsSettings' {ebpPlacement} -> ebpPlacement) (\s@M2tsSettings' {} a -> s {ebpPlacement = a} :: M2tsSettings)

-- | When set to CBR, inserts null packets into transport stream to fill
-- specified bitrate. When set to VBR, the bitrate setting acts as the
-- maximum bitrate, but the output will not be padded up to that bitrate.
m2tsSettings_rateMode :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsRateMode)
m2tsSettings_rateMode = Lens.lens (\M2tsSettings' {rateMode} -> rateMode) (\s@M2tsSettings' {} a -> s {rateMode = a} :: M2tsSettings)

-- | The segmentation style parameter controls how segmentation markers are
-- inserted into the transport stream. With avails, it is possible that
-- segments may be truncated, which can influence where future segmentation
-- markers are inserted. When a segmentation style of \"reset_cadence\" is
-- selected and a segment is truncated due to an avail, we will reset the
-- segmentation cadence. This means the subsequent segment will have a
-- duration of of $segmentation_time seconds. When a segmentation style of
-- \"maintain_cadence\" is selected and a segment is truncated due to an
-- avail, we will not reset the segmentation cadence. This means the
-- subsequent segment will likely be truncated as well. However, all
-- segments after that will have a duration of $segmentation_time seconds.
-- Note that EBP lookahead is a slight exception to this rule.
m2tsSettings_segmentationStyle :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsSegmentationStyle)
m2tsSettings_segmentationStyle = Lens.lens (\M2tsSettings' {segmentationStyle} -> segmentationStyle) (\s@M2tsSettings' {} a -> s {segmentationStyle = a} :: M2tsSettings)

-- | Use these settings to insert a DVB Time and Date Table (TDT) in the
-- transport stream of this output. When you work directly in your JSON job
-- specification, include this object only when your job has a transport
-- stream output and the container settings contain the object
-- M2tsSettings.
m2tsSettings_dvbTdtSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbTdtSettings)
m2tsSettings_dvbTdtSettings = Lens.lens (\M2tsSettings' {dvbTdtSettings} -> dvbTdtSettings) (\s@M2tsSettings' {} a -> s {dvbTdtSettings = a} :: M2tsSettings)

instance Core.FromJSON M2tsSettings where
  parseJSON =
    Core.withObject
      "M2tsSettings"
      ( \x ->
          M2tsSettings'
            Prelude.<$> (x Core..:? "pmtPid")
            Prelude.<*> (x Core..:? "videoPid")
            Prelude.<*> (x Core..:? "bufferModel")
            Prelude.<*> (x Core..:? "programNumber")
            Prelude.<*> (x Core..:? "scte35Pid")
            Prelude.<*> (x Core..:? "minEbpInterval")
            Prelude.<*> (x Core..:? "transportStreamId")
            Prelude.<*> (x Core..:? "maxPcrInterval")
            Prelude.<*> (x Core..:? "fragmentTime")
            Prelude.<*> (x Core..:? "privateMetadataPid")
            Prelude.<*> (x Core..:? "scte35Esam")
            Prelude.<*> (x Core..:? "audioDuration")
            Prelude.<*> (x Core..:? "pmtInterval")
            Prelude.<*> (x Core..:? "dvbSdtSettings")
            Prelude.<*> (x Core..:? "nullPacketBitrate")
            Prelude.<*> (x Core..:? "audioBufferModel")
            Prelude.<*> (x Core..:? "timedMetadataPid")
            Prelude.<*> (x Core..:? "audioFramesPerPes")
            Prelude.<*> (x Core..:? "pcrPid")
            Prelude.<*> (x Core..:? "segmentationMarkers")
            Prelude.<*> (x Core..:? "dvbSubPids" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "scte35Source")
            Prelude.<*> (x Core..:? "patInterval")
            Prelude.<*> (x Core..:? "forceTsVideoEbpOrder")
            Prelude.<*> (x Core..:? "esRateInPes")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "audioPids" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "dvbTeletextPid")
            Prelude.<*> (x Core..:? "nielsenId3")
            Prelude.<*> (x Core..:? "dataPTSControl")
            Prelude.<*> (x Core..:? "segmentationTime")
            Prelude.<*> (x Core..:? "ebpAudioInterval")
            Prelude.<*> (x Core..:? "dvbNitSettings")
            Prelude.<*> (x Core..:? "pcrControl")
            Prelude.<*> (x Core..:? "ebpPlacement")
            Prelude.<*> (x Core..:? "rateMode")
            Prelude.<*> (x Core..:? "segmentationStyle")
            Prelude.<*> (x Core..:? "dvbTdtSettings")
      )

instance Prelude.Hashable M2tsSettings where
  hashWithSalt salt' M2tsSettings' {..} =
    salt' `Prelude.hashWithSalt` dvbTdtSettings
      `Prelude.hashWithSalt` segmentationStyle
      `Prelude.hashWithSalt` rateMode
      `Prelude.hashWithSalt` ebpPlacement
      `Prelude.hashWithSalt` pcrControl
      `Prelude.hashWithSalt` dvbNitSettings
      `Prelude.hashWithSalt` ebpAudioInterval
      `Prelude.hashWithSalt` segmentationTime
      `Prelude.hashWithSalt` dataPTSControl
      `Prelude.hashWithSalt` nielsenId3
      `Prelude.hashWithSalt` dvbTeletextPid
      `Prelude.hashWithSalt` audioPids
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` esRateInPes
      `Prelude.hashWithSalt` forceTsVideoEbpOrder
      `Prelude.hashWithSalt` patInterval
      `Prelude.hashWithSalt` scte35Source
      `Prelude.hashWithSalt` dvbSubPids
      `Prelude.hashWithSalt` segmentationMarkers
      `Prelude.hashWithSalt` pcrPid
      `Prelude.hashWithSalt` audioFramesPerPes
      `Prelude.hashWithSalt` timedMetadataPid
      `Prelude.hashWithSalt` audioBufferModel
      `Prelude.hashWithSalt` nullPacketBitrate
      `Prelude.hashWithSalt` dvbSdtSettings
      `Prelude.hashWithSalt` pmtInterval
      `Prelude.hashWithSalt` audioDuration
      `Prelude.hashWithSalt` scte35Esam
      `Prelude.hashWithSalt` privateMetadataPid
      `Prelude.hashWithSalt` fragmentTime
      `Prelude.hashWithSalt` maxPcrInterval
      `Prelude.hashWithSalt` transportStreamId
      `Prelude.hashWithSalt` minEbpInterval
      `Prelude.hashWithSalt` scte35Pid
      `Prelude.hashWithSalt` programNumber
      `Prelude.hashWithSalt` bufferModel
      `Prelude.hashWithSalt` videoPid
      `Prelude.hashWithSalt` pmtPid

instance Prelude.NFData M2tsSettings where
  rnf M2tsSettings' {..} =
    Prelude.rnf pmtPid
      `Prelude.seq` Prelude.rnf dvbTdtSettings
      `Prelude.seq` Prelude.rnf segmentationStyle
      `Prelude.seq` Prelude.rnf rateMode
      `Prelude.seq` Prelude.rnf ebpPlacement
      `Prelude.seq` Prelude.rnf pcrControl
      `Prelude.seq` Prelude.rnf dvbNitSettings
      `Prelude.seq` Prelude.rnf ebpAudioInterval
      `Prelude.seq` Prelude.rnf segmentationTime
      `Prelude.seq` Prelude.rnf dataPTSControl
      `Prelude.seq` Prelude.rnf nielsenId3
      `Prelude.seq` Prelude.rnf dvbTeletextPid
      `Prelude.seq` Prelude.rnf audioPids
      `Prelude.seq` Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf esRateInPes
      `Prelude.seq` Prelude.rnf forceTsVideoEbpOrder
      `Prelude.seq` Prelude.rnf patInterval
      `Prelude.seq` Prelude.rnf scte35Source
      `Prelude.seq` Prelude.rnf dvbSubPids
      `Prelude.seq` Prelude.rnf segmentationMarkers
      `Prelude.seq` Prelude.rnf pcrPid
      `Prelude.seq` Prelude.rnf audioFramesPerPes
      `Prelude.seq` Prelude.rnf timedMetadataPid
      `Prelude.seq` Prelude.rnf audioBufferModel
      `Prelude.seq` Prelude.rnf nullPacketBitrate
      `Prelude.seq` Prelude.rnf dvbSdtSettings
      `Prelude.seq` Prelude.rnf pmtInterval
      `Prelude.seq` Prelude.rnf audioDuration
      `Prelude.seq` Prelude.rnf scte35Esam
      `Prelude.seq` Prelude.rnf privateMetadataPid
      `Prelude.seq` Prelude.rnf fragmentTime
      `Prelude.seq` Prelude.rnf maxPcrInterval
      `Prelude.seq` Prelude.rnf transportStreamId
      `Prelude.seq` Prelude.rnf minEbpInterval
      `Prelude.seq` Prelude.rnf scte35Pid
      `Prelude.seq` Prelude.rnf programNumber
      `Prelude.seq` Prelude.rnf bufferModel
      `Prelude.seq` Prelude.rnf videoPid

instance Core.ToJSON M2tsSettings where
  toJSON M2tsSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("pmtPid" Core..=) Prelude.<$> pmtPid,
            ("videoPid" Core..=) Prelude.<$> videoPid,
            ("bufferModel" Core..=) Prelude.<$> bufferModel,
            ("programNumber" Core..=) Prelude.<$> programNumber,
            ("scte35Pid" Core..=) Prelude.<$> scte35Pid,
            ("minEbpInterval" Core..=)
              Prelude.<$> minEbpInterval,
            ("transportStreamId" Core..=)
              Prelude.<$> transportStreamId,
            ("maxPcrInterval" Core..=)
              Prelude.<$> maxPcrInterval,
            ("fragmentTime" Core..=) Prelude.<$> fragmentTime,
            ("privateMetadataPid" Core..=)
              Prelude.<$> privateMetadataPid,
            ("scte35Esam" Core..=) Prelude.<$> scte35Esam,
            ("audioDuration" Core..=) Prelude.<$> audioDuration,
            ("pmtInterval" Core..=) Prelude.<$> pmtInterval,
            ("dvbSdtSettings" Core..=)
              Prelude.<$> dvbSdtSettings,
            ("nullPacketBitrate" Core..=)
              Prelude.<$> nullPacketBitrate,
            ("audioBufferModel" Core..=)
              Prelude.<$> audioBufferModel,
            ("timedMetadataPid" Core..=)
              Prelude.<$> timedMetadataPid,
            ("audioFramesPerPes" Core..=)
              Prelude.<$> audioFramesPerPes,
            ("pcrPid" Core..=) Prelude.<$> pcrPid,
            ("segmentationMarkers" Core..=)
              Prelude.<$> segmentationMarkers,
            ("dvbSubPids" Core..=) Prelude.<$> dvbSubPids,
            ("scte35Source" Core..=) Prelude.<$> scte35Source,
            ("patInterval" Core..=) Prelude.<$> patInterval,
            ("forceTsVideoEbpOrder" Core..=)
              Prelude.<$> forceTsVideoEbpOrder,
            ("esRateInPes" Core..=) Prelude.<$> esRateInPes,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("audioPids" Core..=) Prelude.<$> audioPids,
            ("dvbTeletextPid" Core..=)
              Prelude.<$> dvbTeletextPid,
            ("nielsenId3" Core..=) Prelude.<$> nielsenId3,
            ("dataPTSControl" Core..=)
              Prelude.<$> dataPTSControl,
            ("segmentationTime" Core..=)
              Prelude.<$> segmentationTime,
            ("ebpAudioInterval" Core..=)
              Prelude.<$> ebpAudioInterval,
            ("dvbNitSettings" Core..=)
              Prelude.<$> dvbNitSettings,
            ("pcrControl" Core..=) Prelude.<$> pcrControl,
            ("ebpPlacement" Core..=) Prelude.<$> ebpPlacement,
            ("rateMode" Core..=) Prelude.<$> rateMode,
            ("segmentationStyle" Core..=)
              Prelude.<$> segmentationStyle,
            ("dvbTdtSettings" Core..=)
              Prelude.<$> dvbTdtSettings
          ]
      )
