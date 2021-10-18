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
-- Module      : Network.AWS.MediaConvert.Types.M2tsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DvbNitSettings
import Network.AWS.MediaConvert.Types.DvbSdtSettings
import Network.AWS.MediaConvert.Types.DvbTdtSettings
import Network.AWS.MediaConvert.Types.M2tsAudioBufferModel
import Network.AWS.MediaConvert.Types.M2tsAudioDuration
import Network.AWS.MediaConvert.Types.M2tsBufferModel
import Network.AWS.MediaConvert.Types.M2tsDataPtsControl
import Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval
import Network.AWS.MediaConvert.Types.M2tsEbpPlacement
import Network.AWS.MediaConvert.Types.M2tsEsRateInPes
import Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
import Network.AWS.MediaConvert.Types.M2tsNielsenId3
import Network.AWS.MediaConvert.Types.M2tsPcrControl
import Network.AWS.MediaConvert.Types.M2tsRateMode
import Network.AWS.MediaConvert.Types.M2tsScte35Esam
import Network.AWS.MediaConvert.Types.M2tsScte35Source
import Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers
import Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
import qualified Network.AWS.Prelude as Prelude

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
  { -- | Inserts segmentation markers at each segmentation_time period.
    -- rai_segstart sets the Random Access Indicator bit in the adaptation
    -- field. rai_adapt sets the RAI bit and adds the current timecode in the
    -- private data bytes. psi_segstart inserts PAT and PMT tables at the start
    -- of segments. ebp adds Encoder Boundary Point information to the
    -- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
    -- ebp_legacy adds Encoder Boundary Point information to the adaptation
    -- field using a legacy proprietary format.
    segmentationMarkers :: Prelude.Maybe M2tsSegmentationMarkers,
    -- | Specify the packet identifier (PID) for the program map table (PMT)
    -- itself. Default is 480.
    pmtPid :: Prelude.Maybe Prelude.Natural,
    -- | Specify the packet identifier (PID) of the elementary video stream in
    -- the transport stream.
    videoPid :: Prelude.Maybe Prelude.Natural,
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
    -- | Specify the packet identifier (PID) for timed metadata in this output.
    -- Default is 502.
    timedMetadataPid :: Prelude.Maybe Prelude.Natural,
    -- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
    audioBufferModel :: Prelude.Maybe M2tsAudioBufferModel,
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
    -- | Specify the number of milliseconds between instances of the program map
    -- table (PMT) in the output transport stream.
    pmtInterval :: Prelude.Maybe Prelude.Natural,
    -- | Use these settings to insert a DVB Network Information Table (NIT) in
    -- the transport stream of this output. When you work directly in your JSON
    -- job specification, include this object only when your job has a
    -- transport stream output and the container settings contain the object
    -- M2tsSettings.
    dvbNitSettings :: Prelude.Maybe DvbNitSettings,
    -- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
    -- to partitions 3 and 4. The interval between these additional markers
    -- will be fixed, and will be slightly shorter than the video EBP marker
    -- interval. When set to VIDEO_INTERVAL, these additional markers will not
    -- be inserted. Only applicable when EBP segmentation markers are is
    -- selected (segmentationMarkers is EBP or EBP_LEGACY).
    ebpAudioInterval :: Prelude.Maybe M2tsEbpAudioInterval,
    -- | Value in bits per second of extra null packets to insert into the
    -- transport stream. This can be used if a downstream encryption system
    -- requires periodic null packets.
    nullPacketBitrate :: Prelude.Maybe Prelude.Double,
    -- | Specify the packet identifiers (PIDs) for any elementary audio streams
    -- you include in this output. Specify multiple PIDs as a JSON array.
    -- Default is the range 482-492.
    audioPids :: Prelude.Maybe [Prelude.Natural],
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    patInterval :: Prelude.Maybe Prelude.Natural,
    -- | Specify the maximum time, in milliseconds, between Program Clock
    -- References (PCRs) inserted into the transport stream.
    maxPcrInterval :: Prelude.Maybe Prelude.Natural,
    -- | When set, enforces that Encoder Boundary Points do not come within the
    -- specified time interval of each other by looking ahead at input video.
    -- If another EBP is going to come in within the specified time interval,
    -- the current EBP is not emitted, and the segment is \"stretched\" to the
    -- next marker. The lookahead value does not add latency to the system. The
    -- Live Event must be configured elsewhere to create sufficient latency to
    -- make the lookahead accurate.
    minEbpInterval :: Prelude.Maybe Prelude.Natural,
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
    -- | Specify the packet identifier (PID) for the program clock reference
    -- (PCR) in this output. If you do not specify a value, the service will
    -- use the value for Video PID (VideoPid).
    pcrPid :: Prelude.Maybe Prelude.Natural,
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Prelude.Maybe Prelude.Natural,
    -- | When set to CBR, inserts null packets into transport stream to fill
    -- specified bitrate. When set to VBR, the bitrate setting acts as the
    -- maximum bitrate, but the output will not be padded up to that bitrate.
    rateMode :: Prelude.Maybe M2tsRateMode,
    -- | Use these settings to insert a DVB Time and Date Table (TDT) in the
    -- transport stream of this output. When you work directly in your JSON job
    -- specification, include this object only when your job has a transport
    -- stream output and the container settings contain the object
    -- M2tsSettings.
    dvbTdtSettings :: Prelude.Maybe DvbTdtSettings,
    -- | Use these settings to insert a DVB Service Description Table (SDT) in
    -- the transport stream of this output. When you work directly in your JSON
    -- job specification, include this object only when your job has a
    -- transport stream output and the container settings contain the object
    -- M2tsSettings.
    dvbSdtSettings :: Prelude.Maybe DvbSdtSettings,
    -- | Specify the length, in seconds, of each segment. Required unless markers
    -- is set to _none_.
    segmentationTime :: Prelude.Maybe Prelude.Double,
    -- | If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
    -- packets with Presentation Timestamp (PTS) values greater than or equal
    -- to the first video packet PTS (MediaConvert drops captions and data
    -- packets with lesser PTS values). Keep the default value (AUTO) to allow
    -- all PTS values.
    dataPTSControl :: Prelude.Maybe M2tsDataPtsControl,
    -- | If INSERT, Nielsen inaudible tones for media tracking will be detected
    -- in the input audio and an equivalent ID3 tag will be inserted in the
    -- output.
    nielsenId3 :: Prelude.Maybe M2tsNielsenId3,
    -- | Specify the packet identifier (PID) for DVB teletext data you include in
    -- this output. Default is 499.
    dvbTeletextPid :: Prelude.Maybe Prelude.Natural,
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
    -- | The length, in seconds, of each fragment. Only used with EBP markers.
    fragmentTime :: Prelude.Maybe Prelude.Double,
    -- | Specify the output bitrate of the transport stream in bits per second.
    -- Setting to 0 lets the muxer automatically determine the appropriate
    -- bitrate. Other common values are 3750000, 7500000, and 15000000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the packet identifier (PID) of the private metadata stream.
    -- Default is 503.
    privateMetadataPid :: Prelude.Maybe Prelude.Natural,
    -- | Controls whether to include the ES Rate field in the PES header.
    esRateInPes :: Prelude.Maybe M2tsEsRateInPes,
    -- | Include this in your job settings to put SCTE-35 markers in your HLS and
    -- transport stream outputs at the insertion points that you specify in an
    -- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
    scte35Esam :: Prelude.Maybe M2tsScte35Esam,
    -- | Specify the ID for the transport stream itself in the program map table
    -- for this output. Transport stream IDs and program map tables are parts
    -- of MPEG-2 transport stream containers, used for organizing data.
    transportStreamId :: Prelude.Maybe Prelude.Natural,
    -- | Specify the packet identifiers (PIDs) for DVB subtitle data included in
    -- this output. Specify multiple PIDs as a JSON array. Default is the range
    -- 460-479.
    dvbSubPids :: Prelude.Maybe [Prelude.Natural],
    -- | Specify the packet identifier (PID) of the SCTE-35 stream in the
    -- transport stream.
    scte35Pid :: Prelude.Maybe Prelude.Natural,
    -- | Keep the default value (DEFAULT) unless you know that your audio EBP
    -- markers are incorrectly appearing before your video EBP markers. To
    -- correct this problem, set this value to Force (FORCE).
    forceTsVideoEbpOrder :: Prelude.Maybe M2tsForceTsVideoEbpOrder,
    -- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
    -- if you want SCTE-35 markers that appear in your input to also appear in
    -- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
    -- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
    -- (NONE). Also provide the ESAM XML as a string in the setting Signal
    -- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
    -- the property scte35Esam).
    scte35Source :: Prelude.Maybe M2tsScte35Source
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
-- 'segmentationMarkers', 'm2tsSettings_segmentationMarkers' - Inserts segmentation markers at each segmentation_time period.
-- rai_segstart sets the Random Access Indicator bit in the adaptation
-- field. rai_adapt sets the RAI bit and adds the current timecode in the
-- private data bytes. psi_segstart inserts PAT and PMT tables at the start
-- of segments. ebp adds Encoder Boundary Point information to the
-- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
-- ebp_legacy adds Encoder Boundary Point information to the adaptation
-- field using a legacy proprietary format.
--
-- 'pmtPid', 'm2tsSettings_pmtPid' - Specify the packet identifier (PID) for the program map table (PMT)
-- itself. Default is 480.
--
-- 'videoPid', 'm2tsSettings_videoPid' - Specify the packet identifier (PID) of the elementary video stream in
-- the transport stream.
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
-- 'timedMetadataPid', 'm2tsSettings_timedMetadataPid' - Specify the packet identifier (PID) for timed metadata in this output.
-- Default is 502.
--
-- 'audioBufferModel', 'm2tsSettings_audioBufferModel' - Selects between the DVB and ATSC buffer models for Dolby Digital audio.
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
-- 'pmtInterval', 'm2tsSettings_pmtInterval' - Specify the number of milliseconds between instances of the program map
-- table (PMT) in the output transport stream.
--
-- 'dvbNitSettings', 'm2tsSettings_dvbNitSettings' - Use these settings to insert a DVB Network Information Table (NIT) in
-- the transport stream of this output. When you work directly in your JSON
-- job specification, include this object only when your job has a
-- transport stream output and the container settings contain the object
-- M2tsSettings.
--
-- 'ebpAudioInterval', 'm2tsSettings_ebpAudioInterval' - When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. When set to VIDEO_INTERVAL, these additional markers will not
-- be inserted. Only applicable when EBP segmentation markers are is
-- selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- 'nullPacketBitrate', 'm2tsSettings_nullPacketBitrate' - Value in bits per second of extra null packets to insert into the
-- transport stream. This can be used if a downstream encryption system
-- requires periodic null packets.
--
-- 'audioPids', 'm2tsSettings_audioPids' - Specify the packet identifiers (PIDs) for any elementary audio streams
-- you include in this output. Specify multiple PIDs as a JSON array.
-- Default is the range 482-492.
--
-- 'patInterval', 'm2tsSettings_patInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'maxPcrInterval', 'm2tsSettings_maxPcrInterval' - Specify the maximum time, in milliseconds, between Program Clock
-- References (PCRs) inserted into the transport stream.
--
-- 'minEbpInterval', 'm2tsSettings_minEbpInterval' - When set, enforces that Encoder Boundary Points do not come within the
-- specified time interval of each other by looking ahead at input video.
-- If another EBP is going to come in within the specified time interval,
-- the current EBP is not emitted, and the segment is \"stretched\" to the
-- next marker. The lookahead value does not add latency to the system. The
-- Live Event must be configured elsewhere to create sufficient latency to
-- make the lookahead accurate.
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
-- 'pcrPid', 'm2tsSettings_pcrPid' - Specify the packet identifier (PID) for the program clock reference
-- (PCR) in this output. If you do not specify a value, the service will
-- use the value for Video PID (VideoPid).
--
-- 'audioFramesPerPes', 'm2tsSettings_audioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- 'rateMode', 'm2tsSettings_rateMode' - When set to CBR, inserts null packets into transport stream to fill
-- specified bitrate. When set to VBR, the bitrate setting acts as the
-- maximum bitrate, but the output will not be padded up to that bitrate.
--
-- 'dvbTdtSettings', 'm2tsSettings_dvbTdtSettings' - Use these settings to insert a DVB Time and Date Table (TDT) in the
-- transport stream of this output. When you work directly in your JSON job
-- specification, include this object only when your job has a transport
-- stream output and the container settings contain the object
-- M2tsSettings.
--
-- 'dvbSdtSettings', 'm2tsSettings_dvbSdtSettings' - Use these settings to insert a DVB Service Description Table (SDT) in
-- the transport stream of this output. When you work directly in your JSON
-- job specification, include this object only when your job has a
-- transport stream output and the container settings contain the object
-- M2tsSettings.
--
-- 'segmentationTime', 'm2tsSettings_segmentationTime' - Specify the length, in seconds, of each segment. Required unless markers
-- is set to _none_.
--
-- 'dataPTSControl', 'm2tsSettings_dataPTSControl' - If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
-- packets with Presentation Timestamp (PTS) values greater than or equal
-- to the first video packet PTS (MediaConvert drops captions and data
-- packets with lesser PTS values). Keep the default value (AUTO) to allow
-- all PTS values.
--
-- 'nielsenId3', 'm2tsSettings_nielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
--
-- 'dvbTeletextPid', 'm2tsSettings_dvbTeletextPid' - Specify the packet identifier (PID) for DVB teletext data you include in
-- this output. Default is 499.
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
-- 'fragmentTime', 'm2tsSettings_fragmentTime' - The length, in seconds, of each fragment. Only used with EBP markers.
--
-- 'bitrate', 'm2tsSettings_bitrate' - Specify the output bitrate of the transport stream in bits per second.
-- Setting to 0 lets the muxer automatically determine the appropriate
-- bitrate. Other common values are 3750000, 7500000, and 15000000.
--
-- 'privateMetadataPid', 'm2tsSettings_privateMetadataPid' - Specify the packet identifier (PID) of the private metadata stream.
-- Default is 503.
--
-- 'esRateInPes', 'm2tsSettings_esRateInPes' - Controls whether to include the ES Rate field in the PES header.
--
-- 'scte35Esam', 'm2tsSettings_scte35Esam' - Include this in your job settings to put SCTE-35 markers in your HLS and
-- transport stream outputs at the insertion points that you specify in an
-- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- 'transportStreamId', 'm2tsSettings_transportStreamId' - Specify the ID for the transport stream itself in the program map table
-- for this output. Transport stream IDs and program map tables are parts
-- of MPEG-2 transport stream containers, used for organizing data.
--
-- 'dvbSubPids', 'm2tsSettings_dvbSubPids' - Specify the packet identifiers (PIDs) for DVB subtitle data included in
-- this output. Specify multiple PIDs as a JSON array. Default is the range
-- 460-479.
--
-- 'scte35Pid', 'm2tsSettings_scte35Pid' - Specify the packet identifier (PID) of the SCTE-35 stream in the
-- transport stream.
--
-- 'forceTsVideoEbpOrder', 'm2tsSettings_forceTsVideoEbpOrder' - Keep the default value (DEFAULT) unless you know that your audio EBP
-- markers are incorrectly appearing before your video EBP markers. To
-- correct this problem, set this value to Force (FORCE).
--
-- 'scte35Source', 'm2tsSettings_scte35Source' - For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE). Also provide the ESAM XML as a string in the setting Signal
-- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
-- the property scte35Esam).
newM2tsSettings ::
  M2tsSettings
newM2tsSettings =
  M2tsSettings'
    { segmentationMarkers =
        Prelude.Nothing,
      pmtPid = Prelude.Nothing,
      videoPid = Prelude.Nothing,
      segmentationStyle = Prelude.Nothing,
      timedMetadataPid = Prelude.Nothing,
      audioBufferModel = Prelude.Nothing,
      pcrControl = Prelude.Nothing,
      ebpPlacement = Prelude.Nothing,
      pmtInterval = Prelude.Nothing,
      dvbNitSettings = Prelude.Nothing,
      ebpAudioInterval = Prelude.Nothing,
      nullPacketBitrate = Prelude.Nothing,
      audioPids = Prelude.Nothing,
      patInterval = Prelude.Nothing,
      maxPcrInterval = Prelude.Nothing,
      minEbpInterval = Prelude.Nothing,
      bufferModel = Prelude.Nothing,
      programNumber = Prelude.Nothing,
      pcrPid = Prelude.Nothing,
      audioFramesPerPes = Prelude.Nothing,
      rateMode = Prelude.Nothing,
      dvbTdtSettings = Prelude.Nothing,
      dvbSdtSettings = Prelude.Nothing,
      segmentationTime = Prelude.Nothing,
      dataPTSControl = Prelude.Nothing,
      nielsenId3 = Prelude.Nothing,
      dvbTeletextPid = Prelude.Nothing,
      audioDuration = Prelude.Nothing,
      fragmentTime = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      privateMetadataPid = Prelude.Nothing,
      esRateInPes = Prelude.Nothing,
      scte35Esam = Prelude.Nothing,
      transportStreamId = Prelude.Nothing,
      dvbSubPids = Prelude.Nothing,
      scte35Pid = Prelude.Nothing,
      forceTsVideoEbpOrder = Prelude.Nothing,
      scte35Source = Prelude.Nothing
    }

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

-- | Specify the packet identifier (PID) for the program map table (PMT)
-- itself. Default is 480.
m2tsSettings_pmtPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_pmtPid = Lens.lens (\M2tsSettings' {pmtPid} -> pmtPid) (\s@M2tsSettings' {} a -> s {pmtPid = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) of the elementary video stream in
-- the transport stream.
m2tsSettings_videoPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_videoPid = Lens.lens (\M2tsSettings' {videoPid} -> videoPid) (\s@M2tsSettings' {} a -> s {videoPid = a} :: M2tsSettings)

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

-- | Specify the packet identifier (PID) for timed metadata in this output.
-- Default is 502.
m2tsSettings_timedMetadataPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_timedMetadataPid = Lens.lens (\M2tsSettings' {timedMetadataPid} -> timedMetadataPid) (\s@M2tsSettings' {} a -> s {timedMetadataPid = a} :: M2tsSettings)

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
m2tsSettings_audioBufferModel :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsAudioBufferModel)
m2tsSettings_audioBufferModel = Lens.lens (\M2tsSettings' {audioBufferModel} -> audioBufferModel) (\s@M2tsSettings' {} a -> s {audioBufferModel = a} :: M2tsSettings)

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

-- | Specify the number of milliseconds between instances of the program map
-- table (PMT) in the output transport stream.
m2tsSettings_pmtInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_pmtInterval = Lens.lens (\M2tsSettings' {pmtInterval} -> pmtInterval) (\s@M2tsSettings' {} a -> s {pmtInterval = a} :: M2tsSettings)

-- | Use these settings to insert a DVB Network Information Table (NIT) in
-- the transport stream of this output. When you work directly in your JSON
-- job specification, include this object only when your job has a
-- transport stream output and the container settings contain the object
-- M2tsSettings.
m2tsSettings_dvbNitSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbNitSettings)
m2tsSettings_dvbNitSettings = Lens.lens (\M2tsSettings' {dvbNitSettings} -> dvbNitSettings) (\s@M2tsSettings' {} a -> s {dvbNitSettings = a} :: M2tsSettings)

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. When set to VIDEO_INTERVAL, these additional markers will not
-- be inserted. Only applicable when EBP segmentation markers are is
-- selected (segmentationMarkers is EBP or EBP_LEGACY).
m2tsSettings_ebpAudioInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsEbpAudioInterval)
m2tsSettings_ebpAudioInterval = Lens.lens (\M2tsSettings' {ebpAudioInterval} -> ebpAudioInterval) (\s@M2tsSettings' {} a -> s {ebpAudioInterval = a} :: M2tsSettings)

-- | Value in bits per second of extra null packets to insert into the
-- transport stream. This can be used if a downstream encryption system
-- requires periodic null packets.
m2tsSettings_nullPacketBitrate :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_nullPacketBitrate = Lens.lens (\M2tsSettings' {nullPacketBitrate} -> nullPacketBitrate) (\s@M2tsSettings' {} a -> s {nullPacketBitrate = a} :: M2tsSettings)

-- | Specify the packet identifiers (PIDs) for any elementary audio streams
-- you include in this output. Specify multiple PIDs as a JSON array.
-- Default is the range 482-492.
m2tsSettings_audioPids :: Lens.Lens' M2tsSettings (Prelude.Maybe [Prelude.Natural])
m2tsSettings_audioPids = Lens.lens (\M2tsSettings' {audioPids} -> audioPids) (\s@M2tsSettings' {} a -> s {audioPids = a} :: M2tsSettings) Prelude.. Lens.mapping Lens._Coerce

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
m2tsSettings_patInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_patInterval = Lens.lens (\M2tsSettings' {patInterval} -> patInterval) (\s@M2tsSettings' {} a -> s {patInterval = a} :: M2tsSettings)

-- | Specify the maximum time, in milliseconds, between Program Clock
-- References (PCRs) inserted into the transport stream.
m2tsSettings_maxPcrInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_maxPcrInterval = Lens.lens (\M2tsSettings' {maxPcrInterval} -> maxPcrInterval) (\s@M2tsSettings' {} a -> s {maxPcrInterval = a} :: M2tsSettings)

-- | When set, enforces that Encoder Boundary Points do not come within the
-- specified time interval of each other by looking ahead at input video.
-- If another EBP is going to come in within the specified time interval,
-- the current EBP is not emitted, and the segment is \"stretched\" to the
-- next marker. The lookahead value does not add latency to the system. The
-- Live Event must be configured elsewhere to create sufficient latency to
-- make the lookahead accurate.
m2tsSettings_minEbpInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_minEbpInterval = Lens.lens (\M2tsSettings' {minEbpInterval} -> minEbpInterval) (\s@M2tsSettings' {} a -> s {minEbpInterval = a} :: M2tsSettings)

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

-- | Specify the packet identifier (PID) for the program clock reference
-- (PCR) in this output. If you do not specify a value, the service will
-- use the value for Video PID (VideoPid).
m2tsSettings_pcrPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_pcrPid = Lens.lens (\M2tsSettings' {pcrPid} -> pcrPid) (\s@M2tsSettings' {} a -> s {pcrPid = a} :: M2tsSettings)

-- | The number of audio frames to insert for each PES packet.
m2tsSettings_audioFramesPerPes :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_audioFramesPerPes = Lens.lens (\M2tsSettings' {audioFramesPerPes} -> audioFramesPerPes) (\s@M2tsSettings' {} a -> s {audioFramesPerPes = a} :: M2tsSettings)

-- | When set to CBR, inserts null packets into transport stream to fill
-- specified bitrate. When set to VBR, the bitrate setting acts as the
-- maximum bitrate, but the output will not be padded up to that bitrate.
m2tsSettings_rateMode :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsRateMode)
m2tsSettings_rateMode = Lens.lens (\M2tsSettings' {rateMode} -> rateMode) (\s@M2tsSettings' {} a -> s {rateMode = a} :: M2tsSettings)

-- | Use these settings to insert a DVB Time and Date Table (TDT) in the
-- transport stream of this output. When you work directly in your JSON job
-- specification, include this object only when your job has a transport
-- stream output and the container settings contain the object
-- M2tsSettings.
m2tsSettings_dvbTdtSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbTdtSettings)
m2tsSettings_dvbTdtSettings = Lens.lens (\M2tsSettings' {dvbTdtSettings} -> dvbTdtSettings) (\s@M2tsSettings' {} a -> s {dvbTdtSettings = a} :: M2tsSettings)

-- | Use these settings to insert a DVB Service Description Table (SDT) in
-- the transport stream of this output. When you work directly in your JSON
-- job specification, include this object only when your job has a
-- transport stream output and the container settings contain the object
-- M2tsSettings.
m2tsSettings_dvbSdtSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbSdtSettings)
m2tsSettings_dvbSdtSettings = Lens.lens (\M2tsSettings' {dvbSdtSettings} -> dvbSdtSettings) (\s@M2tsSettings' {} a -> s {dvbSdtSettings = a} :: M2tsSettings)

-- | Specify the length, in seconds, of each segment. Required unless markers
-- is set to _none_.
m2tsSettings_segmentationTime :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_segmentationTime = Lens.lens (\M2tsSettings' {segmentationTime} -> segmentationTime) (\s@M2tsSettings' {} a -> s {segmentationTime = a} :: M2tsSettings)

-- | If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
-- packets with Presentation Timestamp (PTS) values greater than or equal
-- to the first video packet PTS (MediaConvert drops captions and data
-- packets with lesser PTS values). Keep the default value (AUTO) to allow
-- all PTS values.
m2tsSettings_dataPTSControl :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsDataPtsControl)
m2tsSettings_dataPTSControl = Lens.lens (\M2tsSettings' {dataPTSControl} -> dataPTSControl) (\s@M2tsSettings' {} a -> s {dataPTSControl = a} :: M2tsSettings)

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
m2tsSettings_nielsenId3 :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsNielsenId3)
m2tsSettings_nielsenId3 = Lens.lens (\M2tsSettings' {nielsenId3} -> nielsenId3) (\s@M2tsSettings' {} a -> s {nielsenId3 = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) for DVB teletext data you include in
-- this output. Default is 499.
m2tsSettings_dvbTeletextPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_dvbTeletextPid = Lens.lens (\M2tsSettings' {dvbTeletextPid} -> dvbTeletextPid) (\s@M2tsSettings' {} a -> s {dvbTeletextPid = a} :: M2tsSettings)

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

-- | The length, in seconds, of each fragment. Only used with EBP markers.
m2tsSettings_fragmentTime :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_fragmentTime = Lens.lens (\M2tsSettings' {fragmentTime} -> fragmentTime) (\s@M2tsSettings' {} a -> s {fragmentTime = a} :: M2tsSettings)

-- | Specify the output bitrate of the transport stream in bits per second.
-- Setting to 0 lets the muxer automatically determine the appropriate
-- bitrate. Other common values are 3750000, 7500000, and 15000000.
m2tsSettings_bitrate :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_bitrate = Lens.lens (\M2tsSettings' {bitrate} -> bitrate) (\s@M2tsSettings' {} a -> s {bitrate = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) of the private metadata stream.
-- Default is 503.
m2tsSettings_privateMetadataPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_privateMetadataPid = Lens.lens (\M2tsSettings' {privateMetadataPid} -> privateMetadataPid) (\s@M2tsSettings' {} a -> s {privateMetadataPid = a} :: M2tsSettings)

-- | Controls whether to include the ES Rate field in the PES header.
m2tsSettings_esRateInPes :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsEsRateInPes)
m2tsSettings_esRateInPes = Lens.lens (\M2tsSettings' {esRateInPes} -> esRateInPes) (\s@M2tsSettings' {} a -> s {esRateInPes = a} :: M2tsSettings)

-- | Include this in your job settings to put SCTE-35 markers in your HLS and
-- transport stream outputs at the insertion points that you specify in an
-- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
m2tsSettings_scte35Esam :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsScte35Esam)
m2tsSettings_scte35Esam = Lens.lens (\M2tsSettings' {scte35Esam} -> scte35Esam) (\s@M2tsSettings' {} a -> s {scte35Esam = a} :: M2tsSettings)

-- | Specify the ID for the transport stream itself in the program map table
-- for this output. Transport stream IDs and program map tables are parts
-- of MPEG-2 transport stream containers, used for organizing data.
m2tsSettings_transportStreamId :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_transportStreamId = Lens.lens (\M2tsSettings' {transportStreamId} -> transportStreamId) (\s@M2tsSettings' {} a -> s {transportStreamId = a} :: M2tsSettings)

-- | Specify the packet identifiers (PIDs) for DVB subtitle data included in
-- this output. Specify multiple PIDs as a JSON array. Default is the range
-- 460-479.
m2tsSettings_dvbSubPids :: Lens.Lens' M2tsSettings (Prelude.Maybe [Prelude.Natural])
m2tsSettings_dvbSubPids = Lens.lens (\M2tsSettings' {dvbSubPids} -> dvbSubPids) (\s@M2tsSettings' {} a -> s {dvbSubPids = a} :: M2tsSettings) Prelude.. Lens.mapping Lens._Coerce

-- | Specify the packet identifier (PID) of the SCTE-35 stream in the
-- transport stream.
m2tsSettings_scte35Pid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_scte35Pid = Lens.lens (\M2tsSettings' {scte35Pid} -> scte35Pid) (\s@M2tsSettings' {} a -> s {scte35Pid = a} :: M2tsSettings)

-- | Keep the default value (DEFAULT) unless you know that your audio EBP
-- markers are incorrectly appearing before your video EBP markers. To
-- correct this problem, set this value to Force (FORCE).
m2tsSettings_forceTsVideoEbpOrder :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsForceTsVideoEbpOrder)
m2tsSettings_forceTsVideoEbpOrder = Lens.lens (\M2tsSettings' {forceTsVideoEbpOrder} -> forceTsVideoEbpOrder) (\s@M2tsSettings' {} a -> s {forceTsVideoEbpOrder = a} :: M2tsSettings)

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE). Also provide the ESAM XML as a string in the setting Signal
-- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
-- the property scte35Esam).
m2tsSettings_scte35Source :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsScte35Source)
m2tsSettings_scte35Source = Lens.lens (\M2tsSettings' {scte35Source} -> scte35Source) (\s@M2tsSettings' {} a -> s {scte35Source = a} :: M2tsSettings)

instance Core.FromJSON M2tsSettings where
  parseJSON =
    Core.withObject
      "M2tsSettings"
      ( \x ->
          M2tsSettings'
            Prelude.<$> (x Core..:? "segmentationMarkers")
            Prelude.<*> (x Core..:? "pmtPid")
            Prelude.<*> (x Core..:? "videoPid")
            Prelude.<*> (x Core..:? "segmentationStyle")
            Prelude.<*> (x Core..:? "timedMetadataPid")
            Prelude.<*> (x Core..:? "audioBufferModel")
            Prelude.<*> (x Core..:? "pcrControl")
            Prelude.<*> (x Core..:? "ebpPlacement")
            Prelude.<*> (x Core..:? "pmtInterval")
            Prelude.<*> (x Core..:? "dvbNitSettings")
            Prelude.<*> (x Core..:? "ebpAudioInterval")
            Prelude.<*> (x Core..:? "nullPacketBitrate")
            Prelude.<*> (x Core..:? "audioPids" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "patInterval")
            Prelude.<*> (x Core..:? "maxPcrInterval")
            Prelude.<*> (x Core..:? "minEbpInterval")
            Prelude.<*> (x Core..:? "bufferModel")
            Prelude.<*> (x Core..:? "programNumber")
            Prelude.<*> (x Core..:? "pcrPid")
            Prelude.<*> (x Core..:? "audioFramesPerPes")
            Prelude.<*> (x Core..:? "rateMode")
            Prelude.<*> (x Core..:? "dvbTdtSettings")
            Prelude.<*> (x Core..:? "dvbSdtSettings")
            Prelude.<*> (x Core..:? "segmentationTime")
            Prelude.<*> (x Core..:? "dataPTSControl")
            Prelude.<*> (x Core..:? "nielsenId3")
            Prelude.<*> (x Core..:? "dvbTeletextPid")
            Prelude.<*> (x Core..:? "audioDuration")
            Prelude.<*> (x Core..:? "fragmentTime")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "privateMetadataPid")
            Prelude.<*> (x Core..:? "esRateInPes")
            Prelude.<*> (x Core..:? "scte35Esam")
            Prelude.<*> (x Core..:? "transportStreamId")
            Prelude.<*> (x Core..:? "dvbSubPids" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "scte35Pid")
            Prelude.<*> (x Core..:? "forceTsVideoEbpOrder")
            Prelude.<*> (x Core..:? "scte35Source")
      )

instance Prelude.Hashable M2tsSettings

instance Prelude.NFData M2tsSettings

instance Core.ToJSON M2tsSettings where
  toJSON M2tsSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("segmentationMarkers" Core..=)
              Prelude.<$> segmentationMarkers,
            ("pmtPid" Core..=) Prelude.<$> pmtPid,
            ("videoPid" Core..=) Prelude.<$> videoPid,
            ("segmentationStyle" Core..=)
              Prelude.<$> segmentationStyle,
            ("timedMetadataPid" Core..=)
              Prelude.<$> timedMetadataPid,
            ("audioBufferModel" Core..=)
              Prelude.<$> audioBufferModel,
            ("pcrControl" Core..=) Prelude.<$> pcrControl,
            ("ebpPlacement" Core..=) Prelude.<$> ebpPlacement,
            ("pmtInterval" Core..=) Prelude.<$> pmtInterval,
            ("dvbNitSettings" Core..=)
              Prelude.<$> dvbNitSettings,
            ("ebpAudioInterval" Core..=)
              Prelude.<$> ebpAudioInterval,
            ("nullPacketBitrate" Core..=)
              Prelude.<$> nullPacketBitrate,
            ("audioPids" Core..=) Prelude.<$> audioPids,
            ("patInterval" Core..=) Prelude.<$> patInterval,
            ("maxPcrInterval" Core..=)
              Prelude.<$> maxPcrInterval,
            ("minEbpInterval" Core..=)
              Prelude.<$> minEbpInterval,
            ("bufferModel" Core..=) Prelude.<$> bufferModel,
            ("programNumber" Core..=) Prelude.<$> programNumber,
            ("pcrPid" Core..=) Prelude.<$> pcrPid,
            ("audioFramesPerPes" Core..=)
              Prelude.<$> audioFramesPerPes,
            ("rateMode" Core..=) Prelude.<$> rateMode,
            ("dvbTdtSettings" Core..=)
              Prelude.<$> dvbTdtSettings,
            ("dvbSdtSettings" Core..=)
              Prelude.<$> dvbSdtSettings,
            ("segmentationTime" Core..=)
              Prelude.<$> segmentationTime,
            ("dataPTSControl" Core..=)
              Prelude.<$> dataPTSControl,
            ("nielsenId3" Core..=) Prelude.<$> nielsenId3,
            ("dvbTeletextPid" Core..=)
              Prelude.<$> dvbTeletextPid,
            ("audioDuration" Core..=) Prelude.<$> audioDuration,
            ("fragmentTime" Core..=) Prelude.<$> fragmentTime,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("privateMetadataPid" Core..=)
              Prelude.<$> privateMetadataPid,
            ("esRateInPes" Core..=) Prelude.<$> esRateInPes,
            ("scte35Esam" Core..=) Prelude.<$> scte35Esam,
            ("transportStreamId" Core..=)
              Prelude.<$> transportStreamId,
            ("dvbSubPids" Core..=) Prelude.<$> dvbSubPids,
            ("scte35Pid" Core..=) Prelude.<$> scte35Pid,
            ("forceTsVideoEbpOrder" Core..=)
              Prelude.<$> forceTsVideoEbpOrder,
            ("scte35Source" Core..=) Prelude.<$> scte35Source
          ]
      )
