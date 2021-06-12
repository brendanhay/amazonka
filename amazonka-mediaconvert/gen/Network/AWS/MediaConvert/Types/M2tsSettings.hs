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
    segmentationMarkers :: Core.Maybe M2tsSegmentationMarkers,
    -- | Specify the packet identifier (PID) for the program map table (PMT)
    -- itself. Default is 480.
    pmtPid :: Core.Maybe Core.Natural,
    -- | Specify the packet identifier (PID) of the elementary video stream in
    -- the transport stream.
    videoPid :: Core.Maybe Core.Natural,
    -- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
    audioBufferModel :: Core.Maybe M2tsAudioBufferModel,
    -- | Specify the packet identifier (PID) for timed metadata in this output.
    -- Default is 502.
    timedMetadataPid :: Core.Maybe Core.Natural,
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
    segmentationStyle :: Core.Maybe M2tsSegmentationStyle,
    -- | Inserts DVB Network Information Table (NIT) at the specified table
    -- repetition interval.
    dvbNitSettings :: Core.Maybe DvbNitSettings,
    -- | Value in bits per second of extra null packets to insert into the
    -- transport stream. This can be used if a downstream encryption system
    -- requires periodic null packets.
    nullPacketBitrate :: Core.Maybe Core.Double,
    -- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is
    -- inserted for every Packetized Elementary Stream (PES) header. This is
    -- effective only when the PCR PID is the same as the video or audio
    -- elementary stream.
    pcrControl :: Core.Maybe M2tsPcrControl,
    -- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
    -- to partitions 3 and 4. The interval between these additional markers
    -- will be fixed, and will be slightly shorter than the video EBP marker
    -- interval. When set to VIDEO_INTERVAL, these additional markers will not
    -- be inserted. Only applicable when EBP segmentation markers are is
    -- selected (segmentationMarkers is EBP or EBP_LEGACY).
    ebpAudioInterval :: Core.Maybe M2tsEbpAudioInterval,
    -- | Selects which PIDs to place EBP markers on. They can either be placed
    -- only on the video PID, or on both the video PID and all audio PIDs. Only
    -- applicable when EBP segmentation markers are is selected
    -- (segmentationMarkers is EBP or EBP_LEGACY).
    ebpPlacement :: Core.Maybe M2tsEbpPlacement,
    -- | Specify the number of milliseconds between instances of the program map
    -- table (PMT) in the output transport stream.
    pmtInterval :: Core.Maybe Core.Natural,
    -- | Specify the packet identifiers (PIDs) for any elementary audio streams
    -- you include in this output. Specify multiple PIDs as a JSON array.
    -- Default is the range 482-492.
    audioPids :: Core.Maybe [Core.Natural],
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    patInterval :: Core.Maybe Core.Natural,
    -- | When set, enforces that Encoder Boundary Points do not come within the
    -- specified time interval of each other by looking ahead at input video.
    -- If another EBP is going to come in within the specified time interval,
    -- the current EBP is not emitted, and the segment is \"stretched\" to the
    -- next marker. The lookahead value does not add latency to the system. The
    -- Live Event must be configured elsewhere to create sufficient latency to
    -- make the lookahead accurate.
    minEbpInterval :: Core.Maybe Core.Natural,
    -- | Specify the maximum time, in milliseconds, between Program Clock
    -- References (PCRs) inserted into the transport stream.
    maxPcrInterval :: Core.Maybe Core.Natural,
    -- | Use Program number (programNumber) to specify the program number used in
    -- the program map table (PMT) for this output. Default is 1. Program
    -- numbers and program map tables are parts of MPEG-2 transport stream
    -- containers, used for organizing data.
    programNumber :: Core.Maybe Core.Natural,
    -- | Controls what buffer model to use for accurate interleaving. If set to
    -- MULTIPLEX, use multiplex buffer model. If set to NONE, this can lead to
    -- lower latency, but low-memory devices may not be able to play back the
    -- stream without interruptions.
    bufferModel :: Core.Maybe M2tsBufferModel,
    -- | Specify the packet identifier (PID) for the program clock reference
    -- (PCR) in this output. If you do not specify a value, the service will
    -- use the value for Video PID (VideoPid).
    pcrPid :: Core.Maybe Core.Natural,
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Core.Maybe Core.Natural,
    -- | When set to CBR, inserts null packets into transport stream to fill
    -- specified bitrate. When set to VBR, the bitrate setting acts as the
    -- maximum bitrate, but the output will not be padded up to that bitrate.
    rateMode :: Core.Maybe M2tsRateMode,
    -- | Inserts DVB Time and Date Table (TDT) at the specified table repetition
    -- interval.
    dvbTdtSettings :: Core.Maybe DvbTdtSettings,
    -- | Inserts DVB Service Description Table (NIT) at the specified table
    -- repetition interval.
    dvbSdtSettings :: Core.Maybe DvbSdtSettings,
    -- | Specify the length, in seconds, of each segment. Required unless markers
    -- is set to _none_.
    segmentationTime :: Core.Maybe Core.Double,
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
    audioDuration :: Core.Maybe M2tsAudioDuration,
    -- | If INSERT, Nielsen inaudible tones for media tracking will be detected
    -- in the input audio and an equivalent ID3 tag will be inserted in the
    -- output.
    nielsenId3 :: Core.Maybe M2tsNielsenId3,
    -- | Specify the packet identifier (PID) for DVB teletext data you include in
    -- this output. Default is 499.
    dvbTeletextPid :: Core.Maybe Core.Natural,
    -- | Specify the output bitrate of the transport stream in bits per second.
    -- Setting to 0 lets the muxer automatically determine the appropriate
    -- bitrate. Other common values are 3750000, 7500000, and 15000000.
    bitrate :: Core.Maybe Core.Natural,
    -- | The length, in seconds, of each fragment. Only used with EBP markers.
    fragmentTime :: Core.Maybe Core.Double,
    -- | Controls whether to include the ES Rate field in the PES header.
    esRateInPes :: Core.Maybe M2tsEsRateInPes,
    -- | Specify the packet identifier (PID) of the private metadata stream.
    -- Default is 503.
    privateMetadataPid :: Core.Maybe Core.Natural,
    -- | Include this in your job settings to put SCTE-35 markers in your HLS and
    -- transport stream outputs at the insertion points that you specify in an
    -- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
    scte35Esam :: Core.Maybe M2tsScte35Esam,
    -- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
    -- if you want SCTE-35 markers that appear in your input to also appear in
    -- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
    -- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
    -- (NONE). Also provide the ESAM XML as a string in the setting Signal
    -- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
    -- the property scte35Esam).
    scte35Source :: Core.Maybe M2tsScte35Source,
    -- | Keep the default value (DEFAULT) unless you know that your audio EBP
    -- markers are incorrectly appearing before your video EBP markers. To
    -- correct this problem, set this value to Force (FORCE).
    forceTsVideoEbpOrder :: Core.Maybe M2tsForceTsVideoEbpOrder,
    -- | Specify the ID for the transport stream itself in the program map table
    -- for this output. Transport stream IDs and program map tables are parts
    -- of MPEG-2 transport stream containers, used for organizing data.
    transportStreamId :: Core.Maybe Core.Natural,
    -- | Specify the packet identifiers (PIDs) for DVB subtitle data included in
    -- this output. Specify multiple PIDs as a JSON array. Default is the range
    -- 460-479.
    dvbSubPids :: Core.Maybe [Core.Natural],
    -- | Specify the packet identifier (PID) of the SCTE-35 stream in the
    -- transport stream.
    scte35Pid :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'audioBufferModel', 'm2tsSettings_audioBufferModel' - Selects between the DVB and ATSC buffer models for Dolby Digital audio.
--
-- 'timedMetadataPid', 'm2tsSettings_timedMetadataPid' - Specify the packet identifier (PID) for timed metadata in this output.
-- Default is 502.
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
-- 'dvbNitSettings', 'm2tsSettings_dvbNitSettings' - Inserts DVB Network Information Table (NIT) at the specified table
-- repetition interval.
--
-- 'nullPacketBitrate', 'm2tsSettings_nullPacketBitrate' - Value in bits per second of extra null packets to insert into the
-- transport stream. This can be used if a downstream encryption system
-- requires periodic null packets.
--
-- 'pcrControl', 'm2tsSettings_pcrControl' - When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This is
-- effective only when the PCR PID is the same as the video or audio
-- elementary stream.
--
-- 'ebpAudioInterval', 'm2tsSettings_ebpAudioInterval' - When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. When set to VIDEO_INTERVAL, these additional markers will not
-- be inserted. Only applicable when EBP segmentation markers are is
-- selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- 'ebpPlacement', 'm2tsSettings_ebpPlacement' - Selects which PIDs to place EBP markers on. They can either be placed
-- only on the video PID, or on both the video PID and all audio PIDs. Only
-- applicable when EBP segmentation markers are is selected
-- (segmentationMarkers is EBP or EBP_LEGACY).
--
-- 'pmtInterval', 'm2tsSettings_pmtInterval' - Specify the number of milliseconds between instances of the program map
-- table (PMT) in the output transport stream.
--
-- 'audioPids', 'm2tsSettings_audioPids' - Specify the packet identifiers (PIDs) for any elementary audio streams
-- you include in this output. Specify multiple PIDs as a JSON array.
-- Default is the range 482-492.
--
-- 'patInterval', 'm2tsSettings_patInterval' - The number of milliseconds between instances of this table in the output
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
-- 'maxPcrInterval', 'm2tsSettings_maxPcrInterval' - Specify the maximum time, in milliseconds, between Program Clock
-- References (PCRs) inserted into the transport stream.
--
-- 'programNumber', 'm2tsSettings_programNumber' - Use Program number (programNumber) to specify the program number used in
-- the program map table (PMT) for this output. Default is 1. Program
-- numbers and program map tables are parts of MPEG-2 transport stream
-- containers, used for organizing data.
--
-- 'bufferModel', 'm2tsSettings_bufferModel' - Controls what buffer model to use for accurate interleaving. If set to
-- MULTIPLEX, use multiplex buffer model. If set to NONE, this can lead to
-- lower latency, but low-memory devices may not be able to play back the
-- stream without interruptions.
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
-- 'dvbTdtSettings', 'm2tsSettings_dvbTdtSettings' - Inserts DVB Time and Date Table (TDT) at the specified table repetition
-- interval.
--
-- 'dvbSdtSettings', 'm2tsSettings_dvbSdtSettings' - Inserts DVB Service Description Table (NIT) at the specified table
-- repetition interval.
--
-- 'segmentationTime', 'm2tsSettings_segmentationTime' - Specify the length, in seconds, of each segment. Required unless markers
-- is set to _none_.
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
-- 'nielsenId3', 'm2tsSettings_nielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
--
-- 'dvbTeletextPid', 'm2tsSettings_dvbTeletextPid' - Specify the packet identifier (PID) for DVB teletext data you include in
-- this output. Default is 499.
--
-- 'bitrate', 'm2tsSettings_bitrate' - Specify the output bitrate of the transport stream in bits per second.
-- Setting to 0 lets the muxer automatically determine the appropriate
-- bitrate. Other common values are 3750000, 7500000, and 15000000.
--
-- 'fragmentTime', 'm2tsSettings_fragmentTime' - The length, in seconds, of each fragment. Only used with EBP markers.
--
-- 'esRateInPes', 'm2tsSettings_esRateInPes' - Controls whether to include the ES Rate field in the PES header.
--
-- 'privateMetadataPid', 'm2tsSettings_privateMetadataPid' - Specify the packet identifier (PID) of the private metadata stream.
-- Default is 503.
--
-- 'scte35Esam', 'm2tsSettings_scte35Esam' - Include this in your job settings to put SCTE-35 markers in your HLS and
-- transport stream outputs at the insertion points that you specify in an
-- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- 'scte35Source', 'm2tsSettings_scte35Source' - For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE). Also provide the ESAM XML as a string in the setting Signal
-- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
-- the property scte35Esam).
--
-- 'forceTsVideoEbpOrder', 'm2tsSettings_forceTsVideoEbpOrder' - Keep the default value (DEFAULT) unless you know that your audio EBP
-- markers are incorrectly appearing before your video EBP markers. To
-- correct this problem, set this value to Force (FORCE).
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
newM2tsSettings ::
  M2tsSettings
newM2tsSettings =
  M2tsSettings'
    { segmentationMarkers = Core.Nothing,
      pmtPid = Core.Nothing,
      videoPid = Core.Nothing,
      audioBufferModel = Core.Nothing,
      timedMetadataPid = Core.Nothing,
      segmentationStyle = Core.Nothing,
      dvbNitSettings = Core.Nothing,
      nullPacketBitrate = Core.Nothing,
      pcrControl = Core.Nothing,
      ebpAudioInterval = Core.Nothing,
      ebpPlacement = Core.Nothing,
      pmtInterval = Core.Nothing,
      audioPids = Core.Nothing,
      patInterval = Core.Nothing,
      minEbpInterval = Core.Nothing,
      maxPcrInterval = Core.Nothing,
      programNumber = Core.Nothing,
      bufferModel = Core.Nothing,
      pcrPid = Core.Nothing,
      audioFramesPerPes = Core.Nothing,
      rateMode = Core.Nothing,
      dvbTdtSettings = Core.Nothing,
      dvbSdtSettings = Core.Nothing,
      segmentationTime = Core.Nothing,
      audioDuration = Core.Nothing,
      nielsenId3 = Core.Nothing,
      dvbTeletextPid = Core.Nothing,
      bitrate = Core.Nothing,
      fragmentTime = Core.Nothing,
      esRateInPes = Core.Nothing,
      privateMetadataPid = Core.Nothing,
      scte35Esam = Core.Nothing,
      scte35Source = Core.Nothing,
      forceTsVideoEbpOrder = Core.Nothing,
      transportStreamId = Core.Nothing,
      dvbSubPids = Core.Nothing,
      scte35Pid = Core.Nothing
    }

-- | Inserts segmentation markers at each segmentation_time period.
-- rai_segstart sets the Random Access Indicator bit in the adaptation
-- field. rai_adapt sets the RAI bit and adds the current timecode in the
-- private data bytes. psi_segstart inserts PAT and PMT tables at the start
-- of segments. ebp adds Encoder Boundary Point information to the
-- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
-- ebp_legacy adds Encoder Boundary Point information to the adaptation
-- field using a legacy proprietary format.
m2tsSettings_segmentationMarkers :: Lens.Lens' M2tsSettings (Core.Maybe M2tsSegmentationMarkers)
m2tsSettings_segmentationMarkers = Lens.lens (\M2tsSettings' {segmentationMarkers} -> segmentationMarkers) (\s@M2tsSettings' {} a -> s {segmentationMarkers = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) for the program map table (PMT)
-- itself. Default is 480.
m2tsSettings_pmtPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_pmtPid = Lens.lens (\M2tsSettings' {pmtPid} -> pmtPid) (\s@M2tsSettings' {} a -> s {pmtPid = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) of the elementary video stream in
-- the transport stream.
m2tsSettings_videoPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_videoPid = Lens.lens (\M2tsSettings' {videoPid} -> videoPid) (\s@M2tsSettings' {} a -> s {videoPid = a} :: M2tsSettings)

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
m2tsSettings_audioBufferModel :: Lens.Lens' M2tsSettings (Core.Maybe M2tsAudioBufferModel)
m2tsSettings_audioBufferModel = Lens.lens (\M2tsSettings' {audioBufferModel} -> audioBufferModel) (\s@M2tsSettings' {} a -> s {audioBufferModel = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) for timed metadata in this output.
-- Default is 502.
m2tsSettings_timedMetadataPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_timedMetadataPid = Lens.lens (\M2tsSettings' {timedMetadataPid} -> timedMetadataPid) (\s@M2tsSettings' {} a -> s {timedMetadataPid = a} :: M2tsSettings)

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
m2tsSettings_segmentationStyle :: Lens.Lens' M2tsSettings (Core.Maybe M2tsSegmentationStyle)
m2tsSettings_segmentationStyle = Lens.lens (\M2tsSettings' {segmentationStyle} -> segmentationStyle) (\s@M2tsSettings' {} a -> s {segmentationStyle = a} :: M2tsSettings)

-- | Inserts DVB Network Information Table (NIT) at the specified table
-- repetition interval.
m2tsSettings_dvbNitSettings :: Lens.Lens' M2tsSettings (Core.Maybe DvbNitSettings)
m2tsSettings_dvbNitSettings = Lens.lens (\M2tsSettings' {dvbNitSettings} -> dvbNitSettings) (\s@M2tsSettings' {} a -> s {dvbNitSettings = a} :: M2tsSettings)

-- | Value in bits per second of extra null packets to insert into the
-- transport stream. This can be used if a downstream encryption system
-- requires periodic null packets.
m2tsSettings_nullPacketBitrate :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
m2tsSettings_nullPacketBitrate = Lens.lens (\M2tsSettings' {nullPacketBitrate} -> nullPacketBitrate) (\s@M2tsSettings' {} a -> s {nullPacketBitrate = a} :: M2tsSettings)

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This is
-- effective only when the PCR PID is the same as the video or audio
-- elementary stream.
m2tsSettings_pcrControl :: Lens.Lens' M2tsSettings (Core.Maybe M2tsPcrControl)
m2tsSettings_pcrControl = Lens.lens (\M2tsSettings' {pcrControl} -> pcrControl) (\s@M2tsSettings' {} a -> s {pcrControl = a} :: M2tsSettings)

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. When set to VIDEO_INTERVAL, these additional markers will not
-- be inserted. Only applicable when EBP segmentation markers are is
-- selected (segmentationMarkers is EBP or EBP_LEGACY).
m2tsSettings_ebpAudioInterval :: Lens.Lens' M2tsSettings (Core.Maybe M2tsEbpAudioInterval)
m2tsSettings_ebpAudioInterval = Lens.lens (\M2tsSettings' {ebpAudioInterval} -> ebpAudioInterval) (\s@M2tsSettings' {} a -> s {ebpAudioInterval = a} :: M2tsSettings)

-- | Selects which PIDs to place EBP markers on. They can either be placed
-- only on the video PID, or on both the video PID and all audio PIDs. Only
-- applicable when EBP segmentation markers are is selected
-- (segmentationMarkers is EBP or EBP_LEGACY).
m2tsSettings_ebpPlacement :: Lens.Lens' M2tsSettings (Core.Maybe M2tsEbpPlacement)
m2tsSettings_ebpPlacement = Lens.lens (\M2tsSettings' {ebpPlacement} -> ebpPlacement) (\s@M2tsSettings' {} a -> s {ebpPlacement = a} :: M2tsSettings)

-- | Specify the number of milliseconds between instances of the program map
-- table (PMT) in the output transport stream.
m2tsSettings_pmtInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_pmtInterval = Lens.lens (\M2tsSettings' {pmtInterval} -> pmtInterval) (\s@M2tsSettings' {} a -> s {pmtInterval = a} :: M2tsSettings)

-- | Specify the packet identifiers (PIDs) for any elementary audio streams
-- you include in this output. Specify multiple PIDs as a JSON array.
-- Default is the range 482-492.
m2tsSettings_audioPids :: Lens.Lens' M2tsSettings (Core.Maybe [Core.Natural])
m2tsSettings_audioPids = Lens.lens (\M2tsSettings' {audioPids} -> audioPids) (\s@M2tsSettings' {} a -> s {audioPids = a} :: M2tsSettings) Core.. Lens.mapping Lens._Coerce

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
m2tsSettings_patInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_patInterval = Lens.lens (\M2tsSettings' {patInterval} -> patInterval) (\s@M2tsSettings' {} a -> s {patInterval = a} :: M2tsSettings)

-- | When set, enforces that Encoder Boundary Points do not come within the
-- specified time interval of each other by looking ahead at input video.
-- If another EBP is going to come in within the specified time interval,
-- the current EBP is not emitted, and the segment is \"stretched\" to the
-- next marker. The lookahead value does not add latency to the system. The
-- Live Event must be configured elsewhere to create sufficient latency to
-- make the lookahead accurate.
m2tsSettings_minEbpInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_minEbpInterval = Lens.lens (\M2tsSettings' {minEbpInterval} -> minEbpInterval) (\s@M2tsSettings' {} a -> s {minEbpInterval = a} :: M2tsSettings)

-- | Specify the maximum time, in milliseconds, between Program Clock
-- References (PCRs) inserted into the transport stream.
m2tsSettings_maxPcrInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_maxPcrInterval = Lens.lens (\M2tsSettings' {maxPcrInterval} -> maxPcrInterval) (\s@M2tsSettings' {} a -> s {maxPcrInterval = a} :: M2tsSettings)

-- | Use Program number (programNumber) to specify the program number used in
-- the program map table (PMT) for this output. Default is 1. Program
-- numbers and program map tables are parts of MPEG-2 transport stream
-- containers, used for organizing data.
m2tsSettings_programNumber :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_programNumber = Lens.lens (\M2tsSettings' {programNumber} -> programNumber) (\s@M2tsSettings' {} a -> s {programNumber = a} :: M2tsSettings)

-- | Controls what buffer model to use for accurate interleaving. If set to
-- MULTIPLEX, use multiplex buffer model. If set to NONE, this can lead to
-- lower latency, but low-memory devices may not be able to play back the
-- stream without interruptions.
m2tsSettings_bufferModel :: Lens.Lens' M2tsSettings (Core.Maybe M2tsBufferModel)
m2tsSettings_bufferModel = Lens.lens (\M2tsSettings' {bufferModel} -> bufferModel) (\s@M2tsSettings' {} a -> s {bufferModel = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) for the program clock reference
-- (PCR) in this output. If you do not specify a value, the service will
-- use the value for Video PID (VideoPid).
m2tsSettings_pcrPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_pcrPid = Lens.lens (\M2tsSettings' {pcrPid} -> pcrPid) (\s@M2tsSettings' {} a -> s {pcrPid = a} :: M2tsSettings)

-- | The number of audio frames to insert for each PES packet.
m2tsSettings_audioFramesPerPes :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_audioFramesPerPes = Lens.lens (\M2tsSettings' {audioFramesPerPes} -> audioFramesPerPes) (\s@M2tsSettings' {} a -> s {audioFramesPerPes = a} :: M2tsSettings)

-- | When set to CBR, inserts null packets into transport stream to fill
-- specified bitrate. When set to VBR, the bitrate setting acts as the
-- maximum bitrate, but the output will not be padded up to that bitrate.
m2tsSettings_rateMode :: Lens.Lens' M2tsSettings (Core.Maybe M2tsRateMode)
m2tsSettings_rateMode = Lens.lens (\M2tsSettings' {rateMode} -> rateMode) (\s@M2tsSettings' {} a -> s {rateMode = a} :: M2tsSettings)

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition
-- interval.
m2tsSettings_dvbTdtSettings :: Lens.Lens' M2tsSettings (Core.Maybe DvbTdtSettings)
m2tsSettings_dvbTdtSettings = Lens.lens (\M2tsSettings' {dvbTdtSettings} -> dvbTdtSettings) (\s@M2tsSettings' {} a -> s {dvbTdtSettings = a} :: M2tsSettings)

-- | Inserts DVB Service Description Table (NIT) at the specified table
-- repetition interval.
m2tsSettings_dvbSdtSettings :: Lens.Lens' M2tsSettings (Core.Maybe DvbSdtSettings)
m2tsSettings_dvbSdtSettings = Lens.lens (\M2tsSettings' {dvbSdtSettings} -> dvbSdtSettings) (\s@M2tsSettings' {} a -> s {dvbSdtSettings = a} :: M2tsSettings)

-- | Specify the length, in seconds, of each segment. Required unless markers
-- is set to _none_.
m2tsSettings_segmentationTime :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
m2tsSettings_segmentationTime = Lens.lens (\M2tsSettings' {segmentationTime} -> segmentationTime) (\s@M2tsSettings' {} a -> s {segmentationTime = a} :: M2tsSettings)

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
m2tsSettings_audioDuration :: Lens.Lens' M2tsSettings (Core.Maybe M2tsAudioDuration)
m2tsSettings_audioDuration = Lens.lens (\M2tsSettings' {audioDuration} -> audioDuration) (\s@M2tsSettings' {} a -> s {audioDuration = a} :: M2tsSettings)

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
m2tsSettings_nielsenId3 :: Lens.Lens' M2tsSettings (Core.Maybe M2tsNielsenId3)
m2tsSettings_nielsenId3 = Lens.lens (\M2tsSettings' {nielsenId3} -> nielsenId3) (\s@M2tsSettings' {} a -> s {nielsenId3 = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) for DVB teletext data you include in
-- this output. Default is 499.
m2tsSettings_dvbTeletextPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_dvbTeletextPid = Lens.lens (\M2tsSettings' {dvbTeletextPid} -> dvbTeletextPid) (\s@M2tsSettings' {} a -> s {dvbTeletextPid = a} :: M2tsSettings)

-- | Specify the output bitrate of the transport stream in bits per second.
-- Setting to 0 lets the muxer automatically determine the appropriate
-- bitrate. Other common values are 3750000, 7500000, and 15000000.
m2tsSettings_bitrate :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_bitrate = Lens.lens (\M2tsSettings' {bitrate} -> bitrate) (\s@M2tsSettings' {} a -> s {bitrate = a} :: M2tsSettings)

-- | The length, in seconds, of each fragment. Only used with EBP markers.
m2tsSettings_fragmentTime :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
m2tsSettings_fragmentTime = Lens.lens (\M2tsSettings' {fragmentTime} -> fragmentTime) (\s@M2tsSettings' {} a -> s {fragmentTime = a} :: M2tsSettings)

-- | Controls whether to include the ES Rate field in the PES header.
m2tsSettings_esRateInPes :: Lens.Lens' M2tsSettings (Core.Maybe M2tsEsRateInPes)
m2tsSettings_esRateInPes = Lens.lens (\M2tsSettings' {esRateInPes} -> esRateInPes) (\s@M2tsSettings' {} a -> s {esRateInPes = a} :: M2tsSettings)

-- | Specify the packet identifier (PID) of the private metadata stream.
-- Default is 503.
m2tsSettings_privateMetadataPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_privateMetadataPid = Lens.lens (\M2tsSettings' {privateMetadataPid} -> privateMetadataPid) (\s@M2tsSettings' {} a -> s {privateMetadataPid = a} :: M2tsSettings)

-- | Include this in your job settings to put SCTE-35 markers in your HLS and
-- transport stream outputs at the insertion points that you specify in an
-- ESAM XML document. Provide the document in the setting SCC XML (sccXml).
m2tsSettings_scte35Esam :: Lens.Lens' M2tsSettings (Core.Maybe M2tsScte35Esam)
m2tsSettings_scte35Esam = Lens.lens (\M2tsSettings' {scte35Esam} -> scte35Esam) (\s@M2tsSettings' {} a -> s {scte35Esam = a} :: M2tsSettings)

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE). Also provide the ESAM XML as a string in the setting Signal
-- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
-- the property scte35Esam).
m2tsSettings_scte35Source :: Lens.Lens' M2tsSettings (Core.Maybe M2tsScte35Source)
m2tsSettings_scte35Source = Lens.lens (\M2tsSettings' {scte35Source} -> scte35Source) (\s@M2tsSettings' {} a -> s {scte35Source = a} :: M2tsSettings)

-- | Keep the default value (DEFAULT) unless you know that your audio EBP
-- markers are incorrectly appearing before your video EBP markers. To
-- correct this problem, set this value to Force (FORCE).
m2tsSettings_forceTsVideoEbpOrder :: Lens.Lens' M2tsSettings (Core.Maybe M2tsForceTsVideoEbpOrder)
m2tsSettings_forceTsVideoEbpOrder = Lens.lens (\M2tsSettings' {forceTsVideoEbpOrder} -> forceTsVideoEbpOrder) (\s@M2tsSettings' {} a -> s {forceTsVideoEbpOrder = a} :: M2tsSettings)

-- | Specify the ID for the transport stream itself in the program map table
-- for this output. Transport stream IDs and program map tables are parts
-- of MPEG-2 transport stream containers, used for organizing data.
m2tsSettings_transportStreamId :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_transportStreamId = Lens.lens (\M2tsSettings' {transportStreamId} -> transportStreamId) (\s@M2tsSettings' {} a -> s {transportStreamId = a} :: M2tsSettings)

-- | Specify the packet identifiers (PIDs) for DVB subtitle data included in
-- this output. Specify multiple PIDs as a JSON array. Default is the range
-- 460-479.
m2tsSettings_dvbSubPids :: Lens.Lens' M2tsSettings (Core.Maybe [Core.Natural])
m2tsSettings_dvbSubPids = Lens.lens (\M2tsSettings' {dvbSubPids} -> dvbSubPids) (\s@M2tsSettings' {} a -> s {dvbSubPids = a} :: M2tsSettings) Core.. Lens.mapping Lens._Coerce

-- | Specify the packet identifier (PID) of the SCTE-35 stream in the
-- transport stream.
m2tsSettings_scte35Pid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
m2tsSettings_scte35Pid = Lens.lens (\M2tsSettings' {scte35Pid} -> scte35Pid) (\s@M2tsSettings' {} a -> s {scte35Pid = a} :: M2tsSettings)

instance Core.FromJSON M2tsSettings where
  parseJSON =
    Core.withObject
      "M2tsSettings"
      ( \x ->
          M2tsSettings'
            Core.<$> (x Core..:? "segmentationMarkers")
            Core.<*> (x Core..:? "pmtPid")
            Core.<*> (x Core..:? "videoPid")
            Core.<*> (x Core..:? "audioBufferModel")
            Core.<*> (x Core..:? "timedMetadataPid")
            Core.<*> (x Core..:? "segmentationStyle")
            Core.<*> (x Core..:? "dvbNitSettings")
            Core.<*> (x Core..:? "nullPacketBitrate")
            Core.<*> (x Core..:? "pcrControl")
            Core.<*> (x Core..:? "ebpAudioInterval")
            Core.<*> (x Core..:? "ebpPlacement")
            Core.<*> (x Core..:? "pmtInterval")
            Core.<*> (x Core..:? "audioPids" Core..!= Core.mempty)
            Core.<*> (x Core..:? "patInterval")
            Core.<*> (x Core..:? "minEbpInterval")
            Core.<*> (x Core..:? "maxPcrInterval")
            Core.<*> (x Core..:? "programNumber")
            Core.<*> (x Core..:? "bufferModel")
            Core.<*> (x Core..:? "pcrPid")
            Core.<*> (x Core..:? "audioFramesPerPes")
            Core.<*> (x Core..:? "rateMode")
            Core.<*> (x Core..:? "dvbTdtSettings")
            Core.<*> (x Core..:? "dvbSdtSettings")
            Core.<*> (x Core..:? "segmentationTime")
            Core.<*> (x Core..:? "audioDuration")
            Core.<*> (x Core..:? "nielsenId3")
            Core.<*> (x Core..:? "dvbTeletextPid")
            Core.<*> (x Core..:? "bitrate")
            Core.<*> (x Core..:? "fragmentTime")
            Core.<*> (x Core..:? "esRateInPes")
            Core.<*> (x Core..:? "privateMetadataPid")
            Core.<*> (x Core..:? "scte35Esam")
            Core.<*> (x Core..:? "scte35Source")
            Core.<*> (x Core..:? "forceTsVideoEbpOrder")
            Core.<*> (x Core..:? "transportStreamId")
            Core.<*> (x Core..:? "dvbSubPids" Core..!= Core.mempty)
            Core.<*> (x Core..:? "scte35Pid")
      )

instance Core.Hashable M2tsSettings

instance Core.NFData M2tsSettings

instance Core.ToJSON M2tsSettings where
  toJSON M2tsSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("segmentationMarkers" Core..=)
              Core.<$> segmentationMarkers,
            ("pmtPid" Core..=) Core.<$> pmtPid,
            ("videoPid" Core..=) Core.<$> videoPid,
            ("audioBufferModel" Core..=)
              Core.<$> audioBufferModel,
            ("timedMetadataPid" Core..=)
              Core.<$> timedMetadataPid,
            ("segmentationStyle" Core..=)
              Core.<$> segmentationStyle,
            ("dvbNitSettings" Core..=) Core.<$> dvbNitSettings,
            ("nullPacketBitrate" Core..=)
              Core.<$> nullPacketBitrate,
            ("pcrControl" Core..=) Core.<$> pcrControl,
            ("ebpAudioInterval" Core..=)
              Core.<$> ebpAudioInterval,
            ("ebpPlacement" Core..=) Core.<$> ebpPlacement,
            ("pmtInterval" Core..=) Core.<$> pmtInterval,
            ("audioPids" Core..=) Core.<$> audioPids,
            ("patInterval" Core..=) Core.<$> patInterval,
            ("minEbpInterval" Core..=) Core.<$> minEbpInterval,
            ("maxPcrInterval" Core..=) Core.<$> maxPcrInterval,
            ("programNumber" Core..=) Core.<$> programNumber,
            ("bufferModel" Core..=) Core.<$> bufferModel,
            ("pcrPid" Core..=) Core.<$> pcrPid,
            ("audioFramesPerPes" Core..=)
              Core.<$> audioFramesPerPes,
            ("rateMode" Core..=) Core.<$> rateMode,
            ("dvbTdtSettings" Core..=) Core.<$> dvbTdtSettings,
            ("dvbSdtSettings" Core..=) Core.<$> dvbSdtSettings,
            ("segmentationTime" Core..=)
              Core.<$> segmentationTime,
            ("audioDuration" Core..=) Core.<$> audioDuration,
            ("nielsenId3" Core..=) Core.<$> nielsenId3,
            ("dvbTeletextPid" Core..=) Core.<$> dvbTeletextPid,
            ("bitrate" Core..=) Core.<$> bitrate,
            ("fragmentTime" Core..=) Core.<$> fragmentTime,
            ("esRateInPes" Core..=) Core.<$> esRateInPes,
            ("privateMetadataPid" Core..=)
              Core.<$> privateMetadataPid,
            ("scte35Esam" Core..=) Core.<$> scte35Esam,
            ("scte35Source" Core..=) Core.<$> scte35Source,
            ("forceTsVideoEbpOrder" Core..=)
              Core.<$> forceTsVideoEbpOrder,
            ("transportStreamId" Core..=)
              Core.<$> transportStreamId,
            ("dvbSubPids" Core..=) Core.<$> dvbSubPids,
            ("scte35Pid" Core..=) Core.<$> scte35Pid
          ]
      )
