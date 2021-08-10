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
-- Module      : Network.AWS.MediaLive.Types.M2tsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.DvbNitSettings
import Network.AWS.MediaLive.Types.DvbSdtSettings
import Network.AWS.MediaLive.Types.DvbTdtSettings
import Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior
import Network.AWS.MediaLive.Types.M2tsArib
import Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
import Network.AWS.MediaLive.Types.M2tsAudioBufferModel
import Network.AWS.MediaLive.Types.M2tsAudioInterval
import Network.AWS.MediaLive.Types.M2tsAudioStreamType
import Network.AWS.MediaLive.Types.M2tsBufferModel
import Network.AWS.MediaLive.Types.M2tsCcDescriptor
import Network.AWS.MediaLive.Types.M2tsEbifControl
import Network.AWS.MediaLive.Types.M2tsEbpPlacement
import Network.AWS.MediaLive.Types.M2tsEsRateInPes
import Network.AWS.MediaLive.Types.M2tsKlv
import Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
import Network.AWS.MediaLive.Types.M2tsPcrControl
import Network.AWS.MediaLive.Types.M2tsRateMode
import Network.AWS.MediaLive.Types.M2tsScte35Control
import Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
import Network.AWS.MediaLive.Types.M2tsSegmentationStyle
import Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
import qualified Network.AWS.Prelude as Prelude

-- | M2ts Settings
--
-- /See:/ 'newM2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'
  { -- | Inserts segmentation markers at each segmentationTime period.
    -- raiSegstart sets the Random Access Indicator bit in the adaptation
    -- field. raiAdapt sets the RAI bit and adds the current timecode in the
    -- private data bytes. psiSegstart inserts PAT and PMT tables at the start
    -- of segments. ebp adds Encoder Boundary Point information to the
    -- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
    -- ebpLegacy adds Encoder Boundary Point information to the adaptation
    -- field using a legacy proprietary format.
    segmentationMarkers :: Prelude.Maybe M2tsSegmentationMarkers,
    -- | Packet Identifier (PID) for input source KLV data to this output.
    -- Multiple values are accepted, and can be entered in ranges and\/or by
    -- comma separation. Can be entered as decimal or hexadecimal values. Each
    -- PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
    klvDataPids :: Prelude.Maybe Prelude.Text,
    -- | Packet Identifier (PID) for input source ETV Signal data to this output.
    -- Can be entered as a decimal or hexadecimal value. Valid values are 32
    -- (or 0x20)..8182 (or 0x1ff6).
    etvSignalPid :: Prelude.Maybe Prelude.Text,
    -- | Maximum time in milliseconds between Program Clock Reference (PCRs)
    -- inserted into the transport stream.
    pcrPeriod :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
    -- stream. Can be entered as a decimal or hexadecimal value. Valid values
    -- are 32 (or 0x20)..8182 (or 0x1ff6).
    pmtPid :: Prelude.Maybe Prelude.Text,
    -- | Packet Identifier (PID) of the elementary video stream in the transport
    -- stream. Can be entered as a decimal or hexadecimal value. Valid values
    -- are 32 (or 0x20)..8182 (or 0x1ff6).
    videoPid :: Prelude.Maybe Prelude.Text,
    -- | If set to passthrough, Nielsen inaudible tones for media tracking will
    -- be detected in the input audio and an equivalent ID3 tag will be
    -- inserted in the output.
    nielsenId3Behavior :: Prelude.Maybe M2tsNielsenId3Behavior,
    -- | When set to dvb, uses DVB buffer model for Dolby Digital audio. When set
    -- to atsc, the ATSC model is used.
    audioBufferModel :: Prelude.Maybe M2tsAudioBufferModel,
    -- | Packet Identifier (PID) of the timed metadata stream in the transport
    -- stream. Can be entered as a decimal or hexadecimal value. Valid values
    -- are 32 (or 0x20)..8182 (or 0x1ff6).
    timedMetadataPid :: Prelude.Maybe Prelude.Text,
    -- | The segmentation style parameter controls how segmentation markers are
    -- inserted into the transport stream. With avails, it is possible that
    -- segments may be truncated, which can influence where future segmentation
    -- markers are inserted. When a segmentation style of \"resetCadence\" is
    -- selected and a segment is truncated due to an avail, we will reset the
    -- segmentation cadence. This means the subsequent segment will have a
    -- duration of $segmentationTime seconds. When a segmentation style of
    -- \"maintainCadence\" is selected and a segment is truncated due to an
    -- avail, we will not reset the segmentation cadence. This means the
    -- subsequent segment will likely be truncated as well. However, all
    -- segments after that will have a duration of $segmentationTime seconds.
    -- Note that EBP lookahead is a slight exception to this rule.
    segmentationStyle :: Prelude.Maybe M2tsSegmentationStyle,
    -- | Inserts DVB Network Information Table (NIT) at the specified table
    -- repetition interval.
    dvbNitSettings :: Prelude.Maybe DvbNitSettings,
    -- | Value in bits per second of extra null packets to insert into the
    -- transport stream. This can be used if a downstream encryption system
    -- requires periodic null packets.
    nullPacketBitrate :: Prelude.Maybe Prelude.Double,
    -- | When set to pcrEveryPesPacket, a Program Clock Reference value is
    -- inserted for every Packetized Elementary Stream (PES) header. This
    -- parameter is effective only when the PCR PID is the same as the video or
    -- audio elementary stream.
    pcrControl :: Prelude.Maybe M2tsPcrControl,
    -- | When videoAndFixedIntervals is selected, audio EBP markers will be added
    -- to partitions 3 and 4. The interval between these additional markers
    -- will be fixed, and will be slightly shorter than the video EBP marker
    -- interval. Only available when EBP Cablelabs segmentation markers are
    -- selected. Partitions 1 and 2 will always follow the video interval.
    ebpAudioInterval :: Prelude.Maybe M2tsAudioInterval,
    -- | Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids,
    -- EBP markers will be placed on the video PID and all audio PIDs. If set
    -- to videoPid, EBP markers will be placed on only the video PID.
    ebpPlacement :: Prelude.Maybe M2tsEbpPlacement,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream. Valid values are 0, 10..1000.
    pmtInterval :: Prelude.Maybe Prelude.Natural,
    -- | When set to enabled, generates captionServiceDescriptor in PMT.
    ccDescriptor :: Prelude.Maybe M2tsCcDescriptor,
    -- | Optionally pass SCTE-35 signals from the input source to this output.
    scte35Control :: Prelude.Maybe M2tsScte35Control,
    -- | Packet Identifier (PID) for ARIB Captions in the transport stream. Can
    -- be entered as a decimal or hexadecimal value. Valid values are 32 (or
    -- 0x20)..8182 (or 0x1ff6).
    aribCaptionsPid :: Prelude.Maybe Prelude.Text,
    -- | Packet Identifier (PID) of the elementary audio stream(s) in the
    -- transport stream. Multiple values are accepted, and can be entered in
    -- ranges and\/or by comma separation. Can be entered as decimal or
    -- hexadecimal values. Each PID specified must be in the range of 32 (or
    -- 0x20)..8182 (or 0x1ff6).
    audioPids :: Prelude.Maybe Prelude.Text,
    -- | Packet Identifier (PID) for input source ETV Platform data to this
    -- output. Can be entered as a decimal or hexadecimal value. Valid values
    -- are 32 (or 0x20)..8182 (or 0x1ff6).
    etvPlatformPid :: Prelude.Maybe Prelude.Text,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream. Valid values are 0, 10..1000.
    patInterval :: Prelude.Maybe Prelude.Natural,
    -- | The value of the program number field in the Program Map Table.
    programNum :: Prelude.Maybe Prelude.Natural,
    -- | When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87
    -- for EAC3. When set to dvb, uses stream type = 0x06.
    audioStreamType :: Prelude.Maybe M2tsAudioStreamType,
    -- | When set, enforces that Encoder Boundary Points do not come within the
    -- specified time interval of each other by looking ahead at input video.
    -- If another EBP is going to come in within the specified time interval,
    -- the current EBP is not emitted, and the segment is \"stretched\" to the
    -- next marker. The lookahead value does not add latency to the system. The
    -- Live Event must be configured elsewhere to create sufficient latency to
    -- make the lookahead accurate.
    ebpLookaheadMs :: Prelude.Maybe Prelude.Natural,
    -- | Controls the timing accuracy for output network traffic. Leave as
    -- MULTIPLEX to ensure accurate network packet timing. Or set to NONE,
    -- which might result in lower latency but will result in more variability
    -- in output network packet timing. This variability might cause
    -- interruptions, jitter, or bursty behavior in your playback or receiving
    -- devices.
    bufferModel :: Prelude.Maybe M2tsBufferModel,
    -- | If set to auto, pid number used for ARIB Captions will be auto-selected
    -- from unused pids. If set to useConfigured, ARIB Captions will be on the
    -- configured pid number.
    aribCaptionsPidControl :: Prelude.Maybe M2tsAribCaptionsPidControl,
    -- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
    -- transport stream. When no value is given, the encoder will assign the
    -- same value as the Video PID. Can be entered as a decimal or hexadecimal
    -- value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
    pcrPid :: Prelude.Maybe Prelude.Text,
    -- | If set to passthrough, passes any KLV data from the input source to this
    -- output.
    klv :: Prelude.Maybe M2tsKlv,
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Prelude.Maybe Prelude.Natural,
    -- | When vbr, does not insert null packets into transport stream to fill
    -- specified bitrate. The bitrate setting acts as the maximum bitrate when
    -- vbr is set.
    rateMode :: Prelude.Maybe M2tsRateMode,
    -- | Inserts DVB Time and Date Table (TDT) at the specified table repetition
    -- interval.
    dvbTdtSettings :: Prelude.Maybe DvbTdtSettings,
    -- | This field is unused and deprecated.
    ecmPid :: Prelude.Maybe Prelude.Text,
    -- | Inserts DVB Service Description Table (SDT) at the specified table
    -- repetition interval.
    dvbSdtSettings :: Prelude.Maybe DvbSdtSettings,
    -- | When set to drop, output audio streams will be removed from the program
    -- if the selected input audio stream is removed from the input. This
    -- allows the output audio configuration to dynamically change based on
    -- input configuration. If this is set to encodeSilence, all output audio
    -- streams will output encoded silence when not connected to an active
    -- input stream.
    absentInputAudioBehavior :: Prelude.Maybe M2tsAbsentInputAudioBehavior,
    -- | The length in seconds of each segment. Required unless markers is set to
    -- _none_.
    segmentationTime :: Prelude.Maybe Prelude.Double,
    -- | If set to passthrough, passes any EBIF data from the input source to
    -- this output.
    ebif :: Prelude.Maybe M2tsEbifControl,
    -- | Packet Identifier (PID) for input source DVB Teletext data to this
    -- output. Can be entered as a decimal or hexadecimal value. Valid values
    -- are 32 (or 0x20)..8182 (or 0x1ff6).
    dvbTeletextPid :: Prelude.Maybe Prelude.Text,
    -- | When set to passthrough, timed metadata will be passed through from
    -- input to output.
    timedMetadataBehavior :: Prelude.Maybe M2tsTimedMetadataBehavior,
    -- | When set to enabled, uses ARIB-compliant field muxing and removes video
    -- descriptor.
    arib :: Prelude.Maybe M2tsArib,
    -- | The output bitrate of the transport stream in bits per second. Setting
    -- to 0 lets the muxer automatically determine the appropriate bitrate.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | The length in seconds of each fragment. Only used with EBP markers.
    fragmentTime :: Prelude.Maybe Prelude.Double,
    -- | Include or exclude the ES Rate field in the PES header.
    esRateInPes :: Prelude.Maybe M2tsEsRateInPes,
    -- | Packet Identifier (PID) for input source SCTE-27 data to this output.
    -- Multiple values are accepted, and can be entered in ranges and\/or by
    -- comma separation. Can be entered as decimal or hexadecimal values. Each
    -- PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
    scte27Pids :: Prelude.Maybe Prelude.Text,
    -- | The value of the transport stream ID field in the Program Map Table.
    transportStreamId :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) for input source DVB Subtitle data to this
    -- output. Multiple values are accepted, and can be entered in ranges
    -- and\/or by comma separation. Can be entered as decimal or hexadecimal
    -- values. Each PID specified must be in the range of 32 (or 0x20)..8182
    -- (or 0x1ff6).
    dvbSubPids :: Prelude.Maybe Prelude.Text,
    -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
    -- Can be entered as a decimal or hexadecimal value. Valid values are 32
    -- (or 0x20)..8182 (or 0x1ff6).
    scte35Pid :: Prelude.Maybe Prelude.Text
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
-- 'segmentationMarkers', 'm2tsSettings_segmentationMarkers' - Inserts segmentation markers at each segmentationTime period.
-- raiSegstart sets the Random Access Indicator bit in the adaptation
-- field. raiAdapt sets the RAI bit and adds the current timecode in the
-- private data bytes. psiSegstart inserts PAT and PMT tables at the start
-- of segments. ebp adds Encoder Boundary Point information to the
-- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
-- ebpLegacy adds Encoder Boundary Point information to the adaptation
-- field using a legacy proprietary format.
--
-- 'klvDataPids', 'm2tsSettings_klvDataPids' - Packet Identifier (PID) for input source KLV data to this output.
-- Multiple values are accepted, and can be entered in ranges and\/or by
-- comma separation. Can be entered as decimal or hexadecimal values. Each
-- PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'etvSignalPid', 'm2tsSettings_etvSignalPid' - Packet Identifier (PID) for input source ETV Signal data to this output.
-- Can be entered as a decimal or hexadecimal value. Valid values are 32
-- (or 0x20)..8182 (or 0x1ff6).
--
-- 'pcrPeriod', 'm2tsSettings_pcrPeriod' - Maximum time in milliseconds between Program Clock Reference (PCRs)
-- inserted into the transport stream.
--
-- 'pmtPid', 'm2tsSettings_pmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'videoPid', 'm2tsSettings_videoPid' - Packet Identifier (PID) of the elementary video stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'nielsenId3Behavior', 'm2tsSettings_nielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will
-- be detected in the input audio and an equivalent ID3 tag will be
-- inserted in the output.
--
-- 'audioBufferModel', 'm2tsSettings_audioBufferModel' - When set to dvb, uses DVB buffer model for Dolby Digital audio. When set
-- to atsc, the ATSC model is used.
--
-- 'timedMetadataPid', 'm2tsSettings_timedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'segmentationStyle', 'm2tsSettings_segmentationStyle' - The segmentation style parameter controls how segmentation markers are
-- inserted into the transport stream. With avails, it is possible that
-- segments may be truncated, which can influence where future segmentation
-- markers are inserted. When a segmentation style of \"resetCadence\" is
-- selected and a segment is truncated due to an avail, we will reset the
-- segmentation cadence. This means the subsequent segment will have a
-- duration of $segmentationTime seconds. When a segmentation style of
-- \"maintainCadence\" is selected and a segment is truncated due to an
-- avail, we will not reset the segmentation cadence. This means the
-- subsequent segment will likely be truncated as well. However, all
-- segments after that will have a duration of $segmentationTime seconds.
-- Note that EBP lookahead is a slight exception to this rule.
--
-- 'dvbNitSettings', 'm2tsSettings_dvbNitSettings' - Inserts DVB Network Information Table (NIT) at the specified table
-- repetition interval.
--
-- 'nullPacketBitrate', 'm2tsSettings_nullPacketBitrate' - Value in bits per second of extra null packets to insert into the
-- transport stream. This can be used if a downstream encryption system
-- requires periodic null packets.
--
-- 'pcrControl', 'm2tsSettings_pcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
--
-- 'ebpAudioInterval', 'm2tsSettings_ebpAudioInterval' - When videoAndFixedIntervals is selected, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. Only available when EBP Cablelabs segmentation markers are
-- selected. Partitions 1 and 2 will always follow the video interval.
--
-- 'ebpPlacement', 'm2tsSettings_ebpPlacement' - Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids,
-- EBP markers will be placed on the video PID and all audio PIDs. If set
-- to videoPid, EBP markers will be placed on only the video PID.
--
-- 'pmtInterval', 'm2tsSettings_pmtInterval' - The number of milliseconds between instances of this table in the output
-- transport stream. Valid values are 0, 10..1000.
--
-- 'ccDescriptor', 'm2tsSettings_ccDescriptor' - When set to enabled, generates captionServiceDescriptor in PMT.
--
-- 'scte35Control', 'm2tsSettings_scte35Control' - Optionally pass SCTE-35 signals from the input source to this output.
--
-- 'aribCaptionsPid', 'm2tsSettings_aribCaptionsPid' - Packet Identifier (PID) for ARIB Captions in the transport stream. Can
-- be entered as a decimal or hexadecimal value. Valid values are 32 (or
-- 0x20)..8182 (or 0x1ff6).
--
-- 'audioPids', 'm2tsSettings_audioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation. Can be entered as decimal or
-- hexadecimal values. Each PID specified must be in the range of 32 (or
-- 0x20)..8182 (or 0x1ff6).
--
-- 'etvPlatformPid', 'm2tsSettings_etvPlatformPid' - Packet Identifier (PID) for input source ETV Platform data to this
-- output. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'patInterval', 'm2tsSettings_patInterval' - The number of milliseconds between instances of this table in the output
-- transport stream. Valid values are 0, 10..1000.
--
-- 'programNum', 'm2tsSettings_programNum' - The value of the program number field in the Program Map Table.
--
-- 'audioStreamType', 'm2tsSettings_audioStreamType' - When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87
-- for EAC3. When set to dvb, uses stream type = 0x06.
--
-- 'ebpLookaheadMs', 'm2tsSettings_ebpLookaheadMs' - When set, enforces that Encoder Boundary Points do not come within the
-- specified time interval of each other by looking ahead at input video.
-- If another EBP is going to come in within the specified time interval,
-- the current EBP is not emitted, and the segment is \"stretched\" to the
-- next marker. The lookahead value does not add latency to the system. The
-- Live Event must be configured elsewhere to create sufficient latency to
-- make the lookahead accurate.
--
-- 'bufferModel', 'm2tsSettings_bufferModel' - Controls the timing accuracy for output network traffic. Leave as
-- MULTIPLEX to ensure accurate network packet timing. Or set to NONE,
-- which might result in lower latency but will result in more variability
-- in output network packet timing. This variability might cause
-- interruptions, jitter, or bursty behavior in your playback or receiving
-- devices.
--
-- 'aribCaptionsPidControl', 'm2tsSettings_aribCaptionsPidControl' - If set to auto, pid number used for ARIB Captions will be auto-selected
-- from unused pids. If set to useConfigured, ARIB Captions will be on the
-- configured pid number.
--
-- 'pcrPid', 'm2tsSettings_pcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID. Can be entered as a decimal or hexadecimal
-- value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'klv', 'm2tsSettings_klv' - If set to passthrough, passes any KLV data from the input source to this
-- output.
--
-- 'audioFramesPerPes', 'm2tsSettings_audioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- 'rateMode', 'm2tsSettings_rateMode' - When vbr, does not insert null packets into transport stream to fill
-- specified bitrate. The bitrate setting acts as the maximum bitrate when
-- vbr is set.
--
-- 'dvbTdtSettings', 'm2tsSettings_dvbTdtSettings' - Inserts DVB Time and Date Table (TDT) at the specified table repetition
-- interval.
--
-- 'ecmPid', 'm2tsSettings_ecmPid' - This field is unused and deprecated.
--
-- 'dvbSdtSettings', 'm2tsSettings_dvbSdtSettings' - Inserts DVB Service Description Table (SDT) at the specified table
-- repetition interval.
--
-- 'absentInputAudioBehavior', 'm2tsSettings_absentInputAudioBehavior' - When set to drop, output audio streams will be removed from the program
-- if the selected input audio stream is removed from the input. This
-- allows the output audio configuration to dynamically change based on
-- input configuration. If this is set to encodeSilence, all output audio
-- streams will output encoded silence when not connected to an active
-- input stream.
--
-- 'segmentationTime', 'm2tsSettings_segmentationTime' - The length in seconds of each segment. Required unless markers is set to
-- _none_.
--
-- 'ebif', 'm2tsSettings_ebif' - If set to passthrough, passes any EBIF data from the input source to
-- this output.
--
-- 'dvbTeletextPid', 'm2tsSettings_dvbTeletextPid' - Packet Identifier (PID) for input source DVB Teletext data to this
-- output. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'timedMetadataBehavior', 'm2tsSettings_timedMetadataBehavior' - When set to passthrough, timed metadata will be passed through from
-- input to output.
--
-- 'arib', 'm2tsSettings_arib' - When set to enabled, uses ARIB-compliant field muxing and removes video
-- descriptor.
--
-- 'bitrate', 'm2tsSettings_bitrate' - The output bitrate of the transport stream in bits per second. Setting
-- to 0 lets the muxer automatically determine the appropriate bitrate.
--
-- 'fragmentTime', 'm2tsSettings_fragmentTime' - The length in seconds of each fragment. Only used with EBP markers.
--
-- 'esRateInPes', 'm2tsSettings_esRateInPes' - Include or exclude the ES Rate field in the PES header.
--
-- 'scte27Pids', 'm2tsSettings_scte27Pids' - Packet Identifier (PID) for input source SCTE-27 data to this output.
-- Multiple values are accepted, and can be entered in ranges and\/or by
-- comma separation. Can be entered as decimal or hexadecimal values. Each
-- PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'transportStreamId', 'm2tsSettings_transportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- 'dvbSubPids', 'm2tsSettings_dvbSubPids' - Packet Identifier (PID) for input source DVB Subtitle data to this
-- output. Multiple values are accepted, and can be entered in ranges
-- and\/or by comma separation. Can be entered as decimal or hexadecimal
-- values. Each PID specified must be in the range of 32 (or 0x20)..8182
-- (or 0x1ff6).
--
-- 'scte35Pid', 'm2tsSettings_scte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
-- Can be entered as a decimal or hexadecimal value. Valid values are 32
-- (or 0x20)..8182 (or 0x1ff6).
newM2tsSettings ::
  M2tsSettings
newM2tsSettings =
  M2tsSettings'
    { segmentationMarkers =
        Prelude.Nothing,
      klvDataPids = Prelude.Nothing,
      etvSignalPid = Prelude.Nothing,
      pcrPeriod = Prelude.Nothing,
      pmtPid = Prelude.Nothing,
      videoPid = Prelude.Nothing,
      nielsenId3Behavior = Prelude.Nothing,
      audioBufferModel = Prelude.Nothing,
      timedMetadataPid = Prelude.Nothing,
      segmentationStyle = Prelude.Nothing,
      dvbNitSettings = Prelude.Nothing,
      nullPacketBitrate = Prelude.Nothing,
      pcrControl = Prelude.Nothing,
      ebpAudioInterval = Prelude.Nothing,
      ebpPlacement = Prelude.Nothing,
      pmtInterval = Prelude.Nothing,
      ccDescriptor = Prelude.Nothing,
      scte35Control = Prelude.Nothing,
      aribCaptionsPid = Prelude.Nothing,
      audioPids = Prelude.Nothing,
      etvPlatformPid = Prelude.Nothing,
      patInterval = Prelude.Nothing,
      programNum = Prelude.Nothing,
      audioStreamType = Prelude.Nothing,
      ebpLookaheadMs = Prelude.Nothing,
      bufferModel = Prelude.Nothing,
      aribCaptionsPidControl = Prelude.Nothing,
      pcrPid = Prelude.Nothing,
      klv = Prelude.Nothing,
      audioFramesPerPes = Prelude.Nothing,
      rateMode = Prelude.Nothing,
      dvbTdtSettings = Prelude.Nothing,
      ecmPid = Prelude.Nothing,
      dvbSdtSettings = Prelude.Nothing,
      absentInputAudioBehavior = Prelude.Nothing,
      segmentationTime = Prelude.Nothing,
      ebif = Prelude.Nothing,
      dvbTeletextPid = Prelude.Nothing,
      timedMetadataBehavior = Prelude.Nothing,
      arib = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      fragmentTime = Prelude.Nothing,
      esRateInPes = Prelude.Nothing,
      scte27Pids = Prelude.Nothing,
      transportStreamId = Prelude.Nothing,
      dvbSubPids = Prelude.Nothing,
      scte35Pid = Prelude.Nothing
    }

-- | Inserts segmentation markers at each segmentationTime period.
-- raiSegstart sets the Random Access Indicator bit in the adaptation
-- field. raiAdapt sets the RAI bit and adds the current timecode in the
-- private data bytes. psiSegstart inserts PAT and PMT tables at the start
-- of segments. ebp adds Encoder Boundary Point information to the
-- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
-- ebpLegacy adds Encoder Boundary Point information to the adaptation
-- field using a legacy proprietary format.
m2tsSettings_segmentationMarkers :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsSegmentationMarkers)
m2tsSettings_segmentationMarkers = Lens.lens (\M2tsSettings' {segmentationMarkers} -> segmentationMarkers) (\s@M2tsSettings' {} a -> s {segmentationMarkers = a} :: M2tsSettings)

-- | Packet Identifier (PID) for input source KLV data to this output.
-- Multiple values are accepted, and can be entered in ranges and\/or by
-- comma separation. Can be entered as decimal or hexadecimal values. Each
-- PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_klvDataPids :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_klvDataPids = Lens.lens (\M2tsSettings' {klvDataPids} -> klvDataPids) (\s@M2tsSettings' {} a -> s {klvDataPids = a} :: M2tsSettings)

-- | Packet Identifier (PID) for input source ETV Signal data to this output.
-- Can be entered as a decimal or hexadecimal value. Valid values are 32
-- (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_etvSignalPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_etvSignalPid = Lens.lens (\M2tsSettings' {etvSignalPid} -> etvSignalPid) (\s@M2tsSettings' {} a -> s {etvSignalPid = a} :: M2tsSettings)

-- | Maximum time in milliseconds between Program Clock Reference (PCRs)
-- inserted into the transport stream.
m2tsSettings_pcrPeriod :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_pcrPeriod = Lens.lens (\M2tsSettings' {pcrPeriod} -> pcrPeriod) (\s@M2tsSettings' {} a -> s {pcrPeriod = a} :: M2tsSettings)

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_pmtPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_pmtPid = Lens.lens (\M2tsSettings' {pmtPid} -> pmtPid) (\s@M2tsSettings' {} a -> s {pmtPid = a} :: M2tsSettings)

-- | Packet Identifier (PID) of the elementary video stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_videoPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_videoPid = Lens.lens (\M2tsSettings' {videoPid} -> videoPid) (\s@M2tsSettings' {} a -> s {videoPid = a} :: M2tsSettings)

-- | If set to passthrough, Nielsen inaudible tones for media tracking will
-- be detected in the input audio and an equivalent ID3 tag will be
-- inserted in the output.
m2tsSettings_nielsenId3Behavior :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsNielsenId3Behavior)
m2tsSettings_nielsenId3Behavior = Lens.lens (\M2tsSettings' {nielsenId3Behavior} -> nielsenId3Behavior) (\s@M2tsSettings' {} a -> s {nielsenId3Behavior = a} :: M2tsSettings)

-- | When set to dvb, uses DVB buffer model for Dolby Digital audio. When set
-- to atsc, the ATSC model is used.
m2tsSettings_audioBufferModel :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsAudioBufferModel)
m2tsSettings_audioBufferModel = Lens.lens (\M2tsSettings' {audioBufferModel} -> audioBufferModel) (\s@M2tsSettings' {} a -> s {audioBufferModel = a} :: M2tsSettings)

-- | Packet Identifier (PID) of the timed metadata stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_timedMetadataPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_timedMetadataPid = Lens.lens (\M2tsSettings' {timedMetadataPid} -> timedMetadataPid) (\s@M2tsSettings' {} a -> s {timedMetadataPid = a} :: M2tsSettings)

-- | The segmentation style parameter controls how segmentation markers are
-- inserted into the transport stream. With avails, it is possible that
-- segments may be truncated, which can influence where future segmentation
-- markers are inserted. When a segmentation style of \"resetCadence\" is
-- selected and a segment is truncated due to an avail, we will reset the
-- segmentation cadence. This means the subsequent segment will have a
-- duration of $segmentationTime seconds. When a segmentation style of
-- \"maintainCadence\" is selected and a segment is truncated due to an
-- avail, we will not reset the segmentation cadence. This means the
-- subsequent segment will likely be truncated as well. However, all
-- segments after that will have a duration of $segmentationTime seconds.
-- Note that EBP lookahead is a slight exception to this rule.
m2tsSettings_segmentationStyle :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsSegmentationStyle)
m2tsSettings_segmentationStyle = Lens.lens (\M2tsSettings' {segmentationStyle} -> segmentationStyle) (\s@M2tsSettings' {} a -> s {segmentationStyle = a} :: M2tsSettings)

-- | Inserts DVB Network Information Table (NIT) at the specified table
-- repetition interval.
m2tsSettings_dvbNitSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbNitSettings)
m2tsSettings_dvbNitSettings = Lens.lens (\M2tsSettings' {dvbNitSettings} -> dvbNitSettings) (\s@M2tsSettings' {} a -> s {dvbNitSettings = a} :: M2tsSettings)

-- | Value in bits per second of extra null packets to insert into the
-- transport stream. This can be used if a downstream encryption system
-- requires periodic null packets.
m2tsSettings_nullPacketBitrate :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_nullPacketBitrate = Lens.lens (\M2tsSettings' {nullPacketBitrate} -> nullPacketBitrate) (\s@M2tsSettings' {} a -> s {nullPacketBitrate = a} :: M2tsSettings)

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
m2tsSettings_pcrControl :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsPcrControl)
m2tsSettings_pcrControl = Lens.lens (\M2tsSettings' {pcrControl} -> pcrControl) (\s@M2tsSettings' {} a -> s {pcrControl = a} :: M2tsSettings)

-- | When videoAndFixedIntervals is selected, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. Only available when EBP Cablelabs segmentation markers are
-- selected. Partitions 1 and 2 will always follow the video interval.
m2tsSettings_ebpAudioInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsAudioInterval)
m2tsSettings_ebpAudioInterval = Lens.lens (\M2tsSettings' {ebpAudioInterval} -> ebpAudioInterval) (\s@M2tsSettings' {} a -> s {ebpAudioInterval = a} :: M2tsSettings)

-- | Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids,
-- EBP markers will be placed on the video PID and all audio PIDs. If set
-- to videoPid, EBP markers will be placed on only the video PID.
m2tsSettings_ebpPlacement :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsEbpPlacement)
m2tsSettings_ebpPlacement = Lens.lens (\M2tsSettings' {ebpPlacement} -> ebpPlacement) (\s@M2tsSettings' {} a -> s {ebpPlacement = a} :: M2tsSettings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream. Valid values are 0, 10..1000.
m2tsSettings_pmtInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_pmtInterval = Lens.lens (\M2tsSettings' {pmtInterval} -> pmtInterval) (\s@M2tsSettings' {} a -> s {pmtInterval = a} :: M2tsSettings)

-- | When set to enabled, generates captionServiceDescriptor in PMT.
m2tsSettings_ccDescriptor :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsCcDescriptor)
m2tsSettings_ccDescriptor = Lens.lens (\M2tsSettings' {ccDescriptor} -> ccDescriptor) (\s@M2tsSettings' {} a -> s {ccDescriptor = a} :: M2tsSettings)

-- | Optionally pass SCTE-35 signals from the input source to this output.
m2tsSettings_scte35Control :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsScte35Control)
m2tsSettings_scte35Control = Lens.lens (\M2tsSettings' {scte35Control} -> scte35Control) (\s@M2tsSettings' {} a -> s {scte35Control = a} :: M2tsSettings)

-- | Packet Identifier (PID) for ARIB Captions in the transport stream. Can
-- be entered as a decimal or hexadecimal value. Valid values are 32 (or
-- 0x20)..8182 (or 0x1ff6).
m2tsSettings_aribCaptionsPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_aribCaptionsPid = Lens.lens (\M2tsSettings' {aribCaptionsPid} -> aribCaptionsPid) (\s@M2tsSettings' {} a -> s {aribCaptionsPid = a} :: M2tsSettings)

-- | Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation. Can be entered as decimal or
-- hexadecimal values. Each PID specified must be in the range of 32 (or
-- 0x20)..8182 (or 0x1ff6).
m2tsSettings_audioPids :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_audioPids = Lens.lens (\M2tsSettings' {audioPids} -> audioPids) (\s@M2tsSettings' {} a -> s {audioPids = a} :: M2tsSettings)

-- | Packet Identifier (PID) for input source ETV Platform data to this
-- output. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_etvPlatformPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_etvPlatformPid = Lens.lens (\M2tsSettings' {etvPlatformPid} -> etvPlatformPid) (\s@M2tsSettings' {} a -> s {etvPlatformPid = a} :: M2tsSettings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream. Valid values are 0, 10..1000.
m2tsSettings_patInterval :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_patInterval = Lens.lens (\M2tsSettings' {patInterval} -> patInterval) (\s@M2tsSettings' {} a -> s {patInterval = a} :: M2tsSettings)

-- | The value of the program number field in the Program Map Table.
m2tsSettings_programNum :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_programNum = Lens.lens (\M2tsSettings' {programNum} -> programNum) (\s@M2tsSettings' {} a -> s {programNum = a} :: M2tsSettings)

-- | When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87
-- for EAC3. When set to dvb, uses stream type = 0x06.
m2tsSettings_audioStreamType :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsAudioStreamType)
m2tsSettings_audioStreamType = Lens.lens (\M2tsSettings' {audioStreamType} -> audioStreamType) (\s@M2tsSettings' {} a -> s {audioStreamType = a} :: M2tsSettings)

-- | When set, enforces that Encoder Boundary Points do not come within the
-- specified time interval of each other by looking ahead at input video.
-- If another EBP is going to come in within the specified time interval,
-- the current EBP is not emitted, and the segment is \"stretched\" to the
-- next marker. The lookahead value does not add latency to the system. The
-- Live Event must be configured elsewhere to create sufficient latency to
-- make the lookahead accurate.
m2tsSettings_ebpLookaheadMs :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_ebpLookaheadMs = Lens.lens (\M2tsSettings' {ebpLookaheadMs} -> ebpLookaheadMs) (\s@M2tsSettings' {} a -> s {ebpLookaheadMs = a} :: M2tsSettings)

-- | Controls the timing accuracy for output network traffic. Leave as
-- MULTIPLEX to ensure accurate network packet timing. Or set to NONE,
-- which might result in lower latency but will result in more variability
-- in output network packet timing. This variability might cause
-- interruptions, jitter, or bursty behavior in your playback or receiving
-- devices.
m2tsSettings_bufferModel :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsBufferModel)
m2tsSettings_bufferModel = Lens.lens (\M2tsSettings' {bufferModel} -> bufferModel) (\s@M2tsSettings' {} a -> s {bufferModel = a} :: M2tsSettings)

-- | If set to auto, pid number used for ARIB Captions will be auto-selected
-- from unused pids. If set to useConfigured, ARIB Captions will be on the
-- configured pid number.
m2tsSettings_aribCaptionsPidControl :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsAribCaptionsPidControl)
m2tsSettings_aribCaptionsPidControl = Lens.lens (\M2tsSettings' {aribCaptionsPidControl} -> aribCaptionsPidControl) (\s@M2tsSettings' {} a -> s {aribCaptionsPidControl = a} :: M2tsSettings)

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID. Can be entered as a decimal or hexadecimal
-- value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_pcrPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_pcrPid = Lens.lens (\M2tsSettings' {pcrPid} -> pcrPid) (\s@M2tsSettings' {} a -> s {pcrPid = a} :: M2tsSettings)

-- | If set to passthrough, passes any KLV data from the input source to this
-- output.
m2tsSettings_klv :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsKlv)
m2tsSettings_klv = Lens.lens (\M2tsSettings' {klv} -> klv) (\s@M2tsSettings' {} a -> s {klv = a} :: M2tsSettings)

-- | The number of audio frames to insert for each PES packet.
m2tsSettings_audioFramesPerPes :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_audioFramesPerPes = Lens.lens (\M2tsSettings' {audioFramesPerPes} -> audioFramesPerPes) (\s@M2tsSettings' {} a -> s {audioFramesPerPes = a} :: M2tsSettings)

-- | When vbr, does not insert null packets into transport stream to fill
-- specified bitrate. The bitrate setting acts as the maximum bitrate when
-- vbr is set.
m2tsSettings_rateMode :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsRateMode)
m2tsSettings_rateMode = Lens.lens (\M2tsSettings' {rateMode} -> rateMode) (\s@M2tsSettings' {} a -> s {rateMode = a} :: M2tsSettings)

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition
-- interval.
m2tsSettings_dvbTdtSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbTdtSettings)
m2tsSettings_dvbTdtSettings = Lens.lens (\M2tsSettings' {dvbTdtSettings} -> dvbTdtSettings) (\s@M2tsSettings' {} a -> s {dvbTdtSettings = a} :: M2tsSettings)

-- | This field is unused and deprecated.
m2tsSettings_ecmPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_ecmPid = Lens.lens (\M2tsSettings' {ecmPid} -> ecmPid) (\s@M2tsSettings' {} a -> s {ecmPid = a} :: M2tsSettings)

-- | Inserts DVB Service Description Table (SDT) at the specified table
-- repetition interval.
m2tsSettings_dvbSdtSettings :: Lens.Lens' M2tsSettings (Prelude.Maybe DvbSdtSettings)
m2tsSettings_dvbSdtSettings = Lens.lens (\M2tsSettings' {dvbSdtSettings} -> dvbSdtSettings) (\s@M2tsSettings' {} a -> s {dvbSdtSettings = a} :: M2tsSettings)

-- | When set to drop, output audio streams will be removed from the program
-- if the selected input audio stream is removed from the input. This
-- allows the output audio configuration to dynamically change based on
-- input configuration. If this is set to encodeSilence, all output audio
-- streams will output encoded silence when not connected to an active
-- input stream.
m2tsSettings_absentInputAudioBehavior :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsAbsentInputAudioBehavior)
m2tsSettings_absentInputAudioBehavior = Lens.lens (\M2tsSettings' {absentInputAudioBehavior} -> absentInputAudioBehavior) (\s@M2tsSettings' {} a -> s {absentInputAudioBehavior = a} :: M2tsSettings)

-- | The length in seconds of each segment. Required unless markers is set to
-- _none_.
m2tsSettings_segmentationTime :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_segmentationTime = Lens.lens (\M2tsSettings' {segmentationTime} -> segmentationTime) (\s@M2tsSettings' {} a -> s {segmentationTime = a} :: M2tsSettings)

-- | If set to passthrough, passes any EBIF data from the input source to
-- this output.
m2tsSettings_ebif :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsEbifControl)
m2tsSettings_ebif = Lens.lens (\M2tsSettings' {ebif} -> ebif) (\s@M2tsSettings' {} a -> s {ebif = a} :: M2tsSettings)

-- | Packet Identifier (PID) for input source DVB Teletext data to this
-- output. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_dvbTeletextPid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_dvbTeletextPid = Lens.lens (\M2tsSettings' {dvbTeletextPid} -> dvbTeletextPid) (\s@M2tsSettings' {} a -> s {dvbTeletextPid = a} :: M2tsSettings)

-- | When set to passthrough, timed metadata will be passed through from
-- input to output.
m2tsSettings_timedMetadataBehavior :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsTimedMetadataBehavior)
m2tsSettings_timedMetadataBehavior = Lens.lens (\M2tsSettings' {timedMetadataBehavior} -> timedMetadataBehavior) (\s@M2tsSettings' {} a -> s {timedMetadataBehavior = a} :: M2tsSettings)

-- | When set to enabled, uses ARIB-compliant field muxing and removes video
-- descriptor.
m2tsSettings_arib :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsArib)
m2tsSettings_arib = Lens.lens (\M2tsSettings' {arib} -> arib) (\s@M2tsSettings' {} a -> s {arib = a} :: M2tsSettings)

-- | The output bitrate of the transport stream in bits per second. Setting
-- to 0 lets the muxer automatically determine the appropriate bitrate.
m2tsSettings_bitrate :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_bitrate = Lens.lens (\M2tsSettings' {bitrate} -> bitrate) (\s@M2tsSettings' {} a -> s {bitrate = a} :: M2tsSettings)

-- | The length in seconds of each fragment. Only used with EBP markers.
m2tsSettings_fragmentTime :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Double)
m2tsSettings_fragmentTime = Lens.lens (\M2tsSettings' {fragmentTime} -> fragmentTime) (\s@M2tsSettings' {} a -> s {fragmentTime = a} :: M2tsSettings)

-- | Include or exclude the ES Rate field in the PES header.
m2tsSettings_esRateInPes :: Lens.Lens' M2tsSettings (Prelude.Maybe M2tsEsRateInPes)
m2tsSettings_esRateInPes = Lens.lens (\M2tsSettings' {esRateInPes} -> esRateInPes) (\s@M2tsSettings' {} a -> s {esRateInPes = a} :: M2tsSettings)

-- | Packet Identifier (PID) for input source SCTE-27 data to this output.
-- Multiple values are accepted, and can be entered in ranges and\/or by
-- comma separation. Can be entered as decimal or hexadecimal values. Each
-- PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_scte27Pids :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_scte27Pids = Lens.lens (\M2tsSettings' {scte27Pids} -> scte27Pids) (\s@M2tsSettings' {} a -> s {scte27Pids = a} :: M2tsSettings)

-- | The value of the transport stream ID field in the Program Map Table.
m2tsSettings_transportStreamId :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Natural)
m2tsSettings_transportStreamId = Lens.lens (\M2tsSettings' {transportStreamId} -> transportStreamId) (\s@M2tsSettings' {} a -> s {transportStreamId = a} :: M2tsSettings)

-- | Packet Identifier (PID) for input source DVB Subtitle data to this
-- output. Multiple values are accepted, and can be entered in ranges
-- and\/or by comma separation. Can be entered as decimal or hexadecimal
-- values. Each PID specified must be in the range of 32 (or 0x20)..8182
-- (or 0x1ff6).
m2tsSettings_dvbSubPids :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_dvbSubPids = Lens.lens (\M2tsSettings' {dvbSubPids} -> dvbSubPids) (\s@M2tsSettings' {} a -> s {dvbSubPids = a} :: M2tsSettings)

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
-- Can be entered as a decimal or hexadecimal value. Valid values are 32
-- (or 0x20)..8182 (or 0x1ff6).
m2tsSettings_scte35Pid :: Lens.Lens' M2tsSettings (Prelude.Maybe Prelude.Text)
m2tsSettings_scte35Pid = Lens.lens (\M2tsSettings' {scte35Pid} -> scte35Pid) (\s@M2tsSettings' {} a -> s {scte35Pid = a} :: M2tsSettings)

instance Core.FromJSON M2tsSettings where
  parseJSON =
    Core.withObject
      "M2tsSettings"
      ( \x ->
          M2tsSettings'
            Prelude.<$> (x Core..:? "segmentationMarkers")
            Prelude.<*> (x Core..:? "klvDataPids")
            Prelude.<*> (x Core..:? "etvSignalPid")
            Prelude.<*> (x Core..:? "pcrPeriod")
            Prelude.<*> (x Core..:? "pmtPid")
            Prelude.<*> (x Core..:? "videoPid")
            Prelude.<*> (x Core..:? "nielsenId3Behavior")
            Prelude.<*> (x Core..:? "audioBufferModel")
            Prelude.<*> (x Core..:? "timedMetadataPid")
            Prelude.<*> (x Core..:? "segmentationStyle")
            Prelude.<*> (x Core..:? "dvbNitSettings")
            Prelude.<*> (x Core..:? "nullPacketBitrate")
            Prelude.<*> (x Core..:? "pcrControl")
            Prelude.<*> (x Core..:? "ebpAudioInterval")
            Prelude.<*> (x Core..:? "ebpPlacement")
            Prelude.<*> (x Core..:? "pmtInterval")
            Prelude.<*> (x Core..:? "ccDescriptor")
            Prelude.<*> (x Core..:? "scte35Control")
            Prelude.<*> (x Core..:? "aribCaptionsPid")
            Prelude.<*> (x Core..:? "audioPids")
            Prelude.<*> (x Core..:? "etvPlatformPid")
            Prelude.<*> (x Core..:? "patInterval")
            Prelude.<*> (x Core..:? "programNum")
            Prelude.<*> (x Core..:? "audioStreamType")
            Prelude.<*> (x Core..:? "ebpLookaheadMs")
            Prelude.<*> (x Core..:? "bufferModel")
            Prelude.<*> (x Core..:? "aribCaptionsPidControl")
            Prelude.<*> (x Core..:? "pcrPid")
            Prelude.<*> (x Core..:? "klv")
            Prelude.<*> (x Core..:? "audioFramesPerPes")
            Prelude.<*> (x Core..:? "rateMode")
            Prelude.<*> (x Core..:? "dvbTdtSettings")
            Prelude.<*> (x Core..:? "ecmPid")
            Prelude.<*> (x Core..:? "dvbSdtSettings")
            Prelude.<*> (x Core..:? "absentInputAudioBehavior")
            Prelude.<*> (x Core..:? "segmentationTime")
            Prelude.<*> (x Core..:? "ebif")
            Prelude.<*> (x Core..:? "dvbTeletextPid")
            Prelude.<*> (x Core..:? "timedMetadataBehavior")
            Prelude.<*> (x Core..:? "arib")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "fragmentTime")
            Prelude.<*> (x Core..:? "esRateInPes")
            Prelude.<*> (x Core..:? "scte27Pids")
            Prelude.<*> (x Core..:? "transportStreamId")
            Prelude.<*> (x Core..:? "dvbSubPids")
            Prelude.<*> (x Core..:? "scte35Pid")
      )

instance Prelude.Hashable M2tsSettings

instance Prelude.NFData M2tsSettings

instance Core.ToJSON M2tsSettings where
  toJSON M2tsSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("segmentationMarkers" Core..=)
              Prelude.<$> segmentationMarkers,
            ("klvDataPids" Core..=) Prelude.<$> klvDataPids,
            ("etvSignalPid" Core..=) Prelude.<$> etvSignalPid,
            ("pcrPeriod" Core..=) Prelude.<$> pcrPeriod,
            ("pmtPid" Core..=) Prelude.<$> pmtPid,
            ("videoPid" Core..=) Prelude.<$> videoPid,
            ("nielsenId3Behavior" Core..=)
              Prelude.<$> nielsenId3Behavior,
            ("audioBufferModel" Core..=)
              Prelude.<$> audioBufferModel,
            ("timedMetadataPid" Core..=)
              Prelude.<$> timedMetadataPid,
            ("segmentationStyle" Core..=)
              Prelude.<$> segmentationStyle,
            ("dvbNitSettings" Core..=)
              Prelude.<$> dvbNitSettings,
            ("nullPacketBitrate" Core..=)
              Prelude.<$> nullPacketBitrate,
            ("pcrControl" Core..=) Prelude.<$> pcrControl,
            ("ebpAudioInterval" Core..=)
              Prelude.<$> ebpAudioInterval,
            ("ebpPlacement" Core..=) Prelude.<$> ebpPlacement,
            ("pmtInterval" Core..=) Prelude.<$> pmtInterval,
            ("ccDescriptor" Core..=) Prelude.<$> ccDescriptor,
            ("scte35Control" Core..=) Prelude.<$> scte35Control,
            ("aribCaptionsPid" Core..=)
              Prelude.<$> aribCaptionsPid,
            ("audioPids" Core..=) Prelude.<$> audioPids,
            ("etvPlatformPid" Core..=)
              Prelude.<$> etvPlatformPid,
            ("patInterval" Core..=) Prelude.<$> patInterval,
            ("programNum" Core..=) Prelude.<$> programNum,
            ("audioStreamType" Core..=)
              Prelude.<$> audioStreamType,
            ("ebpLookaheadMs" Core..=)
              Prelude.<$> ebpLookaheadMs,
            ("bufferModel" Core..=) Prelude.<$> bufferModel,
            ("aribCaptionsPidControl" Core..=)
              Prelude.<$> aribCaptionsPidControl,
            ("pcrPid" Core..=) Prelude.<$> pcrPid,
            ("klv" Core..=) Prelude.<$> klv,
            ("audioFramesPerPes" Core..=)
              Prelude.<$> audioFramesPerPes,
            ("rateMode" Core..=) Prelude.<$> rateMode,
            ("dvbTdtSettings" Core..=)
              Prelude.<$> dvbTdtSettings,
            ("ecmPid" Core..=) Prelude.<$> ecmPid,
            ("dvbSdtSettings" Core..=)
              Prelude.<$> dvbSdtSettings,
            ("absentInputAudioBehavior" Core..=)
              Prelude.<$> absentInputAudioBehavior,
            ("segmentationTime" Core..=)
              Prelude.<$> segmentationTime,
            ("ebif" Core..=) Prelude.<$> ebif,
            ("dvbTeletextPid" Core..=)
              Prelude.<$> dvbTeletextPid,
            ("timedMetadataBehavior" Core..=)
              Prelude.<$> timedMetadataBehavior,
            ("arib" Core..=) Prelude.<$> arib,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("fragmentTime" Core..=) Prelude.<$> fragmentTime,
            ("esRateInPes" Core..=) Prelude.<$> esRateInPes,
            ("scte27Pids" Core..=) Prelude.<$> scte27Pids,
            ("transportStreamId" Core..=)
              Prelude.<$> transportStreamId,
            ("dvbSubPids" Core..=) Prelude.<$> dvbSubPids,
            ("scte35Pid" Core..=) Prelude.<$> scte35Pid
          ]
      )
