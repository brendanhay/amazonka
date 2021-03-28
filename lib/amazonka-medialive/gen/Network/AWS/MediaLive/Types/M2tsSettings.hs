{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsSettings
  ( M2tsSettings (..)
  -- * Smart constructor
  , mkM2tsSettings
  -- * Lenses
  , mAbsentInputAudioBehavior
  , mArib
  , mAribCaptionsPid
  , mAribCaptionsPidControl
  , mAudioBufferModel
  , mAudioFramesPerPes
  , mAudioPids
  , mAudioStreamType
  , mBitrate
  , mBufferModel
  , mCcDescriptor
  , mDvbNitSettings
  , mDvbSdtSettings
  , mDvbSubPids
  , mDvbTdtSettings
  , mDvbTeletextPid
  , mEbif
  , mEbpAudioInterval
  , mEbpLookaheadMs
  , mEbpPlacement
  , mEcmPid
  , mEsRateInPes
  , mEtvPlatformPid
  , mEtvSignalPid
  , mFragmentTime
  , mKlv
  , mKlvDataPids
  , mNielsenId3Behavior
  , mNullPacketBitrate
  , mPatInterval
  , mPcrControl
  , mPcrPeriod
  , mPcrPid
  , mPmtInterval
  , mPmtPid
  , mProgramNum
  , mRateMode
  , mScte27Pids
  , mScte35Control
  , mScte35Pid
  , mSegmentationMarkers
  , mSegmentationStyle
  , mSegmentationTime
  , mTimedMetadataBehavior
  , mTimedMetadataPid
  , mTransportStreamId
  , mVideoPid
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.DvbNitSettings as Types
import qualified Network.AWS.MediaLive.Types.DvbSdtSettings as Types
import qualified Network.AWS.MediaLive.Types.DvbTdtSettings as Types
import qualified Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior as Types
import qualified Network.AWS.MediaLive.Types.M2tsArib as Types
import qualified Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl as Types
import qualified Network.AWS.MediaLive.Types.M2tsAudioBufferModel as Types
import qualified Network.AWS.MediaLive.Types.M2tsAudioInterval as Types
import qualified Network.AWS.MediaLive.Types.M2tsAudioStreamType as Types
import qualified Network.AWS.MediaLive.Types.M2tsBufferModel as Types
import qualified Network.AWS.MediaLive.Types.M2tsCcDescriptor as Types
import qualified Network.AWS.MediaLive.Types.M2tsEbifControl as Types
import qualified Network.AWS.MediaLive.Types.M2tsEbpPlacement as Types
import qualified Network.AWS.MediaLive.Types.M2tsEsRateInPes as Types
import qualified Network.AWS.MediaLive.Types.M2tsKlv as Types
import qualified Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior as Types
import qualified Network.AWS.MediaLive.Types.M2tsPcrControl as Types
import qualified Network.AWS.MediaLive.Types.M2tsRateMode as Types
import qualified Network.AWS.MediaLive.Types.M2tsScte35Control as Types
import qualified Network.AWS.MediaLive.Types.M2tsSegmentationMarkers as Types
import qualified Network.AWS.MediaLive.Types.M2tsSegmentationStyle as Types
import qualified Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | M2ts Settings
--
-- /See:/ 'mkM2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'
  { absentInputAudioBehavior :: Core.Maybe Types.M2tsAbsentInputAudioBehavior
    -- ^ When set to drop, output audio streams will be removed from the program if the selected input audio stream is removed from the input. This allows the output audio configuration to dynamically change based on input configuration. If this is set to encodeSilence, all output audio streams will output encoded silence when not connected to an active input stream.
  , arib :: Core.Maybe Types.M2tsArib
    -- ^ When set to enabled, uses ARIB-compliant field muxing and removes video descriptor.
  , aribCaptionsPid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) for ARIB Captions in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  , aribCaptionsPidControl :: Core.Maybe Types.M2tsAribCaptionsPidControl
    -- ^ If set to auto, pid number used for ARIB Captions will be auto-selected from unused pids.  If set to useConfigured, ARIB Captions will be on the configured pid number.
  , audioBufferModel :: Core.Maybe Types.M2tsAudioBufferModel
    -- ^ When set to dvb, uses DVB buffer model for Dolby Digital audio.  When set to atsc, the ATSC model is used.
  , audioFramesPerPes :: Core.Maybe Core.Natural
    -- ^ The number of audio frames to insert for each PES packet.
  , audioPids :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values. Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
  , audioStreamType :: Core.Maybe Types.M2tsAudioStreamType
    -- ^ When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87 for EAC3. When set to dvb, uses stream type = 0x06.
  , bitrate :: Core.Maybe Core.Natural
    -- ^ The output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate.
  , bufferModel :: Core.Maybe Types.M2tsBufferModel
    -- ^ Controls the timing accuracy for output network traffic. Leave as MULTIPLEX to ensure accurate network packet timing. Or set to NONE, which might result in lower latency but will result in more variability in output network packet timing. This variability might cause interruptions, jitter, or bursty behavior in your playback or receiving devices.
  , ccDescriptor :: Core.Maybe Types.M2tsCcDescriptor
    -- ^ When set to enabled, generates captionServiceDescriptor in PMT.
  , dvbNitSettings :: Core.Maybe Types.DvbNitSettings
    -- ^ Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
  , dvbSdtSettings :: Core.Maybe Types.DvbSdtSettings
    -- ^ Inserts DVB Service Description Table (SDT) at the specified table repetition interval.
  , dvbSubPids :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) for input source DVB Subtitle data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
  , dvbTdtSettings :: Core.Maybe Types.DvbTdtSettings
    -- ^ Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
  , dvbTeletextPid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) for input source DVB Teletext data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  , ebif :: Core.Maybe Types.M2tsEbifControl
    -- ^ If set to passthrough, passes any EBIF data from the input source to this output.
  , ebpAudioInterval :: Core.Maybe Types.M2tsAudioInterval
    -- ^ When videoAndFixedIntervals is selected, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. Only available when EBP Cablelabs segmentation markers are selected.  Partitions 1 and 2 will always follow the video interval.
  , ebpLookaheadMs :: Core.Maybe Core.Natural
    -- ^ When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker.  The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
  , ebpPlacement :: Core.Maybe Types.M2tsEbpPlacement
    -- ^ Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids, EBP markers will be placed on the video PID and all audio PIDs.  If set to videoPid, EBP markers will be placed on only the video PID.
  , ecmPid :: Core.Maybe Core.Text
    -- ^ This field is unused and deprecated.
  , esRateInPes :: Core.Maybe Types.M2tsEsRateInPes
    -- ^ Include or exclude the ES Rate field in the PES header.
  , etvPlatformPid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) for input source ETV Platform data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  , etvSignalPid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) for input source ETV Signal data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  , fragmentTime :: Core.Maybe Core.Double
    -- ^ The length in seconds of each fragment. Only used with EBP markers.
  , klv :: Core.Maybe Types.M2tsKlv
    -- ^ If set to passthrough, passes any KLV data from the input source to this output.
  , klvDataPids :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) for input source KLV data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
  , nielsenId3Behavior :: Core.Maybe Types.M2tsNielsenId3Behavior
    -- ^ If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
  , nullPacketBitrate :: Core.Maybe Core.Double
    -- ^ Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
  , patInterval :: Core.Maybe Core.Natural
    -- ^ The number of milliseconds between instances of this table in the output transport stream.  Valid values are 0, 10..1000.
  , pcrControl :: Core.Maybe Types.M2tsPcrControl
    -- ^ When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
  , pcrPeriod :: Core.Maybe Core.Natural
    -- ^ Maximum time in milliseconds between Program Clock Reference (PCRs) inserted into the transport stream.
  , pcrPid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  , pmtInterval :: Core.Maybe Core.Natural
    -- ^ The number of milliseconds between instances of this table in the output transport stream. Valid values are 0, 10..1000.
  , pmtPid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  , programNum :: Core.Maybe Core.Natural
    -- ^ The value of the program number field in the Program Map Table.
  , rateMode :: Core.Maybe Types.M2tsRateMode
    -- ^ When vbr, does not insert null packets into transport stream to fill specified bitrate. The bitrate setting acts as the maximum bitrate when vbr is set.
  , scte27Pids :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) for input source SCTE-27 data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
  , scte35Control :: Core.Maybe Types.M2tsScte35Control
    -- ^ Optionally pass SCTE-35 signals from the input source to this output.
  , scte35Pid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  , segmentationMarkers :: Core.Maybe Types.M2tsSegmentationMarkers
    -- ^ Inserts segmentation markers at each segmentationTime period. raiSegstart sets the Random Access Indicator bit in the adaptation field. raiAdapt sets the RAI bit and adds the current timecode in the private data bytes. psiSegstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebpLegacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
  , segmentationStyle :: Core.Maybe Types.M2tsSegmentationStyle
    -- ^ The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted.
--
--
-- When a segmentation style of "resetCadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of $segmentationTime seconds.
--
-- When a segmentation style of "maintainCadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentationTime seconds. Note that EBP lookahead is a slight exception to this rule.
  , segmentationTime :: Core.Maybe Core.Double
    -- ^ The length in seconds of each segment. Required unless markers is set to _none_.
  , timedMetadataBehavior :: Core.Maybe Types.M2tsTimedMetadataBehavior
    -- ^ When set to passthrough, timed metadata will be passed through from input to output.
  , timedMetadataPid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  , transportStreamId :: Core.Maybe Core.Natural
    -- ^ The value of the transport stream ID field in the Program Map Table.
  , videoPid :: Core.Maybe Core.Text
    -- ^ Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'M2tsSettings' value with any optional fields omitted.
mkM2tsSettings
    :: M2tsSettings
mkM2tsSettings
  = M2tsSettings'{absentInputAudioBehavior = Core.Nothing,
                  arib = Core.Nothing, aribCaptionsPid = Core.Nothing,
                  aribCaptionsPidControl = Core.Nothing,
                  audioBufferModel = Core.Nothing, audioFramesPerPes = Core.Nothing,
                  audioPids = Core.Nothing, audioStreamType = Core.Nothing,
                  bitrate = Core.Nothing, bufferModel = Core.Nothing,
                  ccDescriptor = Core.Nothing, dvbNitSettings = Core.Nothing,
                  dvbSdtSettings = Core.Nothing, dvbSubPids = Core.Nothing,
                  dvbTdtSettings = Core.Nothing, dvbTeletextPid = Core.Nothing,
                  ebif = Core.Nothing, ebpAudioInterval = Core.Nothing,
                  ebpLookaheadMs = Core.Nothing, ebpPlacement = Core.Nothing,
                  ecmPid = Core.Nothing, esRateInPes = Core.Nothing,
                  etvPlatformPid = Core.Nothing, etvSignalPid = Core.Nothing,
                  fragmentTime = Core.Nothing, klv = Core.Nothing,
                  klvDataPids = Core.Nothing, nielsenId3Behavior = Core.Nothing,
                  nullPacketBitrate = Core.Nothing, patInterval = Core.Nothing,
                  pcrControl = Core.Nothing, pcrPeriod = Core.Nothing,
                  pcrPid = Core.Nothing, pmtInterval = Core.Nothing,
                  pmtPid = Core.Nothing, programNum = Core.Nothing,
                  rateMode = Core.Nothing, scte27Pids = Core.Nothing,
                  scte35Control = Core.Nothing, scte35Pid = Core.Nothing,
                  segmentationMarkers = Core.Nothing,
                  segmentationStyle = Core.Nothing, segmentationTime = Core.Nothing,
                  timedMetadataBehavior = Core.Nothing,
                  timedMetadataPid = Core.Nothing, transportStreamId = Core.Nothing,
                  videoPid = Core.Nothing}

-- | When set to drop, output audio streams will be removed from the program if the selected input audio stream is removed from the input. This allows the output audio configuration to dynamically change based on input configuration. If this is set to encodeSilence, all output audio streams will output encoded silence when not connected to an active input stream.
--
-- /Note:/ Consider using 'absentInputAudioBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAbsentInputAudioBehavior :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsAbsentInputAudioBehavior)
mAbsentInputAudioBehavior = Lens.field @"absentInputAudioBehavior"
{-# INLINEABLE mAbsentInputAudioBehavior #-}
{-# DEPRECATED absentInputAudioBehavior "Use generic-lens or generic-optics with 'absentInputAudioBehavior' instead"  #-}

-- | When set to enabled, uses ARIB-compliant field muxing and removes video descriptor.
--
-- /Note:/ Consider using 'arib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mArib :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsArib)
mArib = Lens.field @"arib"
{-# INLINEABLE mArib #-}
{-# DEPRECATED arib "Use generic-lens or generic-optics with 'arib' instead"  #-}

-- | Packet Identifier (PID) for ARIB Captions in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'aribCaptionsPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAribCaptionsPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mAribCaptionsPid = Lens.field @"aribCaptionsPid"
{-# INLINEABLE mAribCaptionsPid #-}
{-# DEPRECATED aribCaptionsPid "Use generic-lens or generic-optics with 'aribCaptionsPid' instead"  #-}

-- | If set to auto, pid number used for ARIB Captions will be auto-selected from unused pids.  If set to useConfigured, ARIB Captions will be on the configured pid number.
--
-- /Note:/ Consider using 'aribCaptionsPidControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAribCaptionsPidControl :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsAribCaptionsPidControl)
mAribCaptionsPidControl = Lens.field @"aribCaptionsPidControl"
{-# INLINEABLE mAribCaptionsPidControl #-}
{-# DEPRECATED aribCaptionsPidControl "Use generic-lens or generic-optics with 'aribCaptionsPidControl' instead"  #-}

-- | When set to dvb, uses DVB buffer model for Dolby Digital audio.  When set to atsc, the ATSC model is used.
--
-- /Note:/ Consider using 'audioBufferModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioBufferModel :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsAudioBufferModel)
mAudioBufferModel = Lens.field @"audioBufferModel"
{-# INLINEABLE mAudioBufferModel #-}
{-# DEPRECATED audioBufferModel "Use generic-lens or generic-optics with 'audioBufferModel' instead"  #-}

-- | The number of audio frames to insert for each PES packet.
--
-- /Note:/ Consider using 'audioFramesPerPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioFramesPerPes :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mAudioFramesPerPes = Lens.field @"audioFramesPerPes"
{-# INLINEABLE mAudioFramesPerPes #-}
{-# DEPRECATED audioFramesPerPes "Use generic-lens or generic-optics with 'audioFramesPerPes' instead"  #-}

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values. Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioPids :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mAudioPids = Lens.field @"audioPids"
{-# INLINEABLE mAudioPids #-}
{-# DEPRECATED audioPids "Use generic-lens or generic-optics with 'audioPids' instead"  #-}

-- | When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87 for EAC3. When set to dvb, uses stream type = 0x06.
--
-- /Note:/ Consider using 'audioStreamType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioStreamType :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsAudioStreamType)
mAudioStreamType = Lens.field @"audioStreamType"
{-# INLINEABLE mAudioStreamType #-}
{-# DEPRECATED audioStreamType "Use generic-lens or generic-optics with 'audioStreamType' instead"  #-}

-- | The output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBitrate :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mBitrate = Lens.field @"bitrate"
{-# INLINEABLE mBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | Controls the timing accuracy for output network traffic. Leave as MULTIPLEX to ensure accurate network packet timing. Or set to NONE, which might result in lower latency but will result in more variability in output network packet timing. This variability might cause interruptions, jitter, or bursty behavior in your playback or receiving devices.
--
-- /Note:/ Consider using 'bufferModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBufferModel :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsBufferModel)
mBufferModel = Lens.field @"bufferModel"
{-# INLINEABLE mBufferModel #-}
{-# DEPRECATED bufferModel "Use generic-lens or generic-optics with 'bufferModel' instead"  #-}

-- | When set to enabled, generates captionServiceDescriptor in PMT.
--
-- /Note:/ Consider using 'ccDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCcDescriptor :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsCcDescriptor)
mCcDescriptor = Lens.field @"ccDescriptor"
{-# INLINEABLE mCcDescriptor #-}
{-# DEPRECATED ccDescriptor "Use generic-lens or generic-optics with 'ccDescriptor' instead"  #-}

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbNitSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbNitSettings :: Lens.Lens' M2tsSettings (Core.Maybe Types.DvbNitSettings)
mDvbNitSettings = Lens.field @"dvbNitSettings"
{-# INLINEABLE mDvbNitSettings #-}
{-# DEPRECATED dvbNitSettings "Use generic-lens or generic-optics with 'dvbNitSettings' instead"  #-}

-- | Inserts DVB Service Description Table (SDT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbSdtSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbSdtSettings :: Lens.Lens' M2tsSettings (Core.Maybe Types.DvbSdtSettings)
mDvbSdtSettings = Lens.field @"dvbSdtSettings"
{-# INLINEABLE mDvbSdtSettings #-}
{-# DEPRECATED dvbSdtSettings "Use generic-lens or generic-optics with 'dvbSdtSettings' instead"  #-}

-- | Packet Identifier (PID) for input source DVB Subtitle data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'dvbSubPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbSubPids :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mDvbSubPids = Lens.field @"dvbSubPids"
{-# INLINEABLE mDvbSubPids #-}
{-# DEPRECATED dvbSubPids "Use generic-lens or generic-optics with 'dvbSubPids' instead"  #-}

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbTdtSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbTdtSettings :: Lens.Lens' M2tsSettings (Core.Maybe Types.DvbTdtSettings)
mDvbTdtSettings = Lens.field @"dvbTdtSettings"
{-# INLINEABLE mDvbTdtSettings #-}
{-# DEPRECATED dvbTdtSettings "Use generic-lens or generic-optics with 'dvbTdtSettings' instead"  #-}

-- | Packet Identifier (PID) for input source DVB Teletext data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'dvbTeletextPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbTeletextPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mDvbTeletextPid = Lens.field @"dvbTeletextPid"
{-# INLINEABLE mDvbTeletextPid #-}
{-# DEPRECATED dvbTeletextPid "Use generic-lens or generic-optics with 'dvbTeletextPid' instead"  #-}

-- | If set to passthrough, passes any EBIF data from the input source to this output.
--
-- /Note:/ Consider using 'ebif' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEbif :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsEbifControl)
mEbif = Lens.field @"ebif"
{-# INLINEABLE mEbif #-}
{-# DEPRECATED ebif "Use generic-lens or generic-optics with 'ebif' instead"  #-}

-- | When videoAndFixedIntervals is selected, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. Only available when EBP Cablelabs segmentation markers are selected.  Partitions 1 and 2 will always follow the video interval.
--
-- /Note:/ Consider using 'ebpAudioInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEbpAudioInterval :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsAudioInterval)
mEbpAudioInterval = Lens.field @"ebpAudioInterval"
{-# INLINEABLE mEbpAudioInterval #-}
{-# DEPRECATED ebpAudioInterval "Use generic-lens or generic-optics with 'ebpAudioInterval' instead"  #-}

-- | When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker.  The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
--
-- /Note:/ Consider using 'ebpLookaheadMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEbpLookaheadMs :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mEbpLookaheadMs = Lens.field @"ebpLookaheadMs"
{-# INLINEABLE mEbpLookaheadMs #-}
{-# DEPRECATED ebpLookaheadMs "Use generic-lens or generic-optics with 'ebpLookaheadMs' instead"  #-}

-- | Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids, EBP markers will be placed on the video PID and all audio PIDs.  If set to videoPid, EBP markers will be placed on only the video PID.
--
-- /Note:/ Consider using 'ebpPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEbpPlacement :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsEbpPlacement)
mEbpPlacement = Lens.field @"ebpPlacement"
{-# INLINEABLE mEbpPlacement #-}
{-# DEPRECATED ebpPlacement "Use generic-lens or generic-optics with 'ebpPlacement' instead"  #-}

-- | This field is unused and deprecated.
--
-- /Note:/ Consider using 'ecmPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEcmPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mEcmPid = Lens.field @"ecmPid"
{-# INLINEABLE mEcmPid #-}
{-# DEPRECATED ecmPid "Use generic-lens or generic-optics with 'ecmPid' instead"  #-}

-- | Include or exclude the ES Rate field in the PES header.
--
-- /Note:/ Consider using 'esRateInPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEsRateInPes :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsEsRateInPes)
mEsRateInPes = Lens.field @"esRateInPes"
{-# INLINEABLE mEsRateInPes #-}
{-# DEPRECATED esRateInPes "Use generic-lens or generic-optics with 'esRateInPes' instead"  #-}

-- | Packet Identifier (PID) for input source ETV Platform data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'etvPlatformPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEtvPlatformPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mEtvPlatformPid = Lens.field @"etvPlatformPid"
{-# INLINEABLE mEtvPlatformPid #-}
{-# DEPRECATED etvPlatformPid "Use generic-lens or generic-optics with 'etvPlatformPid' instead"  #-}

-- | Packet Identifier (PID) for input source ETV Signal data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'etvSignalPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEtvSignalPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mEtvSignalPid = Lens.field @"etvSignalPid"
{-# INLINEABLE mEtvSignalPid #-}
{-# DEPRECATED etvSignalPid "Use generic-lens or generic-optics with 'etvSignalPid' instead"  #-}

-- | The length in seconds of each fragment. Only used with EBP markers.
--
-- /Note:/ Consider using 'fragmentTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mFragmentTime :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
mFragmentTime = Lens.field @"fragmentTime"
{-# INLINEABLE mFragmentTime #-}
{-# DEPRECATED fragmentTime "Use generic-lens or generic-optics with 'fragmentTime' instead"  #-}

-- | If set to passthrough, passes any KLV data from the input source to this output.
--
-- /Note:/ Consider using 'klv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mKlv :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsKlv)
mKlv = Lens.field @"klv"
{-# INLINEABLE mKlv #-}
{-# DEPRECATED klv "Use generic-lens or generic-optics with 'klv' instead"  #-}

-- | Packet Identifier (PID) for input source KLV data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'klvDataPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mKlvDataPids :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mKlvDataPids = Lens.field @"klvDataPids"
{-# INLINEABLE mKlvDataPids #-}
{-# DEPRECATED klvDataPids "Use generic-lens or generic-optics with 'klvDataPids' instead"  #-}

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mNielsenId3Behavior :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsNielsenId3Behavior)
mNielsenId3Behavior = Lens.field @"nielsenId3Behavior"
{-# INLINEABLE mNielsenId3Behavior #-}
{-# DEPRECATED nielsenId3Behavior "Use generic-lens or generic-optics with 'nielsenId3Behavior' instead"  #-}

-- | Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
--
-- /Note:/ Consider using 'nullPacketBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mNullPacketBitrate :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
mNullPacketBitrate = Lens.field @"nullPacketBitrate"
{-# INLINEABLE mNullPacketBitrate #-}
{-# DEPRECATED nullPacketBitrate "Use generic-lens or generic-optics with 'nullPacketBitrate' instead"  #-}

-- | The number of milliseconds between instances of this table in the output transport stream.  Valid values are 0, 10..1000.
--
-- /Note:/ Consider using 'patInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPatInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mPatInterval = Lens.field @"patInterval"
{-# INLINEABLE mPatInterval #-}
{-# DEPRECATED patInterval "Use generic-lens or generic-optics with 'patInterval' instead"  #-}

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- /Note:/ Consider using 'pcrControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPcrControl :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsPcrControl)
mPcrControl = Lens.field @"pcrControl"
{-# INLINEABLE mPcrControl #-}
{-# DEPRECATED pcrControl "Use generic-lens or generic-optics with 'pcrControl' instead"  #-}

-- | Maximum time in milliseconds between Program Clock Reference (PCRs) inserted into the transport stream.
--
-- /Note:/ Consider using 'pcrPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPcrPeriod :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mPcrPeriod = Lens.field @"pcrPeriod"
{-# INLINEABLE mPcrPeriod #-}
{-# DEPRECATED pcrPeriod "Use generic-lens or generic-optics with 'pcrPeriod' instead"  #-}

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPcrPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mPcrPid = Lens.field @"pcrPid"
{-# INLINEABLE mPcrPid #-}
{-# DEPRECATED pcrPid "Use generic-lens or generic-optics with 'pcrPid' instead"  #-}

-- | The number of milliseconds between instances of this table in the output transport stream. Valid values are 0, 10..1000.
--
-- /Note:/ Consider using 'pmtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPmtInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mPmtInterval = Lens.field @"pmtInterval"
{-# INLINEABLE mPmtInterval #-}
{-# DEPRECATED pmtInterval "Use generic-lens or generic-optics with 'pmtInterval' instead"  #-}

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPmtPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mPmtPid = Lens.field @"pmtPid"
{-# INLINEABLE mPmtPid #-}
{-# DEPRECATED pmtPid "Use generic-lens or generic-optics with 'pmtPid' instead"  #-}

-- | The value of the program number field in the Program Map Table.
--
-- /Note:/ Consider using 'programNum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mProgramNum :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mProgramNum = Lens.field @"programNum"
{-# INLINEABLE mProgramNum #-}
{-# DEPRECATED programNum "Use generic-lens or generic-optics with 'programNum' instead"  #-}

-- | When vbr, does not insert null packets into transport stream to fill specified bitrate. The bitrate setting acts as the maximum bitrate when vbr is set.
--
-- /Note:/ Consider using 'rateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRateMode :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsRateMode)
mRateMode = Lens.field @"rateMode"
{-# INLINEABLE mRateMode #-}
{-# DEPRECATED rateMode "Use generic-lens or generic-optics with 'rateMode' instead"  #-}

-- | Packet Identifier (PID) for input source SCTE-27 data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'scte27Pids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mScte27Pids :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mScte27Pids = Lens.field @"scte27Pids"
{-# INLINEABLE mScte27Pids #-}
{-# DEPRECATED scte27Pids "Use generic-lens or generic-optics with 'scte27Pids' instead"  #-}

-- | Optionally pass SCTE-35 signals from the input source to this output.
--
-- /Note:/ Consider using 'scte35Control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mScte35Control :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsScte35Control)
mScte35Control = Lens.field @"scte35Control"
{-# INLINEABLE mScte35Control #-}
{-# DEPRECATED scte35Control "Use generic-lens or generic-optics with 'scte35Control' instead"  #-}

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mScte35Pid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mScte35Pid = Lens.field @"scte35Pid"
{-# INLINEABLE mScte35Pid #-}
{-# DEPRECATED scte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead"  #-}

-- | Inserts segmentation markers at each segmentationTime period. raiSegstart sets the Random Access Indicator bit in the adaptation field. raiAdapt sets the RAI bit and adds the current timecode in the private data bytes. psiSegstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebpLegacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
--
-- /Note:/ Consider using 'segmentationMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSegmentationMarkers :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsSegmentationMarkers)
mSegmentationMarkers = Lens.field @"segmentationMarkers"
{-# INLINEABLE mSegmentationMarkers #-}
{-# DEPRECATED segmentationMarkers "Use generic-lens or generic-optics with 'segmentationMarkers' instead"  #-}

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted.
--
--
-- When a segmentation style of "resetCadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of $segmentationTime seconds.
--
-- When a segmentation style of "maintainCadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentationTime seconds. Note that EBP lookahead is a slight exception to this rule.
--
-- /Note:/ Consider using 'segmentationStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSegmentationStyle :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsSegmentationStyle)
mSegmentationStyle = Lens.field @"segmentationStyle"
{-# INLINEABLE mSegmentationStyle #-}
{-# DEPRECATED segmentationStyle "Use generic-lens or generic-optics with 'segmentationStyle' instead"  #-}

-- | The length in seconds of each segment. Required unless markers is set to _none_.
--
-- /Note:/ Consider using 'segmentationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSegmentationTime :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
mSegmentationTime = Lens.field @"segmentationTime"
{-# INLINEABLE mSegmentationTime #-}
{-# DEPRECATED segmentationTime "Use generic-lens or generic-optics with 'segmentationTime' instead"  #-}

-- | When set to passthrough, timed metadata will be passed through from input to output.
--
-- /Note:/ Consider using 'timedMetadataBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTimedMetadataBehavior :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsTimedMetadataBehavior)
mTimedMetadataBehavior = Lens.field @"timedMetadataBehavior"
{-# INLINEABLE mTimedMetadataBehavior #-}
{-# DEPRECATED timedMetadataBehavior "Use generic-lens or generic-optics with 'timedMetadataBehavior' instead"  #-}

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTimedMetadataPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mTimedMetadataPid = Lens.field @"timedMetadataPid"
{-# INLINEABLE mTimedMetadataPid #-}
{-# DEPRECATED timedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead"  #-}

-- | The value of the transport stream ID field in the Program Map Table.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTransportStreamId :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mTransportStreamId = Lens.field @"transportStreamId"
{-# INLINEABLE mTransportStreamId #-}
{-# DEPRECATED transportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead"  #-}

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mVideoPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Text)
mVideoPid = Lens.field @"videoPid"
{-# INLINEABLE mVideoPid #-}
{-# DEPRECATED videoPid "Use generic-lens or generic-optics with 'videoPid' instead"  #-}

instance Core.FromJSON M2tsSettings where
        toJSON M2tsSettings{..}
          = Core.object
              (Core.catMaybes
                 [("absentInputAudioBehavior" Core..=) Core.<$>
                    absentInputAudioBehavior,
                  ("arib" Core..=) Core.<$> arib,
                  ("aribCaptionsPid" Core..=) Core.<$> aribCaptionsPid,
                  ("aribCaptionsPidControl" Core..=) Core.<$> aribCaptionsPidControl,
                  ("audioBufferModel" Core..=) Core.<$> audioBufferModel,
                  ("audioFramesPerPes" Core..=) Core.<$> audioFramesPerPes,
                  ("audioPids" Core..=) Core.<$> audioPids,
                  ("audioStreamType" Core..=) Core.<$> audioStreamType,
                  ("bitrate" Core..=) Core.<$> bitrate,
                  ("bufferModel" Core..=) Core.<$> bufferModel,
                  ("ccDescriptor" Core..=) Core.<$> ccDescriptor,
                  ("dvbNitSettings" Core..=) Core.<$> dvbNitSettings,
                  ("dvbSdtSettings" Core..=) Core.<$> dvbSdtSettings,
                  ("dvbSubPids" Core..=) Core.<$> dvbSubPids,
                  ("dvbTdtSettings" Core..=) Core.<$> dvbTdtSettings,
                  ("dvbTeletextPid" Core..=) Core.<$> dvbTeletextPid,
                  ("ebif" Core..=) Core.<$> ebif,
                  ("ebpAudioInterval" Core..=) Core.<$> ebpAudioInterval,
                  ("ebpLookaheadMs" Core..=) Core.<$> ebpLookaheadMs,
                  ("ebpPlacement" Core..=) Core.<$> ebpPlacement,
                  ("ecmPid" Core..=) Core.<$> ecmPid,
                  ("esRateInPes" Core..=) Core.<$> esRateInPes,
                  ("etvPlatformPid" Core..=) Core.<$> etvPlatformPid,
                  ("etvSignalPid" Core..=) Core.<$> etvSignalPid,
                  ("fragmentTime" Core..=) Core.<$> fragmentTime,
                  ("klv" Core..=) Core.<$> klv,
                  ("klvDataPids" Core..=) Core.<$> klvDataPids,
                  ("nielsenId3Behavior" Core..=) Core.<$> nielsenId3Behavior,
                  ("nullPacketBitrate" Core..=) Core.<$> nullPacketBitrate,
                  ("patInterval" Core..=) Core.<$> patInterval,
                  ("pcrControl" Core..=) Core.<$> pcrControl,
                  ("pcrPeriod" Core..=) Core.<$> pcrPeriod,
                  ("pcrPid" Core..=) Core.<$> pcrPid,
                  ("pmtInterval" Core..=) Core.<$> pmtInterval,
                  ("pmtPid" Core..=) Core.<$> pmtPid,
                  ("programNum" Core..=) Core.<$> programNum,
                  ("rateMode" Core..=) Core.<$> rateMode,
                  ("scte27Pids" Core..=) Core.<$> scte27Pids,
                  ("scte35Control" Core..=) Core.<$> scte35Control,
                  ("scte35Pid" Core..=) Core.<$> scte35Pid,
                  ("segmentationMarkers" Core..=) Core.<$> segmentationMarkers,
                  ("segmentationStyle" Core..=) Core.<$> segmentationStyle,
                  ("segmentationTime" Core..=) Core.<$> segmentationTime,
                  ("timedMetadataBehavior" Core..=) Core.<$> timedMetadataBehavior,
                  ("timedMetadataPid" Core..=) Core.<$> timedMetadataPid,
                  ("transportStreamId" Core..=) Core.<$> transportStreamId,
                  ("videoPid" Core..=) Core.<$> videoPid])

instance Core.FromJSON M2tsSettings where
        parseJSON
          = Core.withObject "M2tsSettings" Core.$
              \ x ->
                M2tsSettings' Core.<$>
                  (x Core..:? "absentInputAudioBehavior") Core.<*> x Core..:? "arib"
                    Core.<*> x Core..:? "aribCaptionsPid"
                    Core.<*> x Core..:? "aribCaptionsPidControl"
                    Core.<*> x Core..:? "audioBufferModel"
                    Core.<*> x Core..:? "audioFramesPerPes"
                    Core.<*> x Core..:? "audioPids"
                    Core.<*> x Core..:? "audioStreamType"
                    Core.<*> x Core..:? "bitrate"
                    Core.<*> x Core..:? "bufferModel"
                    Core.<*> x Core..:? "ccDescriptor"
                    Core.<*> x Core..:? "dvbNitSettings"
                    Core.<*> x Core..:? "dvbSdtSettings"
                    Core.<*> x Core..:? "dvbSubPids"
                    Core.<*> x Core..:? "dvbTdtSettings"
                    Core.<*> x Core..:? "dvbTeletextPid"
                    Core.<*> x Core..:? "ebif"
                    Core.<*> x Core..:? "ebpAudioInterval"
                    Core.<*> x Core..:? "ebpLookaheadMs"
                    Core.<*> x Core..:? "ebpPlacement"
                    Core.<*> x Core..:? "ecmPid"
                    Core.<*> x Core..:? "esRateInPes"
                    Core.<*> x Core..:? "etvPlatformPid"
                    Core.<*> x Core..:? "etvSignalPid"
                    Core.<*> x Core..:? "fragmentTime"
                    Core.<*> x Core..:? "klv"
                    Core.<*> x Core..:? "klvDataPids"
                    Core.<*> x Core..:? "nielsenId3Behavior"
                    Core.<*> x Core..:? "nullPacketBitrate"
                    Core.<*> x Core..:? "patInterval"
                    Core.<*> x Core..:? "pcrControl"
                    Core.<*> x Core..:? "pcrPeriod"
                    Core.<*> x Core..:? "pcrPid"
                    Core.<*> x Core..:? "pmtInterval"
                    Core.<*> x Core..:? "pmtPid"
                    Core.<*> x Core..:? "programNum"
                    Core.<*> x Core..:? "rateMode"
                    Core.<*> x Core..:? "scte27Pids"
                    Core.<*> x Core..:? "scte35Control"
                    Core.<*> x Core..:? "scte35Pid"
                    Core.<*> x Core..:? "segmentationMarkers"
                    Core.<*> x Core..:? "segmentationStyle"
                    Core.<*> x Core..:? "segmentationTime"
                    Core.<*> x Core..:? "timedMetadataBehavior"
                    Core.<*> x Core..:? "timedMetadataPid"
                    Core.<*> x Core..:? "transportStreamId"
                    Core.<*> x Core..:? "videoPid"
