{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsSettings
  ( M2tsSettings (..)
  -- * Smart constructor
  , mkM2tsSettings
  -- * Lenses
  , mssAudioBufferModel
  , mssAudioDuration
  , mssAudioFramesPerPes
  , mssAudioPids
  , mssBitrate
  , mssBufferModel
  , mssDvbNitSettings
  , mssDvbSdtSettings
  , mssDvbSubPids
  , mssDvbTdtSettings
  , mssDvbTeletextPid
  , mssEbpAudioInterval
  , mssEbpPlacement
  , mssEsRateInPes
  , mssForceTsVideoEbpOrder
  , mssFragmentTime
  , mssMaxPcrInterval
  , mssMinEbpInterval
  , mssNielsenId3
  , mssNullPacketBitrate
  , mssPatInterval
  , mssPcrControl
  , mssPcrPid
  , mssPmtInterval
  , mssPmtPid
  , mssPrivateMetadataPid
  , mssProgramNumber
  , mssRateMode
  , mssScte35Esam
  , mssScte35Pid
  , mssScte35Source
  , mssSegmentationMarkers
  , mssSegmentationStyle
  , mssSegmentationTime
  , mssTimedMetadataPid
  , mssTransportStreamId
  , mssVideoPid
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.DvbNitSettings as Types
import qualified Network.AWS.MediaConvert.Types.DvbSdtSettings as Types
import qualified Network.AWS.MediaConvert.Types.DvbTdtSettings as Types
import qualified Network.AWS.MediaConvert.Types.M2tsAudioBufferModel as Types
import qualified Network.AWS.MediaConvert.Types.M2tsAudioDuration as Types
import qualified Network.AWS.MediaConvert.Types.M2tsBufferModel as Types
import qualified Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval as Types
import qualified Network.AWS.MediaConvert.Types.M2tsEbpPlacement as Types
import qualified Network.AWS.MediaConvert.Types.M2tsEsRateInPes as Types
import qualified Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder as Types
import qualified Network.AWS.MediaConvert.Types.M2tsNielsenId3 as Types
import qualified Network.AWS.MediaConvert.Types.M2tsPcrControl as Types
import qualified Network.AWS.MediaConvert.Types.M2tsRateMode as Types
import qualified Network.AWS.MediaConvert.Types.M2tsScte35Esam as Types
import qualified Network.AWS.MediaConvert.Types.M2tsScte35Source as Types
import qualified Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers as Types
import qualified Network.AWS.MediaConvert.Types.M2tsSegmentationStyle as Types
import qualified Network.AWS.Prelude as Core

-- | MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
--
-- /See:/ 'mkM2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'
  { audioBufferModel :: Core.Maybe Types.M2tsAudioBufferModel
    -- ^ Selects between the DVB and ATSC buffer models for Dolby Digital audio.
  , audioDuration :: Core.Maybe Types.M2tsAudioDuration
    -- ^ Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
  , audioFramesPerPes :: Core.Maybe Core.Natural
    -- ^ The number of audio frames to insert for each PES packet.
  , audioPids :: Core.Maybe [Core.Natural]
    -- ^ Specify the packet identifiers (PIDs) for any elementary audio streams you include in this output. Specify multiple PIDs as a JSON array. Default is the range 482-492.
  , bitrate :: Core.Maybe Core.Natural
    -- ^ Specify the output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
  , bufferModel :: Core.Maybe Types.M2tsBufferModel
    -- ^ Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
  , dvbNitSettings :: Core.Maybe Types.DvbNitSettings
    -- ^ Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
  , dvbSdtSettings :: Core.Maybe Types.DvbSdtSettings
    -- ^ Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
  , dvbSubPids :: Core.Maybe [Core.Natural]
    -- ^ Specify the packet identifiers (PIDs) for DVB subtitle data included in this output. Specify multiple PIDs as a JSON array. Default is the range 460-479.
  , dvbTdtSettings :: Core.Maybe Types.DvbTdtSettings
    -- ^ Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
  , dvbTeletextPid :: Core.Maybe Core.Natural
    -- ^ Specify the packet identifier (PID) for DVB teletext data you include in this output. Default is 499.
  , ebpAudioInterval :: Core.Maybe Types.M2tsEbpAudioInterval
    -- ^ When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
  , ebpPlacement :: Core.Maybe Types.M2tsEbpPlacement
    -- ^ Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
  , esRateInPes :: Core.Maybe Types.M2tsEsRateInPes
    -- ^ Controls whether to include the ES Rate field in the PES header.
  , forceTsVideoEbpOrder :: Core.Maybe Types.M2tsForceTsVideoEbpOrder
    -- ^ Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
  , fragmentTime :: Core.Maybe Core.Double
    -- ^ The length, in seconds, of each fragment. Only used with EBP markers.
  , maxPcrInterval :: Core.Maybe Core.Natural
    -- ^ Specify the maximum time, in milliseconds, between Program Clock References (PCRs) inserted into the transport stream.
  , minEbpInterval :: Core.Maybe Core.Natural
    -- ^ When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
  , nielsenId3 :: Core.Maybe Types.M2tsNielsenId3
    -- ^ If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
  , nullPacketBitrate :: Core.Maybe Core.Double
    -- ^ Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
  , patInterval :: Core.Maybe Core.Natural
    -- ^ The number of milliseconds between instances of this table in the output transport stream.
  , pcrControl :: Core.Maybe Types.M2tsPcrControl
    -- ^ When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
  , pcrPid :: Core.Maybe Core.Natural
    -- ^ Specify the packet identifier (PID) for the program clock reference (PCR) in this output. If you do not specify a value, the service will use the value for Video PID (VideoPid).
  , pmtInterval :: Core.Maybe Core.Natural
    -- ^ Specify the number of milliseconds between instances of the program map table (PMT) in the output transport stream.
  , pmtPid :: Core.Maybe Core.Natural
    -- ^ Specify the packet identifier (PID) for the program map table (PMT) itself. Default is 480.
  , privateMetadataPid :: Core.Maybe Core.Natural
    -- ^ Specify the packet identifier (PID) of the private metadata stream. Default is 503.
  , programNumber :: Core.Maybe Core.Natural
    -- ^ Use Program number (programNumber) to specify the program number used in the program map table (PMT) for this output. Default is 1. Program numbers and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
  , rateMode :: Core.Maybe Types.M2tsRateMode
    -- ^ When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
  , scte35Esam :: Core.Maybe Types.M2tsScte35Esam
    -- ^ Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
  , scte35Pid :: Core.Maybe Core.Natural
    -- ^ Specify the packet identifier (PID) of the SCTE-35 stream in the transport stream.
  , scte35Source :: Core.Maybe Types.M2tsScte35Source
    -- ^ For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE). Also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml). Also enable ESAM SCTE-35 (include the property scte35Esam).
  , segmentationMarkers :: Core.Maybe Types.M2tsSegmentationMarkers
    -- ^ Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
  , segmentationStyle :: Core.Maybe Types.M2tsSegmentationStyle
    -- ^ The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
  , segmentationTime :: Core.Maybe Core.Double
    -- ^ Specify the length, in seconds, of each segment. Required unless markers is set to _none_.
  , timedMetadataPid :: Core.Maybe Core.Natural
    -- ^ Specify the packet identifier (PID) for timed metadata in this output. Default is 502.
  , transportStreamId :: Core.Maybe Core.Natural
    -- ^ Specify the ID for the transport stream itself in the program map table for this output. Transport stream IDs and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
  , videoPid :: Core.Maybe Core.Natural
    -- ^ Specify the packet identifier (PID) of the elementary video stream in the transport stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'M2tsSettings' value with any optional fields omitted.
mkM2tsSettings
    :: M2tsSettings
mkM2tsSettings
  = M2tsSettings'{audioBufferModel = Core.Nothing,
                  audioDuration = Core.Nothing, audioFramesPerPes = Core.Nothing,
                  audioPids = Core.Nothing, bitrate = Core.Nothing,
                  bufferModel = Core.Nothing, dvbNitSettings = Core.Nothing,
                  dvbSdtSettings = Core.Nothing, dvbSubPids = Core.Nothing,
                  dvbTdtSettings = Core.Nothing, dvbTeletextPid = Core.Nothing,
                  ebpAudioInterval = Core.Nothing, ebpPlacement = Core.Nothing,
                  esRateInPes = Core.Nothing, forceTsVideoEbpOrder = Core.Nothing,
                  fragmentTime = Core.Nothing, maxPcrInterval = Core.Nothing,
                  minEbpInterval = Core.Nothing, nielsenId3 = Core.Nothing,
                  nullPacketBitrate = Core.Nothing, patInterval = Core.Nothing,
                  pcrControl = Core.Nothing, pcrPid = Core.Nothing,
                  pmtInterval = Core.Nothing, pmtPid = Core.Nothing,
                  privateMetadataPid = Core.Nothing, programNumber = Core.Nothing,
                  rateMode = Core.Nothing, scte35Esam = Core.Nothing,
                  scte35Pid = Core.Nothing, scte35Source = Core.Nothing,
                  segmentationMarkers = Core.Nothing,
                  segmentationStyle = Core.Nothing, segmentationTime = Core.Nothing,
                  timedMetadataPid = Core.Nothing, transportStreamId = Core.Nothing,
                  videoPid = Core.Nothing}

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
--
-- /Note:/ Consider using 'audioBufferModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssAudioBufferModel :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsAudioBufferModel)
mssAudioBufferModel = Lens.field @"audioBufferModel"
{-# INLINEABLE mssAudioBufferModel #-}
{-# DEPRECATED audioBufferModel "Use generic-lens or generic-optics with 'audioBufferModel' instead"  #-}

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssAudioDuration :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsAudioDuration)
mssAudioDuration = Lens.field @"audioDuration"
{-# INLINEABLE mssAudioDuration #-}
{-# DEPRECATED audioDuration "Use generic-lens or generic-optics with 'audioDuration' instead"  #-}

-- | The number of audio frames to insert for each PES packet.
--
-- /Note:/ Consider using 'audioFramesPerPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssAudioFramesPerPes :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssAudioFramesPerPes = Lens.field @"audioFramesPerPes"
{-# INLINEABLE mssAudioFramesPerPes #-}
{-# DEPRECATED audioFramesPerPes "Use generic-lens or generic-optics with 'audioFramesPerPes' instead"  #-}

-- | Specify the packet identifiers (PIDs) for any elementary audio streams you include in this output. Specify multiple PIDs as a JSON array. Default is the range 482-492.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssAudioPids :: Lens.Lens' M2tsSettings (Core.Maybe [Core.Natural])
mssAudioPids = Lens.field @"audioPids"
{-# INLINEABLE mssAudioPids #-}
{-# DEPRECATED audioPids "Use generic-lens or generic-optics with 'audioPids' instead"  #-}

-- | Specify the output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssBitrate :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssBitrate = Lens.field @"bitrate"
{-# INLINEABLE mssBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
--
-- /Note:/ Consider using 'bufferModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssBufferModel :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsBufferModel)
mssBufferModel = Lens.field @"bufferModel"
{-# INLINEABLE mssBufferModel #-}
{-# DEPRECATED bufferModel "Use generic-lens or generic-optics with 'bufferModel' instead"  #-}

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbNitSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssDvbNitSettings :: Lens.Lens' M2tsSettings (Core.Maybe Types.DvbNitSettings)
mssDvbNitSettings = Lens.field @"dvbNitSettings"
{-# INLINEABLE mssDvbNitSettings #-}
{-# DEPRECATED dvbNitSettings "Use generic-lens or generic-optics with 'dvbNitSettings' instead"  #-}

-- | Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbSdtSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssDvbSdtSettings :: Lens.Lens' M2tsSettings (Core.Maybe Types.DvbSdtSettings)
mssDvbSdtSettings = Lens.field @"dvbSdtSettings"
{-# INLINEABLE mssDvbSdtSettings #-}
{-# DEPRECATED dvbSdtSettings "Use generic-lens or generic-optics with 'dvbSdtSettings' instead"  #-}

-- | Specify the packet identifiers (PIDs) for DVB subtitle data included in this output. Specify multiple PIDs as a JSON array. Default is the range 460-479.
--
-- /Note:/ Consider using 'dvbSubPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssDvbSubPids :: Lens.Lens' M2tsSettings (Core.Maybe [Core.Natural])
mssDvbSubPids = Lens.field @"dvbSubPids"
{-# INLINEABLE mssDvbSubPids #-}
{-# DEPRECATED dvbSubPids "Use generic-lens or generic-optics with 'dvbSubPids' instead"  #-}

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbTdtSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssDvbTdtSettings :: Lens.Lens' M2tsSettings (Core.Maybe Types.DvbTdtSettings)
mssDvbTdtSettings = Lens.field @"dvbTdtSettings"
{-# INLINEABLE mssDvbTdtSettings #-}
{-# DEPRECATED dvbTdtSettings "Use generic-lens or generic-optics with 'dvbTdtSettings' instead"  #-}

-- | Specify the packet identifier (PID) for DVB teletext data you include in this output. Default is 499.
--
-- /Note:/ Consider using 'dvbTeletextPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssDvbTeletextPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssDvbTeletextPid = Lens.field @"dvbTeletextPid"
{-# INLINEABLE mssDvbTeletextPid #-}
{-# DEPRECATED dvbTeletextPid "Use generic-lens or generic-optics with 'dvbTeletextPid' instead"  #-}

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- /Note:/ Consider using 'ebpAudioInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssEbpAudioInterval :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsEbpAudioInterval)
mssEbpAudioInterval = Lens.field @"ebpAudioInterval"
{-# INLINEABLE mssEbpAudioInterval #-}
{-# DEPRECATED ebpAudioInterval "Use generic-lens or generic-optics with 'ebpAudioInterval' instead"  #-}

-- | Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- /Note:/ Consider using 'ebpPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssEbpPlacement :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsEbpPlacement)
mssEbpPlacement = Lens.field @"ebpPlacement"
{-# INLINEABLE mssEbpPlacement #-}
{-# DEPRECATED ebpPlacement "Use generic-lens or generic-optics with 'ebpPlacement' instead"  #-}

-- | Controls whether to include the ES Rate field in the PES header.
--
-- /Note:/ Consider using 'esRateInPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssEsRateInPes :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsEsRateInPes)
mssEsRateInPes = Lens.field @"esRateInPes"
{-# INLINEABLE mssEsRateInPes #-}
{-# DEPRECATED esRateInPes "Use generic-lens or generic-optics with 'esRateInPes' instead"  #-}

-- | Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
--
-- /Note:/ Consider using 'forceTsVideoEbpOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssForceTsVideoEbpOrder :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsForceTsVideoEbpOrder)
mssForceTsVideoEbpOrder = Lens.field @"forceTsVideoEbpOrder"
{-# INLINEABLE mssForceTsVideoEbpOrder #-}
{-# DEPRECATED forceTsVideoEbpOrder "Use generic-lens or generic-optics with 'forceTsVideoEbpOrder' instead"  #-}

-- | The length, in seconds, of each fragment. Only used with EBP markers.
--
-- /Note:/ Consider using 'fragmentTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssFragmentTime :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
mssFragmentTime = Lens.field @"fragmentTime"
{-# INLINEABLE mssFragmentTime #-}
{-# DEPRECATED fragmentTime "Use generic-lens or generic-optics with 'fragmentTime' instead"  #-}

-- | Specify the maximum time, in milliseconds, between Program Clock References (PCRs) inserted into the transport stream.
--
-- /Note:/ Consider using 'maxPcrInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMaxPcrInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssMaxPcrInterval = Lens.field @"maxPcrInterval"
{-# INLINEABLE mssMaxPcrInterval #-}
{-# DEPRECATED maxPcrInterval "Use generic-lens or generic-optics with 'maxPcrInterval' instead"  #-}

-- | When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
--
-- /Note:/ Consider using 'minEbpInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMinEbpInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssMinEbpInterval = Lens.field @"minEbpInterval"
{-# INLINEABLE mssMinEbpInterval #-}
{-# DEPRECATED minEbpInterval "Use generic-lens or generic-optics with 'minEbpInterval' instead"  #-}

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssNielsenId3 :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsNielsenId3)
mssNielsenId3 = Lens.field @"nielsenId3"
{-# INLINEABLE mssNielsenId3 #-}
{-# DEPRECATED nielsenId3 "Use generic-lens or generic-optics with 'nielsenId3' instead"  #-}

-- | Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
--
-- /Note:/ Consider using 'nullPacketBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssNullPacketBitrate :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
mssNullPacketBitrate = Lens.field @"nullPacketBitrate"
{-# INLINEABLE mssNullPacketBitrate #-}
{-# DEPRECATED nullPacketBitrate "Use generic-lens or generic-optics with 'nullPacketBitrate' instead"  #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'patInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPatInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssPatInterval = Lens.field @"patInterval"
{-# INLINEABLE mssPatInterval #-}
{-# DEPRECATED patInterval "Use generic-lens or generic-optics with 'patInterval' instead"  #-}

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- /Note:/ Consider using 'pcrControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPcrControl :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsPcrControl)
mssPcrControl = Lens.field @"pcrControl"
{-# INLINEABLE mssPcrControl #-}
{-# DEPRECATED pcrControl "Use generic-lens or generic-optics with 'pcrControl' instead"  #-}

-- | Specify the packet identifier (PID) for the program clock reference (PCR) in this output. If you do not specify a value, the service will use the value for Video PID (VideoPid).
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPcrPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssPcrPid = Lens.field @"pcrPid"
{-# INLINEABLE mssPcrPid #-}
{-# DEPRECATED pcrPid "Use generic-lens or generic-optics with 'pcrPid' instead"  #-}

-- | Specify the number of milliseconds between instances of the program map table (PMT) in the output transport stream.
--
-- /Note:/ Consider using 'pmtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPmtInterval :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssPmtInterval = Lens.field @"pmtInterval"
{-# INLINEABLE mssPmtInterval #-}
{-# DEPRECATED pmtInterval "Use generic-lens or generic-optics with 'pmtInterval' instead"  #-}

-- | Specify the packet identifier (PID) for the program map table (PMT) itself. Default is 480.
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPmtPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssPmtPid = Lens.field @"pmtPid"
{-# INLINEABLE mssPmtPid #-}
{-# DEPRECATED pmtPid "Use generic-lens or generic-optics with 'pmtPid' instead"  #-}

-- | Specify the packet identifier (PID) of the private metadata stream. Default is 503.
--
-- /Note:/ Consider using 'privateMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPrivateMetadataPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssPrivateMetadataPid = Lens.field @"privateMetadataPid"
{-# INLINEABLE mssPrivateMetadataPid #-}
{-# DEPRECATED privateMetadataPid "Use generic-lens or generic-optics with 'privateMetadataPid' instead"  #-}

-- | Use Program number (programNumber) to specify the program number used in the program map table (PMT) for this output. Default is 1. Program numbers and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssProgramNumber :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssProgramNumber = Lens.field @"programNumber"
{-# INLINEABLE mssProgramNumber #-}
{-# DEPRECATED programNumber "Use generic-lens or generic-optics with 'programNumber' instead"  #-}

-- | When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
--
-- /Note:/ Consider using 'rateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssRateMode :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsRateMode)
mssRateMode = Lens.field @"rateMode"
{-# INLINEABLE mssRateMode #-}
{-# DEPRECATED rateMode "Use generic-lens or generic-optics with 'rateMode' instead"  #-}

-- | Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- /Note:/ Consider using 'scte35Esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScte35Esam :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsScte35Esam)
mssScte35Esam = Lens.field @"scte35Esam"
{-# INLINEABLE mssScte35Esam #-}
{-# DEPRECATED scte35Esam "Use generic-lens or generic-optics with 'scte35Esam' instead"  #-}

-- | Specify the packet identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScte35Pid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssScte35Pid = Lens.field @"scte35Pid"
{-# INLINEABLE mssScte35Pid #-}
{-# DEPRECATED scte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead"  #-}

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE). Also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml). Also enable ESAM SCTE-35 (include the property scte35Esam).
--
-- /Note:/ Consider using 'scte35Source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScte35Source :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsScte35Source)
mssScte35Source = Lens.field @"scte35Source"
{-# INLINEABLE mssScte35Source #-}
{-# DEPRECATED scte35Source "Use generic-lens or generic-optics with 'scte35Source' instead"  #-}

-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
--
-- /Note:/ Consider using 'segmentationMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssSegmentationMarkers :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsSegmentationMarkers)
mssSegmentationMarkers = Lens.field @"segmentationMarkers"
{-# INLINEABLE mssSegmentationMarkers #-}
{-# DEPRECATED segmentationMarkers "Use generic-lens or generic-optics with 'segmentationMarkers' instead"  #-}

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
--
-- /Note:/ Consider using 'segmentationStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssSegmentationStyle :: Lens.Lens' M2tsSettings (Core.Maybe Types.M2tsSegmentationStyle)
mssSegmentationStyle = Lens.field @"segmentationStyle"
{-# INLINEABLE mssSegmentationStyle #-}
{-# DEPRECATED segmentationStyle "Use generic-lens or generic-optics with 'segmentationStyle' instead"  #-}

-- | Specify the length, in seconds, of each segment. Required unless markers is set to _none_.
--
-- /Note:/ Consider using 'segmentationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssSegmentationTime :: Lens.Lens' M2tsSettings (Core.Maybe Core.Double)
mssSegmentationTime = Lens.field @"segmentationTime"
{-# INLINEABLE mssSegmentationTime #-}
{-# DEPRECATED segmentationTime "Use generic-lens or generic-optics with 'segmentationTime' instead"  #-}

-- | Specify the packet identifier (PID) for timed metadata in this output. Default is 502.
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTimedMetadataPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssTimedMetadataPid = Lens.field @"timedMetadataPid"
{-# INLINEABLE mssTimedMetadataPid #-}
{-# DEPRECATED timedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead"  #-}

-- | Specify the ID for the transport stream itself in the program map table for this output. Transport stream IDs and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTransportStreamId :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssTransportStreamId = Lens.field @"transportStreamId"
{-# INLINEABLE mssTransportStreamId #-}
{-# DEPRECATED transportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead"  #-}

-- | Specify the packet identifier (PID) of the elementary video stream in the transport stream.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssVideoPid :: Lens.Lens' M2tsSettings (Core.Maybe Core.Natural)
mssVideoPid = Lens.field @"videoPid"
{-# INLINEABLE mssVideoPid #-}
{-# DEPRECATED videoPid "Use generic-lens or generic-optics with 'videoPid' instead"  #-}

instance Core.FromJSON M2tsSettings where
        toJSON M2tsSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioBufferModel" Core..=) Core.<$> audioBufferModel,
                  ("audioDuration" Core..=) Core.<$> audioDuration,
                  ("audioFramesPerPes" Core..=) Core.<$> audioFramesPerPes,
                  ("audioPids" Core..=) Core.<$> audioPids,
                  ("bitrate" Core..=) Core.<$> bitrate,
                  ("bufferModel" Core..=) Core.<$> bufferModel,
                  ("dvbNitSettings" Core..=) Core.<$> dvbNitSettings,
                  ("dvbSdtSettings" Core..=) Core.<$> dvbSdtSettings,
                  ("dvbSubPids" Core..=) Core.<$> dvbSubPids,
                  ("dvbTdtSettings" Core..=) Core.<$> dvbTdtSettings,
                  ("dvbTeletextPid" Core..=) Core.<$> dvbTeletextPid,
                  ("ebpAudioInterval" Core..=) Core.<$> ebpAudioInterval,
                  ("ebpPlacement" Core..=) Core.<$> ebpPlacement,
                  ("esRateInPes" Core..=) Core.<$> esRateInPes,
                  ("forceTsVideoEbpOrder" Core..=) Core.<$> forceTsVideoEbpOrder,
                  ("fragmentTime" Core..=) Core.<$> fragmentTime,
                  ("maxPcrInterval" Core..=) Core.<$> maxPcrInterval,
                  ("minEbpInterval" Core..=) Core.<$> minEbpInterval,
                  ("nielsenId3" Core..=) Core.<$> nielsenId3,
                  ("nullPacketBitrate" Core..=) Core.<$> nullPacketBitrate,
                  ("patInterval" Core..=) Core.<$> patInterval,
                  ("pcrControl" Core..=) Core.<$> pcrControl,
                  ("pcrPid" Core..=) Core.<$> pcrPid,
                  ("pmtInterval" Core..=) Core.<$> pmtInterval,
                  ("pmtPid" Core..=) Core.<$> pmtPid,
                  ("privateMetadataPid" Core..=) Core.<$> privateMetadataPid,
                  ("programNumber" Core..=) Core.<$> programNumber,
                  ("rateMode" Core..=) Core.<$> rateMode,
                  ("scte35Esam" Core..=) Core.<$> scte35Esam,
                  ("scte35Pid" Core..=) Core.<$> scte35Pid,
                  ("scte35Source" Core..=) Core.<$> scte35Source,
                  ("segmentationMarkers" Core..=) Core.<$> segmentationMarkers,
                  ("segmentationStyle" Core..=) Core.<$> segmentationStyle,
                  ("segmentationTime" Core..=) Core.<$> segmentationTime,
                  ("timedMetadataPid" Core..=) Core.<$> timedMetadataPid,
                  ("transportStreamId" Core..=) Core.<$> transportStreamId,
                  ("videoPid" Core..=) Core.<$> videoPid])

instance Core.FromJSON M2tsSettings where
        parseJSON
          = Core.withObject "M2tsSettings" Core.$
              \ x ->
                M2tsSettings' Core.<$>
                  (x Core..:? "audioBufferModel") Core.<*> x Core..:? "audioDuration"
                    Core.<*> x Core..:? "audioFramesPerPes"
                    Core.<*> x Core..:? "audioPids"
                    Core.<*> x Core..:? "bitrate"
                    Core.<*> x Core..:? "bufferModel"
                    Core.<*> x Core..:? "dvbNitSettings"
                    Core.<*> x Core..:? "dvbSdtSettings"
                    Core.<*> x Core..:? "dvbSubPids"
                    Core.<*> x Core..:? "dvbTdtSettings"
                    Core.<*> x Core..:? "dvbTeletextPid"
                    Core.<*> x Core..:? "ebpAudioInterval"
                    Core.<*> x Core..:? "ebpPlacement"
                    Core.<*> x Core..:? "esRateInPes"
                    Core.<*> x Core..:? "forceTsVideoEbpOrder"
                    Core.<*> x Core..:? "fragmentTime"
                    Core.<*> x Core..:? "maxPcrInterval"
                    Core.<*> x Core..:? "minEbpInterval"
                    Core.<*> x Core..:? "nielsenId3"
                    Core.<*> x Core..:? "nullPacketBitrate"
                    Core.<*> x Core..:? "patInterval"
                    Core.<*> x Core..:? "pcrControl"
                    Core.<*> x Core..:? "pcrPid"
                    Core.<*> x Core..:? "pmtInterval"
                    Core.<*> x Core..:? "pmtPid"
                    Core.<*> x Core..:? "privateMetadataPid"
                    Core.<*> x Core..:? "programNumber"
                    Core.<*> x Core..:? "rateMode"
                    Core.<*> x Core..:? "scte35Esam"
                    Core.<*> x Core..:? "scte35Pid"
                    Core.<*> x Core..:? "scte35Source"
                    Core.<*> x Core..:? "segmentationMarkers"
                    Core.<*> x Core..:? "segmentationStyle"
                    Core.<*> x Core..:? "segmentationTime"
                    Core.<*> x Core..:? "timedMetadataPid"
                    Core.<*> x Core..:? "transportStreamId"
                    Core.<*> x Core..:? "videoPid"
