-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsSettings
  ( M2tsSettings (..),

    -- * Smart constructor
    mkM2tsSettings,

    -- * Lenses
    mPmtPid,
    mVideoPid,
    mBufferModel,
    mProgramNumber,
    mScte35Pid,
    mMinEbpInterval,
    mTransportStreamId,
    mMaxPcrInterval,
    mFragmentTime,
    mPrivateMetadataPid,
    mScte35Esam,
    mAudioDuration,
    mPmtInterval,
    mDvbSdtSettings,
    mNullPacketBitrate,
    mAudioBufferModel,
    mTimedMetadataPid,
    mAudioFramesPerPes,
    mPcrPid,
    mSegmentationMarkers,
    mDvbSubPids,
    mScte35Source,
    mPatInterval,
    mForceTsVideoEbpOrder,
    mEsRateInPes,
    mBitrate,
    mAudioPids,
    mDvbTeletextPid,
    mNielsenId3,
    mSegmentationTime,
    mEbpAudioInterval,
    mDvbNitSettings,
    mPcrControl,
    mEbpPlacement,
    mRateMode,
    mSegmentationStyle,
    mDvbTdtSettings,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
--
-- /See:/ 'mkM2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'
  { pmtPid ::
      Lude.Maybe Lude.Natural,
    videoPid :: Lude.Maybe Lude.Natural,
    bufferModel :: Lude.Maybe M2tsBufferModel,
    programNumber :: Lude.Maybe Lude.Natural,
    scte35Pid :: Lude.Maybe Lude.Natural,
    minEbpInterval :: Lude.Maybe Lude.Natural,
    transportStreamId :: Lude.Maybe Lude.Natural,
    maxPcrInterval :: Lude.Maybe Lude.Natural,
    fragmentTime :: Lude.Maybe Lude.Double,
    privateMetadataPid :: Lude.Maybe Lude.Natural,
    scte35Esam :: Lude.Maybe M2tsScte35Esam,
    audioDuration :: Lude.Maybe M2tsAudioDuration,
    pmtInterval :: Lude.Maybe Lude.Natural,
    dvbSdtSettings :: Lude.Maybe DvbSdtSettings,
    nullPacketBitrate :: Lude.Maybe Lude.Double,
    audioBufferModel :: Lude.Maybe M2tsAudioBufferModel,
    timedMetadataPid :: Lude.Maybe Lude.Natural,
    audioFramesPerPes :: Lude.Maybe Lude.Natural,
    pcrPid :: Lude.Maybe Lude.Natural,
    segmentationMarkers :: Lude.Maybe M2tsSegmentationMarkers,
    dvbSubPids :: Lude.Maybe [Lude.Natural],
    scte35Source :: Lude.Maybe M2tsScte35Source,
    patInterval :: Lude.Maybe Lude.Natural,
    forceTsVideoEbpOrder :: Lude.Maybe M2tsForceTsVideoEbpOrder,
    esRateInPes :: Lude.Maybe M2tsEsRateInPes,
    bitrate :: Lude.Maybe Lude.Natural,
    audioPids :: Lude.Maybe [Lude.Natural],
    dvbTeletextPid :: Lude.Maybe Lude.Natural,
    nielsenId3 :: Lude.Maybe M2tsNielsenId3,
    segmentationTime :: Lude.Maybe Lude.Double,
    ebpAudioInterval :: Lude.Maybe M2tsEbpAudioInterval,
    dvbNitSettings :: Lude.Maybe DvbNitSettings,
    pcrControl :: Lude.Maybe M2tsPcrControl,
    ebpPlacement :: Lude.Maybe M2tsEbpPlacement,
    rateMode :: Lude.Maybe M2tsRateMode,
    segmentationStyle :: Lude.Maybe M2tsSegmentationStyle,
    dvbTdtSettings :: Lude.Maybe DvbTdtSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'M2tsSettings' with the minimum fields required to make a request.
--
-- * 'audioBufferModel' - Selects between the DVB and ATSC buffer models for Dolby Digital audio.
-- * 'audioDuration' - Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
-- * 'audioFramesPerPes' - The number of audio frames to insert for each PES packet.
-- * 'audioPids' - Specify the packet identifiers (PIDs) for any elementary audio streams you include in this output. Specify multiple PIDs as a JSON array. Default is the range 482-492.
-- * 'bitrate' - Specify the output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
-- * 'bufferModel' - Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
-- * 'dvbNitSettings' - Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
-- * 'dvbSdtSettings' - Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
-- * 'dvbSubPids' - Specify the packet identifiers (PIDs) for DVB subtitle data included in this output. Specify multiple PIDs as a JSON array. Default is the range 460-479.
-- * 'dvbTdtSettings' - Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
-- * 'dvbTeletextPid' - Specify the packet identifier (PID) for DVB teletext data you include in this output. Default is 499.
-- * 'ebpAudioInterval' - When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
-- * 'ebpPlacement' - Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
-- * 'esRateInPes' - Controls whether to include the ES Rate field in the PES header.
-- * 'forceTsVideoEbpOrder' - Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
-- * 'fragmentTime' - The length, in seconds, of each fragment. Only used with EBP markers.
-- * 'maxPcrInterval' - Specify the maximum time, in milliseconds, between Program Clock References (PCRs) inserted into the transport stream.
-- * 'minEbpInterval' - When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
-- * 'nielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
-- * 'nullPacketBitrate' - Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
-- * 'patInterval' - The number of milliseconds between instances of this table in the output transport stream.
-- * 'pcrControl' - When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
-- * 'pcrPid' - Specify the packet identifier (PID) for the program clock reference (PCR) in this output. If you do not specify a value, the service will use the value for Video PID (VideoPid).
-- * 'pmtInterval' - Specify the number of milliseconds between instances of the program map table (PMT) in the output transport stream.
-- * 'pmtPid' - Specify the packet identifier (PID) for the program map table (PMT) itself. Default is 480.
-- * 'privateMetadataPid' - Specify the packet identifier (PID) of the private metadata stream. Default is 503.
-- * 'programNumber' - Use Program number (programNumber) to specify the program number used in the program map table (PMT) for this output. Default is 1. Program numbers and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
-- * 'rateMode' - When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
-- * 'scte35Esam' - Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
-- * 'scte35Pid' - Specify the packet identifier (PID) of the SCTE-35 stream in the transport stream.
-- * 'scte35Source' - For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE). Also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml). Also enable ESAM SCTE-35 (include the property scte35Esam).
-- * 'segmentationMarkers' - Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
-- * 'segmentationStyle' - The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
-- * 'segmentationTime' - Specify the length, in seconds, of each segment. Required unless markers is set to _none_.
-- * 'timedMetadataPid' - Specify the packet identifier (PID) for timed metadata in this output. Default is 502.
-- * 'transportStreamId' - Specify the ID for the transport stream itself in the program map table for this output. Transport stream IDs and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
-- * 'videoPid' - Specify the packet identifier (PID) of the elementary video stream in the transport stream.
mkM2tsSettings ::
  M2tsSettings
mkM2tsSettings =
  M2tsSettings'
    { pmtPid = Lude.Nothing,
      videoPid = Lude.Nothing,
      bufferModel = Lude.Nothing,
      programNumber = Lude.Nothing,
      scte35Pid = Lude.Nothing,
      minEbpInterval = Lude.Nothing,
      transportStreamId = Lude.Nothing,
      maxPcrInterval = Lude.Nothing,
      fragmentTime = Lude.Nothing,
      privateMetadataPid = Lude.Nothing,
      scte35Esam = Lude.Nothing,
      audioDuration = Lude.Nothing,
      pmtInterval = Lude.Nothing,
      dvbSdtSettings = Lude.Nothing,
      nullPacketBitrate = Lude.Nothing,
      audioBufferModel = Lude.Nothing,
      timedMetadataPid = Lude.Nothing,
      audioFramesPerPes = Lude.Nothing,
      pcrPid = Lude.Nothing,
      segmentationMarkers = Lude.Nothing,
      dvbSubPids = Lude.Nothing,
      scte35Source = Lude.Nothing,
      patInterval = Lude.Nothing,
      forceTsVideoEbpOrder = Lude.Nothing,
      esRateInPes = Lude.Nothing,
      bitrate = Lude.Nothing,
      audioPids = Lude.Nothing,
      dvbTeletextPid = Lude.Nothing,
      nielsenId3 = Lude.Nothing,
      segmentationTime = Lude.Nothing,
      ebpAudioInterval = Lude.Nothing,
      dvbNitSettings = Lude.Nothing,
      pcrControl = Lude.Nothing,
      ebpPlacement = Lude.Nothing,
      rateMode = Lude.Nothing,
      segmentationStyle = Lude.Nothing,
      dvbTdtSettings = Lude.Nothing
    }

-- | Specify the packet identifier (PID) for the program map table (PMT) itself. Default is 480.
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPmtPid :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mPmtPid = Lens.lens (pmtPid :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {pmtPid = a} :: M2tsSettings)
{-# DEPRECATED mPmtPid "Use generic-lens or generic-optics with 'pmtPid' instead." #-}

-- | Specify the packet identifier (PID) of the elementary video stream in the transport stream.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mVideoPid :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mVideoPid = Lens.lens (videoPid :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {videoPid = a} :: M2tsSettings)
{-# DEPRECATED mVideoPid "Use generic-lens or generic-optics with 'videoPid' instead." #-}

-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
--
-- /Note:/ Consider using 'bufferModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBufferModel :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsBufferModel)
mBufferModel = Lens.lens (bufferModel :: M2tsSettings -> Lude.Maybe M2tsBufferModel) (\s a -> s {bufferModel = a} :: M2tsSettings)
{-# DEPRECATED mBufferModel "Use generic-lens or generic-optics with 'bufferModel' instead." #-}

-- | Use Program number (programNumber) to specify the program number used in the program map table (PMT) for this output. Default is 1. Program numbers and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mProgramNumber :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mProgramNumber = Lens.lens (programNumber :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {programNumber = a} :: M2tsSettings)
{-# DEPRECATED mProgramNumber "Use generic-lens or generic-optics with 'programNumber' instead." #-}

-- | Specify the packet identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mScte35Pid :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mScte35Pid = Lens.lens (scte35Pid :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {scte35Pid = a} :: M2tsSettings)
{-# DEPRECATED mScte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead." #-}

-- | When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
--
-- /Note:/ Consider using 'minEbpInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMinEbpInterval :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mMinEbpInterval = Lens.lens (minEbpInterval :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {minEbpInterval = a} :: M2tsSettings)
{-# DEPRECATED mMinEbpInterval "Use generic-lens or generic-optics with 'minEbpInterval' instead." #-}

-- | Specify the ID for the transport stream itself in the program map table for this output. Transport stream IDs and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTransportStreamId :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mTransportStreamId = Lens.lens (transportStreamId :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {transportStreamId = a} :: M2tsSettings)
{-# DEPRECATED mTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

-- | Specify the maximum time, in milliseconds, between Program Clock References (PCRs) inserted into the transport stream.
--
-- /Note:/ Consider using 'maxPcrInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMaxPcrInterval :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mMaxPcrInterval = Lens.lens (maxPcrInterval :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {maxPcrInterval = a} :: M2tsSettings)
{-# DEPRECATED mMaxPcrInterval "Use generic-lens or generic-optics with 'maxPcrInterval' instead." #-}

-- | The length, in seconds, of each fragment. Only used with EBP markers.
--
-- /Note:/ Consider using 'fragmentTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mFragmentTime :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Double)
mFragmentTime = Lens.lens (fragmentTime :: M2tsSettings -> Lude.Maybe Lude.Double) (\s a -> s {fragmentTime = a} :: M2tsSettings)
{-# DEPRECATED mFragmentTime "Use generic-lens or generic-optics with 'fragmentTime' instead." #-}

-- | Specify the packet identifier (PID) of the private metadata stream. Default is 503.
--
-- /Note:/ Consider using 'privateMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPrivateMetadataPid :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mPrivateMetadataPid = Lens.lens (privateMetadataPid :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {privateMetadataPid = a} :: M2tsSettings)
{-# DEPRECATED mPrivateMetadataPid "Use generic-lens or generic-optics with 'privateMetadataPid' instead." #-}

-- | Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- /Note:/ Consider using 'scte35Esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mScte35Esam :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsScte35Esam)
mScte35Esam = Lens.lens (scte35Esam :: M2tsSettings -> Lude.Maybe M2tsScte35Esam) (\s a -> s {scte35Esam = a} :: M2tsSettings)
{-# DEPRECATED mScte35Esam "Use generic-lens or generic-optics with 'scte35Esam' instead." #-}

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioDuration :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsAudioDuration)
mAudioDuration = Lens.lens (audioDuration :: M2tsSettings -> Lude.Maybe M2tsAudioDuration) (\s a -> s {audioDuration = a} :: M2tsSettings)
{-# DEPRECATED mAudioDuration "Use generic-lens or generic-optics with 'audioDuration' instead." #-}

-- | Specify the number of milliseconds between instances of the program map table (PMT) in the output transport stream.
--
-- /Note:/ Consider using 'pmtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPmtInterval :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mPmtInterval = Lens.lens (pmtInterval :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {pmtInterval = a} :: M2tsSettings)
{-# DEPRECATED mPmtInterval "Use generic-lens or generic-optics with 'pmtInterval' instead." #-}

-- | Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbSdtSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbSdtSettings :: Lens.Lens' M2tsSettings (Lude.Maybe DvbSdtSettings)
mDvbSdtSettings = Lens.lens (dvbSdtSettings :: M2tsSettings -> Lude.Maybe DvbSdtSettings) (\s a -> s {dvbSdtSettings = a} :: M2tsSettings)
{-# DEPRECATED mDvbSdtSettings "Use generic-lens or generic-optics with 'dvbSdtSettings' instead." #-}

-- | Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
--
-- /Note:/ Consider using 'nullPacketBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mNullPacketBitrate :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Double)
mNullPacketBitrate = Lens.lens (nullPacketBitrate :: M2tsSettings -> Lude.Maybe Lude.Double) (\s a -> s {nullPacketBitrate = a} :: M2tsSettings)
{-# DEPRECATED mNullPacketBitrate "Use generic-lens or generic-optics with 'nullPacketBitrate' instead." #-}

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
--
-- /Note:/ Consider using 'audioBufferModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioBufferModel :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsAudioBufferModel)
mAudioBufferModel = Lens.lens (audioBufferModel :: M2tsSettings -> Lude.Maybe M2tsAudioBufferModel) (\s a -> s {audioBufferModel = a} :: M2tsSettings)
{-# DEPRECATED mAudioBufferModel "Use generic-lens or generic-optics with 'audioBufferModel' instead." #-}

-- | Specify the packet identifier (PID) for timed metadata in this output. Default is 502.
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTimedMetadataPid :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mTimedMetadataPid = Lens.lens (timedMetadataPid :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {timedMetadataPid = a} :: M2tsSettings)
{-# DEPRECATED mTimedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead." #-}

-- | The number of audio frames to insert for each PES packet.
--
-- /Note:/ Consider using 'audioFramesPerPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioFramesPerPes :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mAudioFramesPerPes = Lens.lens (audioFramesPerPes :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {audioFramesPerPes = a} :: M2tsSettings)
{-# DEPRECATED mAudioFramesPerPes "Use generic-lens or generic-optics with 'audioFramesPerPes' instead." #-}

-- | Specify the packet identifier (PID) for the program clock reference (PCR) in this output. If you do not specify a value, the service will use the value for Video PID (VideoPid).
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPcrPid :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mPcrPid = Lens.lens (pcrPid :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {pcrPid = a} :: M2tsSettings)
{-# DEPRECATED mPcrPid "Use generic-lens or generic-optics with 'pcrPid' instead." #-}

-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
--
-- /Note:/ Consider using 'segmentationMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSegmentationMarkers :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsSegmentationMarkers)
mSegmentationMarkers = Lens.lens (segmentationMarkers :: M2tsSettings -> Lude.Maybe M2tsSegmentationMarkers) (\s a -> s {segmentationMarkers = a} :: M2tsSettings)
{-# DEPRECATED mSegmentationMarkers "Use generic-lens or generic-optics with 'segmentationMarkers' instead." #-}

-- | Specify the packet identifiers (PIDs) for DVB subtitle data included in this output. Specify multiple PIDs as a JSON array. Default is the range 460-479.
--
-- /Note:/ Consider using 'dvbSubPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbSubPids :: Lens.Lens' M2tsSettings (Lude.Maybe [Lude.Natural])
mDvbSubPids = Lens.lens (dvbSubPids :: M2tsSettings -> Lude.Maybe [Lude.Natural]) (\s a -> s {dvbSubPids = a} :: M2tsSettings)
{-# DEPRECATED mDvbSubPids "Use generic-lens or generic-optics with 'dvbSubPids' instead." #-}

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE). Also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml). Also enable ESAM SCTE-35 (include the property scte35Esam).
--
-- /Note:/ Consider using 'scte35Source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mScte35Source :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsScte35Source)
mScte35Source = Lens.lens (scte35Source :: M2tsSettings -> Lude.Maybe M2tsScte35Source) (\s a -> s {scte35Source = a} :: M2tsSettings)
{-# DEPRECATED mScte35Source "Use generic-lens or generic-optics with 'scte35Source' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'patInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPatInterval :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mPatInterval = Lens.lens (patInterval :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {patInterval = a} :: M2tsSettings)
{-# DEPRECATED mPatInterval "Use generic-lens or generic-optics with 'patInterval' instead." #-}

-- | Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
--
-- /Note:/ Consider using 'forceTsVideoEbpOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mForceTsVideoEbpOrder :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsForceTsVideoEbpOrder)
mForceTsVideoEbpOrder = Lens.lens (forceTsVideoEbpOrder :: M2tsSettings -> Lude.Maybe M2tsForceTsVideoEbpOrder) (\s a -> s {forceTsVideoEbpOrder = a} :: M2tsSettings)
{-# DEPRECATED mForceTsVideoEbpOrder "Use generic-lens or generic-optics with 'forceTsVideoEbpOrder' instead." #-}

-- | Controls whether to include the ES Rate field in the PES header.
--
-- /Note:/ Consider using 'esRateInPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEsRateInPes :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsEsRateInPes)
mEsRateInPes = Lens.lens (esRateInPes :: M2tsSettings -> Lude.Maybe M2tsEsRateInPes) (\s a -> s {esRateInPes = a} :: M2tsSettings)
{-# DEPRECATED mEsRateInPes "Use generic-lens or generic-optics with 'esRateInPes' instead." #-}

-- | Specify the output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBitrate :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mBitrate = Lens.lens (bitrate :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: M2tsSettings)
{-# DEPRECATED mBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Specify the packet identifiers (PIDs) for any elementary audio streams you include in this output. Specify multiple PIDs as a JSON array. Default is the range 482-492.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioPids :: Lens.Lens' M2tsSettings (Lude.Maybe [Lude.Natural])
mAudioPids = Lens.lens (audioPids :: M2tsSettings -> Lude.Maybe [Lude.Natural]) (\s a -> s {audioPids = a} :: M2tsSettings)
{-# DEPRECATED mAudioPids "Use generic-lens or generic-optics with 'audioPids' instead." #-}

-- | Specify the packet identifier (PID) for DVB teletext data you include in this output. Default is 499.
--
-- /Note:/ Consider using 'dvbTeletextPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbTeletextPid :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Natural)
mDvbTeletextPid = Lens.lens (dvbTeletextPid :: M2tsSettings -> Lude.Maybe Lude.Natural) (\s a -> s {dvbTeletextPid = a} :: M2tsSettings)
{-# DEPRECATED mDvbTeletextPid "Use generic-lens or generic-optics with 'dvbTeletextPid' instead." #-}

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mNielsenId3 :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsNielsenId3)
mNielsenId3 = Lens.lens (nielsenId3 :: M2tsSettings -> Lude.Maybe M2tsNielsenId3) (\s a -> s {nielsenId3 = a} :: M2tsSettings)
{-# DEPRECATED mNielsenId3 "Use generic-lens or generic-optics with 'nielsenId3' instead." #-}

-- | Specify the length, in seconds, of each segment. Required unless markers is set to _none_.
--
-- /Note:/ Consider using 'segmentationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSegmentationTime :: Lens.Lens' M2tsSettings (Lude.Maybe Lude.Double)
mSegmentationTime = Lens.lens (segmentationTime :: M2tsSettings -> Lude.Maybe Lude.Double) (\s a -> s {segmentationTime = a} :: M2tsSettings)
{-# DEPRECATED mSegmentationTime "Use generic-lens or generic-optics with 'segmentationTime' instead." #-}

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- /Note:/ Consider using 'ebpAudioInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEbpAudioInterval :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsEbpAudioInterval)
mEbpAudioInterval = Lens.lens (ebpAudioInterval :: M2tsSettings -> Lude.Maybe M2tsEbpAudioInterval) (\s a -> s {ebpAudioInterval = a} :: M2tsSettings)
{-# DEPRECATED mEbpAudioInterval "Use generic-lens or generic-optics with 'ebpAudioInterval' instead." #-}

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbNitSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbNitSettings :: Lens.Lens' M2tsSettings (Lude.Maybe DvbNitSettings)
mDvbNitSettings = Lens.lens (dvbNitSettings :: M2tsSettings -> Lude.Maybe DvbNitSettings) (\s a -> s {dvbNitSettings = a} :: M2tsSettings)
{-# DEPRECATED mDvbNitSettings "Use generic-lens or generic-optics with 'dvbNitSettings' instead." #-}

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- /Note:/ Consider using 'pcrControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPcrControl :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsPcrControl)
mPcrControl = Lens.lens (pcrControl :: M2tsSettings -> Lude.Maybe M2tsPcrControl) (\s a -> s {pcrControl = a} :: M2tsSettings)
{-# DEPRECATED mPcrControl "Use generic-lens or generic-optics with 'pcrControl' instead." #-}

-- | Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- /Note:/ Consider using 'ebpPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEbpPlacement :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsEbpPlacement)
mEbpPlacement = Lens.lens (ebpPlacement :: M2tsSettings -> Lude.Maybe M2tsEbpPlacement) (\s a -> s {ebpPlacement = a} :: M2tsSettings)
{-# DEPRECATED mEbpPlacement "Use generic-lens or generic-optics with 'ebpPlacement' instead." #-}

-- | When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
--
-- /Note:/ Consider using 'rateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRateMode :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsRateMode)
mRateMode = Lens.lens (rateMode :: M2tsSettings -> Lude.Maybe M2tsRateMode) (\s a -> s {rateMode = a} :: M2tsSettings)
{-# DEPRECATED mRateMode "Use generic-lens or generic-optics with 'rateMode' instead." #-}

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
--
-- /Note:/ Consider using 'segmentationStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSegmentationStyle :: Lens.Lens' M2tsSettings (Lude.Maybe M2tsSegmentationStyle)
mSegmentationStyle = Lens.lens (segmentationStyle :: M2tsSettings -> Lude.Maybe M2tsSegmentationStyle) (\s a -> s {segmentationStyle = a} :: M2tsSettings)
{-# DEPRECATED mSegmentationStyle "Use generic-lens or generic-optics with 'segmentationStyle' instead." #-}

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
--
-- /Note:/ Consider using 'dvbTdtSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDvbTdtSettings :: Lens.Lens' M2tsSettings (Lude.Maybe DvbTdtSettings)
mDvbTdtSettings = Lens.lens (dvbTdtSettings :: M2tsSettings -> Lude.Maybe DvbTdtSettings) (\s a -> s {dvbTdtSettings = a} :: M2tsSettings)
{-# DEPRECATED mDvbTdtSettings "Use generic-lens or generic-optics with 'dvbTdtSettings' instead." #-}

instance Lude.FromJSON M2tsSettings where
  parseJSON =
    Lude.withObject
      "M2tsSettings"
      ( \x ->
          M2tsSettings'
            Lude.<$> (x Lude..:? "pmtPid")
            Lude.<*> (x Lude..:? "videoPid")
            Lude.<*> (x Lude..:? "bufferModel")
            Lude.<*> (x Lude..:? "programNumber")
            Lude.<*> (x Lude..:? "scte35Pid")
            Lude.<*> (x Lude..:? "minEbpInterval")
            Lude.<*> (x Lude..:? "transportStreamId")
            Lude.<*> (x Lude..:? "maxPcrInterval")
            Lude.<*> (x Lude..:? "fragmentTime")
            Lude.<*> (x Lude..:? "privateMetadataPid")
            Lude.<*> (x Lude..:? "scte35Esam")
            Lude.<*> (x Lude..:? "audioDuration")
            Lude.<*> (x Lude..:? "pmtInterval")
            Lude.<*> (x Lude..:? "dvbSdtSettings")
            Lude.<*> (x Lude..:? "nullPacketBitrate")
            Lude.<*> (x Lude..:? "audioBufferModel")
            Lude.<*> (x Lude..:? "timedMetadataPid")
            Lude.<*> (x Lude..:? "audioFramesPerPes")
            Lude.<*> (x Lude..:? "pcrPid")
            Lude.<*> (x Lude..:? "segmentationMarkers")
            Lude.<*> (x Lude..:? "dvbSubPids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "scte35Source")
            Lude.<*> (x Lude..:? "patInterval")
            Lude.<*> (x Lude..:? "forceTsVideoEbpOrder")
            Lude.<*> (x Lude..:? "esRateInPes")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "audioPids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "dvbTeletextPid")
            Lude.<*> (x Lude..:? "nielsenId3")
            Lude.<*> (x Lude..:? "segmentationTime")
            Lude.<*> (x Lude..:? "ebpAudioInterval")
            Lude.<*> (x Lude..:? "dvbNitSettings")
            Lude.<*> (x Lude..:? "pcrControl")
            Lude.<*> (x Lude..:? "ebpPlacement")
            Lude.<*> (x Lude..:? "rateMode")
            Lude.<*> (x Lude..:? "segmentationStyle")
            Lude.<*> (x Lude..:? "dvbTdtSettings")
      )

instance Lude.ToJSON M2tsSettings where
  toJSON M2tsSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("pmtPid" Lude..=) Lude.<$> pmtPid,
            ("videoPid" Lude..=) Lude.<$> videoPid,
            ("bufferModel" Lude..=) Lude.<$> bufferModel,
            ("programNumber" Lude..=) Lude.<$> programNumber,
            ("scte35Pid" Lude..=) Lude.<$> scte35Pid,
            ("minEbpInterval" Lude..=) Lude.<$> minEbpInterval,
            ("transportStreamId" Lude..=) Lude.<$> transportStreamId,
            ("maxPcrInterval" Lude..=) Lude.<$> maxPcrInterval,
            ("fragmentTime" Lude..=) Lude.<$> fragmentTime,
            ("privateMetadataPid" Lude..=) Lude.<$> privateMetadataPid,
            ("scte35Esam" Lude..=) Lude.<$> scte35Esam,
            ("audioDuration" Lude..=) Lude.<$> audioDuration,
            ("pmtInterval" Lude..=) Lude.<$> pmtInterval,
            ("dvbSdtSettings" Lude..=) Lude.<$> dvbSdtSettings,
            ("nullPacketBitrate" Lude..=) Lude.<$> nullPacketBitrate,
            ("audioBufferModel" Lude..=) Lude.<$> audioBufferModel,
            ("timedMetadataPid" Lude..=) Lude.<$> timedMetadataPid,
            ("audioFramesPerPes" Lude..=) Lude.<$> audioFramesPerPes,
            ("pcrPid" Lude..=) Lude.<$> pcrPid,
            ("segmentationMarkers" Lude..=) Lude.<$> segmentationMarkers,
            ("dvbSubPids" Lude..=) Lude.<$> dvbSubPids,
            ("scte35Source" Lude..=) Lude.<$> scte35Source,
            ("patInterval" Lude..=) Lude.<$> patInterval,
            ("forceTsVideoEbpOrder" Lude..=) Lude.<$> forceTsVideoEbpOrder,
            ("esRateInPes" Lude..=) Lude.<$> esRateInPes,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("audioPids" Lude..=) Lude.<$> audioPids,
            ("dvbTeletextPid" Lude..=) Lude.<$> dvbTeletextPid,
            ("nielsenId3" Lude..=) Lude.<$> nielsenId3,
            ("segmentationTime" Lude..=) Lude.<$> segmentationTime,
            ("ebpAudioInterval" Lude..=) Lude.<$> ebpAudioInterval,
            ("dvbNitSettings" Lude..=) Lude.<$> dvbNitSettings,
            ("pcrControl" Lude..=) Lude.<$> pcrControl,
            ("ebpPlacement" Lude..=) Lude.<$> ebpPlacement,
            ("rateMode" Lude..=) Lude.<$> rateMode,
            ("segmentationStyle" Lude..=) Lude.<$> segmentationStyle,
            ("dvbTdtSettings" Lude..=) Lude.<$> dvbTdtSettings
          ]
      )
