{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsSettings where

import Network.AWS.Lens
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
import Network.AWS.Prelude

-- | MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
--
-- /See:/ 'm2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'
  { _mssPmtPid :: !(Maybe Nat),
    _mssVideoPid :: !(Maybe Nat),
    _mssBufferModel :: !(Maybe M2tsBufferModel),
    _mssProgramNumber :: !(Maybe Nat),
    _mssScte35Pid :: !(Maybe Nat),
    _mssMinEbpInterval :: !(Maybe Nat),
    _mssTransportStreamId :: !(Maybe Nat),
    _mssMaxPcrInterval :: !(Maybe Nat),
    _mssFragmentTime :: !(Maybe Double),
    _mssPrivateMetadataPid :: !(Maybe Nat),
    _mssScte35Esam :: !(Maybe M2tsScte35Esam),
    _mssAudioDuration :: !(Maybe M2tsAudioDuration),
    _mssPmtInterval :: !(Maybe Nat),
    _mssDvbSdtSettings :: !(Maybe DvbSdtSettings),
    _mssNullPacketBitrate :: !(Maybe Double),
    _mssAudioBufferModel :: !(Maybe M2tsAudioBufferModel),
    _mssTimedMetadataPid :: !(Maybe Nat),
    _mssAudioFramesPerPes :: !(Maybe Nat),
    _mssPcrPid :: !(Maybe Nat),
    _mssSegmentationMarkers :: !(Maybe M2tsSegmentationMarkers),
    _mssDvbSubPids :: !(Maybe [Nat]),
    _mssScte35Source :: !(Maybe M2tsScte35Source),
    _mssPatInterval :: !(Maybe Nat),
    _mssForceTsVideoEbpOrder :: !(Maybe M2tsForceTsVideoEbpOrder),
    _mssEsRateInPes :: !(Maybe M2tsEsRateInPes),
    _mssBitrate :: !(Maybe Nat),
    _mssAudioPids :: !(Maybe [Nat]),
    _mssDvbTeletextPid :: !(Maybe Nat),
    _mssNielsenId3 :: !(Maybe M2tsNielsenId3),
    _mssSegmentationTime :: !(Maybe Double),
    _mssEbpAudioInterval :: !(Maybe M2tsEbpAudioInterval),
    _mssDvbNitSettings :: !(Maybe DvbNitSettings),
    _mssPcrControl :: !(Maybe M2tsPcrControl),
    _mssEbpPlacement :: !(Maybe M2tsEbpPlacement),
    _mssRateMode :: !(Maybe M2tsRateMode),
    _mssSegmentationStyle :: !(Maybe M2tsSegmentationStyle),
    _mssDvbTdtSettings :: !(Maybe DvbTdtSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'M2tsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssPmtPid' - Specify the packet identifier (PID) for the program map table (PMT) itself. Default is 480.
--
-- * 'mssVideoPid' - Specify the packet identifier (PID) of the elementary video stream in the transport stream.
--
-- * 'mssBufferModel' - Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
--
-- * 'mssProgramNumber' - Use Program number (programNumber) to specify the program number used in the program map table (PMT) for this output. Default is 1. Program numbers and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
--
-- * 'mssScte35Pid' - Specify the packet identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- * 'mssMinEbpInterval' - When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
--
-- * 'mssTransportStreamId' - Specify the ID for the transport stream itself in the program map table for this output. Transport stream IDs and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
--
-- * 'mssMaxPcrInterval' - Specify the maximum time, in milliseconds, between Program Clock References (PCRs) inserted into the transport stream.
--
-- * 'mssFragmentTime' - The length, in seconds, of each fragment. Only used with EBP markers.
--
-- * 'mssPrivateMetadataPid' - Specify the packet identifier (PID) of the private metadata stream. Default is 503.
--
-- * 'mssScte35Esam' - Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- * 'mssAudioDuration' - Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- * 'mssPmtInterval' - Specify the number of milliseconds between instances of the program map table (PMT) in the output transport stream.
--
-- * 'mssDvbSdtSettings' - Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
--
-- * 'mssNullPacketBitrate' - Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
--
-- * 'mssAudioBufferModel' - Selects between the DVB and ATSC buffer models for Dolby Digital audio.
--
-- * 'mssTimedMetadataPid' - Specify the packet identifier (PID) for timed metadata in this output. Default is 502.
--
-- * 'mssAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'mssPcrPid' - Specify the packet identifier (PID) for the program clock reference (PCR) in this output. If you do not specify a value, the service will use the value for Video PID (VideoPid).
--
-- * 'mssSegmentationMarkers' - Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
--
-- * 'mssDvbSubPids' - Specify the packet identifiers (PIDs) for DVB subtitle data included in this output. Specify multiple PIDs as a JSON array. Default is the range 460-479.
--
-- * 'mssScte35Source' - For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE). Also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml). Also enable ESAM SCTE-35 (include the property scte35Esam).
--
-- * 'mssPatInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'mssForceTsVideoEbpOrder' - Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
--
-- * 'mssEsRateInPes' - Controls whether to include the ES Rate field in the PES header.
--
-- * 'mssBitrate' - Specify the output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
--
-- * 'mssAudioPids' - Specify the packet identifiers (PIDs) for any elementary audio streams you include in this output. Specify multiple PIDs as a JSON array. Default is the range 482-492.
--
-- * 'mssDvbTeletextPid' - Specify the packet identifier (PID) for DVB teletext data you include in this output. Default is 499.
--
-- * 'mssNielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'mssSegmentationTime' - Specify the length, in seconds, of each segment. Required unless markers is set to _none_.
--
-- * 'mssEbpAudioInterval' - When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- * 'mssDvbNitSettings' - Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- * 'mssPcrControl' - When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- * 'mssEbpPlacement' - Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
--
-- * 'mssRateMode' - When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
--
-- * 'mssSegmentationStyle' - The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
--
-- * 'mssDvbTdtSettings' - Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
m2tsSettings ::
  M2tsSettings
m2tsSettings =
  M2tsSettings'
    { _mssPmtPid = Nothing,
      _mssVideoPid = Nothing,
      _mssBufferModel = Nothing,
      _mssProgramNumber = Nothing,
      _mssScte35Pid = Nothing,
      _mssMinEbpInterval = Nothing,
      _mssTransportStreamId = Nothing,
      _mssMaxPcrInterval = Nothing,
      _mssFragmentTime = Nothing,
      _mssPrivateMetadataPid = Nothing,
      _mssScte35Esam = Nothing,
      _mssAudioDuration = Nothing,
      _mssPmtInterval = Nothing,
      _mssDvbSdtSettings = Nothing,
      _mssNullPacketBitrate = Nothing,
      _mssAudioBufferModel = Nothing,
      _mssTimedMetadataPid = Nothing,
      _mssAudioFramesPerPes = Nothing,
      _mssPcrPid = Nothing,
      _mssSegmentationMarkers = Nothing,
      _mssDvbSubPids = Nothing,
      _mssScte35Source = Nothing,
      _mssPatInterval = Nothing,
      _mssForceTsVideoEbpOrder = Nothing,
      _mssEsRateInPes = Nothing,
      _mssBitrate = Nothing,
      _mssAudioPids = Nothing,
      _mssDvbTeletextPid = Nothing,
      _mssNielsenId3 = Nothing,
      _mssSegmentationTime = Nothing,
      _mssEbpAudioInterval = Nothing,
      _mssDvbNitSettings = Nothing,
      _mssPcrControl = Nothing,
      _mssEbpPlacement = Nothing,
      _mssRateMode = Nothing,
      _mssSegmentationStyle = Nothing,
      _mssDvbTdtSettings = Nothing
    }

-- | Specify the packet identifier (PID) for the program map table (PMT) itself. Default is 480.
mssPmtPid :: Lens' M2tsSettings (Maybe Natural)
mssPmtPid = lens _mssPmtPid (\s a -> s {_mssPmtPid = a}) . mapping _Nat

-- | Specify the packet identifier (PID) of the elementary video stream in the transport stream.
mssVideoPid :: Lens' M2tsSettings (Maybe Natural)
mssVideoPid = lens _mssVideoPid (\s a -> s {_mssVideoPid = a}) . mapping _Nat

-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
mssBufferModel :: Lens' M2tsSettings (Maybe M2tsBufferModel)
mssBufferModel = lens _mssBufferModel (\s a -> s {_mssBufferModel = a})

-- | Use Program number (programNumber) to specify the program number used in the program map table (PMT) for this output. Default is 1. Program numbers and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
mssProgramNumber :: Lens' M2tsSettings (Maybe Natural)
mssProgramNumber = lens _mssProgramNumber (\s a -> s {_mssProgramNumber = a}) . mapping _Nat

-- | Specify the packet identifier (PID) of the SCTE-35 stream in the transport stream.
mssScte35Pid :: Lens' M2tsSettings (Maybe Natural)
mssScte35Pid = lens _mssScte35Pid (\s a -> s {_mssScte35Pid = a}) . mapping _Nat

-- | When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
mssMinEbpInterval :: Lens' M2tsSettings (Maybe Natural)
mssMinEbpInterval = lens _mssMinEbpInterval (\s a -> s {_mssMinEbpInterval = a}) . mapping _Nat

-- | Specify the ID for the transport stream itself in the program map table for this output. Transport stream IDs and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
mssTransportStreamId :: Lens' M2tsSettings (Maybe Natural)
mssTransportStreamId = lens _mssTransportStreamId (\s a -> s {_mssTransportStreamId = a}) . mapping _Nat

-- | Specify the maximum time, in milliseconds, between Program Clock References (PCRs) inserted into the transport stream.
mssMaxPcrInterval :: Lens' M2tsSettings (Maybe Natural)
mssMaxPcrInterval = lens _mssMaxPcrInterval (\s a -> s {_mssMaxPcrInterval = a}) . mapping _Nat

-- | The length, in seconds, of each fragment. Only used with EBP markers.
mssFragmentTime :: Lens' M2tsSettings (Maybe Double)
mssFragmentTime = lens _mssFragmentTime (\s a -> s {_mssFragmentTime = a})

-- | Specify the packet identifier (PID) of the private metadata stream. Default is 503.
mssPrivateMetadataPid :: Lens' M2tsSettings (Maybe Natural)
mssPrivateMetadataPid = lens _mssPrivateMetadataPid (\s a -> s {_mssPrivateMetadataPid = a}) . mapping _Nat

-- | Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
mssScte35Esam :: Lens' M2tsSettings (Maybe M2tsScte35Esam)
mssScte35Esam = lens _mssScte35Esam (\s a -> s {_mssScte35Esam = a})

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
mssAudioDuration :: Lens' M2tsSettings (Maybe M2tsAudioDuration)
mssAudioDuration = lens _mssAudioDuration (\s a -> s {_mssAudioDuration = a})

-- | Specify the number of milliseconds between instances of the program map table (PMT) in the output transport stream.
mssPmtInterval :: Lens' M2tsSettings (Maybe Natural)
mssPmtInterval = lens _mssPmtInterval (\s a -> s {_mssPmtInterval = a}) . mapping _Nat

-- | Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
mssDvbSdtSettings :: Lens' M2tsSettings (Maybe DvbSdtSettings)
mssDvbSdtSettings = lens _mssDvbSdtSettings (\s a -> s {_mssDvbSdtSettings = a})

-- | Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
mssNullPacketBitrate :: Lens' M2tsSettings (Maybe Double)
mssNullPacketBitrate = lens _mssNullPacketBitrate (\s a -> s {_mssNullPacketBitrate = a})

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
mssAudioBufferModel :: Lens' M2tsSettings (Maybe M2tsAudioBufferModel)
mssAudioBufferModel = lens _mssAudioBufferModel (\s a -> s {_mssAudioBufferModel = a})

-- | Specify the packet identifier (PID) for timed metadata in this output. Default is 502.
mssTimedMetadataPid :: Lens' M2tsSettings (Maybe Natural)
mssTimedMetadataPid = lens _mssTimedMetadataPid (\s a -> s {_mssTimedMetadataPid = a}) . mapping _Nat

-- | The number of audio frames to insert for each PES packet.
mssAudioFramesPerPes :: Lens' M2tsSettings (Maybe Natural)
mssAudioFramesPerPes = lens _mssAudioFramesPerPes (\s a -> s {_mssAudioFramesPerPes = a}) . mapping _Nat

-- | Specify the packet identifier (PID) for the program clock reference (PCR) in this output. If you do not specify a value, the service will use the value for Video PID (VideoPid).
mssPcrPid :: Lens' M2tsSettings (Maybe Natural)
mssPcrPid = lens _mssPcrPid (\s a -> s {_mssPcrPid = a}) . mapping _Nat

-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
mssSegmentationMarkers :: Lens' M2tsSettings (Maybe M2tsSegmentationMarkers)
mssSegmentationMarkers = lens _mssSegmentationMarkers (\s a -> s {_mssSegmentationMarkers = a})

-- | Specify the packet identifiers (PIDs) for DVB subtitle data included in this output. Specify multiple PIDs as a JSON array. Default is the range 460-479.
mssDvbSubPids :: Lens' M2tsSettings [Natural]
mssDvbSubPids = lens _mssDvbSubPids (\s a -> s {_mssDvbSubPids = a}) . _Default . _Coerce

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE). Also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml). Also enable ESAM SCTE-35 (include the property scte35Esam).
mssScte35Source :: Lens' M2tsSettings (Maybe M2tsScte35Source)
mssScte35Source = lens _mssScte35Source (\s a -> s {_mssScte35Source = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
mssPatInterval :: Lens' M2tsSettings (Maybe Natural)
mssPatInterval = lens _mssPatInterval (\s a -> s {_mssPatInterval = a}) . mapping _Nat

-- | Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
mssForceTsVideoEbpOrder :: Lens' M2tsSettings (Maybe M2tsForceTsVideoEbpOrder)
mssForceTsVideoEbpOrder = lens _mssForceTsVideoEbpOrder (\s a -> s {_mssForceTsVideoEbpOrder = a})

-- | Controls whether to include the ES Rate field in the PES header.
mssEsRateInPes :: Lens' M2tsSettings (Maybe M2tsEsRateInPes)
mssEsRateInPes = lens _mssEsRateInPes (\s a -> s {_mssEsRateInPes = a})

-- | Specify the output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
mssBitrate :: Lens' M2tsSettings (Maybe Natural)
mssBitrate = lens _mssBitrate (\s a -> s {_mssBitrate = a}) . mapping _Nat

-- | Specify the packet identifiers (PIDs) for any elementary audio streams you include in this output. Specify multiple PIDs as a JSON array. Default is the range 482-492.
mssAudioPids :: Lens' M2tsSettings [Natural]
mssAudioPids = lens _mssAudioPids (\s a -> s {_mssAudioPids = a}) . _Default . _Coerce

-- | Specify the packet identifier (PID) for DVB teletext data you include in this output. Default is 499.
mssDvbTeletextPid :: Lens' M2tsSettings (Maybe Natural)
mssDvbTeletextPid = lens _mssDvbTeletextPid (\s a -> s {_mssDvbTeletextPid = a}) . mapping _Nat

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
mssNielsenId3 :: Lens' M2tsSettings (Maybe M2tsNielsenId3)
mssNielsenId3 = lens _mssNielsenId3 (\s a -> s {_mssNielsenId3 = a})

-- | Specify the length, in seconds, of each segment. Required unless markers is set to _none_.
mssSegmentationTime :: Lens' M2tsSettings (Maybe Double)
mssSegmentationTime = lens _mssSegmentationTime (\s a -> s {_mssSegmentationTime = a})

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
mssEbpAudioInterval :: Lens' M2tsSettings (Maybe M2tsEbpAudioInterval)
mssEbpAudioInterval = lens _mssEbpAudioInterval (\s a -> s {_mssEbpAudioInterval = a})

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
mssDvbNitSettings :: Lens' M2tsSettings (Maybe DvbNitSettings)
mssDvbNitSettings = lens _mssDvbNitSettings (\s a -> s {_mssDvbNitSettings = a})

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
mssPcrControl :: Lens' M2tsSettings (Maybe M2tsPcrControl)
mssPcrControl = lens _mssPcrControl (\s a -> s {_mssPcrControl = a})

-- | Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
mssEbpPlacement :: Lens' M2tsSettings (Maybe M2tsEbpPlacement)
mssEbpPlacement = lens _mssEbpPlacement (\s a -> s {_mssEbpPlacement = a})

-- | When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
mssRateMode :: Lens' M2tsSettings (Maybe M2tsRateMode)
mssRateMode = lens _mssRateMode (\s a -> s {_mssRateMode = a})

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
mssSegmentationStyle :: Lens' M2tsSettings (Maybe M2tsSegmentationStyle)
mssSegmentationStyle = lens _mssSegmentationStyle (\s a -> s {_mssSegmentationStyle = a})

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
mssDvbTdtSettings :: Lens' M2tsSettings (Maybe DvbTdtSettings)
mssDvbTdtSettings = lens _mssDvbTdtSettings (\s a -> s {_mssDvbTdtSettings = a})

instance FromJSON M2tsSettings where
  parseJSON =
    withObject
      "M2tsSettings"
      ( \x ->
          M2tsSettings'
            <$> (x .:? "pmtPid")
            <*> (x .:? "videoPid")
            <*> (x .:? "bufferModel")
            <*> (x .:? "programNumber")
            <*> (x .:? "scte35Pid")
            <*> (x .:? "minEbpInterval")
            <*> (x .:? "transportStreamId")
            <*> (x .:? "maxPcrInterval")
            <*> (x .:? "fragmentTime")
            <*> (x .:? "privateMetadataPid")
            <*> (x .:? "scte35Esam")
            <*> (x .:? "audioDuration")
            <*> (x .:? "pmtInterval")
            <*> (x .:? "dvbSdtSettings")
            <*> (x .:? "nullPacketBitrate")
            <*> (x .:? "audioBufferModel")
            <*> (x .:? "timedMetadataPid")
            <*> (x .:? "audioFramesPerPes")
            <*> (x .:? "pcrPid")
            <*> (x .:? "segmentationMarkers")
            <*> (x .:? "dvbSubPids" .!= mempty)
            <*> (x .:? "scte35Source")
            <*> (x .:? "patInterval")
            <*> (x .:? "forceTsVideoEbpOrder")
            <*> (x .:? "esRateInPes")
            <*> (x .:? "bitrate")
            <*> (x .:? "audioPids" .!= mempty)
            <*> (x .:? "dvbTeletextPid")
            <*> (x .:? "nielsenId3")
            <*> (x .:? "segmentationTime")
            <*> (x .:? "ebpAudioInterval")
            <*> (x .:? "dvbNitSettings")
            <*> (x .:? "pcrControl")
            <*> (x .:? "ebpPlacement")
            <*> (x .:? "rateMode")
            <*> (x .:? "segmentationStyle")
            <*> (x .:? "dvbTdtSettings")
      )

instance Hashable M2tsSettings

instance NFData M2tsSettings

instance ToJSON M2tsSettings where
  toJSON M2tsSettings' {..} =
    object
      ( catMaybes
          [ ("pmtPid" .=) <$> _mssPmtPid,
            ("videoPid" .=) <$> _mssVideoPid,
            ("bufferModel" .=) <$> _mssBufferModel,
            ("programNumber" .=) <$> _mssProgramNumber,
            ("scte35Pid" .=) <$> _mssScte35Pid,
            ("minEbpInterval" .=) <$> _mssMinEbpInterval,
            ("transportStreamId" .=) <$> _mssTransportStreamId,
            ("maxPcrInterval" .=) <$> _mssMaxPcrInterval,
            ("fragmentTime" .=) <$> _mssFragmentTime,
            ("privateMetadataPid" .=) <$> _mssPrivateMetadataPid,
            ("scte35Esam" .=) <$> _mssScte35Esam,
            ("audioDuration" .=) <$> _mssAudioDuration,
            ("pmtInterval" .=) <$> _mssPmtInterval,
            ("dvbSdtSettings" .=) <$> _mssDvbSdtSettings,
            ("nullPacketBitrate" .=) <$> _mssNullPacketBitrate,
            ("audioBufferModel" .=) <$> _mssAudioBufferModel,
            ("timedMetadataPid" .=) <$> _mssTimedMetadataPid,
            ("audioFramesPerPes" .=) <$> _mssAudioFramesPerPes,
            ("pcrPid" .=) <$> _mssPcrPid,
            ("segmentationMarkers" .=) <$> _mssSegmentationMarkers,
            ("dvbSubPids" .=) <$> _mssDvbSubPids,
            ("scte35Source" .=) <$> _mssScte35Source,
            ("patInterval" .=) <$> _mssPatInterval,
            ("forceTsVideoEbpOrder" .=) <$> _mssForceTsVideoEbpOrder,
            ("esRateInPes" .=) <$> _mssEsRateInPes,
            ("bitrate" .=) <$> _mssBitrate,
            ("audioPids" .=) <$> _mssAudioPids,
            ("dvbTeletextPid" .=) <$> _mssDvbTeletextPid,
            ("nielsenId3" .=) <$> _mssNielsenId3,
            ("segmentationTime" .=) <$> _mssSegmentationTime,
            ("ebpAudioInterval" .=) <$> _mssEbpAudioInterval,
            ("dvbNitSettings" .=) <$> _mssDvbNitSettings,
            ("pcrControl" .=) <$> _mssPcrControl,
            ("ebpPlacement" .=) <$> _mssEbpPlacement,
            ("rateMode" .=) <$> _mssRateMode,
            ("segmentationStyle" .=) <$> _mssSegmentationStyle,
            ("dvbTdtSettings" .=) <$> _mssDvbTdtSettings
          ]
      )
