{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8Settings
  ( M3u8Settings (..),

    -- * Smart constructor
    mkM3u8Settings,

    -- * Lenses
    msAudioDuration,
    msAudioFramesPerPes,
    msAudioPids,
    msNielsenId3,
    msPatInterval,
    msPcrControl,
    msPcrPid,
    msPmtInterval,
    msPmtPid,
    msPrivateMetadataPid,
    msProgramNumber,
    msScte35Pid,
    msScte35Source,
    msTimedMetadata,
    msTimedMetadataPid,
    msTransportStreamId,
    msVideoPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.M3u8AudioDuration as Types
import qualified Network.AWS.MediaConvert.Types.M3u8NielsenId3 as Types
import qualified Network.AWS.MediaConvert.Types.M3u8PcrControl as Types
import qualified Network.AWS.MediaConvert.Types.M3u8Scte35Source as Types
import qualified Network.AWS.MediaConvert.Types.TimedMetadata as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for TS segments in HLS
--
-- /See:/ 'mkM3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { -- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
    audioDuration :: Core.Maybe Types.M3u8AudioDuration,
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
    audioPids :: Core.Maybe [Core.Natural],
    -- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
    nielsenId3 :: Core.Maybe Types.M3u8NielsenId3,
    -- | The number of milliseconds between instances of this table in the output transport stream.
    patInterval :: Core.Maybe Core.Natural,
    -- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
    pcrControl :: Core.Maybe Types.M3u8PcrControl,
    -- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
    pcrPid :: Core.Maybe Core.Natural,
    -- | The number of milliseconds between instances of this table in the output transport stream.
    pmtInterval :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
    pmtPid :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the private metadata stream in the transport stream.
    privateMetadataPid :: Core.Maybe Core.Natural,
    -- | The value of the program number field in the Program Map Table.
    programNumber :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
    scte35Pid :: Core.Maybe Core.Natural,
    -- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE) if you don't want manifest conditioning. Choose Passthrough (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest conditioning. In both cases, also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml).
    scte35Source :: Core.Maybe Types.M3u8Scte35Source,
    -- | Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
    timedMetadata :: Core.Maybe Types.TimedMetadata,
    -- | Packet Identifier (PID) of the timed metadata stream in the transport stream.
    timedMetadataPid :: Core.Maybe Core.Natural,
    -- | The value of the transport stream ID field in the Program Map Table.
    transportStreamId :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the elementary video stream in the transport stream.
    videoPid :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'M3u8Settings' value with any optional fields omitted.
mkM3u8Settings ::
  M3u8Settings
mkM3u8Settings =
  M3u8Settings'
    { audioDuration = Core.Nothing,
      audioFramesPerPes = Core.Nothing,
      audioPids = Core.Nothing,
      nielsenId3 = Core.Nothing,
      patInterval = Core.Nothing,
      pcrControl = Core.Nothing,
      pcrPid = Core.Nothing,
      pmtInterval = Core.Nothing,
      pmtPid = Core.Nothing,
      privateMetadataPid = Core.Nothing,
      programNumber = Core.Nothing,
      scte35Pid = Core.Nothing,
      scte35Source = Core.Nothing,
      timedMetadata = Core.Nothing,
      timedMetadataPid = Core.Nothing,
      transportStreamId = Core.Nothing,
      videoPid = Core.Nothing
    }

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAudioDuration :: Lens.Lens' M3u8Settings (Core.Maybe Types.M3u8AudioDuration)
msAudioDuration = Lens.field @"audioDuration"
{-# DEPRECATED msAudioDuration "Use generic-lens or generic-optics with 'audioDuration' instead." #-}

-- | The number of audio frames to insert for each PES packet.
--
-- /Note:/ Consider using 'audioFramesPerPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAudioFramesPerPes :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msAudioFramesPerPes = Lens.field @"audioFramesPerPes"
{-# DEPRECATED msAudioFramesPerPes "Use generic-lens or generic-optics with 'audioFramesPerPes' instead." #-}

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAudioPids :: Lens.Lens' M3u8Settings (Core.Maybe [Core.Natural])
msAudioPids = Lens.field @"audioPids"
{-# DEPRECATED msAudioPids "Use generic-lens or generic-optics with 'audioPids' instead." #-}

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msNielsenId3 :: Lens.Lens' M3u8Settings (Core.Maybe Types.M3u8NielsenId3)
msNielsenId3 = Lens.field @"nielsenId3"
{-# DEPRECATED msNielsenId3 "Use generic-lens or generic-optics with 'nielsenId3' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'patInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPatInterval :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msPatInterval = Lens.field @"patInterval"
{-# DEPRECATED msPatInterval "Use generic-lens or generic-optics with 'patInterval' instead." #-}

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- /Note:/ Consider using 'pcrControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPcrControl :: Lens.Lens' M3u8Settings (Core.Maybe Types.M3u8PcrControl)
msPcrControl = Lens.field @"pcrControl"
{-# DEPRECATED msPcrControl "Use generic-lens or generic-optics with 'pcrControl' instead." #-}

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPcrPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msPcrPid = Lens.field @"pcrPid"
{-# DEPRECATED msPcrPid "Use generic-lens or generic-optics with 'pcrPid' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'pmtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPmtInterval :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msPmtInterval = Lens.field @"pmtInterval"
{-# DEPRECATED msPmtInterval "Use generic-lens or generic-optics with 'pmtInterval' instead." #-}

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPmtPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msPmtPid = Lens.field @"pmtPid"
{-# DEPRECATED msPmtPid "Use generic-lens or generic-optics with 'pmtPid' instead." #-}

-- | Packet Identifier (PID) of the private metadata stream in the transport stream.
--
-- /Note:/ Consider using 'privateMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPrivateMetadataPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msPrivateMetadataPid = Lens.field @"privateMetadataPid"
{-# DEPRECATED msPrivateMetadataPid "Use generic-lens or generic-optics with 'privateMetadataPid' instead." #-}

-- | The value of the program number field in the Program Map Table.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msProgramNumber :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msProgramNumber = Lens.field @"programNumber"
{-# DEPRECATED msProgramNumber "Use generic-lens or generic-optics with 'programNumber' instead." #-}

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msScte35Pid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msScte35Pid = Lens.field @"scte35Pid"
{-# DEPRECATED msScte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead." #-}

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE) if you don't want manifest conditioning. Choose Passthrough (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest conditioning. In both cases, also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml).
--
-- /Note:/ Consider using 'scte35Source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msScte35Source :: Lens.Lens' M3u8Settings (Core.Maybe Types.M3u8Scte35Source)
msScte35Source = Lens.field @"scte35Source"
{-# DEPRECATED msScte35Source "Use generic-lens or generic-optics with 'scte35Source' instead." #-}

-- | Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
--
-- /Note:/ Consider using 'timedMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTimedMetadata :: Lens.Lens' M3u8Settings (Core.Maybe Types.TimedMetadata)
msTimedMetadata = Lens.field @"timedMetadata"
{-# DEPRECATED msTimedMetadata "Use generic-lens or generic-optics with 'timedMetadata' instead." #-}

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream.
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTimedMetadataPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msTimedMetadataPid = Lens.field @"timedMetadataPid"
{-# DEPRECATED msTimedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead." #-}

-- | The value of the transport stream ID field in the Program Map Table.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTransportStreamId :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msTransportStreamId = Lens.field @"transportStreamId"
{-# DEPRECATED msTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

-- | Packet Identifier (PID) of the elementary video stream in the transport stream.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msVideoPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msVideoPid = Lens.field @"videoPid"
{-# DEPRECATED msVideoPid "Use generic-lens or generic-optics with 'videoPid' instead." #-}

instance Core.FromJSON M3u8Settings where
  toJSON M3u8Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("audioDuration" Core..=) Core.<$> audioDuration,
            ("audioFramesPerPes" Core..=) Core.<$> audioFramesPerPes,
            ("audioPids" Core..=) Core.<$> audioPids,
            ("nielsenId3" Core..=) Core.<$> nielsenId3,
            ("patInterval" Core..=) Core.<$> patInterval,
            ("pcrControl" Core..=) Core.<$> pcrControl,
            ("pcrPid" Core..=) Core.<$> pcrPid,
            ("pmtInterval" Core..=) Core.<$> pmtInterval,
            ("pmtPid" Core..=) Core.<$> pmtPid,
            ("privateMetadataPid" Core..=) Core.<$> privateMetadataPid,
            ("programNumber" Core..=) Core.<$> programNumber,
            ("scte35Pid" Core..=) Core.<$> scte35Pid,
            ("scte35Source" Core..=) Core.<$> scte35Source,
            ("timedMetadata" Core..=) Core.<$> timedMetadata,
            ("timedMetadataPid" Core..=) Core.<$> timedMetadataPid,
            ("transportStreamId" Core..=) Core.<$> transportStreamId,
            ("videoPid" Core..=) Core.<$> videoPid
          ]
      )

instance Core.FromJSON M3u8Settings where
  parseJSON =
    Core.withObject "M3u8Settings" Core.$
      \x ->
        M3u8Settings'
          Core.<$> (x Core..:? "audioDuration")
          Core.<*> (x Core..:? "audioFramesPerPes")
          Core.<*> (x Core..:? "audioPids")
          Core.<*> (x Core..:? "nielsenId3")
          Core.<*> (x Core..:? "patInterval")
          Core.<*> (x Core..:? "pcrControl")
          Core.<*> (x Core..:? "pcrPid")
          Core.<*> (x Core..:? "pmtInterval")
          Core.<*> (x Core..:? "pmtPid")
          Core.<*> (x Core..:? "privateMetadataPid")
          Core.<*> (x Core..:? "programNumber")
          Core.<*> (x Core..:? "scte35Pid")
          Core.<*> (x Core..:? "scte35Source")
          Core.<*> (x Core..:? "timedMetadata")
          Core.<*> (x Core..:? "timedMetadataPid")
          Core.<*> (x Core..:? "transportStreamId")
          Core.<*> (x Core..:? "videoPid")
