{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8Settings
  ( M3u8Settings (..),

    -- * Smart constructor
    mkM3u8Settings,

    -- * Lenses
    msAudioFramesPerPes,
    msAudioPids,
    msEcmPid,
    msNielsenId3Behavior,
    msPatInterval,
    msPcrControl,
    msPcrPeriod,
    msPcrPid,
    msPmtInterval,
    msPmtPid,
    msProgramNum,
    msScte35Behavior,
    msScte35Pid,
    msTimedMetadataBehavior,
    msTimedMetadataPid,
    msTransportStreamId,
    msVideoPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior as Types
import qualified Network.AWS.MediaLive.Types.M3u8PcrControl as Types
import qualified Network.AWS.MediaLive.Types.M3u8Scte35Behavior as Types
import qualified Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | Settings information for the .m3u8 container
--
-- /See:/ 'mkM3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
    audioPids :: Core.Maybe Core.Text,
    -- | This parameter is unused and deprecated.
    ecmPid :: Core.Maybe Core.Text,
    -- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
    nielsenId3Behavior :: Core.Maybe Types.M3u8NielsenId3Behavior,
    -- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
    patInterval :: Core.Maybe Core.Natural,
    -- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
    pcrControl :: Core.Maybe Types.M3u8PcrControl,
    -- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
    pcrPeriod :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
    pcrPid :: Core.Maybe Core.Text,
    -- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
    pmtInterval :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
    pmtPid :: Core.Maybe Core.Text,
    -- | The value of the program number field in the Program Map Table.
    programNum :: Core.Maybe Core.Natural,
    -- | If set to passthrough, passes any SCTE-35 signals from the input source to this output.
    scte35Behavior :: Core.Maybe Types.M3u8Scte35Behavior,
    -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
    scte35Pid :: Core.Maybe Core.Text,
    -- | When set to passthrough, timed metadata is passed through from input to output.
    timedMetadataBehavior :: Core.Maybe Types.M3u8TimedMetadataBehavior,
    -- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
    timedMetadataPid :: Core.Maybe Core.Text,
    -- | The value of the transport stream ID field in the Program Map Table.
    transportStreamId :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
    videoPid :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'M3u8Settings' value with any optional fields omitted.
mkM3u8Settings ::
  M3u8Settings
mkM3u8Settings =
  M3u8Settings'
    { audioFramesPerPes = Core.Nothing,
      audioPids = Core.Nothing,
      ecmPid = Core.Nothing,
      nielsenId3Behavior = Core.Nothing,
      patInterval = Core.Nothing,
      pcrControl = Core.Nothing,
      pcrPeriod = Core.Nothing,
      pcrPid = Core.Nothing,
      pmtInterval = Core.Nothing,
      pmtPid = Core.Nothing,
      programNum = Core.Nothing,
      scte35Behavior = Core.Nothing,
      scte35Pid = Core.Nothing,
      timedMetadataBehavior = Core.Nothing,
      timedMetadataPid = Core.Nothing,
      transportStreamId = Core.Nothing,
      videoPid = Core.Nothing
    }

-- | The number of audio frames to insert for each PES packet.
--
-- /Note:/ Consider using 'audioFramesPerPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAudioFramesPerPes :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msAudioFramesPerPes = Lens.field @"audioFramesPerPes"
{-# DEPRECATED msAudioFramesPerPes "Use generic-lens or generic-optics with 'audioFramesPerPes' instead." #-}

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAudioPids :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
msAudioPids = Lens.field @"audioPids"
{-# DEPRECATED msAudioPids "Use generic-lens or generic-optics with 'audioPids' instead." #-}

-- | This parameter is unused and deprecated.
--
-- /Note:/ Consider using 'ecmPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msEcmPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
msEcmPid = Lens.field @"ecmPid"
{-# DEPRECATED msEcmPid "Use generic-lens or generic-optics with 'ecmPid' instead." #-}

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msNielsenId3Behavior :: Lens.Lens' M3u8Settings (Core.Maybe Types.M3u8NielsenId3Behavior)
msNielsenId3Behavior = Lens.field @"nielsenId3Behavior"
{-# DEPRECATED msNielsenId3Behavior "Use generic-lens or generic-optics with 'nielsenId3Behavior' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- /Note:/ Consider using 'patInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPatInterval :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msPatInterval = Lens.field @"patInterval"
{-# DEPRECATED msPatInterval "Use generic-lens or generic-optics with 'patInterval' instead." #-}

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- /Note:/ Consider using 'pcrControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPcrControl :: Lens.Lens' M3u8Settings (Core.Maybe Types.M3u8PcrControl)
msPcrControl = Lens.field @"pcrControl"
{-# DEPRECATED msPcrControl "Use generic-lens or generic-optics with 'pcrControl' instead." #-}

-- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
--
-- /Note:/ Consider using 'pcrPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPcrPeriod :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msPcrPeriod = Lens.field @"pcrPeriod"
{-# DEPRECATED msPcrPeriod "Use generic-lens or generic-optics with 'pcrPeriod' instead." #-}

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPcrPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
msPcrPid = Lens.field @"pcrPid"
{-# DEPRECATED msPcrPid "Use generic-lens or generic-optics with 'pcrPid' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- /Note:/ Consider using 'pmtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPmtInterval :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msPmtInterval = Lens.field @"pmtInterval"
{-# DEPRECATED msPmtInterval "Use generic-lens or generic-optics with 'pmtInterval' instead." #-}

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPmtPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
msPmtPid = Lens.field @"pmtPid"
{-# DEPRECATED msPmtPid "Use generic-lens or generic-optics with 'pmtPid' instead." #-}

-- | The value of the program number field in the Program Map Table.
--
-- /Note:/ Consider using 'programNum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msProgramNum :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msProgramNum = Lens.field @"programNum"
{-# DEPRECATED msProgramNum "Use generic-lens or generic-optics with 'programNum' instead." #-}

-- | If set to passthrough, passes any SCTE-35 signals from the input source to this output.
--
-- /Note:/ Consider using 'scte35Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msScte35Behavior :: Lens.Lens' M3u8Settings (Core.Maybe Types.M3u8Scte35Behavior)
msScte35Behavior = Lens.field @"scte35Behavior"
{-# DEPRECATED msScte35Behavior "Use generic-lens or generic-optics with 'scte35Behavior' instead." #-}

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msScte35Pid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
msScte35Pid = Lens.field @"scte35Pid"
{-# DEPRECATED msScte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead." #-}

-- | When set to passthrough, timed metadata is passed through from input to output.
--
-- /Note:/ Consider using 'timedMetadataBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTimedMetadataBehavior :: Lens.Lens' M3u8Settings (Core.Maybe Types.M3u8TimedMetadataBehavior)
msTimedMetadataBehavior = Lens.field @"timedMetadataBehavior"
{-# DEPRECATED msTimedMetadataBehavior "Use generic-lens or generic-optics with 'timedMetadataBehavior' instead." #-}

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTimedMetadataPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
msTimedMetadataPid = Lens.field @"timedMetadataPid"
{-# DEPRECATED msTimedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead." #-}

-- | The value of the transport stream ID field in the Program Map Table.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTransportStreamId :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
msTransportStreamId = Lens.field @"transportStreamId"
{-# DEPRECATED msTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msVideoPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
msVideoPid = Lens.field @"videoPid"
{-# DEPRECATED msVideoPid "Use generic-lens or generic-optics with 'videoPid' instead." #-}

instance Core.FromJSON M3u8Settings where
  toJSON M3u8Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("audioFramesPerPes" Core..=) Core.<$> audioFramesPerPes,
            ("audioPids" Core..=) Core.<$> audioPids,
            ("ecmPid" Core..=) Core.<$> ecmPid,
            ("nielsenId3Behavior" Core..=) Core.<$> nielsenId3Behavior,
            ("patInterval" Core..=) Core.<$> patInterval,
            ("pcrControl" Core..=) Core.<$> pcrControl,
            ("pcrPeriod" Core..=) Core.<$> pcrPeriod,
            ("pcrPid" Core..=) Core.<$> pcrPid,
            ("pmtInterval" Core..=) Core.<$> pmtInterval,
            ("pmtPid" Core..=) Core.<$> pmtPid,
            ("programNum" Core..=) Core.<$> programNum,
            ("scte35Behavior" Core..=) Core.<$> scte35Behavior,
            ("scte35Pid" Core..=) Core.<$> scte35Pid,
            ("timedMetadataBehavior" Core..=) Core.<$> timedMetadataBehavior,
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
          Core.<$> (x Core..:? "audioFramesPerPes")
          Core.<*> (x Core..:? "audioPids")
          Core.<*> (x Core..:? "ecmPid")
          Core.<*> (x Core..:? "nielsenId3Behavior")
          Core.<*> (x Core..:? "patInterval")
          Core.<*> (x Core..:? "pcrControl")
          Core.<*> (x Core..:? "pcrPeriod")
          Core.<*> (x Core..:? "pcrPid")
          Core.<*> (x Core..:? "pmtInterval")
          Core.<*> (x Core..:? "pmtPid")
          Core.<*> (x Core..:? "programNum")
          Core.<*> (x Core..:? "scte35Behavior")
          Core.<*> (x Core..:? "scte35Pid")
          Core.<*> (x Core..:? "timedMetadataBehavior")
          Core.<*> (x Core..:? "timedMetadataPid")
          Core.<*> (x Core..:? "transportStreamId")
          Core.<*> (x Core..:? "videoPid")
