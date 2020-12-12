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
    mssPmtPid,
    mssVideoPid,
    mssNielsenId3Behavior,
    mssScte35Pid,
    mssTransportStreamId,
    mssProgramNum,
    mssTimedMetadataBehavior,
    mssPmtInterval,
    mssEcmPid,
    mssTimedMetadataPid,
    mssAudioFramesPerPes,
    mssPcrPeriod,
    mssPcrPid,
    mssPatInterval,
    mssAudioPids,
    mssScte35Behavior,
    mssPcrControl,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
import Network.AWS.MediaLive.Types.M3u8PcrControl
import Network.AWS.MediaLive.Types.M3u8Scte35Behavior
import Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
import qualified Network.AWS.Prelude as Lude

-- | Settings information for the .m3u8 container
--
-- /See:/ 'mkM3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { pmtPid :: Lude.Maybe Lude.Text,
    videoPid :: Lude.Maybe Lude.Text,
    nielsenId3Behavior :: Lude.Maybe M3u8NielsenId3Behavior,
    scte35Pid :: Lude.Maybe Lude.Text,
    transportStreamId :: Lude.Maybe Lude.Natural,
    programNum :: Lude.Maybe Lude.Natural,
    timedMetadataBehavior :: Lude.Maybe M3u8TimedMetadataBehavior,
    pmtInterval :: Lude.Maybe Lude.Natural,
    ecmPid :: Lude.Maybe Lude.Text,
    timedMetadataPid :: Lude.Maybe Lude.Text,
    audioFramesPerPes :: Lude.Maybe Lude.Natural,
    pcrPeriod :: Lude.Maybe Lude.Natural,
    pcrPid :: Lude.Maybe Lude.Text,
    patInterval :: Lude.Maybe Lude.Natural,
    audioPids :: Lude.Maybe Lude.Text,
    scte35Behavior :: Lude.Maybe M3u8Scte35Behavior,
    pcrControl :: Lude.Maybe M3u8PcrControl
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'M3u8Settings' with the minimum fields required to make a request.
--
-- * 'audioFramesPerPes' - The number of audio frames to insert for each PES packet.
-- * 'audioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
-- * 'ecmPid' - This parameter is unused and deprecated.
-- * 'nielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
-- * 'patInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
-- * 'pcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
-- * 'pcrPeriod' - Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
-- * 'pcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
-- * 'pmtInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
-- * 'pmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
-- * 'programNum' - The value of the program number field in the Program Map Table.
-- * 'scte35Behavior' - If set to passthrough, passes any SCTE-35 signals from the input source to this output.
-- * 'scte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
-- * 'timedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to output.
-- * 'timedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
-- * 'transportStreamId' - The value of the transport stream ID field in the Program Map Table.
-- * 'videoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
mkM3u8Settings ::
  M3u8Settings
mkM3u8Settings =
  M3u8Settings'
    { pmtPid = Lude.Nothing,
      videoPid = Lude.Nothing,
      nielsenId3Behavior = Lude.Nothing,
      scte35Pid = Lude.Nothing,
      transportStreamId = Lude.Nothing,
      programNum = Lude.Nothing,
      timedMetadataBehavior = Lude.Nothing,
      pmtInterval = Lude.Nothing,
      ecmPid = Lude.Nothing,
      timedMetadataPid = Lude.Nothing,
      audioFramesPerPes = Lude.Nothing,
      pcrPeriod = Lude.Nothing,
      pcrPid = Lude.Nothing,
      patInterval = Lude.Nothing,
      audioPids = Lude.Nothing,
      scte35Behavior = Lude.Nothing,
      pcrControl = Lude.Nothing
    }

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPmtPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mssPmtPid = Lens.lens (pmtPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {pmtPid = a} :: M3u8Settings)
{-# DEPRECATED mssPmtPid "Use generic-lens or generic-optics with 'pmtPid' instead." #-}

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssVideoPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mssVideoPid = Lens.lens (videoPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {videoPid = a} :: M3u8Settings)
{-# DEPRECATED mssVideoPid "Use generic-lens or generic-optics with 'videoPid' instead." #-}

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssNielsenId3Behavior :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8NielsenId3Behavior)
mssNielsenId3Behavior = Lens.lens (nielsenId3Behavior :: M3u8Settings -> Lude.Maybe M3u8NielsenId3Behavior) (\s a -> s {nielsenId3Behavior = a} :: M3u8Settings)
{-# DEPRECATED mssNielsenId3Behavior "Use generic-lens or generic-optics with 'nielsenId3Behavior' instead." #-}

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScte35Pid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mssScte35Pid = Lens.lens (scte35Pid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {scte35Pid = a} :: M3u8Settings)
{-# DEPRECATED mssScte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead." #-}

-- | The value of the transport stream ID field in the Program Map Table.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTransportStreamId :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mssTransportStreamId = Lens.lens (transportStreamId :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {transportStreamId = a} :: M3u8Settings)
{-# DEPRECATED mssTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

-- | The value of the program number field in the Program Map Table.
--
-- /Note:/ Consider using 'programNum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssProgramNum :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mssProgramNum = Lens.lens (programNum :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {programNum = a} :: M3u8Settings)
{-# DEPRECATED mssProgramNum "Use generic-lens or generic-optics with 'programNum' instead." #-}

-- | When set to passthrough, timed metadata is passed through from input to output.
--
-- /Note:/ Consider using 'timedMetadataBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTimedMetadataBehavior :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8TimedMetadataBehavior)
mssTimedMetadataBehavior = Lens.lens (timedMetadataBehavior :: M3u8Settings -> Lude.Maybe M3u8TimedMetadataBehavior) (\s a -> s {timedMetadataBehavior = a} :: M3u8Settings)
{-# DEPRECATED mssTimedMetadataBehavior "Use generic-lens or generic-optics with 'timedMetadataBehavior' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- /Note:/ Consider using 'pmtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPmtInterval :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mssPmtInterval = Lens.lens (pmtInterval :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {pmtInterval = a} :: M3u8Settings)
{-# DEPRECATED mssPmtInterval "Use generic-lens or generic-optics with 'pmtInterval' instead." #-}

-- | This parameter is unused and deprecated.
--
-- /Note:/ Consider using 'ecmPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssEcmPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mssEcmPid = Lens.lens (ecmPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {ecmPid = a} :: M3u8Settings)
{-# DEPRECATED mssEcmPid "Use generic-lens or generic-optics with 'ecmPid' instead." #-}

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTimedMetadataPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mssTimedMetadataPid = Lens.lens (timedMetadataPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {timedMetadataPid = a} :: M3u8Settings)
{-# DEPRECATED mssTimedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead." #-}

-- | The number of audio frames to insert for each PES packet.
--
-- /Note:/ Consider using 'audioFramesPerPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssAudioFramesPerPes :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mssAudioFramesPerPes = Lens.lens (audioFramesPerPes :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {audioFramesPerPes = a} :: M3u8Settings)
{-# DEPRECATED mssAudioFramesPerPes "Use generic-lens or generic-optics with 'audioFramesPerPes' instead." #-}

-- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
--
-- /Note:/ Consider using 'pcrPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPcrPeriod :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mssPcrPeriod = Lens.lens (pcrPeriod :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {pcrPeriod = a} :: M3u8Settings)
{-# DEPRECATED mssPcrPeriod "Use generic-lens or generic-optics with 'pcrPeriod' instead." #-}

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPcrPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mssPcrPid = Lens.lens (pcrPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {pcrPid = a} :: M3u8Settings)
{-# DEPRECATED mssPcrPid "Use generic-lens or generic-optics with 'pcrPid' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- /Note:/ Consider using 'patInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPatInterval :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mssPatInterval = Lens.lens (patInterval :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {patInterval = a} :: M3u8Settings)
{-# DEPRECATED mssPatInterval "Use generic-lens or generic-optics with 'patInterval' instead." #-}

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssAudioPids :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mssAudioPids = Lens.lens (audioPids :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {audioPids = a} :: M3u8Settings)
{-# DEPRECATED mssAudioPids "Use generic-lens or generic-optics with 'audioPids' instead." #-}

-- | If set to passthrough, passes any SCTE-35 signals from the input source to this output.
--
-- /Note:/ Consider using 'scte35Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScte35Behavior :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8Scte35Behavior)
mssScte35Behavior = Lens.lens (scte35Behavior :: M3u8Settings -> Lude.Maybe M3u8Scte35Behavior) (\s a -> s {scte35Behavior = a} :: M3u8Settings)
{-# DEPRECATED mssScte35Behavior "Use generic-lens or generic-optics with 'scte35Behavior' instead." #-}

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- /Note:/ Consider using 'pcrControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssPcrControl :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8PcrControl)
mssPcrControl = Lens.lens (pcrControl :: M3u8Settings -> Lude.Maybe M3u8PcrControl) (\s a -> s {pcrControl = a} :: M3u8Settings)
{-# DEPRECATED mssPcrControl "Use generic-lens or generic-optics with 'pcrControl' instead." #-}

instance Lude.FromJSON M3u8Settings where
  parseJSON =
    Lude.withObject
      "M3u8Settings"
      ( \x ->
          M3u8Settings'
            Lude.<$> (x Lude..:? "pmtPid")
            Lude.<*> (x Lude..:? "videoPid")
            Lude.<*> (x Lude..:? "nielsenId3Behavior")
            Lude.<*> (x Lude..:? "scte35Pid")
            Lude.<*> (x Lude..:? "transportStreamId")
            Lude.<*> (x Lude..:? "programNum")
            Lude.<*> (x Lude..:? "timedMetadataBehavior")
            Lude.<*> (x Lude..:? "pmtInterval")
            Lude.<*> (x Lude..:? "ecmPid")
            Lude.<*> (x Lude..:? "timedMetadataPid")
            Lude.<*> (x Lude..:? "audioFramesPerPes")
            Lude.<*> (x Lude..:? "pcrPeriod")
            Lude.<*> (x Lude..:? "pcrPid")
            Lude.<*> (x Lude..:? "patInterval")
            Lude.<*> (x Lude..:? "audioPids")
            Lude.<*> (x Lude..:? "scte35Behavior")
            Lude.<*> (x Lude..:? "pcrControl")
      )

instance Lude.ToJSON M3u8Settings where
  toJSON M3u8Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("pmtPid" Lude..=) Lude.<$> pmtPid,
            ("videoPid" Lude..=) Lude.<$> videoPid,
            ("nielsenId3Behavior" Lude..=) Lude.<$> nielsenId3Behavior,
            ("scte35Pid" Lude..=) Lude.<$> scte35Pid,
            ("transportStreamId" Lude..=) Lude.<$> transportStreamId,
            ("programNum" Lude..=) Lude.<$> programNum,
            ("timedMetadataBehavior" Lude..=) Lude.<$> timedMetadataBehavior,
            ("pmtInterval" Lude..=) Lude.<$> pmtInterval,
            ("ecmPid" Lude..=) Lude.<$> ecmPid,
            ("timedMetadataPid" Lude..=) Lude.<$> timedMetadataPid,
            ("audioFramesPerPes" Lude..=) Lude.<$> audioFramesPerPes,
            ("pcrPeriod" Lude..=) Lude.<$> pcrPeriod,
            ("pcrPid" Lude..=) Lude.<$> pcrPid,
            ("patInterval" Lude..=) Lude.<$> patInterval,
            ("audioPids" Lude..=) Lude.<$> audioPids,
            ("scte35Behavior" Lude..=) Lude.<$> scte35Behavior,
            ("pcrControl" Lude..=) Lude.<$> pcrControl
          ]
      )
