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
    mPmtPid,
    mVideoPid,
    mNielsenId3Behavior,
    mScte35Pid,
    mTransportStreamId,
    mProgramNum,
    mTimedMetadataBehavior,
    mPmtInterval,
    mEcmPid,
    mTimedMetadataPid,
    mAudioFramesPerPes,
    mPcrPeriod,
    mPcrPid,
    mPatInterval,
    mAudioPids,
    mScte35Behavior,
    mPcrControl,
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
  { -- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
    pmtPid :: Lude.Maybe Lude.Text,
    -- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
    videoPid :: Lude.Maybe Lude.Text,
    -- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
    nielsenId3Behavior :: Lude.Maybe M3u8NielsenId3Behavior,
    -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
    scte35Pid :: Lude.Maybe Lude.Text,
    -- | The value of the transport stream ID field in the Program Map Table.
    transportStreamId :: Lude.Maybe Lude.Natural,
    -- | The value of the program number field in the Program Map Table.
    programNum :: Lude.Maybe Lude.Natural,
    -- | When set to passthrough, timed metadata is passed through from input to output.
    timedMetadataBehavior :: Lude.Maybe M3u8TimedMetadataBehavior,
    -- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
    pmtInterval :: Lude.Maybe Lude.Natural,
    -- | This parameter is unused and deprecated.
    ecmPid :: Lude.Maybe Lude.Text,
    -- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
    timedMetadataPid :: Lude.Maybe Lude.Text,
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Lude.Maybe Lude.Natural,
    -- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
    pcrPeriod :: Lude.Maybe Lude.Natural,
    -- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
    pcrPid :: Lude.Maybe Lude.Text,
    -- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
    patInterval :: Lude.Maybe Lude.Natural,
    -- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
    audioPids :: Lude.Maybe Lude.Text,
    -- | If set to passthrough, passes any SCTE-35 signals from the input source to this output.
    scte35Behavior :: Lude.Maybe M3u8Scte35Behavior,
    -- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
    pcrControl :: Lude.Maybe M3u8PcrControl
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'M3u8Settings' with the minimum fields required to make a request.
--
-- * 'pmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
-- * 'videoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
-- * 'nielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
-- * 'scte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
-- * 'transportStreamId' - The value of the transport stream ID field in the Program Map Table.
-- * 'programNum' - The value of the program number field in the Program Map Table.
-- * 'timedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to output.
-- * 'pmtInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
-- * 'ecmPid' - This parameter is unused and deprecated.
-- * 'timedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
-- * 'audioFramesPerPes' - The number of audio frames to insert for each PES packet.
-- * 'pcrPeriod' - Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
-- * 'pcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
-- * 'patInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
-- * 'audioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
-- * 'scte35Behavior' - If set to passthrough, passes any SCTE-35 signals from the input source to this output.
-- * 'pcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
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
mPmtPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mPmtPid = Lens.lens (pmtPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {pmtPid = a} :: M3u8Settings)
{-# DEPRECATED mPmtPid "Use generic-lens or generic-optics with 'pmtPid' instead." #-}

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mVideoPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mVideoPid = Lens.lens (videoPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {videoPid = a} :: M3u8Settings)
{-# DEPRECATED mVideoPid "Use generic-lens or generic-optics with 'videoPid' instead." #-}

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mNielsenId3Behavior :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8NielsenId3Behavior)
mNielsenId3Behavior = Lens.lens (nielsenId3Behavior :: M3u8Settings -> Lude.Maybe M3u8NielsenId3Behavior) (\s a -> s {nielsenId3Behavior = a} :: M3u8Settings)
{-# DEPRECATED mNielsenId3Behavior "Use generic-lens or generic-optics with 'nielsenId3Behavior' instead." #-}

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mScte35Pid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mScte35Pid = Lens.lens (scte35Pid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {scte35Pid = a} :: M3u8Settings)
{-# DEPRECATED mScte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead." #-}

-- | The value of the transport stream ID field in the Program Map Table.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTransportStreamId :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mTransportStreamId = Lens.lens (transportStreamId :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {transportStreamId = a} :: M3u8Settings)
{-# DEPRECATED mTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

-- | The value of the program number field in the Program Map Table.
--
-- /Note:/ Consider using 'programNum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mProgramNum :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mProgramNum = Lens.lens (programNum :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {programNum = a} :: M3u8Settings)
{-# DEPRECATED mProgramNum "Use generic-lens or generic-optics with 'programNum' instead." #-}

-- | When set to passthrough, timed metadata is passed through from input to output.
--
-- /Note:/ Consider using 'timedMetadataBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTimedMetadataBehavior :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8TimedMetadataBehavior)
mTimedMetadataBehavior = Lens.lens (timedMetadataBehavior :: M3u8Settings -> Lude.Maybe M3u8TimedMetadataBehavior) (\s a -> s {timedMetadataBehavior = a} :: M3u8Settings)
{-# DEPRECATED mTimedMetadataBehavior "Use generic-lens or generic-optics with 'timedMetadataBehavior' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- /Note:/ Consider using 'pmtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPmtInterval :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mPmtInterval = Lens.lens (pmtInterval :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {pmtInterval = a} :: M3u8Settings)
{-# DEPRECATED mPmtInterval "Use generic-lens or generic-optics with 'pmtInterval' instead." #-}

-- | This parameter is unused and deprecated.
--
-- /Note:/ Consider using 'ecmPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEcmPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mEcmPid = Lens.lens (ecmPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {ecmPid = a} :: M3u8Settings)
{-# DEPRECATED mEcmPid "Use generic-lens or generic-optics with 'ecmPid' instead." #-}

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTimedMetadataPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mTimedMetadataPid = Lens.lens (timedMetadataPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {timedMetadataPid = a} :: M3u8Settings)
{-# DEPRECATED mTimedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead." #-}

-- | The number of audio frames to insert for each PES packet.
--
-- /Note:/ Consider using 'audioFramesPerPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioFramesPerPes :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mAudioFramesPerPes = Lens.lens (audioFramesPerPes :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {audioFramesPerPes = a} :: M3u8Settings)
{-# DEPRECATED mAudioFramesPerPes "Use generic-lens or generic-optics with 'audioFramesPerPes' instead." #-}

-- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
--
-- /Note:/ Consider using 'pcrPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPcrPeriod :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mPcrPeriod = Lens.lens (pcrPeriod :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {pcrPeriod = a} :: M3u8Settings)
{-# DEPRECATED mPcrPeriod "Use generic-lens or generic-optics with 'pcrPeriod' instead." #-}

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPcrPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mPcrPid = Lens.lens (pcrPid :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {pcrPid = a} :: M3u8Settings)
{-# DEPRECATED mPcrPid "Use generic-lens or generic-optics with 'pcrPid' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- /Note:/ Consider using 'patInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPatInterval :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
mPatInterval = Lens.lens (patInterval :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {patInterval = a} :: M3u8Settings)
{-# DEPRECATED mPatInterval "Use generic-lens or generic-optics with 'patInterval' instead." #-}

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioPids :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Text)
mAudioPids = Lens.lens (audioPids :: M3u8Settings -> Lude.Maybe Lude.Text) (\s a -> s {audioPids = a} :: M3u8Settings)
{-# DEPRECATED mAudioPids "Use generic-lens or generic-optics with 'audioPids' instead." #-}

-- | If set to passthrough, passes any SCTE-35 signals from the input source to this output.
--
-- /Note:/ Consider using 'scte35Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mScte35Behavior :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8Scte35Behavior)
mScte35Behavior = Lens.lens (scte35Behavior :: M3u8Settings -> Lude.Maybe M3u8Scte35Behavior) (\s a -> s {scte35Behavior = a} :: M3u8Settings)
{-# DEPRECATED mScte35Behavior "Use generic-lens or generic-optics with 'scte35Behavior' instead." #-}

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- /Note:/ Consider using 'pcrControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPcrControl :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8PcrControl)
mPcrControl = Lens.lens (pcrControl :: M3u8Settings -> Lude.Maybe M3u8PcrControl) (\s a -> s {pcrControl = a} :: M3u8Settings)
{-# DEPRECATED mPcrControl "Use generic-lens or generic-optics with 'pcrControl' instead." #-}

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
