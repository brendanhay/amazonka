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
-- Module      : Network.AWS.MediaLive.Types.M3u8Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
import Network.AWS.MediaLive.Types.M3u8PcrControl
import Network.AWS.MediaLive.Types.M3u8Scte35Behavior
import Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior

-- | Settings information for the .m3u8 container
--
-- /See:/ 'newM3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { -- | Maximum time in milliseconds between Program Clock References (PCRs)
    -- inserted into the transport stream.
    pcrPeriod :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
    -- stream. Can be entered as a decimal or hexadecimal value.
    pmtPid :: Core.Maybe Core.Text,
    -- | Packet Identifier (PID) of the elementary video stream in the transport
    -- stream. Can be entered as a decimal or hexadecimal value.
    videoPid :: Core.Maybe Core.Text,
    -- | If set to passthrough, Nielsen inaudible tones for media tracking will
    -- be detected in the input audio and an equivalent ID3 tag will be
    -- inserted in the output.
    nielsenId3Behavior :: Core.Maybe M3u8NielsenId3Behavior,
    -- | Packet Identifier (PID) of the timed metadata stream in the transport
    -- stream. Can be entered as a decimal or hexadecimal value. Valid values
    -- are 32 (or 0x20)..8182 (or 0x1ff6).
    timedMetadataPid :: Core.Maybe Core.Text,
    -- | When set to pcrEveryPesPacket, a Program Clock Reference value is
    -- inserted for every Packetized Elementary Stream (PES) header. This
    -- parameter is effective only when the PCR PID is the same as the video or
    -- audio elementary stream.
    pcrControl :: Core.Maybe M3u8PcrControl,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream. A value of \\\"0\\\" writes out the PMT once per
    -- segment file.
    pmtInterval :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the elementary audio stream(s) in the
    -- transport stream. Multiple values are accepted, and can be entered in
    -- ranges and\/or by comma separation. Can be entered as decimal or
    -- hexadecimal values.
    audioPids :: Core.Maybe Core.Text,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream. A value of \\\"0\\\" writes out the PMT once per
    -- segment file.
    patInterval :: Core.Maybe Core.Natural,
    -- | The value of the program number field in the Program Map Table.
    programNum :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
    -- transport stream. When no value is given, the encoder will assign the
    -- same value as the Video PID. Can be entered as a decimal or hexadecimal
    -- value.
    pcrPid :: Core.Maybe Core.Text,
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Core.Maybe Core.Natural,
    -- | This parameter is unused and deprecated.
    ecmPid :: Core.Maybe Core.Text,
    -- | If set to passthrough, passes any SCTE-35 signals from the input source
    -- to this output.
    scte35Behavior :: Core.Maybe M3u8Scte35Behavior,
    -- | When set to passthrough, timed metadata is passed through from input to
    -- output.
    timedMetadataBehavior :: Core.Maybe M3u8TimedMetadataBehavior,
    -- | The value of the transport stream ID field in the Program Map Table.
    transportStreamId :: Core.Maybe Core.Natural,
    -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
    -- Can be entered as a decimal or hexadecimal value.
    scte35Pid :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'M3u8Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pcrPeriod', 'm3u8Settings_pcrPeriod' - Maximum time in milliseconds between Program Clock References (PCRs)
-- inserted into the transport stream.
--
-- 'pmtPid', 'm3u8Settings_pmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream. Can be entered as a decimal or hexadecimal value.
--
-- 'videoPid', 'm3u8Settings_videoPid' - Packet Identifier (PID) of the elementary video stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value.
--
-- 'nielsenId3Behavior', 'm3u8Settings_nielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will
-- be detected in the input audio and an equivalent ID3 tag will be
-- inserted in the output.
--
-- 'timedMetadataPid', 'm3u8Settings_timedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'pcrControl', 'm3u8Settings_pcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
--
-- 'pmtInterval', 'm3u8Settings_pmtInterval' - The number of milliseconds between instances of this table in the output
-- transport stream. A value of \\\"0\\\" writes out the PMT once per
-- segment file.
--
-- 'audioPids', 'm3u8Settings_audioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation. Can be entered as decimal or
-- hexadecimal values.
--
-- 'patInterval', 'm3u8Settings_patInterval' - The number of milliseconds between instances of this table in the output
-- transport stream. A value of \\\"0\\\" writes out the PMT once per
-- segment file.
--
-- 'programNum', 'm3u8Settings_programNum' - The value of the program number field in the Program Map Table.
--
-- 'pcrPid', 'm3u8Settings_pcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID. Can be entered as a decimal or hexadecimal
-- value.
--
-- 'audioFramesPerPes', 'm3u8Settings_audioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- 'ecmPid', 'm3u8Settings_ecmPid' - This parameter is unused and deprecated.
--
-- 'scte35Behavior', 'm3u8Settings_scte35Behavior' - If set to passthrough, passes any SCTE-35 signals from the input source
-- to this output.
--
-- 'timedMetadataBehavior', 'm3u8Settings_timedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to
-- output.
--
-- 'transportStreamId', 'm3u8Settings_transportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- 'scte35Pid', 'm3u8Settings_scte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
-- Can be entered as a decimal or hexadecimal value.
newM3u8Settings ::
  M3u8Settings
newM3u8Settings =
  M3u8Settings'
    { pcrPeriod = Core.Nothing,
      pmtPid = Core.Nothing,
      videoPid = Core.Nothing,
      nielsenId3Behavior = Core.Nothing,
      timedMetadataPid = Core.Nothing,
      pcrControl = Core.Nothing,
      pmtInterval = Core.Nothing,
      audioPids = Core.Nothing,
      patInterval = Core.Nothing,
      programNum = Core.Nothing,
      pcrPid = Core.Nothing,
      audioFramesPerPes = Core.Nothing,
      ecmPid = Core.Nothing,
      scte35Behavior = Core.Nothing,
      timedMetadataBehavior = Core.Nothing,
      transportStreamId = Core.Nothing,
      scte35Pid = Core.Nothing
    }

-- | Maximum time in milliseconds between Program Clock References (PCRs)
-- inserted into the transport stream.
m3u8Settings_pcrPeriod :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
m3u8Settings_pcrPeriod = Lens.lens (\M3u8Settings' {pcrPeriod} -> pcrPeriod) (\s@M3u8Settings' {} a -> s {pcrPeriod = a} :: M3u8Settings)

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream. Can be entered as a decimal or hexadecimal value.
m3u8Settings_pmtPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
m3u8Settings_pmtPid = Lens.lens (\M3u8Settings' {pmtPid} -> pmtPid) (\s@M3u8Settings' {} a -> s {pmtPid = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the elementary video stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value.
m3u8Settings_videoPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
m3u8Settings_videoPid = Lens.lens (\M3u8Settings' {videoPid} -> videoPid) (\s@M3u8Settings' {} a -> s {videoPid = a} :: M3u8Settings)

-- | If set to passthrough, Nielsen inaudible tones for media tracking will
-- be detected in the input audio and an equivalent ID3 tag will be
-- inserted in the output.
m3u8Settings_nielsenId3Behavior :: Lens.Lens' M3u8Settings (Core.Maybe M3u8NielsenId3Behavior)
m3u8Settings_nielsenId3Behavior = Lens.lens (\M3u8Settings' {nielsenId3Behavior} -> nielsenId3Behavior) (\s@M3u8Settings' {} a -> s {nielsenId3Behavior = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the timed metadata stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
m3u8Settings_timedMetadataPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
m3u8Settings_timedMetadataPid = Lens.lens (\M3u8Settings' {timedMetadataPid} -> timedMetadataPid) (\s@M3u8Settings' {} a -> s {timedMetadataPid = a} :: M3u8Settings)

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
m3u8Settings_pcrControl :: Lens.Lens' M3u8Settings (Core.Maybe M3u8PcrControl)
m3u8Settings_pcrControl = Lens.lens (\M3u8Settings' {pcrControl} -> pcrControl) (\s@M3u8Settings' {} a -> s {pcrControl = a} :: M3u8Settings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream. A value of \\\"0\\\" writes out the PMT once per
-- segment file.
m3u8Settings_pmtInterval :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
m3u8Settings_pmtInterval = Lens.lens (\M3u8Settings' {pmtInterval} -> pmtInterval) (\s@M3u8Settings' {} a -> s {pmtInterval = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation. Can be entered as decimal or
-- hexadecimal values.
m3u8Settings_audioPids :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
m3u8Settings_audioPids = Lens.lens (\M3u8Settings' {audioPids} -> audioPids) (\s@M3u8Settings' {} a -> s {audioPids = a} :: M3u8Settings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream. A value of \\\"0\\\" writes out the PMT once per
-- segment file.
m3u8Settings_patInterval :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
m3u8Settings_patInterval = Lens.lens (\M3u8Settings' {patInterval} -> patInterval) (\s@M3u8Settings' {} a -> s {patInterval = a} :: M3u8Settings)

-- | The value of the program number field in the Program Map Table.
m3u8Settings_programNum :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
m3u8Settings_programNum = Lens.lens (\M3u8Settings' {programNum} -> programNum) (\s@M3u8Settings' {} a -> s {programNum = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID. Can be entered as a decimal or hexadecimal
-- value.
m3u8Settings_pcrPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
m3u8Settings_pcrPid = Lens.lens (\M3u8Settings' {pcrPid} -> pcrPid) (\s@M3u8Settings' {} a -> s {pcrPid = a} :: M3u8Settings)

-- | The number of audio frames to insert for each PES packet.
m3u8Settings_audioFramesPerPes :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
m3u8Settings_audioFramesPerPes = Lens.lens (\M3u8Settings' {audioFramesPerPes} -> audioFramesPerPes) (\s@M3u8Settings' {} a -> s {audioFramesPerPes = a} :: M3u8Settings)

-- | This parameter is unused and deprecated.
m3u8Settings_ecmPid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
m3u8Settings_ecmPid = Lens.lens (\M3u8Settings' {ecmPid} -> ecmPid) (\s@M3u8Settings' {} a -> s {ecmPid = a} :: M3u8Settings)

-- | If set to passthrough, passes any SCTE-35 signals from the input source
-- to this output.
m3u8Settings_scte35Behavior :: Lens.Lens' M3u8Settings (Core.Maybe M3u8Scte35Behavior)
m3u8Settings_scte35Behavior = Lens.lens (\M3u8Settings' {scte35Behavior} -> scte35Behavior) (\s@M3u8Settings' {} a -> s {scte35Behavior = a} :: M3u8Settings)

-- | When set to passthrough, timed metadata is passed through from input to
-- output.
m3u8Settings_timedMetadataBehavior :: Lens.Lens' M3u8Settings (Core.Maybe M3u8TimedMetadataBehavior)
m3u8Settings_timedMetadataBehavior = Lens.lens (\M3u8Settings' {timedMetadataBehavior} -> timedMetadataBehavior) (\s@M3u8Settings' {} a -> s {timedMetadataBehavior = a} :: M3u8Settings)

-- | The value of the transport stream ID field in the Program Map Table.
m3u8Settings_transportStreamId :: Lens.Lens' M3u8Settings (Core.Maybe Core.Natural)
m3u8Settings_transportStreamId = Lens.lens (\M3u8Settings' {transportStreamId} -> transportStreamId) (\s@M3u8Settings' {} a -> s {transportStreamId = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
-- Can be entered as a decimal or hexadecimal value.
m3u8Settings_scte35Pid :: Lens.Lens' M3u8Settings (Core.Maybe Core.Text)
m3u8Settings_scte35Pid = Lens.lens (\M3u8Settings' {scte35Pid} -> scte35Pid) (\s@M3u8Settings' {} a -> s {scte35Pid = a} :: M3u8Settings)

instance Core.FromJSON M3u8Settings where
  parseJSON =
    Core.withObject
      "M3u8Settings"
      ( \x ->
          M3u8Settings'
            Core.<$> (x Core..:? "pcrPeriod")
            Core.<*> (x Core..:? "pmtPid")
            Core.<*> (x Core..:? "videoPid")
            Core.<*> (x Core..:? "nielsenId3Behavior")
            Core.<*> (x Core..:? "timedMetadataPid")
            Core.<*> (x Core..:? "pcrControl")
            Core.<*> (x Core..:? "pmtInterval")
            Core.<*> (x Core..:? "audioPids")
            Core.<*> (x Core..:? "patInterval")
            Core.<*> (x Core..:? "programNum")
            Core.<*> (x Core..:? "pcrPid")
            Core.<*> (x Core..:? "audioFramesPerPes")
            Core.<*> (x Core..:? "ecmPid")
            Core.<*> (x Core..:? "scte35Behavior")
            Core.<*> (x Core..:? "timedMetadataBehavior")
            Core.<*> (x Core..:? "transportStreamId")
            Core.<*> (x Core..:? "scte35Pid")
      )

instance Core.Hashable M3u8Settings

instance Core.NFData M3u8Settings

instance Core.ToJSON M3u8Settings where
  toJSON M3u8Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("pcrPeriod" Core..=) Core.<$> pcrPeriod,
            ("pmtPid" Core..=) Core.<$> pmtPid,
            ("videoPid" Core..=) Core.<$> videoPid,
            ("nielsenId3Behavior" Core..=)
              Core.<$> nielsenId3Behavior,
            ("timedMetadataPid" Core..=)
              Core.<$> timedMetadataPid,
            ("pcrControl" Core..=) Core.<$> pcrControl,
            ("pmtInterval" Core..=) Core.<$> pmtInterval,
            ("audioPids" Core..=) Core.<$> audioPids,
            ("patInterval" Core..=) Core.<$> patInterval,
            ("programNum" Core..=) Core.<$> programNum,
            ("pcrPid" Core..=) Core.<$> pcrPid,
            ("audioFramesPerPes" Core..=)
              Core.<$> audioFramesPerPes,
            ("ecmPid" Core..=) Core.<$> ecmPid,
            ("scte35Behavior" Core..=) Core.<$> scte35Behavior,
            ("timedMetadataBehavior" Core..=)
              Core.<$> timedMetadataBehavior,
            ("transportStreamId" Core..=)
              Core.<$> transportStreamId,
            ("scte35Pid" Core..=) Core.<$> scte35Pid
          ]
      )
