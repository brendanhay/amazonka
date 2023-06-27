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
-- Module      : Amazonka.MediaLive.Types.M3u8Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M3u8Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.M3u8NielsenId3Behavior
import Amazonka.MediaLive.Types.M3u8PcrControl
import Amazonka.MediaLive.Types.M3u8Scte35Behavior
import Amazonka.MediaLive.Types.M3u8TimedMetadataBehavior
import qualified Amazonka.Prelude as Prelude

-- | Settings information for the .m3u8 container
--
-- /See:/ 'newM3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the elementary audio stream(s) in the
    -- transport stream. Multiple values are accepted, and can be entered in
    -- ranges and\/or by comma separation. Can be entered as decimal or
    -- hexadecimal values.
    audioPids :: Prelude.Maybe Prelude.Text,
    -- | This parameter is unused and deprecated.
    ecmPid :: Prelude.Maybe Prelude.Text,
    -- | If set to passthrough, Nielsen inaudible tones for media tracking will
    -- be detected in the input audio and an equivalent ID3 tag will be
    -- inserted in the output.
    nielsenId3Behavior :: Prelude.Maybe M3u8NielsenId3Behavior,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream. A value of \\\"0\\\" writes out the PMT once per
    -- segment file.
    patInterval :: Prelude.Maybe Prelude.Natural,
    -- | When set to pcrEveryPesPacket, a Program Clock Reference value is
    -- inserted for every Packetized Elementary Stream (PES) header. This
    -- parameter is effective only when the PCR PID is the same as the video or
    -- audio elementary stream.
    pcrControl :: Prelude.Maybe M3u8PcrControl,
    -- | Maximum time in milliseconds between Program Clock References (PCRs)
    -- inserted into the transport stream.
    pcrPeriod :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
    -- transport stream. When no value is given, the encoder will assign the
    -- same value as the Video PID. Can be entered as a decimal or hexadecimal
    -- value.
    pcrPid :: Prelude.Maybe Prelude.Text,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream. A value of \\\"0\\\" writes out the PMT once per
    -- segment file.
    pmtInterval :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
    -- stream. Can be entered as a decimal or hexadecimal value.
    pmtPid :: Prelude.Maybe Prelude.Text,
    -- | The value of the program number field in the Program Map Table.
    programNum :: Prelude.Maybe Prelude.Natural,
    -- | If set to passthrough, passes any SCTE-35 signals from the input source
    -- to this output.
    scte35Behavior :: Prelude.Maybe M3u8Scte35Behavior,
    -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
    -- Can be entered as a decimal or hexadecimal value.
    scte35Pid :: Prelude.Maybe Prelude.Text,
    -- | When set to passthrough, timed metadata is passed through from input to
    -- output.
    timedMetadataBehavior :: Prelude.Maybe M3u8TimedMetadataBehavior,
    -- | Packet Identifier (PID) of the timed metadata stream in the transport
    -- stream. Can be entered as a decimal or hexadecimal value. Valid values
    -- are 32 (or 0x20)..8182 (or 0x1ff6).
    timedMetadataPid :: Prelude.Maybe Prelude.Text,
    -- | The value of the transport stream ID field in the Program Map Table.
    transportStreamId :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the elementary video stream in the transport
    -- stream. Can be entered as a decimal or hexadecimal value.
    videoPid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'M3u8Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioFramesPerPes', 'm3u8Settings_audioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- 'audioPids', 'm3u8Settings_audioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation. Can be entered as decimal or
-- hexadecimal values.
--
-- 'ecmPid', 'm3u8Settings_ecmPid' - This parameter is unused and deprecated.
--
-- 'nielsenId3Behavior', 'm3u8Settings_nielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will
-- be detected in the input audio and an equivalent ID3 tag will be
-- inserted in the output.
--
-- 'patInterval', 'm3u8Settings_patInterval' - The number of milliseconds between instances of this table in the output
-- transport stream. A value of \\\"0\\\" writes out the PMT once per
-- segment file.
--
-- 'pcrControl', 'm3u8Settings_pcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
--
-- 'pcrPeriod', 'm3u8Settings_pcrPeriod' - Maximum time in milliseconds between Program Clock References (PCRs)
-- inserted into the transport stream.
--
-- 'pcrPid', 'm3u8Settings_pcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID. Can be entered as a decimal or hexadecimal
-- value.
--
-- 'pmtInterval', 'm3u8Settings_pmtInterval' - The number of milliseconds between instances of this table in the output
-- transport stream. A value of \\\"0\\\" writes out the PMT once per
-- segment file.
--
-- 'pmtPid', 'm3u8Settings_pmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream. Can be entered as a decimal or hexadecimal value.
--
-- 'programNum', 'm3u8Settings_programNum' - The value of the program number field in the Program Map Table.
--
-- 'scte35Behavior', 'm3u8Settings_scte35Behavior' - If set to passthrough, passes any SCTE-35 signals from the input source
-- to this output.
--
-- 'scte35Pid', 'm3u8Settings_scte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
-- Can be entered as a decimal or hexadecimal value.
--
-- 'timedMetadataBehavior', 'm3u8Settings_timedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to
-- output.
--
-- 'timedMetadataPid', 'm3u8Settings_timedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- 'transportStreamId', 'm3u8Settings_transportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- 'videoPid', 'm3u8Settings_videoPid' - Packet Identifier (PID) of the elementary video stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value.
newM3u8Settings ::
  M3u8Settings
newM3u8Settings =
  M3u8Settings'
    { audioFramesPerPes = Prelude.Nothing,
      audioPids = Prelude.Nothing,
      ecmPid = Prelude.Nothing,
      nielsenId3Behavior = Prelude.Nothing,
      patInterval = Prelude.Nothing,
      pcrControl = Prelude.Nothing,
      pcrPeriod = Prelude.Nothing,
      pcrPid = Prelude.Nothing,
      pmtInterval = Prelude.Nothing,
      pmtPid = Prelude.Nothing,
      programNum = Prelude.Nothing,
      scte35Behavior = Prelude.Nothing,
      scte35Pid = Prelude.Nothing,
      timedMetadataBehavior = Prelude.Nothing,
      timedMetadataPid = Prelude.Nothing,
      transportStreamId = Prelude.Nothing,
      videoPid = Prelude.Nothing
    }

-- | The number of audio frames to insert for each PES packet.
m3u8Settings_audioFramesPerPes :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_audioFramesPerPes = Lens.lens (\M3u8Settings' {audioFramesPerPes} -> audioFramesPerPes) (\s@M3u8Settings' {} a -> s {audioFramesPerPes = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation. Can be entered as decimal or
-- hexadecimal values.
m3u8Settings_audioPids :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Text)
m3u8Settings_audioPids = Lens.lens (\M3u8Settings' {audioPids} -> audioPids) (\s@M3u8Settings' {} a -> s {audioPids = a} :: M3u8Settings)

-- | This parameter is unused and deprecated.
m3u8Settings_ecmPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Text)
m3u8Settings_ecmPid = Lens.lens (\M3u8Settings' {ecmPid} -> ecmPid) (\s@M3u8Settings' {} a -> s {ecmPid = a} :: M3u8Settings)

-- | If set to passthrough, Nielsen inaudible tones for media tracking will
-- be detected in the input audio and an equivalent ID3 tag will be
-- inserted in the output.
m3u8Settings_nielsenId3Behavior :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8NielsenId3Behavior)
m3u8Settings_nielsenId3Behavior = Lens.lens (\M3u8Settings' {nielsenId3Behavior} -> nielsenId3Behavior) (\s@M3u8Settings' {} a -> s {nielsenId3Behavior = a} :: M3u8Settings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream. A value of \\\"0\\\" writes out the PMT once per
-- segment file.
m3u8Settings_patInterval :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_patInterval = Lens.lens (\M3u8Settings' {patInterval} -> patInterval) (\s@M3u8Settings' {} a -> s {patInterval = a} :: M3u8Settings)

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
m3u8Settings_pcrControl :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8PcrControl)
m3u8Settings_pcrControl = Lens.lens (\M3u8Settings' {pcrControl} -> pcrControl) (\s@M3u8Settings' {} a -> s {pcrControl = a} :: M3u8Settings)

-- | Maximum time in milliseconds between Program Clock References (PCRs)
-- inserted into the transport stream.
m3u8Settings_pcrPeriod :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_pcrPeriod = Lens.lens (\M3u8Settings' {pcrPeriod} -> pcrPeriod) (\s@M3u8Settings' {} a -> s {pcrPeriod = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID. Can be entered as a decimal or hexadecimal
-- value.
m3u8Settings_pcrPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Text)
m3u8Settings_pcrPid = Lens.lens (\M3u8Settings' {pcrPid} -> pcrPid) (\s@M3u8Settings' {} a -> s {pcrPid = a} :: M3u8Settings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream. A value of \\\"0\\\" writes out the PMT once per
-- segment file.
m3u8Settings_pmtInterval :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_pmtInterval = Lens.lens (\M3u8Settings' {pmtInterval} -> pmtInterval) (\s@M3u8Settings' {} a -> s {pmtInterval = a} :: M3u8Settings)

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream. Can be entered as a decimal or hexadecimal value.
m3u8Settings_pmtPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Text)
m3u8Settings_pmtPid = Lens.lens (\M3u8Settings' {pmtPid} -> pmtPid) (\s@M3u8Settings' {} a -> s {pmtPid = a} :: M3u8Settings)

-- | The value of the program number field in the Program Map Table.
m3u8Settings_programNum :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_programNum = Lens.lens (\M3u8Settings' {programNum} -> programNum) (\s@M3u8Settings' {} a -> s {programNum = a} :: M3u8Settings)

-- | If set to passthrough, passes any SCTE-35 signals from the input source
-- to this output.
m3u8Settings_scte35Behavior :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8Scte35Behavior)
m3u8Settings_scte35Behavior = Lens.lens (\M3u8Settings' {scte35Behavior} -> scte35Behavior) (\s@M3u8Settings' {} a -> s {scte35Behavior = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
-- Can be entered as a decimal or hexadecimal value.
m3u8Settings_scte35Pid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Text)
m3u8Settings_scte35Pid = Lens.lens (\M3u8Settings' {scte35Pid} -> scte35Pid) (\s@M3u8Settings' {} a -> s {scte35Pid = a} :: M3u8Settings)

-- | When set to passthrough, timed metadata is passed through from input to
-- output.
m3u8Settings_timedMetadataBehavior :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8TimedMetadataBehavior)
m3u8Settings_timedMetadataBehavior = Lens.lens (\M3u8Settings' {timedMetadataBehavior} -> timedMetadataBehavior) (\s@M3u8Settings' {} a -> s {timedMetadataBehavior = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the timed metadata stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value. Valid values
-- are 32 (or 0x20)..8182 (or 0x1ff6).
m3u8Settings_timedMetadataPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Text)
m3u8Settings_timedMetadataPid = Lens.lens (\M3u8Settings' {timedMetadataPid} -> timedMetadataPid) (\s@M3u8Settings' {} a -> s {timedMetadataPid = a} :: M3u8Settings)

-- | The value of the transport stream ID field in the Program Map Table.
m3u8Settings_transportStreamId :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_transportStreamId = Lens.lens (\M3u8Settings' {transportStreamId} -> transportStreamId) (\s@M3u8Settings' {} a -> s {transportStreamId = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the elementary video stream in the transport
-- stream. Can be entered as a decimal or hexadecimal value.
m3u8Settings_videoPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Text)
m3u8Settings_videoPid = Lens.lens (\M3u8Settings' {videoPid} -> videoPid) (\s@M3u8Settings' {} a -> s {videoPid = a} :: M3u8Settings)

instance Data.FromJSON M3u8Settings where
  parseJSON =
    Data.withObject
      "M3u8Settings"
      ( \x ->
          M3u8Settings'
            Prelude.<$> (x Data..:? "audioFramesPerPes")
            Prelude.<*> (x Data..:? "audioPids")
            Prelude.<*> (x Data..:? "ecmPid")
            Prelude.<*> (x Data..:? "nielsenId3Behavior")
            Prelude.<*> (x Data..:? "patInterval")
            Prelude.<*> (x Data..:? "pcrControl")
            Prelude.<*> (x Data..:? "pcrPeriod")
            Prelude.<*> (x Data..:? "pcrPid")
            Prelude.<*> (x Data..:? "pmtInterval")
            Prelude.<*> (x Data..:? "pmtPid")
            Prelude.<*> (x Data..:? "programNum")
            Prelude.<*> (x Data..:? "scte35Behavior")
            Prelude.<*> (x Data..:? "scte35Pid")
            Prelude.<*> (x Data..:? "timedMetadataBehavior")
            Prelude.<*> (x Data..:? "timedMetadataPid")
            Prelude.<*> (x Data..:? "transportStreamId")
            Prelude.<*> (x Data..:? "videoPid")
      )

instance Prelude.Hashable M3u8Settings where
  hashWithSalt _salt M3u8Settings' {..} =
    _salt
      `Prelude.hashWithSalt` audioFramesPerPes
      `Prelude.hashWithSalt` audioPids
      `Prelude.hashWithSalt` ecmPid
      `Prelude.hashWithSalt` nielsenId3Behavior
      `Prelude.hashWithSalt` patInterval
      `Prelude.hashWithSalt` pcrControl
      `Prelude.hashWithSalt` pcrPeriod
      `Prelude.hashWithSalt` pcrPid
      `Prelude.hashWithSalt` pmtInterval
      `Prelude.hashWithSalt` pmtPid
      `Prelude.hashWithSalt` programNum
      `Prelude.hashWithSalt` scte35Behavior
      `Prelude.hashWithSalt` scte35Pid
      `Prelude.hashWithSalt` timedMetadataBehavior
      `Prelude.hashWithSalt` timedMetadataPid
      `Prelude.hashWithSalt` transportStreamId
      `Prelude.hashWithSalt` videoPid

instance Prelude.NFData M3u8Settings where
  rnf M3u8Settings' {..} =
    Prelude.rnf audioFramesPerPes
      `Prelude.seq` Prelude.rnf audioPids
      `Prelude.seq` Prelude.rnf ecmPid
      `Prelude.seq` Prelude.rnf nielsenId3Behavior
      `Prelude.seq` Prelude.rnf patInterval
      `Prelude.seq` Prelude.rnf pcrControl
      `Prelude.seq` Prelude.rnf pcrPeriod
      `Prelude.seq` Prelude.rnf pcrPid
      `Prelude.seq` Prelude.rnf pmtInterval
      `Prelude.seq` Prelude.rnf pmtPid
      `Prelude.seq` Prelude.rnf programNum
      `Prelude.seq` Prelude.rnf scte35Behavior
      `Prelude.seq` Prelude.rnf scte35Pid
      `Prelude.seq` Prelude.rnf timedMetadataBehavior
      `Prelude.seq` Prelude.rnf timedMetadataPid
      `Prelude.seq` Prelude.rnf transportStreamId
      `Prelude.seq` Prelude.rnf videoPid

instance Data.ToJSON M3u8Settings where
  toJSON M3u8Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioFramesPerPes" Data..=)
              Prelude.<$> audioFramesPerPes,
            ("audioPids" Data..=) Prelude.<$> audioPids,
            ("ecmPid" Data..=) Prelude.<$> ecmPid,
            ("nielsenId3Behavior" Data..=)
              Prelude.<$> nielsenId3Behavior,
            ("patInterval" Data..=) Prelude.<$> patInterval,
            ("pcrControl" Data..=) Prelude.<$> pcrControl,
            ("pcrPeriod" Data..=) Prelude.<$> pcrPeriod,
            ("pcrPid" Data..=) Prelude.<$> pcrPid,
            ("pmtInterval" Data..=) Prelude.<$> pmtInterval,
            ("pmtPid" Data..=) Prelude.<$> pmtPid,
            ("programNum" Data..=) Prelude.<$> programNum,
            ("scte35Behavior" Data..=)
              Prelude.<$> scte35Behavior,
            ("scte35Pid" Data..=) Prelude.<$> scte35Pid,
            ("timedMetadataBehavior" Data..=)
              Prelude.<$> timedMetadataBehavior,
            ("timedMetadataPid" Data..=)
              Prelude.<$> timedMetadataPid,
            ("transportStreamId" Data..=)
              Prelude.<$> transportStreamId,
            ("videoPid" Data..=) Prelude.<$> videoPid
          ]
      )
