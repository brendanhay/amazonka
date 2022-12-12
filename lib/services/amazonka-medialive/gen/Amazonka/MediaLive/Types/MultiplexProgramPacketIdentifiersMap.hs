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
-- Module      : Amazonka.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexProgramPacketIdentifiersMap where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Packet identifiers map for a given Multiplex program.
--
-- /See:/ 'newMultiplexProgramPacketIdentifiersMap' smart constructor.
data MultiplexProgramPacketIdentifiersMap = MultiplexProgramPacketIdentifiersMap'
  { audioPids :: Prelude.Maybe [Prelude.Int],
    dvbSubPids :: Prelude.Maybe [Prelude.Int],
    dvbTeletextPid :: Prelude.Maybe Prelude.Int,
    etvPlatformPid :: Prelude.Maybe Prelude.Int,
    etvSignalPid :: Prelude.Maybe Prelude.Int,
    klvDataPids :: Prelude.Maybe [Prelude.Int],
    pcrPid :: Prelude.Maybe Prelude.Int,
    pmtPid :: Prelude.Maybe Prelude.Int,
    privateMetadataPid :: Prelude.Maybe Prelude.Int,
    scte27Pids :: Prelude.Maybe [Prelude.Int],
    scte35Pid :: Prelude.Maybe Prelude.Int,
    timedMetadataPid :: Prelude.Maybe Prelude.Int,
    videoPid :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexProgramPacketIdentifiersMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioPids', 'multiplexProgramPacketIdentifiersMap_audioPids' - Undocumented member.
--
-- 'dvbSubPids', 'multiplexProgramPacketIdentifiersMap_dvbSubPids' - Undocumented member.
--
-- 'dvbTeletextPid', 'multiplexProgramPacketIdentifiersMap_dvbTeletextPid' - Undocumented member.
--
-- 'etvPlatformPid', 'multiplexProgramPacketIdentifiersMap_etvPlatformPid' - Undocumented member.
--
-- 'etvSignalPid', 'multiplexProgramPacketIdentifiersMap_etvSignalPid' - Undocumented member.
--
-- 'klvDataPids', 'multiplexProgramPacketIdentifiersMap_klvDataPids' - Undocumented member.
--
-- 'pcrPid', 'multiplexProgramPacketIdentifiersMap_pcrPid' - Undocumented member.
--
-- 'pmtPid', 'multiplexProgramPacketIdentifiersMap_pmtPid' - Undocumented member.
--
-- 'privateMetadataPid', 'multiplexProgramPacketIdentifiersMap_privateMetadataPid' - Undocumented member.
--
-- 'scte27Pids', 'multiplexProgramPacketIdentifiersMap_scte27Pids' - Undocumented member.
--
-- 'scte35Pid', 'multiplexProgramPacketIdentifiersMap_scte35Pid' - Undocumented member.
--
-- 'timedMetadataPid', 'multiplexProgramPacketIdentifiersMap_timedMetadataPid' - Undocumented member.
--
-- 'videoPid', 'multiplexProgramPacketIdentifiersMap_videoPid' - Undocumented member.
newMultiplexProgramPacketIdentifiersMap ::
  MultiplexProgramPacketIdentifiersMap
newMultiplexProgramPacketIdentifiersMap =
  MultiplexProgramPacketIdentifiersMap'
    { audioPids =
        Prelude.Nothing,
      dvbSubPids = Prelude.Nothing,
      dvbTeletextPid = Prelude.Nothing,
      etvPlatformPid = Prelude.Nothing,
      etvSignalPid = Prelude.Nothing,
      klvDataPids = Prelude.Nothing,
      pcrPid = Prelude.Nothing,
      pmtPid = Prelude.Nothing,
      privateMetadataPid = Prelude.Nothing,
      scte27Pids = Prelude.Nothing,
      scte35Pid = Prelude.Nothing,
      timedMetadataPid = Prelude.Nothing,
      videoPid = Prelude.Nothing
    }

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_audioPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe [Prelude.Int])
multiplexProgramPacketIdentifiersMap_audioPids = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {audioPids} -> audioPids) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {audioPids = a} :: MultiplexProgramPacketIdentifiersMap) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_dvbSubPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe [Prelude.Int])
multiplexProgramPacketIdentifiersMap_dvbSubPids = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {dvbSubPids} -> dvbSubPids) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {dvbSubPids = a} :: MultiplexProgramPacketIdentifiersMap) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_dvbTeletextPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_dvbTeletextPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {dvbTeletextPid} -> dvbTeletextPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {dvbTeletextPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_etvPlatformPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_etvPlatformPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {etvPlatformPid} -> etvPlatformPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {etvPlatformPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_etvSignalPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_etvSignalPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {etvSignalPid} -> etvSignalPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {etvSignalPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_klvDataPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe [Prelude.Int])
multiplexProgramPacketIdentifiersMap_klvDataPids = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {klvDataPids} -> klvDataPids) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {klvDataPids = a} :: MultiplexProgramPacketIdentifiersMap) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_pcrPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_pcrPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {pcrPid} -> pcrPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {pcrPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_pmtPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_pmtPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {pmtPid} -> pmtPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {pmtPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_privateMetadataPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_privateMetadataPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {privateMetadataPid} -> privateMetadataPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {privateMetadataPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_scte27Pids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe [Prelude.Int])
multiplexProgramPacketIdentifiersMap_scte27Pids = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {scte27Pids} -> scte27Pids) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {scte27Pids = a} :: MultiplexProgramPacketIdentifiersMap) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_scte35Pid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_scte35Pid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {scte35Pid} -> scte35Pid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {scte35Pid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_timedMetadataPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_timedMetadataPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {timedMetadataPid} -> timedMetadataPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {timedMetadataPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_videoPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Prelude.Maybe Prelude.Int)
multiplexProgramPacketIdentifiersMap_videoPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {videoPid} -> videoPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {videoPid = a} :: MultiplexProgramPacketIdentifiersMap)

instance
  Data.FromJSON
    MultiplexProgramPacketIdentifiersMap
  where
  parseJSON =
    Data.withObject
      "MultiplexProgramPacketIdentifiersMap"
      ( \x ->
          MultiplexProgramPacketIdentifiersMap'
            Prelude.<$> (x Data..:? "audioPids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "dvbSubPids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "dvbTeletextPid")
            Prelude.<*> (x Data..:? "etvPlatformPid")
            Prelude.<*> (x Data..:? "etvSignalPid")
            Prelude.<*> (x Data..:? "klvDataPids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "pcrPid")
            Prelude.<*> (x Data..:? "pmtPid")
            Prelude.<*> (x Data..:? "privateMetadataPid")
            Prelude.<*> (x Data..:? "scte27Pids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "scte35Pid")
            Prelude.<*> (x Data..:? "timedMetadataPid")
            Prelude.<*> (x Data..:? "videoPid")
      )

instance
  Prelude.Hashable
    MultiplexProgramPacketIdentifiersMap
  where
  hashWithSalt
    _salt
    MultiplexProgramPacketIdentifiersMap' {..} =
      _salt `Prelude.hashWithSalt` audioPids
        `Prelude.hashWithSalt` dvbSubPids
        `Prelude.hashWithSalt` dvbTeletextPid
        `Prelude.hashWithSalt` etvPlatformPid
        `Prelude.hashWithSalt` etvSignalPid
        `Prelude.hashWithSalt` klvDataPids
        `Prelude.hashWithSalt` pcrPid
        `Prelude.hashWithSalt` pmtPid
        `Prelude.hashWithSalt` privateMetadataPid
        `Prelude.hashWithSalt` scte27Pids
        `Prelude.hashWithSalt` scte35Pid
        `Prelude.hashWithSalt` timedMetadataPid
        `Prelude.hashWithSalt` videoPid

instance
  Prelude.NFData
    MultiplexProgramPacketIdentifiersMap
  where
  rnf MultiplexProgramPacketIdentifiersMap' {..} =
    Prelude.rnf audioPids
      `Prelude.seq` Prelude.rnf dvbSubPids
      `Prelude.seq` Prelude.rnf dvbTeletextPid
      `Prelude.seq` Prelude.rnf etvPlatformPid
      `Prelude.seq` Prelude.rnf etvSignalPid
      `Prelude.seq` Prelude.rnf klvDataPids
      `Prelude.seq` Prelude.rnf pcrPid
      `Prelude.seq` Prelude.rnf pmtPid
      `Prelude.seq` Prelude.rnf privateMetadataPid
      `Prelude.seq` Prelude.rnf scte27Pids
      `Prelude.seq` Prelude.rnf scte35Pid
      `Prelude.seq` Prelude.rnf timedMetadataPid
      `Prelude.seq` Prelude.rnf videoPid
