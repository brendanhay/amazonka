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
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Packet identifiers map for a given Multiplex program.
--
-- /See:/ 'newMultiplexProgramPacketIdentifiersMap' smart constructor.
data MultiplexProgramPacketIdentifiersMap = MultiplexProgramPacketIdentifiersMap'
  { klvDataPids :: Core.Maybe [Core.Int],
    etvSignalPid :: Core.Maybe Core.Int,
    pmtPid :: Core.Maybe Core.Int,
    videoPid :: Core.Maybe Core.Int,
    timedMetadataPid :: Core.Maybe Core.Int,
    audioPids :: Core.Maybe [Core.Int],
    etvPlatformPid :: Core.Maybe Core.Int,
    pcrPid :: Core.Maybe Core.Int,
    dvbTeletextPid :: Core.Maybe Core.Int,
    privateMetadataPid :: Core.Maybe Core.Int,
    scte27Pids :: Core.Maybe [Core.Int],
    dvbSubPids :: Core.Maybe [Core.Int],
    scte35Pid :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiplexProgramPacketIdentifiersMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'klvDataPids', 'multiplexProgramPacketIdentifiersMap_klvDataPids' - Undocumented member.
--
-- 'etvSignalPid', 'multiplexProgramPacketIdentifiersMap_etvSignalPid' - Undocumented member.
--
-- 'pmtPid', 'multiplexProgramPacketIdentifiersMap_pmtPid' - Undocumented member.
--
-- 'videoPid', 'multiplexProgramPacketIdentifiersMap_videoPid' - Undocumented member.
--
-- 'timedMetadataPid', 'multiplexProgramPacketIdentifiersMap_timedMetadataPid' - Undocumented member.
--
-- 'audioPids', 'multiplexProgramPacketIdentifiersMap_audioPids' - Undocumented member.
--
-- 'etvPlatformPid', 'multiplexProgramPacketIdentifiersMap_etvPlatformPid' - Undocumented member.
--
-- 'pcrPid', 'multiplexProgramPacketIdentifiersMap_pcrPid' - Undocumented member.
--
-- 'dvbTeletextPid', 'multiplexProgramPacketIdentifiersMap_dvbTeletextPid' - Undocumented member.
--
-- 'privateMetadataPid', 'multiplexProgramPacketIdentifiersMap_privateMetadataPid' - Undocumented member.
--
-- 'scte27Pids', 'multiplexProgramPacketIdentifiersMap_scte27Pids' - Undocumented member.
--
-- 'dvbSubPids', 'multiplexProgramPacketIdentifiersMap_dvbSubPids' - Undocumented member.
--
-- 'scte35Pid', 'multiplexProgramPacketIdentifiersMap_scte35Pid' - Undocumented member.
newMultiplexProgramPacketIdentifiersMap ::
  MultiplexProgramPacketIdentifiersMap
newMultiplexProgramPacketIdentifiersMap =
  MultiplexProgramPacketIdentifiersMap'
    { klvDataPids =
        Core.Nothing,
      etvSignalPid = Core.Nothing,
      pmtPid = Core.Nothing,
      videoPid = Core.Nothing,
      timedMetadataPid = Core.Nothing,
      audioPids = Core.Nothing,
      etvPlatformPid = Core.Nothing,
      pcrPid = Core.Nothing,
      dvbTeletextPid = Core.Nothing,
      privateMetadataPid = Core.Nothing,
      scte27Pids = Core.Nothing,
      dvbSubPids = Core.Nothing,
      scte35Pid = Core.Nothing
    }

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_klvDataPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe [Core.Int])
multiplexProgramPacketIdentifiersMap_klvDataPids = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {klvDataPids} -> klvDataPids) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {klvDataPids = a} :: MultiplexProgramPacketIdentifiersMap) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_etvSignalPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_etvSignalPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {etvSignalPid} -> etvSignalPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {etvSignalPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_pmtPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_pmtPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {pmtPid} -> pmtPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {pmtPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_videoPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_videoPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {videoPid} -> videoPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {videoPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_timedMetadataPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_timedMetadataPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {timedMetadataPid} -> timedMetadataPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {timedMetadataPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_audioPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe [Core.Int])
multiplexProgramPacketIdentifiersMap_audioPids = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {audioPids} -> audioPids) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {audioPids = a} :: MultiplexProgramPacketIdentifiersMap) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_etvPlatformPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_etvPlatformPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {etvPlatformPid} -> etvPlatformPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {etvPlatformPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_pcrPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_pcrPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {pcrPid} -> pcrPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {pcrPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_dvbTeletextPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_dvbTeletextPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {dvbTeletextPid} -> dvbTeletextPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {dvbTeletextPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_privateMetadataPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_privateMetadataPid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {privateMetadataPid} -> privateMetadataPid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {privateMetadataPid = a} :: MultiplexProgramPacketIdentifiersMap)

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_scte27Pids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe [Core.Int])
multiplexProgramPacketIdentifiersMap_scte27Pids = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {scte27Pids} -> scte27Pids) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {scte27Pids = a} :: MultiplexProgramPacketIdentifiersMap) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_dvbSubPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe [Core.Int])
multiplexProgramPacketIdentifiersMap_dvbSubPids = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {dvbSubPids} -> dvbSubPids) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {dvbSubPids = a} :: MultiplexProgramPacketIdentifiersMap) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
multiplexProgramPacketIdentifiersMap_scte35Pid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
multiplexProgramPacketIdentifiersMap_scte35Pid = Lens.lens (\MultiplexProgramPacketIdentifiersMap' {scte35Pid} -> scte35Pid) (\s@MultiplexProgramPacketIdentifiersMap' {} a -> s {scte35Pid = a} :: MultiplexProgramPacketIdentifiersMap)

instance
  Core.FromJSON
    MultiplexProgramPacketIdentifiersMap
  where
  parseJSON =
    Core.withObject
      "MultiplexProgramPacketIdentifiersMap"
      ( \x ->
          MultiplexProgramPacketIdentifiersMap'
            Core.<$> (x Core..:? "klvDataPids" Core..!= Core.mempty)
            Core.<*> (x Core..:? "etvSignalPid")
            Core.<*> (x Core..:? "pmtPid")
            Core.<*> (x Core..:? "videoPid")
            Core.<*> (x Core..:? "timedMetadataPid")
            Core.<*> (x Core..:? "audioPids" Core..!= Core.mempty)
            Core.<*> (x Core..:? "etvPlatformPid")
            Core.<*> (x Core..:? "pcrPid")
            Core.<*> (x Core..:? "dvbTeletextPid")
            Core.<*> (x Core..:? "privateMetadataPid")
            Core.<*> (x Core..:? "scte27Pids" Core..!= Core.mempty)
            Core.<*> (x Core..:? "dvbSubPids" Core..!= Core.mempty)
            Core.<*> (x Core..:? "scte35Pid")
      )

instance
  Core.Hashable
    MultiplexProgramPacketIdentifiersMap

instance
  Core.NFData
    MultiplexProgramPacketIdentifiersMap
