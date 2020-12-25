{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
  ( MultiplexProgramPacketIdentifiersMap (..),

    -- * Smart constructor
    mkMultiplexProgramPacketIdentifiersMap,

    -- * Lenses
    mppimAudioPids,
    mppimDvbSubPids,
    mppimDvbTeletextPid,
    mppimEtvPlatformPid,
    mppimEtvSignalPid,
    mppimKlvDataPids,
    mppimPcrPid,
    mppimPmtPid,
    mppimPrivateMetadataPid,
    mppimScte27Pids,
    mppimScte35Pid,
    mppimTimedMetadataPid,
    mppimVideoPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Packet identifiers map for a given Multiplex program.
--
-- /See:/ 'mkMultiplexProgramPacketIdentifiersMap' smart constructor.
data MultiplexProgramPacketIdentifiersMap = MultiplexProgramPacketIdentifiersMap'
  { audioPids :: Core.Maybe [Core.Int],
    dvbSubPids :: Core.Maybe [Core.Int],
    dvbTeletextPid :: Core.Maybe Core.Int,
    etvPlatformPid :: Core.Maybe Core.Int,
    etvSignalPid :: Core.Maybe Core.Int,
    klvDataPids :: Core.Maybe [Core.Int],
    pcrPid :: Core.Maybe Core.Int,
    pmtPid :: Core.Maybe Core.Int,
    privateMetadataPid :: Core.Maybe Core.Int,
    scte27Pids :: Core.Maybe [Core.Int],
    scte35Pid :: Core.Maybe Core.Int,
    timedMetadataPid :: Core.Maybe Core.Int,
    videoPid :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexProgramPacketIdentifiersMap' value with any optional fields omitted.
mkMultiplexProgramPacketIdentifiersMap ::
  MultiplexProgramPacketIdentifiersMap
mkMultiplexProgramPacketIdentifiersMap =
  MultiplexProgramPacketIdentifiersMap'
    { audioPids = Core.Nothing,
      dvbSubPids = Core.Nothing,
      dvbTeletextPid = Core.Nothing,
      etvPlatformPid = Core.Nothing,
      etvSignalPid = Core.Nothing,
      klvDataPids = Core.Nothing,
      pcrPid = Core.Nothing,
      pmtPid = Core.Nothing,
      privateMetadataPid = Core.Nothing,
      scte27Pids = Core.Nothing,
      scte35Pid = Core.Nothing,
      timedMetadataPid = Core.Nothing,
      videoPid = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimAudioPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe [Core.Int])
mppimAudioPids = Lens.field @"audioPids"
{-# DEPRECATED mppimAudioPids "Use generic-lens or generic-optics with 'audioPids' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dvbSubPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimDvbSubPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe [Core.Int])
mppimDvbSubPids = Lens.field @"dvbSubPids"
{-# DEPRECATED mppimDvbSubPids "Use generic-lens or generic-optics with 'dvbSubPids' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dvbTeletextPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimDvbTeletextPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimDvbTeletextPid = Lens.field @"dvbTeletextPid"
{-# DEPRECATED mppimDvbTeletextPid "Use generic-lens or generic-optics with 'dvbTeletextPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'etvPlatformPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimEtvPlatformPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimEtvPlatformPid = Lens.field @"etvPlatformPid"
{-# DEPRECATED mppimEtvPlatformPid "Use generic-lens or generic-optics with 'etvPlatformPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'etvSignalPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimEtvSignalPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimEtvSignalPid = Lens.field @"etvSignalPid"
{-# DEPRECATED mppimEtvSignalPid "Use generic-lens or generic-optics with 'etvSignalPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'klvDataPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimKlvDataPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe [Core.Int])
mppimKlvDataPids = Lens.field @"klvDataPids"
{-# DEPRECATED mppimKlvDataPids "Use generic-lens or generic-optics with 'klvDataPids' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimPcrPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimPcrPid = Lens.field @"pcrPid"
{-# DEPRECATED mppimPcrPid "Use generic-lens or generic-optics with 'pcrPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimPmtPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimPmtPid = Lens.field @"pmtPid"
{-# DEPRECATED mppimPmtPid "Use generic-lens or generic-optics with 'pmtPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'privateMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimPrivateMetadataPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimPrivateMetadataPid = Lens.field @"privateMetadataPid"
{-# DEPRECATED mppimPrivateMetadataPid "Use generic-lens or generic-optics with 'privateMetadataPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte27Pids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimScte27Pids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe [Core.Int])
mppimScte27Pids = Lens.field @"scte27Pids"
{-# DEPRECATED mppimScte27Pids "Use generic-lens or generic-optics with 'scte27Pids' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimScte35Pid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimScte35Pid = Lens.field @"scte35Pid"
{-# DEPRECATED mppimScte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimTimedMetadataPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimTimedMetadataPid = Lens.field @"timedMetadataPid"
{-# DEPRECATED mppimTimedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimVideoPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Core.Maybe Core.Int)
mppimVideoPid = Lens.field @"videoPid"
{-# DEPRECATED mppimVideoPid "Use generic-lens or generic-optics with 'videoPid' instead." #-}

instance Core.FromJSON MultiplexProgramPacketIdentifiersMap where
  parseJSON =
    Core.withObject "MultiplexProgramPacketIdentifiersMap" Core.$
      \x ->
        MultiplexProgramPacketIdentifiersMap'
          Core.<$> (x Core..:? "audioPids")
          Core.<*> (x Core..:? "dvbSubPids")
          Core.<*> (x Core..:? "dvbTeletextPid")
          Core.<*> (x Core..:? "etvPlatformPid")
          Core.<*> (x Core..:? "etvSignalPid")
          Core.<*> (x Core..:? "klvDataPids")
          Core.<*> (x Core..:? "pcrPid")
          Core.<*> (x Core..:? "pmtPid")
          Core.<*> (x Core..:? "privateMetadataPid")
          Core.<*> (x Core..:? "scte27Pids")
          Core.<*> (x Core..:? "scte35Pid")
          Core.<*> (x Core..:? "timedMetadataPid")
          Core.<*> (x Core..:? "videoPid")
