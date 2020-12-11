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
    mppimPmtPid,
    mppimEtvSignalPid,
    mppimVideoPid,
    mppimScte35Pid,
    mppimPrivateMetadataPid,
    mppimTimedMetadataPid,
    mppimPcrPid,
    mppimKlvDataPids,
    mppimDvbSubPids,
    mppimScte27Pids,
    mppimEtvPlatformPid,
    mppimAudioPids,
    mppimDvbTeletextPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Packet identifiers map for a given Multiplex program.
--
-- /See:/ 'mkMultiplexProgramPacketIdentifiersMap' smart constructor.
data MultiplexProgramPacketIdentifiersMap = MultiplexProgramPacketIdentifiersMap'
  { pmtPid ::
      Lude.Maybe
        Lude.Int,
    etvSignalPid ::
      Lude.Maybe
        Lude.Int,
    videoPid ::
      Lude.Maybe
        Lude.Int,
    scte35Pid ::
      Lude.Maybe
        Lude.Int,
    privateMetadataPid ::
      Lude.Maybe
        Lude.Int,
    timedMetadataPid ::
      Lude.Maybe
        Lude.Int,
    pcrPid ::
      Lude.Maybe
        Lude.Int,
    klvDataPids ::
      Lude.Maybe
        [Lude.Int],
    dvbSubPids ::
      Lude.Maybe
        [Lude.Int],
    scte27Pids ::
      Lude.Maybe
        [Lude.Int],
    etvPlatformPid ::
      Lude.Maybe
        Lude.Int,
    audioPids ::
      Lude.Maybe
        [Lude.Int],
    dvbTeletextPid ::
      Lude.Maybe
        Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexProgramPacketIdentifiersMap' with the minimum fields required to make a request.
--
-- * 'audioPids' - Undocumented field.
-- * 'dvbSubPids' - Undocumented field.
-- * 'dvbTeletextPid' - Undocumented field.
-- * 'etvPlatformPid' - Undocumented field.
-- * 'etvSignalPid' - Undocumented field.
-- * 'klvDataPids' - Undocumented field.
-- * 'pcrPid' - Undocumented field.
-- * 'pmtPid' - Undocumented field.
-- * 'privateMetadataPid' - Undocumented field.
-- * 'scte27Pids' - Undocumented field.
-- * 'scte35Pid' - Undocumented field.
-- * 'timedMetadataPid' - Undocumented field.
-- * 'videoPid' - Undocumented field.
mkMultiplexProgramPacketIdentifiersMap ::
  MultiplexProgramPacketIdentifiersMap
mkMultiplexProgramPacketIdentifiersMap =
  MultiplexProgramPacketIdentifiersMap'
    { pmtPid = Lude.Nothing,
      etvSignalPid = Lude.Nothing,
      videoPid = Lude.Nothing,
      scte35Pid = Lude.Nothing,
      privateMetadataPid = Lude.Nothing,
      timedMetadataPid = Lude.Nothing,
      pcrPid = Lude.Nothing,
      klvDataPids = Lude.Nothing,
      dvbSubPids = Lude.Nothing,
      scte27Pids = Lude.Nothing,
      etvPlatformPid = Lude.Nothing,
      audioPids = Lude.Nothing,
      dvbTeletextPid = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimPmtPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimPmtPid = Lens.lens (pmtPid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {pmtPid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimPmtPid "Use generic-lens or generic-optics with 'pmtPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'etvSignalPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimEtvSignalPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimEtvSignalPid = Lens.lens (etvSignalPid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {etvSignalPid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimEtvSignalPid "Use generic-lens or generic-optics with 'etvSignalPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimVideoPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimVideoPid = Lens.lens (videoPid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {videoPid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimVideoPid "Use generic-lens or generic-optics with 'videoPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimScte35Pid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimScte35Pid = Lens.lens (scte35Pid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {scte35Pid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimScte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'privateMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimPrivateMetadataPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimPrivateMetadataPid = Lens.lens (privateMetadataPid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {privateMetadataPid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimPrivateMetadataPid "Use generic-lens or generic-optics with 'privateMetadataPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimTimedMetadataPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimTimedMetadataPid = Lens.lens (timedMetadataPid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {timedMetadataPid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimTimedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimPcrPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimPcrPid = Lens.lens (pcrPid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {pcrPid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimPcrPid "Use generic-lens or generic-optics with 'pcrPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'klvDataPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimKlvDataPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe [Lude.Int])
mppimKlvDataPids = Lens.lens (klvDataPids :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe [Lude.Int]) (\s a -> s {klvDataPids = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimKlvDataPids "Use generic-lens or generic-optics with 'klvDataPids' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dvbSubPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimDvbSubPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe [Lude.Int])
mppimDvbSubPids = Lens.lens (dvbSubPids :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe [Lude.Int]) (\s a -> s {dvbSubPids = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimDvbSubPids "Use generic-lens or generic-optics with 'dvbSubPids' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte27Pids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimScte27Pids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe [Lude.Int])
mppimScte27Pids = Lens.lens (scte27Pids :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe [Lude.Int]) (\s a -> s {scte27Pids = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimScte27Pids "Use generic-lens or generic-optics with 'scte27Pids' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'etvPlatformPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimEtvPlatformPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimEtvPlatformPid = Lens.lens (etvPlatformPid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {etvPlatformPid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimEtvPlatformPid "Use generic-lens or generic-optics with 'etvPlatformPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimAudioPids :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe [Lude.Int])
mppimAudioPids = Lens.lens (audioPids :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe [Lude.Int]) (\s a -> s {audioPids = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimAudioPids "Use generic-lens or generic-optics with 'audioPids' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dvbTeletextPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppimDvbTeletextPid :: Lens.Lens' MultiplexProgramPacketIdentifiersMap (Lude.Maybe Lude.Int)
mppimDvbTeletextPid = Lens.lens (dvbTeletextPid :: MultiplexProgramPacketIdentifiersMap -> Lude.Maybe Lude.Int) (\s a -> s {dvbTeletextPid = a} :: MultiplexProgramPacketIdentifiersMap)
{-# DEPRECATED mppimDvbTeletextPid "Use generic-lens or generic-optics with 'dvbTeletextPid' instead." #-}

instance Lude.FromJSON MultiplexProgramPacketIdentifiersMap where
  parseJSON =
    Lude.withObject
      "MultiplexProgramPacketIdentifiersMap"
      ( \x ->
          MultiplexProgramPacketIdentifiersMap'
            Lude.<$> (x Lude..:? "pmtPid")
            Lude.<*> (x Lude..:? "etvSignalPid")
            Lude.<*> (x Lude..:? "videoPid")
            Lude.<*> (x Lude..:? "scte35Pid")
            Lude.<*> (x Lude..:? "privateMetadataPid")
            Lude.<*> (x Lude..:? "timedMetadataPid")
            Lude.<*> (x Lude..:? "pcrPid")
            Lude.<*> (x Lude..:? "klvDataPids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "dvbSubPids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "scte27Pids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "etvPlatformPid")
            Lude.<*> (x Lude..:? "audioPids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "dvbTeletextPid")
      )
