{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Archive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Archive
  ( Archive (..),

    -- * Smart constructor
    mkArchive,

    -- * Lenses
    aCreationTime,
    aSizeBytes,
    aEventSourceARN,
    aState,
    aEventCount,
    aArchiveName,
    aRetentionDays,
    aStateReason,
  )
where

import Network.AWS.CloudWatchEvents.Types.ArchiveState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An @Archive@ object that contains details about an archive.
--
-- /See:/ 'mkArchive' smart constructor.
data Archive = Archive'
  { creationTime :: Lude.Maybe Lude.Timestamp,
    sizeBytes :: Lude.Maybe Lude.Integer,
    eventSourceARN :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe ArchiveState,
    eventCount :: Lude.Maybe Lude.Integer,
    archiveName :: Lude.Maybe Lude.Text,
    retentionDays :: Lude.Maybe Lude.Natural,
    stateReason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Archive' with the minimum fields required to make a request.
--
-- * 'archiveName' - The name of the archive.
-- * 'creationTime' - The time stamp for the time that the archive was created.
-- * 'eventCount' - The number of events in the archive.
-- * 'eventSourceARN' - The ARN of the event bus associated with the archive. Only events from this event bus are sent to the archive.
-- * 'retentionDays' - The number of days to retain events in the archive before they are deleted.
-- * 'sizeBytes' - The size of the archive, in bytes.
-- * 'state' - The current state of the archive.
-- * 'stateReason' - A description for the reason that the archive is in the current state.
mkArchive ::
  Archive
mkArchive =
  Archive'
    { creationTime = Lude.Nothing,
      sizeBytes = Lude.Nothing,
      eventSourceARN = Lude.Nothing,
      state = Lude.Nothing,
      eventCount = Lude.Nothing,
      archiveName = Lude.Nothing,
      retentionDays = Lude.Nothing,
      stateReason = Lude.Nothing
    }

-- | The time stamp for the time that the archive was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreationTime :: Lens.Lens' Archive (Lude.Maybe Lude.Timestamp)
aCreationTime = Lens.lens (creationTime :: Archive -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Archive)
{-# DEPRECATED aCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The size of the archive, in bytes.
--
-- /Note:/ Consider using 'sizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSizeBytes :: Lens.Lens' Archive (Lude.Maybe Lude.Integer)
aSizeBytes = Lens.lens (sizeBytes :: Archive -> Lude.Maybe Lude.Integer) (\s a -> s {sizeBytes = a} :: Archive)
{-# DEPRECATED aSizeBytes "Use generic-lens or generic-optics with 'sizeBytes' instead." #-}

-- | The ARN of the event bus associated with the archive. Only events from this event bus are sent to the archive.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEventSourceARN :: Lens.Lens' Archive (Lude.Maybe Lude.Text)
aEventSourceARN = Lens.lens (eventSourceARN :: Archive -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: Archive)
{-# DEPRECATED aEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The current state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aState :: Lens.Lens' Archive (Lude.Maybe ArchiveState)
aState = Lens.lens (state :: Archive -> Lude.Maybe ArchiveState) (\s a -> s {state = a} :: Archive)
{-# DEPRECATED aState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The number of events in the archive.
--
-- /Note:/ Consider using 'eventCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEventCount :: Lens.Lens' Archive (Lude.Maybe Lude.Integer)
aEventCount = Lens.lens (eventCount :: Archive -> Lude.Maybe Lude.Integer) (\s a -> s {eventCount = a} :: Archive)
{-# DEPRECATED aEventCount "Use generic-lens or generic-optics with 'eventCount' instead." #-}

-- | The name of the archive.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArchiveName :: Lens.Lens' Archive (Lude.Maybe Lude.Text)
aArchiveName = Lens.lens (archiveName :: Archive -> Lude.Maybe Lude.Text) (\s a -> s {archiveName = a} :: Archive)
{-# DEPRECATED aArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

-- | The number of days to retain events in the archive before they are deleted.
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRetentionDays :: Lens.Lens' Archive (Lude.Maybe Lude.Natural)
aRetentionDays = Lens.lens (retentionDays :: Archive -> Lude.Maybe Lude.Natural) (\s a -> s {retentionDays = a} :: Archive)
{-# DEPRECATED aRetentionDays "Use generic-lens or generic-optics with 'retentionDays' instead." #-}

-- | A description for the reason that the archive is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStateReason :: Lens.Lens' Archive (Lude.Maybe Lude.Text)
aStateReason = Lens.lens (stateReason :: Archive -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: Archive)
{-# DEPRECATED aStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

instance Lude.FromJSON Archive where
  parseJSON =
    Lude.withObject
      "Archive"
      ( \x ->
          Archive'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "SizeBytes")
            Lude.<*> (x Lude..:? "EventSourceArn")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "EventCount")
            Lude.<*> (x Lude..:? "ArchiveName")
            Lude.<*> (x Lude..:? "RetentionDays")
            Lude.<*> (x Lude..:? "StateReason")
      )
