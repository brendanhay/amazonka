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
    aArchiveName,
    aCreationTime,
    aEventCount,
    aEventSourceArn,
    aRetentionDays,
    aSizeBytes,
    aState,
    aStateReason,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.ArchiveName as Types
import qualified Network.AWS.CloudWatchEvents.Types.ArchiveState as Types
import qualified Network.AWS.CloudWatchEvents.Types.ArchiveStateReason as Types
import qualified Network.AWS.CloudWatchEvents.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An @Archive@ object that contains details about an archive.
--
-- /See:/ 'mkArchive' smart constructor.
data Archive = Archive'
  { -- | The name of the archive.
    archiveName :: Core.Maybe Types.ArchiveName,
    -- | The time stamp for the time that the archive was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The number of events in the archive.
    eventCount :: Core.Maybe Core.Integer,
    -- | The ARN of the event bus associated with the archive. Only events from this event bus are sent to the archive.
    eventSourceArn :: Core.Maybe Types.Arn,
    -- | The number of days to retain events in the archive before they are deleted.
    retentionDays :: Core.Maybe Core.Natural,
    -- | The size of the archive, in bytes.
    sizeBytes :: Core.Maybe Core.Integer,
    -- | The current state of the archive.
    state :: Core.Maybe Types.ArchiveState,
    -- | A description for the reason that the archive is in the current state.
    stateReason :: Core.Maybe Types.ArchiveStateReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Archive' value with any optional fields omitted.
mkArchive ::
  Archive
mkArchive =
  Archive'
    { archiveName = Core.Nothing,
      creationTime = Core.Nothing,
      eventCount = Core.Nothing,
      eventSourceArn = Core.Nothing,
      retentionDays = Core.Nothing,
      sizeBytes = Core.Nothing,
      state = Core.Nothing,
      stateReason = Core.Nothing
    }

-- | The name of the archive.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArchiveName :: Lens.Lens' Archive (Core.Maybe Types.ArchiveName)
aArchiveName = Lens.field @"archiveName"
{-# DEPRECATED aArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

-- | The time stamp for the time that the archive was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreationTime :: Lens.Lens' Archive (Core.Maybe Core.NominalDiffTime)
aCreationTime = Lens.field @"creationTime"
{-# DEPRECATED aCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The number of events in the archive.
--
-- /Note:/ Consider using 'eventCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEventCount :: Lens.Lens' Archive (Core.Maybe Core.Integer)
aEventCount = Lens.field @"eventCount"
{-# DEPRECATED aEventCount "Use generic-lens or generic-optics with 'eventCount' instead." #-}

-- | The ARN of the event bus associated with the archive. Only events from this event bus are sent to the archive.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEventSourceArn :: Lens.Lens' Archive (Core.Maybe Types.Arn)
aEventSourceArn = Lens.field @"eventSourceArn"
{-# DEPRECATED aEventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead." #-}

-- | The number of days to retain events in the archive before they are deleted.
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRetentionDays :: Lens.Lens' Archive (Core.Maybe Core.Natural)
aRetentionDays = Lens.field @"retentionDays"
{-# DEPRECATED aRetentionDays "Use generic-lens or generic-optics with 'retentionDays' instead." #-}

-- | The size of the archive, in bytes.
--
-- /Note:/ Consider using 'sizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSizeBytes :: Lens.Lens' Archive (Core.Maybe Core.Integer)
aSizeBytes = Lens.field @"sizeBytes"
{-# DEPRECATED aSizeBytes "Use generic-lens or generic-optics with 'sizeBytes' instead." #-}

-- | The current state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aState :: Lens.Lens' Archive (Core.Maybe Types.ArchiveState)
aState = Lens.field @"state"
{-# DEPRECATED aState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A description for the reason that the archive is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStateReason :: Lens.Lens' Archive (Core.Maybe Types.ArchiveStateReason)
aStateReason = Lens.field @"stateReason"
{-# DEPRECATED aStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

instance Core.FromJSON Archive where
  parseJSON =
    Core.withObject "Archive" Core.$
      \x ->
        Archive'
          Core.<$> (x Core..:? "ArchiveName")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "EventCount")
          Core.<*> (x Core..:? "EventSourceArn")
          Core.<*> (x Core..:? "RetentionDays")
          Core.<*> (x Core..:? "SizeBytes")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "StateReason")
