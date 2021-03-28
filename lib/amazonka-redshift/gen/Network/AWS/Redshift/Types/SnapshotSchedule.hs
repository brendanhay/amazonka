{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.SnapshotSchedule
  ( SnapshotSchedule (..)
  -- * Smart constructor
  , mkSnapshotSchedule
  -- * Lenses
  , ssAssociatedClusterCount
  , ssAssociatedClusters
  , ssNextInvocations
  , ssScheduleDefinitions
  , ssScheduleDescription
  , ssScheduleIdentifier
  , ssTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ClusterAssociatedToSchedule as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes a snapshot schedule. You can set a regular interval for creating snapshots of a cluster. You can also schedule snapshots for specific dates. 
--
-- /See:/ 'mkSnapshotSchedule' smart constructor.
data SnapshotSchedule = SnapshotSchedule'
  { associatedClusterCount :: Core.Maybe Core.Int
    -- ^ The number of clusters associated with the schedule.
  , associatedClusters :: Core.Maybe [Types.ClusterAssociatedToSchedule]
    -- ^ A list of clusters associated with the schedule. A maximum of 100 clusters is returned.
  , nextInvocations :: Core.Maybe [Core.UTCTime]
    -- ^ 
  , scheduleDefinitions :: Core.Maybe [Core.Text]
    -- ^ A list of ScheduleDefinitions.
  , scheduleDescription :: Core.Maybe Core.Text
    -- ^ The description of the schedule.
  , scheduleIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for the schedule.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An optional set of tags describing the schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SnapshotSchedule' value with any optional fields omitted.
mkSnapshotSchedule
    :: SnapshotSchedule
mkSnapshotSchedule
  = SnapshotSchedule'{associatedClusterCount = Core.Nothing,
                      associatedClusters = Core.Nothing, nextInvocations = Core.Nothing,
                      scheduleDefinitions = Core.Nothing,
                      scheduleDescription = Core.Nothing,
                      scheduleIdentifier = Core.Nothing, tags = Core.Nothing}

-- | The number of clusters associated with the schedule.
--
-- /Note:/ Consider using 'associatedClusterCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAssociatedClusterCount :: Lens.Lens' SnapshotSchedule (Core.Maybe Core.Int)
ssAssociatedClusterCount = Lens.field @"associatedClusterCount"
{-# INLINEABLE ssAssociatedClusterCount #-}
{-# DEPRECATED associatedClusterCount "Use generic-lens or generic-optics with 'associatedClusterCount' instead"  #-}

-- | A list of clusters associated with the schedule. A maximum of 100 clusters is returned.
--
-- /Note:/ Consider using 'associatedClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAssociatedClusters :: Lens.Lens' SnapshotSchedule (Core.Maybe [Types.ClusterAssociatedToSchedule])
ssAssociatedClusters = Lens.field @"associatedClusters"
{-# INLINEABLE ssAssociatedClusters #-}
{-# DEPRECATED associatedClusters "Use generic-lens or generic-optics with 'associatedClusters' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'nextInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNextInvocations :: Lens.Lens' SnapshotSchedule (Core.Maybe [Core.UTCTime])
ssNextInvocations = Lens.field @"nextInvocations"
{-# INLINEABLE ssNextInvocations #-}
{-# DEPRECATED nextInvocations "Use generic-lens or generic-optics with 'nextInvocations' instead"  #-}

-- | A list of ScheduleDefinitions.
--
-- /Note:/ Consider using 'scheduleDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssScheduleDefinitions :: Lens.Lens' SnapshotSchedule (Core.Maybe [Core.Text])
ssScheduleDefinitions = Lens.field @"scheduleDefinitions"
{-# INLINEABLE ssScheduleDefinitions #-}
{-# DEPRECATED scheduleDefinitions "Use generic-lens or generic-optics with 'scheduleDefinitions' instead"  #-}

-- | The description of the schedule.
--
-- /Note:/ Consider using 'scheduleDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssScheduleDescription :: Lens.Lens' SnapshotSchedule (Core.Maybe Core.Text)
ssScheduleDescription = Lens.field @"scheduleDescription"
{-# INLINEABLE ssScheduleDescription #-}
{-# DEPRECATED scheduleDescription "Use generic-lens or generic-optics with 'scheduleDescription' instead"  #-}

-- | A unique identifier for the schedule.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssScheduleIdentifier :: Lens.Lens' SnapshotSchedule (Core.Maybe Core.Text)
ssScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# INLINEABLE ssScheduleIdentifier #-}
{-# DEPRECATED scheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead"  #-}

-- | An optional set of tags describing the schedule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTags :: Lens.Lens' SnapshotSchedule (Core.Maybe [Types.Tag])
ssTags = Lens.field @"tags"
{-# INLINEABLE ssTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML SnapshotSchedule where
        parseXML x
          = SnapshotSchedule' Core.<$>
              (x Core..@? "AssociatedClusterCount") Core.<*>
                x Core..@? "AssociatedClusters" Core..<@>
                  Core.parseXMLList "ClusterAssociatedToSchedule"
                Core.<*>
                x Core..@? "NextInvocations" Core..<@>
                  Core.parseXMLList "SnapshotTime"
                Core.<*>
                x Core..@? "ScheduleDefinitions" Core..<@>
                  Core.parseXMLList "ScheduleDefinition"
                Core.<*> x Core..@? "ScheduleDescription"
                Core.<*> x Core..@? "ScheduleIdentifier"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag"
