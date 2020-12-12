{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotSchedule
  ( SnapshotSchedule (..),

    -- * Smart constructor
    mkSnapshotSchedule,

    -- * Lenses
    ssAssociatedClusters,
    ssNextInvocations,
    ssScheduleDefinitions,
    ssScheduleDescription,
    ssScheduleIdentifier,
    ssAssociatedClusterCount,
    ssTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
import Network.AWS.Redshift.Types.Tag

-- | Describes a snapshot schedule. You can set a regular interval for creating snapshots of a cluster. You can also schedule snapshots for specific dates.
--
-- /See:/ 'mkSnapshotSchedule' smart constructor.
data SnapshotSchedule = SnapshotSchedule'
  { associatedClusters ::
      Lude.Maybe [ClusterAssociatedToSchedule],
    nextInvocations :: Lude.Maybe [Lude.DateTime],
    scheduleDefinitions :: Lude.Maybe [Lude.Text],
    scheduleDescription :: Lude.Maybe Lude.Text,
    scheduleIdentifier :: Lude.Maybe Lude.Text,
    associatedClusterCount :: Lude.Maybe Lude.Int,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotSchedule' with the minimum fields required to make a request.
--
-- * 'associatedClusterCount' - The number of clusters associated with the schedule.
-- * 'associatedClusters' - A list of clusters associated with the schedule. A maximum of 100 clusters is returned.
-- * 'nextInvocations' -
-- * 'scheduleDefinitions' - A list of ScheduleDefinitions.
-- * 'scheduleDescription' - The description of the schedule.
-- * 'scheduleIdentifier' - A unique identifier for the schedule.
-- * 'tags' - An optional set of tags describing the schedule.
mkSnapshotSchedule ::
  SnapshotSchedule
mkSnapshotSchedule =
  SnapshotSchedule'
    { associatedClusters = Lude.Nothing,
      nextInvocations = Lude.Nothing,
      scheduleDefinitions = Lude.Nothing,
      scheduleDescription = Lude.Nothing,
      scheduleIdentifier = Lude.Nothing,
      associatedClusterCount = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A list of clusters associated with the schedule. A maximum of 100 clusters is returned.
--
-- /Note:/ Consider using 'associatedClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAssociatedClusters :: Lens.Lens' SnapshotSchedule (Lude.Maybe [ClusterAssociatedToSchedule])
ssAssociatedClusters = Lens.lens (associatedClusters :: SnapshotSchedule -> Lude.Maybe [ClusterAssociatedToSchedule]) (\s a -> s {associatedClusters = a} :: SnapshotSchedule)
{-# DEPRECATED ssAssociatedClusters "Use generic-lens or generic-optics with 'associatedClusters' instead." #-}

-- |
--
-- /Note:/ Consider using 'nextInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNextInvocations :: Lens.Lens' SnapshotSchedule (Lude.Maybe [Lude.DateTime])
ssNextInvocations = Lens.lens (nextInvocations :: SnapshotSchedule -> Lude.Maybe [Lude.DateTime]) (\s a -> s {nextInvocations = a} :: SnapshotSchedule)
{-# DEPRECATED ssNextInvocations "Use generic-lens or generic-optics with 'nextInvocations' instead." #-}

-- | A list of ScheduleDefinitions.
--
-- /Note:/ Consider using 'scheduleDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssScheduleDefinitions :: Lens.Lens' SnapshotSchedule (Lude.Maybe [Lude.Text])
ssScheduleDefinitions = Lens.lens (scheduleDefinitions :: SnapshotSchedule -> Lude.Maybe [Lude.Text]) (\s a -> s {scheduleDefinitions = a} :: SnapshotSchedule)
{-# DEPRECATED ssScheduleDefinitions "Use generic-lens or generic-optics with 'scheduleDefinitions' instead." #-}

-- | The description of the schedule.
--
-- /Note:/ Consider using 'scheduleDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssScheduleDescription :: Lens.Lens' SnapshotSchedule (Lude.Maybe Lude.Text)
ssScheduleDescription = Lens.lens (scheduleDescription :: SnapshotSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleDescription = a} :: SnapshotSchedule)
{-# DEPRECATED ssScheduleDescription "Use generic-lens or generic-optics with 'scheduleDescription' instead." #-}

-- | A unique identifier for the schedule.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssScheduleIdentifier :: Lens.Lens' SnapshotSchedule (Lude.Maybe Lude.Text)
ssScheduleIdentifier = Lens.lens (scheduleIdentifier :: SnapshotSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleIdentifier = a} :: SnapshotSchedule)
{-# DEPRECATED ssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

-- | The number of clusters associated with the schedule.
--
-- /Note:/ Consider using 'associatedClusterCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAssociatedClusterCount :: Lens.Lens' SnapshotSchedule (Lude.Maybe Lude.Int)
ssAssociatedClusterCount = Lens.lens (associatedClusterCount :: SnapshotSchedule -> Lude.Maybe Lude.Int) (\s a -> s {associatedClusterCount = a} :: SnapshotSchedule)
{-# DEPRECATED ssAssociatedClusterCount "Use generic-lens or generic-optics with 'associatedClusterCount' instead." #-}

-- | An optional set of tags describing the schedule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTags :: Lens.Lens' SnapshotSchedule (Lude.Maybe [Tag])
ssTags = Lens.lens (tags :: SnapshotSchedule -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SnapshotSchedule)
{-# DEPRECATED ssTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML SnapshotSchedule where
  parseXML x =
    SnapshotSchedule'
      Lude.<$> ( x Lude..@? "AssociatedClusters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ClusterAssociatedToSchedule")
               )
      Lude.<*> ( x Lude..@? "NextInvocations" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "SnapshotTime")
               )
      Lude.<*> ( x Lude..@? "ScheduleDefinitions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ScheduleDefinition")
               )
      Lude.<*> (x Lude..@? "ScheduleDescription")
      Lude.<*> (x Lude..@? "ScheduleIdentifier")
      Lude.<*> (x Lude..@? "AssociatedClusterCount")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
