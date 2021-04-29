{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.Types.SnapshotSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotSchedule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
import Network.AWS.Redshift.Types.Tag

-- | Describes a snapshot schedule. You can set a regular interval for
-- creating snapshots of a cluster. You can also schedule snapshots for
-- specific dates.
--
-- /See:/ 'newSnapshotSchedule' smart constructor.
data SnapshotSchedule = SnapshotSchedule'
  { nextInvocations :: Prelude.Maybe [Prelude.ISO8601],
    -- | A list of clusters associated with the schedule. A maximum of 100
    -- clusters is returned.
    associatedClusters :: Prelude.Maybe [ClusterAssociatedToSchedule],
    -- | A unique identifier for the schedule.
    scheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The description of the schedule.
    scheduleDescription :: Prelude.Maybe Prelude.Text,
    -- | A list of ScheduleDefinitions.
    scheduleDefinitions :: Prelude.Maybe [Prelude.Text],
    -- | The number of clusters associated with the schedule.
    associatedClusterCount :: Prelude.Maybe Prelude.Int,
    -- | An optional set of tags describing the schedule.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextInvocations', 'snapshotSchedule_nextInvocations' -
--
-- 'associatedClusters', 'snapshotSchedule_associatedClusters' - A list of clusters associated with the schedule. A maximum of 100
-- clusters is returned.
--
-- 'scheduleIdentifier', 'snapshotSchedule_scheduleIdentifier' - A unique identifier for the schedule.
--
-- 'scheduleDescription', 'snapshotSchedule_scheduleDescription' - The description of the schedule.
--
-- 'scheduleDefinitions', 'snapshotSchedule_scheduleDefinitions' - A list of ScheduleDefinitions.
--
-- 'associatedClusterCount', 'snapshotSchedule_associatedClusterCount' - The number of clusters associated with the schedule.
--
-- 'tags', 'snapshotSchedule_tags' - An optional set of tags describing the schedule.
newSnapshotSchedule ::
  SnapshotSchedule
newSnapshotSchedule =
  SnapshotSchedule'
    { nextInvocations =
        Prelude.Nothing,
      associatedClusters = Prelude.Nothing,
      scheduleIdentifier = Prelude.Nothing,
      scheduleDescription = Prelude.Nothing,
      scheduleDefinitions = Prelude.Nothing,
      associatedClusterCount = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- |
snapshotSchedule_nextInvocations :: Lens.Lens' SnapshotSchedule (Prelude.Maybe [Prelude.UTCTime])
snapshotSchedule_nextInvocations = Lens.lens (\SnapshotSchedule' {nextInvocations} -> nextInvocations) (\s@SnapshotSchedule' {} a -> s {nextInvocations = a} :: SnapshotSchedule) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of clusters associated with the schedule. A maximum of 100
-- clusters is returned.
snapshotSchedule_associatedClusters :: Lens.Lens' SnapshotSchedule (Prelude.Maybe [ClusterAssociatedToSchedule])
snapshotSchedule_associatedClusters = Lens.lens (\SnapshotSchedule' {associatedClusters} -> associatedClusters) (\s@SnapshotSchedule' {} a -> s {associatedClusters = a} :: SnapshotSchedule) Prelude.. Lens.mapping Prelude._Coerce

-- | A unique identifier for the schedule.
snapshotSchedule_scheduleIdentifier :: Lens.Lens' SnapshotSchedule (Prelude.Maybe Prelude.Text)
snapshotSchedule_scheduleIdentifier = Lens.lens (\SnapshotSchedule' {scheduleIdentifier} -> scheduleIdentifier) (\s@SnapshotSchedule' {} a -> s {scheduleIdentifier = a} :: SnapshotSchedule)

-- | The description of the schedule.
snapshotSchedule_scheduleDescription :: Lens.Lens' SnapshotSchedule (Prelude.Maybe Prelude.Text)
snapshotSchedule_scheduleDescription = Lens.lens (\SnapshotSchedule' {scheduleDescription} -> scheduleDescription) (\s@SnapshotSchedule' {} a -> s {scheduleDescription = a} :: SnapshotSchedule)

-- | A list of ScheduleDefinitions.
snapshotSchedule_scheduleDefinitions :: Lens.Lens' SnapshotSchedule (Prelude.Maybe [Prelude.Text])
snapshotSchedule_scheduleDefinitions = Lens.lens (\SnapshotSchedule' {scheduleDefinitions} -> scheduleDefinitions) (\s@SnapshotSchedule' {} a -> s {scheduleDefinitions = a} :: SnapshotSchedule) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of clusters associated with the schedule.
snapshotSchedule_associatedClusterCount :: Lens.Lens' SnapshotSchedule (Prelude.Maybe Prelude.Int)
snapshotSchedule_associatedClusterCount = Lens.lens (\SnapshotSchedule' {associatedClusterCount} -> associatedClusterCount) (\s@SnapshotSchedule' {} a -> s {associatedClusterCount = a} :: SnapshotSchedule)

-- | An optional set of tags describing the schedule.
snapshotSchedule_tags :: Lens.Lens' SnapshotSchedule (Prelude.Maybe [Tag])
snapshotSchedule_tags = Lens.lens (\SnapshotSchedule' {tags} -> tags) (\s@SnapshotSchedule' {} a -> s {tags = a} :: SnapshotSchedule) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML SnapshotSchedule where
  parseXML x =
    SnapshotSchedule'
      Prelude.<$> ( x Prelude..@? "NextInvocations"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "SnapshotTime")
                  )
      Prelude.<*> ( x Prelude..@? "AssociatedClusters"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "ClusterAssociatedToSchedule")
                  )
      Prelude.<*> (x Prelude..@? "ScheduleIdentifier")
      Prelude.<*> (x Prelude..@? "ScheduleDescription")
      Prelude.<*> ( x Prelude..@? "ScheduleDefinitions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "ScheduleDefinition")
                  )
      Prelude.<*> (x Prelude..@? "AssociatedClusterCount")
      Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )

instance Prelude.Hashable SnapshotSchedule

instance Prelude.NFData SnapshotSchedule
