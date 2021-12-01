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
-- Module      : Amazonka.Redshift.Types.SnapshotSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.SnapshotSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.ClusterAssociatedToSchedule
import Amazonka.Redshift.Types.Tag

-- | Describes a snapshot schedule. You can set a regular interval for
-- creating snapshots of a cluster. You can also schedule snapshots for
-- specific dates.
--
-- /See:/ 'newSnapshotSchedule' smart constructor.
data SnapshotSchedule = SnapshotSchedule'
  { -- | A list of clusters associated with the schedule. A maximum of 100
    -- clusters is returned.
    associatedClusters :: Prelude.Maybe [ClusterAssociatedToSchedule],
    nextInvocations :: Prelude.Maybe [Core.ISO8601],
    -- | A list of ScheduleDefinitions.
    scheduleDefinitions :: Prelude.Maybe [Prelude.Text],
    -- | The description of the schedule.
    scheduleDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the schedule.
    scheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The number of clusters associated with the schedule.
    associatedClusterCount :: Prelude.Maybe Prelude.Int,
    -- | An optional set of tags describing the schedule.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedClusters', 'snapshotSchedule_associatedClusters' - A list of clusters associated with the schedule. A maximum of 100
-- clusters is returned.
--
-- 'nextInvocations', 'snapshotSchedule_nextInvocations' -
--
-- 'scheduleDefinitions', 'snapshotSchedule_scheduleDefinitions' - A list of ScheduleDefinitions.
--
-- 'scheduleDescription', 'snapshotSchedule_scheduleDescription' - The description of the schedule.
--
-- 'scheduleIdentifier', 'snapshotSchedule_scheduleIdentifier' - A unique identifier for the schedule.
--
-- 'associatedClusterCount', 'snapshotSchedule_associatedClusterCount' - The number of clusters associated with the schedule.
--
-- 'tags', 'snapshotSchedule_tags' - An optional set of tags describing the schedule.
newSnapshotSchedule ::
  SnapshotSchedule
newSnapshotSchedule =
  SnapshotSchedule'
    { associatedClusters =
        Prelude.Nothing,
      nextInvocations = Prelude.Nothing,
      scheduleDefinitions = Prelude.Nothing,
      scheduleDescription = Prelude.Nothing,
      scheduleIdentifier = Prelude.Nothing,
      associatedClusterCount = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A list of clusters associated with the schedule. A maximum of 100
-- clusters is returned.
snapshotSchedule_associatedClusters :: Lens.Lens' SnapshotSchedule (Prelude.Maybe [ClusterAssociatedToSchedule])
snapshotSchedule_associatedClusters = Lens.lens (\SnapshotSchedule' {associatedClusters} -> associatedClusters) (\s@SnapshotSchedule' {} a -> s {associatedClusters = a} :: SnapshotSchedule) Prelude.. Lens.mapping Lens.coerced

-- |
snapshotSchedule_nextInvocations :: Lens.Lens' SnapshotSchedule (Prelude.Maybe [Prelude.UTCTime])
snapshotSchedule_nextInvocations = Lens.lens (\SnapshotSchedule' {nextInvocations} -> nextInvocations) (\s@SnapshotSchedule' {} a -> s {nextInvocations = a} :: SnapshotSchedule) Prelude.. Lens.mapping Lens.coerced

-- | A list of ScheduleDefinitions.
snapshotSchedule_scheduleDefinitions :: Lens.Lens' SnapshotSchedule (Prelude.Maybe [Prelude.Text])
snapshotSchedule_scheduleDefinitions = Lens.lens (\SnapshotSchedule' {scheduleDefinitions} -> scheduleDefinitions) (\s@SnapshotSchedule' {} a -> s {scheduleDefinitions = a} :: SnapshotSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The description of the schedule.
snapshotSchedule_scheduleDescription :: Lens.Lens' SnapshotSchedule (Prelude.Maybe Prelude.Text)
snapshotSchedule_scheduleDescription = Lens.lens (\SnapshotSchedule' {scheduleDescription} -> scheduleDescription) (\s@SnapshotSchedule' {} a -> s {scheduleDescription = a} :: SnapshotSchedule)

-- | A unique identifier for the schedule.
snapshotSchedule_scheduleIdentifier :: Lens.Lens' SnapshotSchedule (Prelude.Maybe Prelude.Text)
snapshotSchedule_scheduleIdentifier = Lens.lens (\SnapshotSchedule' {scheduleIdentifier} -> scheduleIdentifier) (\s@SnapshotSchedule' {} a -> s {scheduleIdentifier = a} :: SnapshotSchedule)

-- | The number of clusters associated with the schedule.
snapshotSchedule_associatedClusterCount :: Lens.Lens' SnapshotSchedule (Prelude.Maybe Prelude.Int)
snapshotSchedule_associatedClusterCount = Lens.lens (\SnapshotSchedule' {associatedClusterCount} -> associatedClusterCount) (\s@SnapshotSchedule' {} a -> s {associatedClusterCount = a} :: SnapshotSchedule)

-- | An optional set of tags describing the schedule.
snapshotSchedule_tags :: Lens.Lens' SnapshotSchedule (Prelude.Maybe [Tag])
snapshotSchedule_tags = Lens.lens (\SnapshotSchedule' {tags} -> tags) (\s@SnapshotSchedule' {} a -> s {tags = a} :: SnapshotSchedule) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML SnapshotSchedule where
  parseXML x =
    SnapshotSchedule'
      Prelude.<$> ( x Core..@? "AssociatedClusters"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "ClusterAssociatedToSchedule")
                  )
      Prelude.<*> ( x Core..@? "NextInvocations" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "SnapshotTime")
                  )
      Prelude.<*> ( x Core..@? "ScheduleDefinitions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ScheduleDefinition")
                  )
      Prelude.<*> (x Core..@? "ScheduleDescription")
      Prelude.<*> (x Core..@? "ScheduleIdentifier")
      Prelude.<*> (x Core..@? "AssociatedClusterCount")
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )

instance Prelude.Hashable SnapshotSchedule where
  hashWithSalt salt' SnapshotSchedule' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` associatedClusterCount
      `Prelude.hashWithSalt` scheduleIdentifier
      `Prelude.hashWithSalt` scheduleDescription
      `Prelude.hashWithSalt` scheduleDefinitions
      `Prelude.hashWithSalt` nextInvocations
      `Prelude.hashWithSalt` associatedClusters

instance Prelude.NFData SnapshotSchedule where
  rnf SnapshotSchedule' {..} =
    Prelude.rnf associatedClusters
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf associatedClusterCount
      `Prelude.seq` Prelude.rnf scheduleIdentifier
      `Prelude.seq` Prelude.rnf scheduleDescription
      `Prelude.seq` Prelude.rnf scheduleDefinitions
      `Prelude.seq` Prelude.rnf nextInvocations
