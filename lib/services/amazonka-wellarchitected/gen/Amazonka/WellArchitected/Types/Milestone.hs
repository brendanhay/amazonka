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
-- Module      : Amazonka.WellArchitected.Types.Milestone
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Milestone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.Workload

-- | A milestone return object.
--
-- /See:/ 'newMilestone' smart constructor.
data Milestone = Milestone'
  { milestoneName :: Prelude.Maybe Prelude.Text,
    milestoneNumber :: Prelude.Maybe Prelude.Natural,
    recordedAt :: Prelude.Maybe Data.POSIX,
    workload :: Prelude.Maybe Workload
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Milestone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'milestoneName', 'milestone_milestoneName' - Undocumented member.
--
-- 'milestoneNumber', 'milestone_milestoneNumber' - Undocumented member.
--
-- 'recordedAt', 'milestone_recordedAt' - Undocumented member.
--
-- 'workload', 'milestone_workload' - Undocumented member.
newMilestone ::
  Milestone
newMilestone =
  Milestone'
    { milestoneName = Prelude.Nothing,
      milestoneNumber = Prelude.Nothing,
      recordedAt = Prelude.Nothing,
      workload = Prelude.Nothing
    }

-- | Undocumented member.
milestone_milestoneName :: Lens.Lens' Milestone (Prelude.Maybe Prelude.Text)
milestone_milestoneName = Lens.lens (\Milestone' {milestoneName} -> milestoneName) (\s@Milestone' {} a -> s {milestoneName = a} :: Milestone)

-- | Undocumented member.
milestone_milestoneNumber :: Lens.Lens' Milestone (Prelude.Maybe Prelude.Natural)
milestone_milestoneNumber = Lens.lens (\Milestone' {milestoneNumber} -> milestoneNumber) (\s@Milestone' {} a -> s {milestoneNumber = a} :: Milestone)

-- | Undocumented member.
milestone_recordedAt :: Lens.Lens' Milestone (Prelude.Maybe Prelude.UTCTime)
milestone_recordedAt = Lens.lens (\Milestone' {recordedAt} -> recordedAt) (\s@Milestone' {} a -> s {recordedAt = a} :: Milestone) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
milestone_workload :: Lens.Lens' Milestone (Prelude.Maybe Workload)
milestone_workload = Lens.lens (\Milestone' {workload} -> workload) (\s@Milestone' {} a -> s {workload = a} :: Milestone)

instance Data.FromJSON Milestone where
  parseJSON =
    Data.withObject
      "Milestone"
      ( \x ->
          Milestone'
            Prelude.<$> (x Data..:? "MilestoneName")
            Prelude.<*> (x Data..:? "MilestoneNumber")
            Prelude.<*> (x Data..:? "RecordedAt")
            Prelude.<*> (x Data..:? "Workload")
      )

instance Prelude.Hashable Milestone where
  hashWithSalt _salt Milestone' {..} =
    _salt
      `Prelude.hashWithSalt` milestoneName
      `Prelude.hashWithSalt` milestoneNumber
      `Prelude.hashWithSalt` recordedAt
      `Prelude.hashWithSalt` workload

instance Prelude.NFData Milestone where
  rnf Milestone' {..} =
    Prelude.rnf milestoneName
      `Prelude.seq` Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf recordedAt
      `Prelude.seq` Prelude.rnf workload
