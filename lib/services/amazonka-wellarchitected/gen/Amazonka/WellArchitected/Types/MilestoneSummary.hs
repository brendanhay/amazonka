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
-- Module      : Amazonka.WellArchitected.Types.MilestoneSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.MilestoneSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.WorkloadSummary

-- | A milestone summary return object.
--
-- /See:/ 'newMilestoneSummary' smart constructor.
data MilestoneSummary = MilestoneSummary'
  { recordedAt :: Prelude.Maybe Data.POSIX,
    milestoneName :: Prelude.Maybe Prelude.Text,
    workloadSummary :: Prelude.Maybe WorkloadSummary,
    milestoneNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MilestoneSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordedAt', 'milestoneSummary_recordedAt' - Undocumented member.
--
-- 'milestoneName', 'milestoneSummary_milestoneName' - Undocumented member.
--
-- 'workloadSummary', 'milestoneSummary_workloadSummary' - Undocumented member.
--
-- 'milestoneNumber', 'milestoneSummary_milestoneNumber' - Undocumented member.
newMilestoneSummary ::
  MilestoneSummary
newMilestoneSummary =
  MilestoneSummary'
    { recordedAt = Prelude.Nothing,
      milestoneName = Prelude.Nothing,
      workloadSummary = Prelude.Nothing,
      milestoneNumber = Prelude.Nothing
    }

-- | Undocumented member.
milestoneSummary_recordedAt :: Lens.Lens' MilestoneSummary (Prelude.Maybe Prelude.UTCTime)
milestoneSummary_recordedAt = Lens.lens (\MilestoneSummary' {recordedAt} -> recordedAt) (\s@MilestoneSummary' {} a -> s {recordedAt = a} :: MilestoneSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
milestoneSummary_milestoneName :: Lens.Lens' MilestoneSummary (Prelude.Maybe Prelude.Text)
milestoneSummary_milestoneName = Lens.lens (\MilestoneSummary' {milestoneName} -> milestoneName) (\s@MilestoneSummary' {} a -> s {milestoneName = a} :: MilestoneSummary)

-- | Undocumented member.
milestoneSummary_workloadSummary :: Lens.Lens' MilestoneSummary (Prelude.Maybe WorkloadSummary)
milestoneSummary_workloadSummary = Lens.lens (\MilestoneSummary' {workloadSummary} -> workloadSummary) (\s@MilestoneSummary' {} a -> s {workloadSummary = a} :: MilestoneSummary)

-- | Undocumented member.
milestoneSummary_milestoneNumber :: Lens.Lens' MilestoneSummary (Prelude.Maybe Prelude.Natural)
milestoneSummary_milestoneNumber = Lens.lens (\MilestoneSummary' {milestoneNumber} -> milestoneNumber) (\s@MilestoneSummary' {} a -> s {milestoneNumber = a} :: MilestoneSummary)

instance Data.FromJSON MilestoneSummary where
  parseJSON =
    Data.withObject
      "MilestoneSummary"
      ( \x ->
          MilestoneSummary'
            Prelude.<$> (x Data..:? "RecordedAt")
            Prelude.<*> (x Data..:? "MilestoneName")
            Prelude.<*> (x Data..:? "WorkloadSummary")
            Prelude.<*> (x Data..:? "MilestoneNumber")
      )

instance Prelude.Hashable MilestoneSummary where
  hashWithSalt _salt MilestoneSummary' {..} =
    _salt `Prelude.hashWithSalt` recordedAt
      `Prelude.hashWithSalt` milestoneName
      `Prelude.hashWithSalt` workloadSummary
      `Prelude.hashWithSalt` milestoneNumber

instance Prelude.NFData MilestoneSummary where
  rnf MilestoneSummary' {..} =
    Prelude.rnf recordedAt
      `Prelude.seq` Prelude.rnf milestoneName
      `Prelude.seq` Prelude.rnf workloadSummary
      `Prelude.seq` Prelude.rnf milestoneNumber
