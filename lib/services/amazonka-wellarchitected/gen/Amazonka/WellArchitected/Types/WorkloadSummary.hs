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
-- Module      : Amazonka.WellArchitected.Types.WorkloadSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.WorkloadSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.Risk
import Amazonka.WellArchitected.Types.WorkloadImprovementStatus

-- | A workload summary return object.
--
-- /See:/ 'newWorkloadSummary' smart constructor.
data WorkloadSummary = WorkloadSummary'
  { improvementStatus :: Prelude.Maybe WorkloadImprovementStatus,
    lenses :: Prelude.Maybe [Prelude.Text],
    owner :: Prelude.Maybe Prelude.Text,
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural),
    updatedAt :: Prelude.Maybe Data.POSIX,
    workloadArn :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkloadSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'improvementStatus', 'workloadSummary_improvementStatus' - Undocumented member.
--
-- 'lenses', 'workloadSummary_lenses' - Undocumented member.
--
-- 'owner', 'workloadSummary_owner' - Undocumented member.
--
-- 'riskCounts', 'workloadSummary_riskCounts' - Undocumented member.
--
-- 'updatedAt', 'workloadSummary_updatedAt' - Undocumented member.
--
-- 'workloadArn', 'workloadSummary_workloadArn' - Undocumented member.
--
-- 'workloadId', 'workloadSummary_workloadId' - Undocumented member.
--
-- 'workloadName', 'workloadSummary_workloadName' - Undocumented member.
newWorkloadSummary ::
  WorkloadSummary
newWorkloadSummary =
  WorkloadSummary'
    { improvementStatus =
        Prelude.Nothing,
      lenses = Prelude.Nothing,
      owner = Prelude.Nothing,
      riskCounts = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      workloadArn = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      workloadName = Prelude.Nothing
    }

-- | Undocumented member.
workloadSummary_improvementStatus :: Lens.Lens' WorkloadSummary (Prelude.Maybe WorkloadImprovementStatus)
workloadSummary_improvementStatus = Lens.lens (\WorkloadSummary' {improvementStatus} -> improvementStatus) (\s@WorkloadSummary' {} a -> s {improvementStatus = a} :: WorkloadSummary)

-- | Undocumented member.
workloadSummary_lenses :: Lens.Lens' WorkloadSummary (Prelude.Maybe [Prelude.Text])
workloadSummary_lenses = Lens.lens (\WorkloadSummary' {lenses} -> lenses) (\s@WorkloadSummary' {} a -> s {lenses = a} :: WorkloadSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workloadSummary_owner :: Lens.Lens' WorkloadSummary (Prelude.Maybe Prelude.Text)
workloadSummary_owner = Lens.lens (\WorkloadSummary' {owner} -> owner) (\s@WorkloadSummary' {} a -> s {owner = a} :: WorkloadSummary)

-- | Undocumented member.
workloadSummary_riskCounts :: Lens.Lens' WorkloadSummary (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
workloadSummary_riskCounts = Lens.lens (\WorkloadSummary' {riskCounts} -> riskCounts) (\s@WorkloadSummary' {} a -> s {riskCounts = a} :: WorkloadSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workloadSummary_updatedAt :: Lens.Lens' WorkloadSummary (Prelude.Maybe Prelude.UTCTime)
workloadSummary_updatedAt = Lens.lens (\WorkloadSummary' {updatedAt} -> updatedAt) (\s@WorkloadSummary' {} a -> s {updatedAt = a} :: WorkloadSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
workloadSummary_workloadArn :: Lens.Lens' WorkloadSummary (Prelude.Maybe Prelude.Text)
workloadSummary_workloadArn = Lens.lens (\WorkloadSummary' {workloadArn} -> workloadArn) (\s@WorkloadSummary' {} a -> s {workloadArn = a} :: WorkloadSummary)

-- | Undocumented member.
workloadSummary_workloadId :: Lens.Lens' WorkloadSummary (Prelude.Maybe Prelude.Text)
workloadSummary_workloadId = Lens.lens (\WorkloadSummary' {workloadId} -> workloadId) (\s@WorkloadSummary' {} a -> s {workloadId = a} :: WorkloadSummary)

-- | Undocumented member.
workloadSummary_workloadName :: Lens.Lens' WorkloadSummary (Prelude.Maybe Prelude.Text)
workloadSummary_workloadName = Lens.lens (\WorkloadSummary' {workloadName} -> workloadName) (\s@WorkloadSummary' {} a -> s {workloadName = a} :: WorkloadSummary)

instance Data.FromJSON WorkloadSummary where
  parseJSON =
    Data.withObject
      "WorkloadSummary"
      ( \x ->
          WorkloadSummary'
            Prelude.<$> (x Data..:? "ImprovementStatus")
            Prelude.<*> (x Data..:? "Lenses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "RiskCounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UpdatedAt")
            Prelude.<*> (x Data..:? "WorkloadArn")
            Prelude.<*> (x Data..:? "WorkloadId")
            Prelude.<*> (x Data..:? "WorkloadName")
      )

instance Prelude.Hashable WorkloadSummary where
  hashWithSalt _salt WorkloadSummary' {..} =
    _salt `Prelude.hashWithSalt` improvementStatus
      `Prelude.hashWithSalt` lenses
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` riskCounts
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` workloadArn
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` workloadName

instance Prelude.NFData WorkloadSummary where
  rnf WorkloadSummary' {..} =
    Prelude.rnf improvementStatus
      `Prelude.seq` Prelude.rnf lenses
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf riskCounts
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf workloadArn
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf workloadName
