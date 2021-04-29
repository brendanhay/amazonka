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
-- Module      : Network.AWS.IoT.Types.TaskStatisticsForAuditCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatisticsForAuditCheck where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides summary counts of how many tasks for findings are in a
-- particular state. This information is included in the response from
-- DescribeAuditMitigationActionsTask.
--
-- /See:/ 'newTaskStatisticsForAuditCheck' smart constructor.
data TaskStatisticsForAuditCheck = TaskStatisticsForAuditCheck'
  { -- | The number of findings for which all mitigation actions succeeded when
    -- applied.
    succeededFindingsCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of findings to which a task is being applied.
    totalFindingsCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of findings for which at least one of the actions failed when
    -- applied.
    failedFindingsCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of findings skipped because of filter conditions provided in
    -- the parameters to the command.
    skippedFindingsCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of findings to which the mitigation action task was canceled
    -- when applied.
    canceledFindingsCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TaskStatisticsForAuditCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'succeededFindingsCount', 'taskStatisticsForAuditCheck_succeededFindingsCount' - The number of findings for which all mitigation actions succeeded when
-- applied.
--
-- 'totalFindingsCount', 'taskStatisticsForAuditCheck_totalFindingsCount' - The total number of findings to which a task is being applied.
--
-- 'failedFindingsCount', 'taskStatisticsForAuditCheck_failedFindingsCount' - The number of findings for which at least one of the actions failed when
-- applied.
--
-- 'skippedFindingsCount', 'taskStatisticsForAuditCheck_skippedFindingsCount' - The number of findings skipped because of filter conditions provided in
-- the parameters to the command.
--
-- 'canceledFindingsCount', 'taskStatisticsForAuditCheck_canceledFindingsCount' - The number of findings to which the mitigation action task was canceled
-- when applied.
newTaskStatisticsForAuditCheck ::
  TaskStatisticsForAuditCheck
newTaskStatisticsForAuditCheck =
  TaskStatisticsForAuditCheck'
    { succeededFindingsCount =
        Prelude.Nothing,
      totalFindingsCount = Prelude.Nothing,
      failedFindingsCount = Prelude.Nothing,
      skippedFindingsCount = Prelude.Nothing,
      canceledFindingsCount = Prelude.Nothing
    }

-- | The number of findings for which all mitigation actions succeeded when
-- applied.
taskStatisticsForAuditCheck_succeededFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Prelude.Maybe Prelude.Integer)
taskStatisticsForAuditCheck_succeededFindingsCount = Lens.lens (\TaskStatisticsForAuditCheck' {succeededFindingsCount} -> succeededFindingsCount) (\s@TaskStatisticsForAuditCheck' {} a -> s {succeededFindingsCount = a} :: TaskStatisticsForAuditCheck)

-- | The total number of findings to which a task is being applied.
taskStatisticsForAuditCheck_totalFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Prelude.Maybe Prelude.Integer)
taskStatisticsForAuditCheck_totalFindingsCount = Lens.lens (\TaskStatisticsForAuditCheck' {totalFindingsCount} -> totalFindingsCount) (\s@TaskStatisticsForAuditCheck' {} a -> s {totalFindingsCount = a} :: TaskStatisticsForAuditCheck)

-- | The number of findings for which at least one of the actions failed when
-- applied.
taskStatisticsForAuditCheck_failedFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Prelude.Maybe Prelude.Integer)
taskStatisticsForAuditCheck_failedFindingsCount = Lens.lens (\TaskStatisticsForAuditCheck' {failedFindingsCount} -> failedFindingsCount) (\s@TaskStatisticsForAuditCheck' {} a -> s {failedFindingsCount = a} :: TaskStatisticsForAuditCheck)

-- | The number of findings skipped because of filter conditions provided in
-- the parameters to the command.
taskStatisticsForAuditCheck_skippedFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Prelude.Maybe Prelude.Integer)
taskStatisticsForAuditCheck_skippedFindingsCount = Lens.lens (\TaskStatisticsForAuditCheck' {skippedFindingsCount} -> skippedFindingsCount) (\s@TaskStatisticsForAuditCheck' {} a -> s {skippedFindingsCount = a} :: TaskStatisticsForAuditCheck)

-- | The number of findings to which the mitigation action task was canceled
-- when applied.
taskStatisticsForAuditCheck_canceledFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Prelude.Maybe Prelude.Integer)
taskStatisticsForAuditCheck_canceledFindingsCount = Lens.lens (\TaskStatisticsForAuditCheck' {canceledFindingsCount} -> canceledFindingsCount) (\s@TaskStatisticsForAuditCheck' {} a -> s {canceledFindingsCount = a} :: TaskStatisticsForAuditCheck)

instance Prelude.FromJSON TaskStatisticsForAuditCheck where
  parseJSON =
    Prelude.withObject
      "TaskStatisticsForAuditCheck"
      ( \x ->
          TaskStatisticsForAuditCheck'
            Prelude.<$> (x Prelude..:? "succeededFindingsCount")
            Prelude.<*> (x Prelude..:? "totalFindingsCount")
            Prelude.<*> (x Prelude..:? "failedFindingsCount")
            Prelude.<*> (x Prelude..:? "skippedFindingsCount")
            Prelude.<*> (x Prelude..:? "canceledFindingsCount")
      )

instance Prelude.Hashable TaskStatisticsForAuditCheck

instance Prelude.NFData TaskStatisticsForAuditCheck
