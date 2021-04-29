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
-- Module      : Network.AWS.IoT.Types.TaskStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatistics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Statistics for the checks performed during the audit.
--
-- /See:/ 'newTaskStatistics' smart constructor.
data TaskStatistics = TaskStatistics'
  { -- | The number of checks in this audit.
    totalChecks :: Prelude.Maybe Prelude.Int,
    -- | The number of checks waiting for data collection.
    waitingForDataCollectionChecks :: Prelude.Maybe Prelude.Int,
    -- | The number of checks that found compliant resources.
    compliantChecks :: Prelude.Maybe Prelude.Int,
    -- | The number of checks in progress.
    inProgressChecks :: Prelude.Maybe Prelude.Int,
    -- | The number of checks.
    failedChecks :: Prelude.Maybe Prelude.Int,
    -- | The number of checks that found noncompliant resources.
    nonCompliantChecks :: Prelude.Maybe Prelude.Int,
    -- | The number of checks that did not run because the audit was canceled.
    canceledChecks :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TaskStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalChecks', 'taskStatistics_totalChecks' - The number of checks in this audit.
--
-- 'waitingForDataCollectionChecks', 'taskStatistics_waitingForDataCollectionChecks' - The number of checks waiting for data collection.
--
-- 'compliantChecks', 'taskStatistics_compliantChecks' - The number of checks that found compliant resources.
--
-- 'inProgressChecks', 'taskStatistics_inProgressChecks' - The number of checks in progress.
--
-- 'failedChecks', 'taskStatistics_failedChecks' - The number of checks.
--
-- 'nonCompliantChecks', 'taskStatistics_nonCompliantChecks' - The number of checks that found noncompliant resources.
--
-- 'canceledChecks', 'taskStatistics_canceledChecks' - The number of checks that did not run because the audit was canceled.
newTaskStatistics ::
  TaskStatistics
newTaskStatistics =
  TaskStatistics'
    { totalChecks = Prelude.Nothing,
      waitingForDataCollectionChecks = Prelude.Nothing,
      compliantChecks = Prelude.Nothing,
      inProgressChecks = Prelude.Nothing,
      failedChecks = Prelude.Nothing,
      nonCompliantChecks = Prelude.Nothing,
      canceledChecks = Prelude.Nothing
    }

-- | The number of checks in this audit.
taskStatistics_totalChecks :: Lens.Lens' TaskStatistics (Prelude.Maybe Prelude.Int)
taskStatistics_totalChecks = Lens.lens (\TaskStatistics' {totalChecks} -> totalChecks) (\s@TaskStatistics' {} a -> s {totalChecks = a} :: TaskStatistics)

-- | The number of checks waiting for data collection.
taskStatistics_waitingForDataCollectionChecks :: Lens.Lens' TaskStatistics (Prelude.Maybe Prelude.Int)
taskStatistics_waitingForDataCollectionChecks = Lens.lens (\TaskStatistics' {waitingForDataCollectionChecks} -> waitingForDataCollectionChecks) (\s@TaskStatistics' {} a -> s {waitingForDataCollectionChecks = a} :: TaskStatistics)

-- | The number of checks that found compliant resources.
taskStatistics_compliantChecks :: Lens.Lens' TaskStatistics (Prelude.Maybe Prelude.Int)
taskStatistics_compliantChecks = Lens.lens (\TaskStatistics' {compliantChecks} -> compliantChecks) (\s@TaskStatistics' {} a -> s {compliantChecks = a} :: TaskStatistics)

-- | The number of checks in progress.
taskStatistics_inProgressChecks :: Lens.Lens' TaskStatistics (Prelude.Maybe Prelude.Int)
taskStatistics_inProgressChecks = Lens.lens (\TaskStatistics' {inProgressChecks} -> inProgressChecks) (\s@TaskStatistics' {} a -> s {inProgressChecks = a} :: TaskStatistics)

-- | The number of checks.
taskStatistics_failedChecks :: Lens.Lens' TaskStatistics (Prelude.Maybe Prelude.Int)
taskStatistics_failedChecks = Lens.lens (\TaskStatistics' {failedChecks} -> failedChecks) (\s@TaskStatistics' {} a -> s {failedChecks = a} :: TaskStatistics)

-- | The number of checks that found noncompliant resources.
taskStatistics_nonCompliantChecks :: Lens.Lens' TaskStatistics (Prelude.Maybe Prelude.Int)
taskStatistics_nonCompliantChecks = Lens.lens (\TaskStatistics' {nonCompliantChecks} -> nonCompliantChecks) (\s@TaskStatistics' {} a -> s {nonCompliantChecks = a} :: TaskStatistics)

-- | The number of checks that did not run because the audit was canceled.
taskStatistics_canceledChecks :: Lens.Lens' TaskStatistics (Prelude.Maybe Prelude.Int)
taskStatistics_canceledChecks = Lens.lens (\TaskStatistics' {canceledChecks} -> canceledChecks) (\s@TaskStatistics' {} a -> s {canceledChecks = a} :: TaskStatistics)

instance Prelude.FromJSON TaskStatistics where
  parseJSON =
    Prelude.withObject
      "TaskStatistics"
      ( \x ->
          TaskStatistics'
            Prelude.<$> (x Prelude..:? "totalChecks")
            Prelude.<*> (x Prelude..:? "waitingForDataCollectionChecks")
            Prelude.<*> (x Prelude..:? "compliantChecks")
            Prelude.<*> (x Prelude..:? "inProgressChecks")
            Prelude.<*> (x Prelude..:? "failedChecks")
            Prelude.<*> (x Prelude..:? "nonCompliantChecks")
            Prelude.<*> (x Prelude..:? "canceledChecks")
      )

instance Prelude.Hashable TaskStatistics

instance Prelude.NFData TaskStatistics
