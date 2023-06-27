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
-- Module      : Amazonka.AutoScaling.Types.RollbackDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.RollbackDetails where

import Amazonka.AutoScaling.Types.InstanceRefreshProgressDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an instance refresh rollback.
--
-- /See:/ 'newRollbackDetails' smart constructor.
data RollbackDetails = RollbackDetails'
  { -- | Indicates the value of @InstancesToUpdate@ at the time the rollback
    -- started.
    instancesToUpdateOnRollback :: Prelude.Maybe Prelude.Natural,
    -- | Indicates the value of @PercentageComplete@ at the time the rollback
    -- started.
    percentageCompleteOnRollback :: Prelude.Maybe Prelude.Natural,
    -- | Reports progress on replacing instances in an Auto Scaling group that
    -- has a warm pool. This includes separate details for instances in the
    -- warm pool and instances in the Auto Scaling group (the live pool).
    progressDetailsOnRollback :: Prelude.Maybe InstanceRefreshProgressDetails,
    -- | The reason for this instance refresh rollback (for example, whether a
    -- manual or automatic rollback was initiated).
    rollbackReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the rollback began.
    rollbackStartTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancesToUpdateOnRollback', 'rollbackDetails_instancesToUpdateOnRollback' - Indicates the value of @InstancesToUpdate@ at the time the rollback
-- started.
--
-- 'percentageCompleteOnRollback', 'rollbackDetails_percentageCompleteOnRollback' - Indicates the value of @PercentageComplete@ at the time the rollback
-- started.
--
-- 'progressDetailsOnRollback', 'rollbackDetails_progressDetailsOnRollback' - Reports progress on replacing instances in an Auto Scaling group that
-- has a warm pool. This includes separate details for instances in the
-- warm pool and instances in the Auto Scaling group (the live pool).
--
-- 'rollbackReason', 'rollbackDetails_rollbackReason' - The reason for this instance refresh rollback (for example, whether a
-- manual or automatic rollback was initiated).
--
-- 'rollbackStartTime', 'rollbackDetails_rollbackStartTime' - The date and time at which the rollback began.
newRollbackDetails ::
  RollbackDetails
newRollbackDetails =
  RollbackDetails'
    { instancesToUpdateOnRollback =
        Prelude.Nothing,
      percentageCompleteOnRollback = Prelude.Nothing,
      progressDetailsOnRollback = Prelude.Nothing,
      rollbackReason = Prelude.Nothing,
      rollbackStartTime = Prelude.Nothing
    }

-- | Indicates the value of @InstancesToUpdate@ at the time the rollback
-- started.
rollbackDetails_instancesToUpdateOnRollback :: Lens.Lens' RollbackDetails (Prelude.Maybe Prelude.Natural)
rollbackDetails_instancesToUpdateOnRollback = Lens.lens (\RollbackDetails' {instancesToUpdateOnRollback} -> instancesToUpdateOnRollback) (\s@RollbackDetails' {} a -> s {instancesToUpdateOnRollback = a} :: RollbackDetails)

-- | Indicates the value of @PercentageComplete@ at the time the rollback
-- started.
rollbackDetails_percentageCompleteOnRollback :: Lens.Lens' RollbackDetails (Prelude.Maybe Prelude.Natural)
rollbackDetails_percentageCompleteOnRollback = Lens.lens (\RollbackDetails' {percentageCompleteOnRollback} -> percentageCompleteOnRollback) (\s@RollbackDetails' {} a -> s {percentageCompleteOnRollback = a} :: RollbackDetails)

-- | Reports progress on replacing instances in an Auto Scaling group that
-- has a warm pool. This includes separate details for instances in the
-- warm pool and instances in the Auto Scaling group (the live pool).
rollbackDetails_progressDetailsOnRollback :: Lens.Lens' RollbackDetails (Prelude.Maybe InstanceRefreshProgressDetails)
rollbackDetails_progressDetailsOnRollback = Lens.lens (\RollbackDetails' {progressDetailsOnRollback} -> progressDetailsOnRollback) (\s@RollbackDetails' {} a -> s {progressDetailsOnRollback = a} :: RollbackDetails)

-- | The reason for this instance refresh rollback (for example, whether a
-- manual or automatic rollback was initiated).
rollbackDetails_rollbackReason :: Lens.Lens' RollbackDetails (Prelude.Maybe Prelude.Text)
rollbackDetails_rollbackReason = Lens.lens (\RollbackDetails' {rollbackReason} -> rollbackReason) (\s@RollbackDetails' {} a -> s {rollbackReason = a} :: RollbackDetails)

-- | The date and time at which the rollback began.
rollbackDetails_rollbackStartTime :: Lens.Lens' RollbackDetails (Prelude.Maybe Prelude.UTCTime)
rollbackDetails_rollbackStartTime = Lens.lens (\RollbackDetails' {rollbackStartTime} -> rollbackStartTime) (\s@RollbackDetails' {} a -> s {rollbackStartTime = a} :: RollbackDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromXML RollbackDetails where
  parseXML x =
    RollbackDetails'
      Prelude.<$> (x Data..@? "InstancesToUpdateOnRollback")
      Prelude.<*> (x Data..@? "PercentageCompleteOnRollback")
      Prelude.<*> (x Data..@? "ProgressDetailsOnRollback")
      Prelude.<*> (x Data..@? "RollbackReason")
      Prelude.<*> (x Data..@? "RollbackStartTime")

instance Prelude.Hashable RollbackDetails where
  hashWithSalt _salt RollbackDetails' {..} =
    _salt
      `Prelude.hashWithSalt` instancesToUpdateOnRollback
      `Prelude.hashWithSalt` percentageCompleteOnRollback
      `Prelude.hashWithSalt` progressDetailsOnRollback
      `Prelude.hashWithSalt` rollbackReason
      `Prelude.hashWithSalt` rollbackStartTime

instance Prelude.NFData RollbackDetails where
  rnf RollbackDetails' {..} =
    Prelude.rnf instancesToUpdateOnRollback
      `Prelude.seq` Prelude.rnf percentageCompleteOnRollback
      `Prelude.seq` Prelude.rnf progressDetailsOnRollback
      `Prelude.seq` Prelude.rnf rollbackReason
      `Prelude.seq` Prelude.rnf rollbackStartTime
