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
-- Module      : Amazonka.DataSync.Types.TaskExecutionResultDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.TaskExecutionResultDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.PhaseStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the detailed result of a @TaskExecution@ operation. This
-- result includes the time in milliseconds spent in each phase, the status
-- of the task execution, and the errors encountered.
--
-- /See:/ 'newTaskExecutionResultDetail' smart constructor.
data TaskExecutionResultDetail = TaskExecutionResultDetail'
  { -- | Errors that DataSync encountered during execution of the task. You can
    -- use this error code to help troubleshoot issues.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | Detailed description of an error that was encountered during the task
    -- execution. You can use this information to help troubleshoot issues.
    errorDetail :: Prelude.Maybe Prelude.Text,
    -- | The total time in milliseconds that DataSync spent in the PREPARING
    -- phase.
    prepareDuration :: Prelude.Maybe Prelude.Natural,
    -- | The status of the PREPARING phase.
    prepareStatus :: Prelude.Maybe PhaseStatus,
    -- | The total time in milliseconds that DataSync took to transfer the file
    -- from the source to the destination location.
    totalDuration :: Prelude.Maybe Prelude.Natural,
    -- | The total time in milliseconds that DataSync spent in the TRANSFERRING
    -- phase.
    transferDuration :: Prelude.Maybe Prelude.Natural,
    -- | The status of the TRANSFERRING phase.
    transferStatus :: Prelude.Maybe PhaseStatus,
    -- | The total time in milliseconds that DataSync spent in the VERIFYING
    -- phase.
    verifyDuration :: Prelude.Maybe Prelude.Natural,
    -- | The status of the VERIFYING phase.
    verifyStatus :: Prelude.Maybe PhaseStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskExecutionResultDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'taskExecutionResultDetail_errorCode' - Errors that DataSync encountered during execution of the task. You can
-- use this error code to help troubleshoot issues.
--
-- 'errorDetail', 'taskExecutionResultDetail_errorDetail' - Detailed description of an error that was encountered during the task
-- execution. You can use this information to help troubleshoot issues.
--
-- 'prepareDuration', 'taskExecutionResultDetail_prepareDuration' - The total time in milliseconds that DataSync spent in the PREPARING
-- phase.
--
-- 'prepareStatus', 'taskExecutionResultDetail_prepareStatus' - The status of the PREPARING phase.
--
-- 'totalDuration', 'taskExecutionResultDetail_totalDuration' - The total time in milliseconds that DataSync took to transfer the file
-- from the source to the destination location.
--
-- 'transferDuration', 'taskExecutionResultDetail_transferDuration' - The total time in milliseconds that DataSync spent in the TRANSFERRING
-- phase.
--
-- 'transferStatus', 'taskExecutionResultDetail_transferStatus' - The status of the TRANSFERRING phase.
--
-- 'verifyDuration', 'taskExecutionResultDetail_verifyDuration' - The total time in milliseconds that DataSync spent in the VERIFYING
-- phase.
--
-- 'verifyStatus', 'taskExecutionResultDetail_verifyStatus' - The status of the VERIFYING phase.
newTaskExecutionResultDetail ::
  TaskExecutionResultDetail
newTaskExecutionResultDetail =
  TaskExecutionResultDetail'
    { errorCode =
        Prelude.Nothing,
      errorDetail = Prelude.Nothing,
      prepareDuration = Prelude.Nothing,
      prepareStatus = Prelude.Nothing,
      totalDuration = Prelude.Nothing,
      transferDuration = Prelude.Nothing,
      transferStatus = Prelude.Nothing,
      verifyDuration = Prelude.Nothing,
      verifyStatus = Prelude.Nothing
    }

-- | Errors that DataSync encountered during execution of the task. You can
-- use this error code to help troubleshoot issues.
taskExecutionResultDetail_errorCode :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe Prelude.Text)
taskExecutionResultDetail_errorCode = Lens.lens (\TaskExecutionResultDetail' {errorCode} -> errorCode) (\s@TaskExecutionResultDetail' {} a -> s {errorCode = a} :: TaskExecutionResultDetail)

-- | Detailed description of an error that was encountered during the task
-- execution. You can use this information to help troubleshoot issues.
taskExecutionResultDetail_errorDetail :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe Prelude.Text)
taskExecutionResultDetail_errorDetail = Lens.lens (\TaskExecutionResultDetail' {errorDetail} -> errorDetail) (\s@TaskExecutionResultDetail' {} a -> s {errorDetail = a} :: TaskExecutionResultDetail)

-- | The total time in milliseconds that DataSync spent in the PREPARING
-- phase.
taskExecutionResultDetail_prepareDuration :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe Prelude.Natural)
taskExecutionResultDetail_prepareDuration = Lens.lens (\TaskExecutionResultDetail' {prepareDuration} -> prepareDuration) (\s@TaskExecutionResultDetail' {} a -> s {prepareDuration = a} :: TaskExecutionResultDetail)

-- | The status of the PREPARING phase.
taskExecutionResultDetail_prepareStatus :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe PhaseStatus)
taskExecutionResultDetail_prepareStatus = Lens.lens (\TaskExecutionResultDetail' {prepareStatus} -> prepareStatus) (\s@TaskExecutionResultDetail' {} a -> s {prepareStatus = a} :: TaskExecutionResultDetail)

-- | The total time in milliseconds that DataSync took to transfer the file
-- from the source to the destination location.
taskExecutionResultDetail_totalDuration :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe Prelude.Natural)
taskExecutionResultDetail_totalDuration = Lens.lens (\TaskExecutionResultDetail' {totalDuration} -> totalDuration) (\s@TaskExecutionResultDetail' {} a -> s {totalDuration = a} :: TaskExecutionResultDetail)

-- | The total time in milliseconds that DataSync spent in the TRANSFERRING
-- phase.
taskExecutionResultDetail_transferDuration :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe Prelude.Natural)
taskExecutionResultDetail_transferDuration = Lens.lens (\TaskExecutionResultDetail' {transferDuration} -> transferDuration) (\s@TaskExecutionResultDetail' {} a -> s {transferDuration = a} :: TaskExecutionResultDetail)

-- | The status of the TRANSFERRING phase.
taskExecutionResultDetail_transferStatus :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe PhaseStatus)
taskExecutionResultDetail_transferStatus = Lens.lens (\TaskExecutionResultDetail' {transferStatus} -> transferStatus) (\s@TaskExecutionResultDetail' {} a -> s {transferStatus = a} :: TaskExecutionResultDetail)

-- | The total time in milliseconds that DataSync spent in the VERIFYING
-- phase.
taskExecutionResultDetail_verifyDuration :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe Prelude.Natural)
taskExecutionResultDetail_verifyDuration = Lens.lens (\TaskExecutionResultDetail' {verifyDuration} -> verifyDuration) (\s@TaskExecutionResultDetail' {} a -> s {verifyDuration = a} :: TaskExecutionResultDetail)

-- | The status of the VERIFYING phase.
taskExecutionResultDetail_verifyStatus :: Lens.Lens' TaskExecutionResultDetail (Prelude.Maybe PhaseStatus)
taskExecutionResultDetail_verifyStatus = Lens.lens (\TaskExecutionResultDetail' {verifyStatus} -> verifyStatus) (\s@TaskExecutionResultDetail' {} a -> s {verifyStatus = a} :: TaskExecutionResultDetail)

instance Data.FromJSON TaskExecutionResultDetail where
  parseJSON =
    Data.withObject
      "TaskExecutionResultDetail"
      ( \x ->
          TaskExecutionResultDetail'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorDetail")
            Prelude.<*> (x Data..:? "PrepareDuration")
            Prelude.<*> (x Data..:? "PrepareStatus")
            Prelude.<*> (x Data..:? "TotalDuration")
            Prelude.<*> (x Data..:? "TransferDuration")
            Prelude.<*> (x Data..:? "TransferStatus")
            Prelude.<*> (x Data..:? "VerifyDuration")
            Prelude.<*> (x Data..:? "VerifyStatus")
      )

instance Prelude.Hashable TaskExecutionResultDetail where
  hashWithSalt _salt TaskExecutionResultDetail' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorDetail
      `Prelude.hashWithSalt` prepareDuration
      `Prelude.hashWithSalt` prepareStatus
      `Prelude.hashWithSalt` totalDuration
      `Prelude.hashWithSalt` transferDuration
      `Prelude.hashWithSalt` transferStatus
      `Prelude.hashWithSalt` verifyDuration
      `Prelude.hashWithSalt` verifyStatus

instance Prelude.NFData TaskExecutionResultDetail where
  rnf TaskExecutionResultDetail' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorDetail
      `Prelude.seq` Prelude.rnf prepareDuration
      `Prelude.seq` Prelude.rnf prepareStatus
      `Prelude.seq` Prelude.rnf totalDuration
      `Prelude.seq` Prelude.rnf transferDuration
      `Prelude.seq` Prelude.rnf transferStatus
      `Prelude.seq` Prelude.rnf verifyDuration
      `Prelude.seq` Prelude.rnf verifyStatus
