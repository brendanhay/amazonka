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
-- Module      : Amazonka.ControlTower.Types.ControlOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ControlTower.Types.ControlOperation where

import Amazonka.ControlTower.Types.ControlOperationStatus
import Amazonka.ControlTower.Types.ControlOperationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An operation performed by the control.
--
-- /See:/ 'newControlOperation' smart constructor.
data ControlOperation = ControlOperation'
  { -- | One of @ENABLE_CONTROL@ or @DISABLE_CONTROL@.
    operationType :: Prelude.Maybe ControlOperationType,
    -- | One of @IN_PROGRESS@, @SUCEEDED@, or @FAILED@.
    status :: Prelude.Maybe ControlOperationStatus,
    -- | The time that the operation finished.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | If the operation result is @FAILED@, this string contains a message
    -- explaining why the operation failed.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The time that the operation began.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationType', 'controlOperation_operationType' - One of @ENABLE_CONTROL@ or @DISABLE_CONTROL@.
--
-- 'status', 'controlOperation_status' - One of @IN_PROGRESS@, @SUCEEDED@, or @FAILED@.
--
-- 'endTime', 'controlOperation_endTime' - The time that the operation finished.
--
-- 'statusMessage', 'controlOperation_statusMessage' - If the operation result is @FAILED@, this string contains a message
-- explaining why the operation failed.
--
-- 'startTime', 'controlOperation_startTime' - The time that the operation began.
newControlOperation ::
  ControlOperation
newControlOperation =
  ControlOperation'
    { operationType = Prelude.Nothing,
      status = Prelude.Nothing,
      endTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | One of @ENABLE_CONTROL@ or @DISABLE_CONTROL@.
controlOperation_operationType :: Lens.Lens' ControlOperation (Prelude.Maybe ControlOperationType)
controlOperation_operationType = Lens.lens (\ControlOperation' {operationType} -> operationType) (\s@ControlOperation' {} a -> s {operationType = a} :: ControlOperation)

-- | One of @IN_PROGRESS@, @SUCEEDED@, or @FAILED@.
controlOperation_status :: Lens.Lens' ControlOperation (Prelude.Maybe ControlOperationStatus)
controlOperation_status = Lens.lens (\ControlOperation' {status} -> status) (\s@ControlOperation' {} a -> s {status = a} :: ControlOperation)

-- | The time that the operation finished.
controlOperation_endTime :: Lens.Lens' ControlOperation (Prelude.Maybe Prelude.UTCTime)
controlOperation_endTime = Lens.lens (\ControlOperation' {endTime} -> endTime) (\s@ControlOperation' {} a -> s {endTime = a} :: ControlOperation) Prelude.. Lens.mapping Data._Time

-- | If the operation result is @FAILED@, this string contains a message
-- explaining why the operation failed.
controlOperation_statusMessage :: Lens.Lens' ControlOperation (Prelude.Maybe Prelude.Text)
controlOperation_statusMessage = Lens.lens (\ControlOperation' {statusMessage} -> statusMessage) (\s@ControlOperation' {} a -> s {statusMessage = a} :: ControlOperation)

-- | The time that the operation began.
controlOperation_startTime :: Lens.Lens' ControlOperation (Prelude.Maybe Prelude.UTCTime)
controlOperation_startTime = Lens.lens (\ControlOperation' {startTime} -> startTime) (\s@ControlOperation' {} a -> s {startTime = a} :: ControlOperation) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ControlOperation where
  parseJSON =
    Data.withObject
      "ControlOperation"
      ( \x ->
          ControlOperation'
            Prelude.<$> (x Data..:? "operationType")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable ControlOperation where
  hashWithSalt _salt ControlOperation' {..} =
    _salt `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ControlOperation where
  rnf ControlOperation' {..} =
    Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf startTime
