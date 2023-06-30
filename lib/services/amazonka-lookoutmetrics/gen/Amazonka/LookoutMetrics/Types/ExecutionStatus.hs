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
-- Module      : Amazonka.LookoutMetrics.Types.ExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.ExecutionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.AnomalyDetectionTaskStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of an anomaly detector run.
--
-- /See:/ 'newExecutionStatus' smart constructor.
data ExecutionStatus = ExecutionStatus'
  { -- | The reason that the run failed, if applicable.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The run\'s status.
    status :: Prelude.Maybe AnomalyDetectionTaskStatus,
    -- | The run\'s timestamp.
    timestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'executionStatus_failureReason' - The reason that the run failed, if applicable.
--
-- 'status', 'executionStatus_status' - The run\'s status.
--
-- 'timestamp', 'executionStatus_timestamp' - The run\'s timestamp.
newExecutionStatus ::
  ExecutionStatus
newExecutionStatus =
  ExecutionStatus'
    { failureReason = Prelude.Nothing,
      status = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The reason that the run failed, if applicable.
executionStatus_failureReason :: Lens.Lens' ExecutionStatus (Prelude.Maybe Prelude.Text)
executionStatus_failureReason = Lens.lens (\ExecutionStatus' {failureReason} -> failureReason) (\s@ExecutionStatus' {} a -> s {failureReason = a} :: ExecutionStatus)

-- | The run\'s status.
executionStatus_status :: Lens.Lens' ExecutionStatus (Prelude.Maybe AnomalyDetectionTaskStatus)
executionStatus_status = Lens.lens (\ExecutionStatus' {status} -> status) (\s@ExecutionStatus' {} a -> s {status = a} :: ExecutionStatus)

-- | The run\'s timestamp.
executionStatus_timestamp :: Lens.Lens' ExecutionStatus (Prelude.Maybe Prelude.Text)
executionStatus_timestamp = Lens.lens (\ExecutionStatus' {timestamp} -> timestamp) (\s@ExecutionStatus' {} a -> s {timestamp = a} :: ExecutionStatus)

instance Data.FromJSON ExecutionStatus where
  parseJSON =
    Data.withObject
      "ExecutionStatus"
      ( \x ->
          ExecutionStatus'
            Prelude.<$> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable ExecutionStatus where
  hashWithSalt _salt ExecutionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData ExecutionStatus where
  rnf ExecutionStatus' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf timestamp
