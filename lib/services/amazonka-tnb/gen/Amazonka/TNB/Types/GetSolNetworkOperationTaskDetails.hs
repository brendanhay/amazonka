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
-- Module      : Amazonka.TNB.Types.GetSolNetworkOperationTaskDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolNetworkOperationTaskDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.ErrorInfo
import Amazonka.TNB.Types.TaskStatus

-- | Gets the details of a network operation.
--
-- A network operation is any operation that is done to your network, such
-- as network instance instantiation or termination.
--
-- /See:/ 'newGetSolNetworkOperationTaskDetails' smart constructor.
data GetSolNetworkOperationTaskDetails = GetSolNetworkOperationTaskDetails'
  { -- | Context for the network operation task.
    taskContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Task end time.
    taskEndTime :: Prelude.Maybe Data.ISO8601,
    -- | Task error details.
    taskErrorDetails :: Prelude.Maybe ErrorInfo,
    -- | Task name.
    taskName :: Prelude.Maybe Prelude.Text,
    -- | Task start time.
    taskStartTime :: Prelude.Maybe Data.ISO8601,
    -- | Task status.
    taskStatus :: Prelude.Maybe TaskStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkOperationTaskDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskContext', 'getSolNetworkOperationTaskDetails_taskContext' - Context for the network operation task.
--
-- 'taskEndTime', 'getSolNetworkOperationTaskDetails_taskEndTime' - Task end time.
--
-- 'taskErrorDetails', 'getSolNetworkOperationTaskDetails_taskErrorDetails' - Task error details.
--
-- 'taskName', 'getSolNetworkOperationTaskDetails_taskName' - Task name.
--
-- 'taskStartTime', 'getSolNetworkOperationTaskDetails_taskStartTime' - Task start time.
--
-- 'taskStatus', 'getSolNetworkOperationTaskDetails_taskStatus' - Task status.
newGetSolNetworkOperationTaskDetails ::
  GetSolNetworkOperationTaskDetails
newGetSolNetworkOperationTaskDetails =
  GetSolNetworkOperationTaskDetails'
    { taskContext =
        Prelude.Nothing,
      taskEndTime = Prelude.Nothing,
      taskErrorDetails = Prelude.Nothing,
      taskName = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      taskStatus = Prelude.Nothing
    }

-- | Context for the network operation task.
getSolNetworkOperationTaskDetails_taskContext :: Lens.Lens' GetSolNetworkOperationTaskDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSolNetworkOperationTaskDetails_taskContext = Lens.lens (\GetSolNetworkOperationTaskDetails' {taskContext} -> taskContext) (\s@GetSolNetworkOperationTaskDetails' {} a -> s {taskContext = a} :: GetSolNetworkOperationTaskDetails) Prelude.. Lens.mapping Lens.coerced

-- | Task end time.
getSolNetworkOperationTaskDetails_taskEndTime :: Lens.Lens' GetSolNetworkOperationTaskDetails (Prelude.Maybe Prelude.UTCTime)
getSolNetworkOperationTaskDetails_taskEndTime = Lens.lens (\GetSolNetworkOperationTaskDetails' {taskEndTime} -> taskEndTime) (\s@GetSolNetworkOperationTaskDetails' {} a -> s {taskEndTime = a} :: GetSolNetworkOperationTaskDetails) Prelude.. Lens.mapping Data._Time

-- | Task error details.
getSolNetworkOperationTaskDetails_taskErrorDetails :: Lens.Lens' GetSolNetworkOperationTaskDetails (Prelude.Maybe ErrorInfo)
getSolNetworkOperationTaskDetails_taskErrorDetails = Lens.lens (\GetSolNetworkOperationTaskDetails' {taskErrorDetails} -> taskErrorDetails) (\s@GetSolNetworkOperationTaskDetails' {} a -> s {taskErrorDetails = a} :: GetSolNetworkOperationTaskDetails)

-- | Task name.
getSolNetworkOperationTaskDetails_taskName :: Lens.Lens' GetSolNetworkOperationTaskDetails (Prelude.Maybe Prelude.Text)
getSolNetworkOperationTaskDetails_taskName = Lens.lens (\GetSolNetworkOperationTaskDetails' {taskName} -> taskName) (\s@GetSolNetworkOperationTaskDetails' {} a -> s {taskName = a} :: GetSolNetworkOperationTaskDetails)

-- | Task start time.
getSolNetworkOperationTaskDetails_taskStartTime :: Lens.Lens' GetSolNetworkOperationTaskDetails (Prelude.Maybe Prelude.UTCTime)
getSolNetworkOperationTaskDetails_taskStartTime = Lens.lens (\GetSolNetworkOperationTaskDetails' {taskStartTime} -> taskStartTime) (\s@GetSolNetworkOperationTaskDetails' {} a -> s {taskStartTime = a} :: GetSolNetworkOperationTaskDetails) Prelude.. Lens.mapping Data._Time

-- | Task status.
getSolNetworkOperationTaskDetails_taskStatus :: Lens.Lens' GetSolNetworkOperationTaskDetails (Prelude.Maybe TaskStatus)
getSolNetworkOperationTaskDetails_taskStatus = Lens.lens (\GetSolNetworkOperationTaskDetails' {taskStatus} -> taskStatus) (\s@GetSolNetworkOperationTaskDetails' {} a -> s {taskStatus = a} :: GetSolNetworkOperationTaskDetails)

instance
  Data.FromJSON
    GetSolNetworkOperationTaskDetails
  where
  parseJSON =
    Data.withObject
      "GetSolNetworkOperationTaskDetails"
      ( \x ->
          GetSolNetworkOperationTaskDetails'
            Prelude.<$> (x Data..:? "taskContext" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "taskEndTime")
            Prelude.<*> (x Data..:? "taskErrorDetails")
            Prelude.<*> (x Data..:? "taskName")
            Prelude.<*> (x Data..:? "taskStartTime")
            Prelude.<*> (x Data..:? "taskStatus")
      )

instance
  Prelude.Hashable
    GetSolNetworkOperationTaskDetails
  where
  hashWithSalt
    _salt
    GetSolNetworkOperationTaskDetails' {..} =
      _salt
        `Prelude.hashWithSalt` taskContext
        `Prelude.hashWithSalt` taskEndTime
        `Prelude.hashWithSalt` taskErrorDetails
        `Prelude.hashWithSalt` taskName
        `Prelude.hashWithSalt` taskStartTime
        `Prelude.hashWithSalt` taskStatus

instance
  Prelude.NFData
    GetSolNetworkOperationTaskDetails
  where
  rnf GetSolNetworkOperationTaskDetails' {..} =
    Prelude.rnf taskContext
      `Prelude.seq` Prelude.rnf taskEndTime
      `Prelude.seq` Prelude.rnf taskErrorDetails
      `Prelude.seq` Prelude.rnf taskName
      `Prelude.seq` Prelude.rnf taskStartTime
      `Prelude.seq` Prelude.rnf taskStatus
