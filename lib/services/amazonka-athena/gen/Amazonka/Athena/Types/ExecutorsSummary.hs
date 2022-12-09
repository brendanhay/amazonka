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
-- Module      : Amazonka.Athena.Types.ExecutorsSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ExecutorsSummary where

import Amazonka.Athena.Types.ExecutorState
import Amazonka.Athena.Types.ExecutorType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about an executor.
--
-- /See:/ 'newExecutorsSummary' smart constructor.
data ExecutorsSummary = ExecutorsSummary'
  { -- | The smallest unit of compute that a session can request from Athena.
    -- Size is measured in data processing unit (DPU) values, a relative
    -- measure of processing power.
    executorSize :: Prelude.Maybe Prelude.Integer,
    -- | The processing state of the executor. A description of each state
    -- follows.
    --
    -- @CREATING@ - The executor is being started, including acquiring
    -- resources.
    --
    -- @CREATED@ - The executor has been started.
    --
    -- @REGISTERED@ - The executor has been registered.
    --
    -- @TERMINATING@ - The executor is in the process of shutting down.
    --
    -- @TERMINATED@ - The executor is no longer running.
    --
    -- @FAILED@ - Due to a failure, the executor is no longer running.
    executorState :: Prelude.Maybe ExecutorState,
    -- | The type of executor used for the application (@COORDINATOR@, @GATEWAY@,
    -- or @WORKER@).
    executorType :: Prelude.Maybe ExecutorType,
    -- | The date and time that the executor started.
    startDateTime :: Prelude.Maybe Prelude.Integer,
    -- | The date and time that the executor was terminated.
    terminationDateTime :: Prelude.Maybe Prelude.Integer,
    -- | The UUID of the executor.
    executorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutorsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executorSize', 'executorsSummary_executorSize' - The smallest unit of compute that a session can request from Athena.
-- Size is measured in data processing unit (DPU) values, a relative
-- measure of processing power.
--
-- 'executorState', 'executorsSummary_executorState' - The processing state of the executor. A description of each state
-- follows.
--
-- @CREATING@ - The executor is being started, including acquiring
-- resources.
--
-- @CREATED@ - The executor has been started.
--
-- @REGISTERED@ - The executor has been registered.
--
-- @TERMINATING@ - The executor is in the process of shutting down.
--
-- @TERMINATED@ - The executor is no longer running.
--
-- @FAILED@ - Due to a failure, the executor is no longer running.
--
-- 'executorType', 'executorsSummary_executorType' - The type of executor used for the application (@COORDINATOR@, @GATEWAY@,
-- or @WORKER@).
--
-- 'startDateTime', 'executorsSummary_startDateTime' - The date and time that the executor started.
--
-- 'terminationDateTime', 'executorsSummary_terminationDateTime' - The date and time that the executor was terminated.
--
-- 'executorId', 'executorsSummary_executorId' - The UUID of the executor.
newExecutorsSummary ::
  -- | 'executorId'
  Prelude.Text ->
  ExecutorsSummary
newExecutorsSummary pExecutorId_ =
  ExecutorsSummary'
    { executorSize = Prelude.Nothing,
      executorState = Prelude.Nothing,
      executorType = Prelude.Nothing,
      startDateTime = Prelude.Nothing,
      terminationDateTime = Prelude.Nothing,
      executorId = pExecutorId_
    }

-- | The smallest unit of compute that a session can request from Athena.
-- Size is measured in data processing unit (DPU) values, a relative
-- measure of processing power.
executorsSummary_executorSize :: Lens.Lens' ExecutorsSummary (Prelude.Maybe Prelude.Integer)
executorsSummary_executorSize = Lens.lens (\ExecutorsSummary' {executorSize} -> executorSize) (\s@ExecutorsSummary' {} a -> s {executorSize = a} :: ExecutorsSummary)

-- | The processing state of the executor. A description of each state
-- follows.
--
-- @CREATING@ - The executor is being started, including acquiring
-- resources.
--
-- @CREATED@ - The executor has been started.
--
-- @REGISTERED@ - The executor has been registered.
--
-- @TERMINATING@ - The executor is in the process of shutting down.
--
-- @TERMINATED@ - The executor is no longer running.
--
-- @FAILED@ - Due to a failure, the executor is no longer running.
executorsSummary_executorState :: Lens.Lens' ExecutorsSummary (Prelude.Maybe ExecutorState)
executorsSummary_executorState = Lens.lens (\ExecutorsSummary' {executorState} -> executorState) (\s@ExecutorsSummary' {} a -> s {executorState = a} :: ExecutorsSummary)

-- | The type of executor used for the application (@COORDINATOR@, @GATEWAY@,
-- or @WORKER@).
executorsSummary_executorType :: Lens.Lens' ExecutorsSummary (Prelude.Maybe ExecutorType)
executorsSummary_executorType = Lens.lens (\ExecutorsSummary' {executorType} -> executorType) (\s@ExecutorsSummary' {} a -> s {executorType = a} :: ExecutorsSummary)

-- | The date and time that the executor started.
executorsSummary_startDateTime :: Lens.Lens' ExecutorsSummary (Prelude.Maybe Prelude.Integer)
executorsSummary_startDateTime = Lens.lens (\ExecutorsSummary' {startDateTime} -> startDateTime) (\s@ExecutorsSummary' {} a -> s {startDateTime = a} :: ExecutorsSummary)

-- | The date and time that the executor was terminated.
executorsSummary_terminationDateTime :: Lens.Lens' ExecutorsSummary (Prelude.Maybe Prelude.Integer)
executorsSummary_terminationDateTime = Lens.lens (\ExecutorsSummary' {terminationDateTime} -> terminationDateTime) (\s@ExecutorsSummary' {} a -> s {terminationDateTime = a} :: ExecutorsSummary)

-- | The UUID of the executor.
executorsSummary_executorId :: Lens.Lens' ExecutorsSummary Prelude.Text
executorsSummary_executorId = Lens.lens (\ExecutorsSummary' {executorId} -> executorId) (\s@ExecutorsSummary' {} a -> s {executorId = a} :: ExecutorsSummary)

instance Data.FromJSON ExecutorsSummary where
  parseJSON =
    Data.withObject
      "ExecutorsSummary"
      ( \x ->
          ExecutorsSummary'
            Prelude.<$> (x Data..:? "ExecutorSize")
            Prelude.<*> (x Data..:? "ExecutorState")
            Prelude.<*> (x Data..:? "ExecutorType")
            Prelude.<*> (x Data..:? "StartDateTime")
            Prelude.<*> (x Data..:? "TerminationDateTime")
            Prelude.<*> (x Data..: "ExecutorId")
      )

instance Prelude.Hashable ExecutorsSummary where
  hashWithSalt _salt ExecutorsSummary' {..} =
    _salt `Prelude.hashWithSalt` executorSize
      `Prelude.hashWithSalt` executorState
      `Prelude.hashWithSalt` executorType
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` terminationDateTime
      `Prelude.hashWithSalt` executorId

instance Prelude.NFData ExecutorsSummary where
  rnf ExecutorsSummary' {..} =
    Prelude.rnf executorSize
      `Prelude.seq` Prelude.rnf executorState
      `Prelude.seq` Prelude.rnf executorType
      `Prelude.seq` Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf terminationDateTime
      `Prelude.seq` Prelude.rnf executorId
