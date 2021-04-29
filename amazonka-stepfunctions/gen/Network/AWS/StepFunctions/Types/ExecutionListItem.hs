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
-- Module      : Network.AWS.StepFunctions.Types.ExecutionListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionListItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.ExecutionStatus

-- | Contains details about an execution.
--
-- /See:/ 'newExecutionListItem' smart constructor.
data ExecutionListItem = ExecutionListItem'
  { -- | If the execution already ended, the date the execution stopped.
    stopDate :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the executed state machine.
    stateMachineArn :: Prelude.Text,
    -- | The name of the execution.
    --
    -- A name must /not/ contain:
    --
    -- -   white space
    --
    -- -   brackets @\< > { } [ ]@
    --
    -- -   wildcard characters @? *@
    --
    -- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
    --
    -- -   control characters (@U+0000-001F@, @U+007F-009F@)
    --
    -- To enable logging with CloudWatch Logs, the name should only contain
    -- 0-9, A-Z, a-z, - and _.
    name :: Prelude.Text,
    -- | The current status of the execution.
    status :: ExecutionStatus,
    -- | The date the execution started.
    startDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExecutionListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopDate', 'executionListItem_stopDate' - If the execution already ended, the date the execution stopped.
--
-- 'executionArn', 'executionListItem_executionArn' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- 'stateMachineArn', 'executionListItem_stateMachineArn' - The Amazon Resource Name (ARN) of the executed state machine.
--
-- 'name', 'executionListItem_name' - The name of the execution.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
--
-- 'status', 'executionListItem_status' - The current status of the execution.
--
-- 'startDate', 'executionListItem_startDate' - The date the execution started.
newExecutionListItem ::
  -- | 'executionArn'
  Prelude.Text ->
  -- | 'stateMachineArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ExecutionStatus ->
  -- | 'startDate'
  Prelude.UTCTime ->
  ExecutionListItem
newExecutionListItem
  pExecutionArn_
  pStateMachineArn_
  pName_
  pStatus_
  pStartDate_ =
    ExecutionListItem'
      { stopDate = Prelude.Nothing,
        executionArn = pExecutionArn_,
        stateMachineArn = pStateMachineArn_,
        name = pName_,
        status = pStatus_,
        startDate = Prelude._Time Lens.# pStartDate_
      }

-- | If the execution already ended, the date the execution stopped.
executionListItem_stopDate :: Lens.Lens' ExecutionListItem (Prelude.Maybe Prelude.UTCTime)
executionListItem_stopDate = Lens.lens (\ExecutionListItem' {stopDate} -> stopDate) (\s@ExecutionListItem' {} a -> s {stopDate = a} :: ExecutionListItem) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) that identifies the execution.
executionListItem_executionArn :: Lens.Lens' ExecutionListItem Prelude.Text
executionListItem_executionArn = Lens.lens (\ExecutionListItem' {executionArn} -> executionArn) (\s@ExecutionListItem' {} a -> s {executionArn = a} :: ExecutionListItem)

-- | The Amazon Resource Name (ARN) of the executed state machine.
executionListItem_stateMachineArn :: Lens.Lens' ExecutionListItem Prelude.Text
executionListItem_stateMachineArn = Lens.lens (\ExecutionListItem' {stateMachineArn} -> stateMachineArn) (\s@ExecutionListItem' {} a -> s {stateMachineArn = a} :: ExecutionListItem)

-- | The name of the execution.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
executionListItem_name :: Lens.Lens' ExecutionListItem Prelude.Text
executionListItem_name = Lens.lens (\ExecutionListItem' {name} -> name) (\s@ExecutionListItem' {} a -> s {name = a} :: ExecutionListItem)

-- | The current status of the execution.
executionListItem_status :: Lens.Lens' ExecutionListItem ExecutionStatus
executionListItem_status = Lens.lens (\ExecutionListItem' {status} -> status) (\s@ExecutionListItem' {} a -> s {status = a} :: ExecutionListItem)

-- | The date the execution started.
executionListItem_startDate :: Lens.Lens' ExecutionListItem Prelude.UTCTime
executionListItem_startDate = Lens.lens (\ExecutionListItem' {startDate} -> startDate) (\s@ExecutionListItem' {} a -> s {startDate = a} :: ExecutionListItem) Prelude.. Prelude._Time

instance Prelude.FromJSON ExecutionListItem where
  parseJSON =
    Prelude.withObject
      "ExecutionListItem"
      ( \x ->
          ExecutionListItem'
            Prelude.<$> (x Prelude..:? "stopDate")
            Prelude.<*> (x Prelude..: "executionArn")
            Prelude.<*> (x Prelude..: "stateMachineArn")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "status")
            Prelude.<*> (x Prelude..: "startDate")
      )

instance Prelude.Hashable ExecutionListItem

instance Prelude.NFData ExecutionListItem
