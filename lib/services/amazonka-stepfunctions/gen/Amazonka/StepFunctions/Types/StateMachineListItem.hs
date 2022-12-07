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
-- Module      : Amazonka.StepFunctions.Types.StateMachineListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.StateMachineListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.StateMachineType

-- | Contains details about the state machine.
--
-- /See:/ 'newStateMachineListItem' smart constructor.
data StateMachineListItem = StateMachineListItem'
  { -- | The Amazon Resource Name (ARN) that identifies the state machine.
    stateMachineArn :: Prelude.Text,
    -- | The name of the state machine.
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
    type' :: StateMachineType,
    -- | The date the state machine is created.
    creationDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StateMachineListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineArn', 'stateMachineListItem_stateMachineArn' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- 'name', 'stateMachineListItem_name' - The name of the state machine.
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
-- 'type'', 'stateMachineListItem_type' -
--
-- 'creationDate', 'stateMachineListItem_creationDate' - The date the state machine is created.
newStateMachineListItem ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  StateMachineType ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  StateMachineListItem
newStateMachineListItem
  pStateMachineArn_
  pName_
  pType_
  pCreationDate_ =
    StateMachineListItem'
      { stateMachineArn =
          pStateMachineArn_,
        name = pName_,
        type' = pType_,
        creationDate = Data._Time Lens.# pCreationDate_
      }

-- | The Amazon Resource Name (ARN) that identifies the state machine.
stateMachineListItem_stateMachineArn :: Lens.Lens' StateMachineListItem Prelude.Text
stateMachineListItem_stateMachineArn = Lens.lens (\StateMachineListItem' {stateMachineArn} -> stateMachineArn) (\s@StateMachineListItem' {} a -> s {stateMachineArn = a} :: StateMachineListItem)

-- | The name of the state machine.
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
stateMachineListItem_name :: Lens.Lens' StateMachineListItem Prelude.Text
stateMachineListItem_name = Lens.lens (\StateMachineListItem' {name} -> name) (\s@StateMachineListItem' {} a -> s {name = a} :: StateMachineListItem)

-- |
stateMachineListItem_type :: Lens.Lens' StateMachineListItem StateMachineType
stateMachineListItem_type = Lens.lens (\StateMachineListItem' {type'} -> type') (\s@StateMachineListItem' {} a -> s {type' = a} :: StateMachineListItem)

-- | The date the state machine is created.
stateMachineListItem_creationDate :: Lens.Lens' StateMachineListItem Prelude.UTCTime
stateMachineListItem_creationDate = Lens.lens (\StateMachineListItem' {creationDate} -> creationDate) (\s@StateMachineListItem' {} a -> s {creationDate = a} :: StateMachineListItem) Prelude.. Data._Time

instance Data.FromJSON StateMachineListItem where
  parseJSON =
    Data.withObject
      "StateMachineListItem"
      ( \x ->
          StateMachineListItem'
            Prelude.<$> (x Data..: "stateMachineArn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "creationDate")
      )

instance Prelude.Hashable StateMachineListItem where
  hashWithSalt _salt StateMachineListItem' {..} =
    _salt `Prelude.hashWithSalt` stateMachineArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData StateMachineListItem where
  rnf StateMachineListItem' {..} =
    Prelude.rnf stateMachineArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf creationDate
