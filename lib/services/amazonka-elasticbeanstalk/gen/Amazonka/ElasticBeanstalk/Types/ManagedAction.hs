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
-- Module      : Amazonka.ElasticBeanstalk.Types.ManagedAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ManagedAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ActionStatus
import Amazonka.ElasticBeanstalk.Types.ActionType
import qualified Amazonka.Prelude as Prelude

-- | The record of an upcoming or in-progress managed action.
--
-- /See:/ 'newManagedAction' smart constructor.
data ManagedAction = ManagedAction'
  { -- | The type of managed action.
    actionType :: Prelude.Maybe ActionType,
    -- | The start time of the maintenance window in which the managed action
    -- will execute.
    windowStartTime :: Prelude.Maybe Data.ISO8601,
    -- | The status of the managed action. If the action is @Scheduled@, you can
    -- apply it immediately with ApplyEnvironmentManagedAction.
    status :: Prelude.Maybe ActionStatus,
    -- | A unique identifier for the managed action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | A description of the managed action.
    actionDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'managedAction_actionType' - The type of managed action.
--
-- 'windowStartTime', 'managedAction_windowStartTime' - The start time of the maintenance window in which the managed action
-- will execute.
--
-- 'status', 'managedAction_status' - The status of the managed action. If the action is @Scheduled@, you can
-- apply it immediately with ApplyEnvironmentManagedAction.
--
-- 'actionId', 'managedAction_actionId' - A unique identifier for the managed action.
--
-- 'actionDescription', 'managedAction_actionDescription' - A description of the managed action.
newManagedAction ::
  ManagedAction
newManagedAction =
  ManagedAction'
    { actionType = Prelude.Nothing,
      windowStartTime = Prelude.Nothing,
      status = Prelude.Nothing,
      actionId = Prelude.Nothing,
      actionDescription = Prelude.Nothing
    }

-- | The type of managed action.
managedAction_actionType :: Lens.Lens' ManagedAction (Prelude.Maybe ActionType)
managedAction_actionType = Lens.lens (\ManagedAction' {actionType} -> actionType) (\s@ManagedAction' {} a -> s {actionType = a} :: ManagedAction)

-- | The start time of the maintenance window in which the managed action
-- will execute.
managedAction_windowStartTime :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.UTCTime)
managedAction_windowStartTime = Lens.lens (\ManagedAction' {windowStartTime} -> windowStartTime) (\s@ManagedAction' {} a -> s {windowStartTime = a} :: ManagedAction) Prelude.. Lens.mapping Data._Time

-- | The status of the managed action. If the action is @Scheduled@, you can
-- apply it immediately with ApplyEnvironmentManagedAction.
managedAction_status :: Lens.Lens' ManagedAction (Prelude.Maybe ActionStatus)
managedAction_status = Lens.lens (\ManagedAction' {status} -> status) (\s@ManagedAction' {} a -> s {status = a} :: ManagedAction)

-- | A unique identifier for the managed action.
managedAction_actionId :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.Text)
managedAction_actionId = Lens.lens (\ManagedAction' {actionId} -> actionId) (\s@ManagedAction' {} a -> s {actionId = a} :: ManagedAction)

-- | A description of the managed action.
managedAction_actionDescription :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.Text)
managedAction_actionDescription = Lens.lens (\ManagedAction' {actionDescription} -> actionDescription) (\s@ManagedAction' {} a -> s {actionDescription = a} :: ManagedAction)

instance Data.FromXML ManagedAction where
  parseXML x =
    ManagedAction'
      Prelude.<$> (x Data..@? "ActionType")
      Prelude.<*> (x Data..@? "WindowStartTime")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "ActionId")
      Prelude.<*> (x Data..@? "ActionDescription")

instance Prelude.Hashable ManagedAction where
  hashWithSalt _salt ManagedAction' {..} =
    _salt `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` windowStartTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` actionDescription

instance Prelude.NFData ManagedAction where
  rnf ManagedAction' {..} =
    Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf windowStartTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf actionDescription
