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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | A description of the managed action.
    actionDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the managed action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The type of managed action.
    actionType :: Prelude.Maybe ActionType,
    -- | The status of the managed action. If the action is @Scheduled@, you can
    -- apply it immediately with ApplyEnvironmentManagedAction.
    status :: Prelude.Maybe ActionStatus,
    -- | The start time of the maintenance window in which the managed action
    -- will execute.
    windowStartTime :: Prelude.Maybe Data.ISO8601
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
-- 'actionDescription', 'managedAction_actionDescription' - A description of the managed action.
--
-- 'actionId', 'managedAction_actionId' - A unique identifier for the managed action.
--
-- 'actionType', 'managedAction_actionType' - The type of managed action.
--
-- 'status', 'managedAction_status' - The status of the managed action. If the action is @Scheduled@, you can
-- apply it immediately with ApplyEnvironmentManagedAction.
--
-- 'windowStartTime', 'managedAction_windowStartTime' - The start time of the maintenance window in which the managed action
-- will execute.
newManagedAction ::
  ManagedAction
newManagedAction =
  ManagedAction'
    { actionDescription = Prelude.Nothing,
      actionId = Prelude.Nothing,
      actionType = Prelude.Nothing,
      status = Prelude.Nothing,
      windowStartTime = Prelude.Nothing
    }

-- | A description of the managed action.
managedAction_actionDescription :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.Text)
managedAction_actionDescription = Lens.lens (\ManagedAction' {actionDescription} -> actionDescription) (\s@ManagedAction' {} a -> s {actionDescription = a} :: ManagedAction)

-- | A unique identifier for the managed action.
managedAction_actionId :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.Text)
managedAction_actionId = Lens.lens (\ManagedAction' {actionId} -> actionId) (\s@ManagedAction' {} a -> s {actionId = a} :: ManagedAction)

-- | The type of managed action.
managedAction_actionType :: Lens.Lens' ManagedAction (Prelude.Maybe ActionType)
managedAction_actionType = Lens.lens (\ManagedAction' {actionType} -> actionType) (\s@ManagedAction' {} a -> s {actionType = a} :: ManagedAction)

-- | The status of the managed action. If the action is @Scheduled@, you can
-- apply it immediately with ApplyEnvironmentManagedAction.
managedAction_status :: Lens.Lens' ManagedAction (Prelude.Maybe ActionStatus)
managedAction_status = Lens.lens (\ManagedAction' {status} -> status) (\s@ManagedAction' {} a -> s {status = a} :: ManagedAction)

-- | The start time of the maintenance window in which the managed action
-- will execute.
managedAction_windowStartTime :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.UTCTime)
managedAction_windowStartTime = Lens.lens (\ManagedAction' {windowStartTime} -> windowStartTime) (\s@ManagedAction' {} a -> s {windowStartTime = a} :: ManagedAction) Prelude.. Lens.mapping Data._Time

instance Data.FromXML ManagedAction where
  parseXML x =
    ManagedAction'
      Prelude.<$> (x Data..@? "ActionDescription")
      Prelude.<*> (x Data..@? "ActionId")
      Prelude.<*> (x Data..@? "ActionType")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "WindowStartTime")

instance Prelude.Hashable ManagedAction where
  hashWithSalt _salt ManagedAction' {..} =
    _salt
      `Prelude.hashWithSalt` actionDescription
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` windowStartTime

instance Prelude.NFData ManagedAction where
  rnf ManagedAction' {..} =
    Prelude.rnf actionDescription
      `Prelude.seq` Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf windowStartTime
