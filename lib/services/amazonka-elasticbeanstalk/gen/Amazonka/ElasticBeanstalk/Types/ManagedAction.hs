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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ManagedAction where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types.ActionStatus
import Amazonka.ElasticBeanstalk.Types.ActionType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The record of an upcoming or in-progress managed action.
--
-- /See:/ 'newManagedAction' smart constructor.
data ManagedAction = ManagedAction'
  { -- | The status of the managed action. If the action is @Scheduled@, you can
    -- apply it immediately with ApplyEnvironmentManagedAction.
    status :: Prelude.Maybe ActionStatus,
    -- | A unique identifier for the managed action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The start time of the maintenance window in which the managed action
    -- will execute.
    windowStartTime :: Prelude.Maybe Core.ISO8601,
    -- | A description of the managed action.
    actionDescription :: Prelude.Maybe Prelude.Text,
    -- | The type of managed action.
    actionType :: Prelude.Maybe ActionType
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
-- 'status', 'managedAction_status' - The status of the managed action. If the action is @Scheduled@, you can
-- apply it immediately with ApplyEnvironmentManagedAction.
--
-- 'actionId', 'managedAction_actionId' - A unique identifier for the managed action.
--
-- 'windowStartTime', 'managedAction_windowStartTime' - The start time of the maintenance window in which the managed action
-- will execute.
--
-- 'actionDescription', 'managedAction_actionDescription' - A description of the managed action.
--
-- 'actionType', 'managedAction_actionType' - The type of managed action.
newManagedAction ::
  ManagedAction
newManagedAction =
  ManagedAction'
    { status = Prelude.Nothing,
      actionId = Prelude.Nothing,
      windowStartTime = Prelude.Nothing,
      actionDescription = Prelude.Nothing,
      actionType = Prelude.Nothing
    }

-- | The status of the managed action. If the action is @Scheduled@, you can
-- apply it immediately with ApplyEnvironmentManagedAction.
managedAction_status :: Lens.Lens' ManagedAction (Prelude.Maybe ActionStatus)
managedAction_status = Lens.lens (\ManagedAction' {status} -> status) (\s@ManagedAction' {} a -> s {status = a} :: ManagedAction)

-- | A unique identifier for the managed action.
managedAction_actionId :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.Text)
managedAction_actionId = Lens.lens (\ManagedAction' {actionId} -> actionId) (\s@ManagedAction' {} a -> s {actionId = a} :: ManagedAction)

-- | The start time of the maintenance window in which the managed action
-- will execute.
managedAction_windowStartTime :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.UTCTime)
managedAction_windowStartTime = Lens.lens (\ManagedAction' {windowStartTime} -> windowStartTime) (\s@ManagedAction' {} a -> s {windowStartTime = a} :: ManagedAction) Prelude.. Lens.mapping Core._Time

-- | A description of the managed action.
managedAction_actionDescription :: Lens.Lens' ManagedAction (Prelude.Maybe Prelude.Text)
managedAction_actionDescription = Lens.lens (\ManagedAction' {actionDescription} -> actionDescription) (\s@ManagedAction' {} a -> s {actionDescription = a} :: ManagedAction)

-- | The type of managed action.
managedAction_actionType :: Lens.Lens' ManagedAction (Prelude.Maybe ActionType)
managedAction_actionType = Lens.lens (\ManagedAction' {actionType} -> actionType) (\s@ManagedAction' {} a -> s {actionType = a} :: ManagedAction)

instance Core.FromXML ManagedAction where
  parseXML x =
    ManagedAction'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "ActionId")
      Prelude.<*> (x Core..@? "WindowStartTime")
      Prelude.<*> (x Core..@? "ActionDescription")
      Prelude.<*> (x Core..@? "ActionType")

instance Prelude.Hashable ManagedAction where
  hashWithSalt _salt ManagedAction' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` windowStartTime
      `Prelude.hashWithSalt` actionDescription
      `Prelude.hashWithSalt` actionType

instance Prelude.NFData ManagedAction where
  rnf ManagedAction' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf windowStartTime
      `Prelude.seq` Prelude.rnf actionDescription
      `Prelude.seq` Prelude.rnf actionType
