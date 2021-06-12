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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ManagedAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ManagedAction where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.ActionStatus
import Network.AWS.ElasticBeanstalk.Types.ActionType
import qualified Network.AWS.Lens as Lens

-- | The record of an upcoming or in-progress managed action.
--
-- /See:/ 'newManagedAction' smart constructor.
data ManagedAction = ManagedAction'
  { -- | The status of the managed action. If the action is @Scheduled@, you can
    -- apply it immediately with ApplyEnvironmentManagedAction.
    status :: Core.Maybe ActionStatus,
    -- | The type of managed action.
    actionType :: Core.Maybe ActionType,
    -- | A unique identifier for the managed action.
    actionId :: Core.Maybe Core.Text,
    -- | A description of the managed action.
    actionDescription :: Core.Maybe Core.Text,
    -- | The start time of the maintenance window in which the managed action
    -- will execute.
    windowStartTime :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'actionType', 'managedAction_actionType' - The type of managed action.
--
-- 'actionId', 'managedAction_actionId' - A unique identifier for the managed action.
--
-- 'actionDescription', 'managedAction_actionDescription' - A description of the managed action.
--
-- 'windowStartTime', 'managedAction_windowStartTime' - The start time of the maintenance window in which the managed action
-- will execute.
newManagedAction ::
  ManagedAction
newManagedAction =
  ManagedAction'
    { status = Core.Nothing,
      actionType = Core.Nothing,
      actionId = Core.Nothing,
      actionDescription = Core.Nothing,
      windowStartTime = Core.Nothing
    }

-- | The status of the managed action. If the action is @Scheduled@, you can
-- apply it immediately with ApplyEnvironmentManagedAction.
managedAction_status :: Lens.Lens' ManagedAction (Core.Maybe ActionStatus)
managedAction_status = Lens.lens (\ManagedAction' {status} -> status) (\s@ManagedAction' {} a -> s {status = a} :: ManagedAction)

-- | The type of managed action.
managedAction_actionType :: Lens.Lens' ManagedAction (Core.Maybe ActionType)
managedAction_actionType = Lens.lens (\ManagedAction' {actionType} -> actionType) (\s@ManagedAction' {} a -> s {actionType = a} :: ManagedAction)

-- | A unique identifier for the managed action.
managedAction_actionId :: Lens.Lens' ManagedAction (Core.Maybe Core.Text)
managedAction_actionId = Lens.lens (\ManagedAction' {actionId} -> actionId) (\s@ManagedAction' {} a -> s {actionId = a} :: ManagedAction)

-- | A description of the managed action.
managedAction_actionDescription :: Lens.Lens' ManagedAction (Core.Maybe Core.Text)
managedAction_actionDescription = Lens.lens (\ManagedAction' {actionDescription} -> actionDescription) (\s@ManagedAction' {} a -> s {actionDescription = a} :: ManagedAction)

-- | The start time of the maintenance window in which the managed action
-- will execute.
managedAction_windowStartTime :: Lens.Lens' ManagedAction (Core.Maybe Core.UTCTime)
managedAction_windowStartTime = Lens.lens (\ManagedAction' {windowStartTime} -> windowStartTime) (\s@ManagedAction' {} a -> s {windowStartTime = a} :: ManagedAction) Core.. Lens.mapping Core._Time

instance Core.FromXML ManagedAction where
  parseXML x =
    ManagedAction'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "ActionType")
      Core.<*> (x Core..@? "ActionId")
      Core.<*> (x Core..@? "ActionDescription")
      Core.<*> (x Core..@? "WindowStartTime")

instance Core.Hashable ManagedAction

instance Core.NFData ManagedAction
