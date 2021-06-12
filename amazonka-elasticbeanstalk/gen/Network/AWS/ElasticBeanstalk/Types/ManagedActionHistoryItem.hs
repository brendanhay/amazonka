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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
import Network.AWS.ElasticBeanstalk.Types.ActionType
import Network.AWS.ElasticBeanstalk.Types.FailureType
import qualified Network.AWS.Lens as Lens

-- | The record of a completed or failed managed action.
--
-- /See:/ 'newManagedActionHistoryItem' smart constructor.
data ManagedActionHistoryItem = ManagedActionHistoryItem'
  { -- | The status of the action.
    status :: Core.Maybe ActionHistoryStatus,
    -- | The type of the managed action.
    actionType :: Core.Maybe ActionType,
    -- | The date and time that the action started executing.
    executedTime :: Core.Maybe Core.ISO8601,
    -- | A unique identifier for the managed action.
    actionId :: Core.Maybe Core.Text,
    -- | A description of the managed action.
    actionDescription :: Core.Maybe Core.Text,
    -- | The date and time that the action finished executing.
    finishedTime :: Core.Maybe Core.ISO8601,
    -- | If the action failed, a description of the failure.
    failureDescription :: Core.Maybe Core.Text,
    -- | If the action failed, the type of failure.
    failureType :: Core.Maybe FailureType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ManagedActionHistoryItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'managedActionHistoryItem_status' - The status of the action.
--
-- 'actionType', 'managedActionHistoryItem_actionType' - The type of the managed action.
--
-- 'executedTime', 'managedActionHistoryItem_executedTime' - The date and time that the action started executing.
--
-- 'actionId', 'managedActionHistoryItem_actionId' - A unique identifier for the managed action.
--
-- 'actionDescription', 'managedActionHistoryItem_actionDescription' - A description of the managed action.
--
-- 'finishedTime', 'managedActionHistoryItem_finishedTime' - The date and time that the action finished executing.
--
-- 'failureDescription', 'managedActionHistoryItem_failureDescription' - If the action failed, a description of the failure.
--
-- 'failureType', 'managedActionHistoryItem_failureType' - If the action failed, the type of failure.
newManagedActionHistoryItem ::
  ManagedActionHistoryItem
newManagedActionHistoryItem =
  ManagedActionHistoryItem'
    { status = Core.Nothing,
      actionType = Core.Nothing,
      executedTime = Core.Nothing,
      actionId = Core.Nothing,
      actionDescription = Core.Nothing,
      finishedTime = Core.Nothing,
      failureDescription = Core.Nothing,
      failureType = Core.Nothing
    }

-- | The status of the action.
managedActionHistoryItem_status :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe ActionHistoryStatus)
managedActionHistoryItem_status = Lens.lens (\ManagedActionHistoryItem' {status} -> status) (\s@ManagedActionHistoryItem' {} a -> s {status = a} :: ManagedActionHistoryItem)

-- | The type of the managed action.
managedActionHistoryItem_actionType :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe ActionType)
managedActionHistoryItem_actionType = Lens.lens (\ManagedActionHistoryItem' {actionType} -> actionType) (\s@ManagedActionHistoryItem' {} a -> s {actionType = a} :: ManagedActionHistoryItem)

-- | The date and time that the action started executing.
managedActionHistoryItem_executedTime :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.UTCTime)
managedActionHistoryItem_executedTime = Lens.lens (\ManagedActionHistoryItem' {executedTime} -> executedTime) (\s@ManagedActionHistoryItem' {} a -> s {executedTime = a} :: ManagedActionHistoryItem) Core.. Lens.mapping Core._Time

-- | A unique identifier for the managed action.
managedActionHistoryItem_actionId :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.Text)
managedActionHistoryItem_actionId = Lens.lens (\ManagedActionHistoryItem' {actionId} -> actionId) (\s@ManagedActionHistoryItem' {} a -> s {actionId = a} :: ManagedActionHistoryItem)

-- | A description of the managed action.
managedActionHistoryItem_actionDescription :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.Text)
managedActionHistoryItem_actionDescription = Lens.lens (\ManagedActionHistoryItem' {actionDescription} -> actionDescription) (\s@ManagedActionHistoryItem' {} a -> s {actionDescription = a} :: ManagedActionHistoryItem)

-- | The date and time that the action finished executing.
managedActionHistoryItem_finishedTime :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.UTCTime)
managedActionHistoryItem_finishedTime = Lens.lens (\ManagedActionHistoryItem' {finishedTime} -> finishedTime) (\s@ManagedActionHistoryItem' {} a -> s {finishedTime = a} :: ManagedActionHistoryItem) Core.. Lens.mapping Core._Time

-- | If the action failed, a description of the failure.
managedActionHistoryItem_failureDescription :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.Text)
managedActionHistoryItem_failureDescription = Lens.lens (\ManagedActionHistoryItem' {failureDescription} -> failureDescription) (\s@ManagedActionHistoryItem' {} a -> s {failureDescription = a} :: ManagedActionHistoryItem)

-- | If the action failed, the type of failure.
managedActionHistoryItem_failureType :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe FailureType)
managedActionHistoryItem_failureType = Lens.lens (\ManagedActionHistoryItem' {failureType} -> failureType) (\s@ManagedActionHistoryItem' {} a -> s {failureType = a} :: ManagedActionHistoryItem)

instance Core.FromXML ManagedActionHistoryItem where
  parseXML x =
    ManagedActionHistoryItem'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "ActionType")
      Core.<*> (x Core..@? "ExecutedTime")
      Core.<*> (x Core..@? "ActionId")
      Core.<*> (x Core..@? "ActionDescription")
      Core.<*> (x Core..@? "FinishedTime")
      Core.<*> (x Core..@? "FailureDescription")
      Core.<*> (x Core..@? "FailureType")

instance Core.Hashable ManagedActionHistoryItem

instance Core.NFData ManagedActionHistoryItem
