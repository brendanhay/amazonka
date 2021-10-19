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
import qualified Network.AWS.Prelude as Prelude

-- | The record of a completed or failed managed action.
--
-- /See:/ 'newManagedActionHistoryItem' smart constructor.
data ManagedActionHistoryItem = ManagedActionHistoryItem'
  { -- | The status of the action.
    status :: Prelude.Maybe ActionHistoryStatus,
    -- | If the action failed, the type of failure.
    failureType :: Prelude.Maybe FailureType,
    -- | A unique identifier for the managed action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | If the action failed, a description of the failure.
    failureDescription :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the action finished executing.
    finishedTime :: Prelude.Maybe Core.ISO8601,
    -- | A description of the managed action.
    actionDescription :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the action started executing.
    executedTime :: Prelude.Maybe Core.ISO8601,
    -- | The type of the managed action.
    actionType :: Prelude.Maybe ActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'failureType', 'managedActionHistoryItem_failureType' - If the action failed, the type of failure.
--
-- 'actionId', 'managedActionHistoryItem_actionId' - A unique identifier for the managed action.
--
-- 'failureDescription', 'managedActionHistoryItem_failureDescription' - If the action failed, a description of the failure.
--
-- 'finishedTime', 'managedActionHistoryItem_finishedTime' - The date and time that the action finished executing.
--
-- 'actionDescription', 'managedActionHistoryItem_actionDescription' - A description of the managed action.
--
-- 'executedTime', 'managedActionHistoryItem_executedTime' - The date and time that the action started executing.
--
-- 'actionType', 'managedActionHistoryItem_actionType' - The type of the managed action.
newManagedActionHistoryItem ::
  ManagedActionHistoryItem
newManagedActionHistoryItem =
  ManagedActionHistoryItem'
    { status = Prelude.Nothing,
      failureType = Prelude.Nothing,
      actionId = Prelude.Nothing,
      failureDescription = Prelude.Nothing,
      finishedTime = Prelude.Nothing,
      actionDescription = Prelude.Nothing,
      executedTime = Prelude.Nothing,
      actionType = Prelude.Nothing
    }

-- | The status of the action.
managedActionHistoryItem_status :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe ActionHistoryStatus)
managedActionHistoryItem_status = Lens.lens (\ManagedActionHistoryItem' {status} -> status) (\s@ManagedActionHistoryItem' {} a -> s {status = a} :: ManagedActionHistoryItem)

-- | If the action failed, the type of failure.
managedActionHistoryItem_failureType :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe FailureType)
managedActionHistoryItem_failureType = Lens.lens (\ManagedActionHistoryItem' {failureType} -> failureType) (\s@ManagedActionHistoryItem' {} a -> s {failureType = a} :: ManagedActionHistoryItem)

-- | A unique identifier for the managed action.
managedActionHistoryItem_actionId :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.Text)
managedActionHistoryItem_actionId = Lens.lens (\ManagedActionHistoryItem' {actionId} -> actionId) (\s@ManagedActionHistoryItem' {} a -> s {actionId = a} :: ManagedActionHistoryItem)

-- | If the action failed, a description of the failure.
managedActionHistoryItem_failureDescription :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.Text)
managedActionHistoryItem_failureDescription = Lens.lens (\ManagedActionHistoryItem' {failureDescription} -> failureDescription) (\s@ManagedActionHistoryItem' {} a -> s {failureDescription = a} :: ManagedActionHistoryItem)

-- | The date and time that the action finished executing.
managedActionHistoryItem_finishedTime :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.UTCTime)
managedActionHistoryItem_finishedTime = Lens.lens (\ManagedActionHistoryItem' {finishedTime} -> finishedTime) (\s@ManagedActionHistoryItem' {} a -> s {finishedTime = a} :: ManagedActionHistoryItem) Prelude.. Lens.mapping Core._Time

-- | A description of the managed action.
managedActionHistoryItem_actionDescription :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.Text)
managedActionHistoryItem_actionDescription = Lens.lens (\ManagedActionHistoryItem' {actionDescription} -> actionDescription) (\s@ManagedActionHistoryItem' {} a -> s {actionDescription = a} :: ManagedActionHistoryItem)

-- | The date and time that the action started executing.
managedActionHistoryItem_executedTime :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.UTCTime)
managedActionHistoryItem_executedTime = Lens.lens (\ManagedActionHistoryItem' {executedTime} -> executedTime) (\s@ManagedActionHistoryItem' {} a -> s {executedTime = a} :: ManagedActionHistoryItem) Prelude.. Lens.mapping Core._Time

-- | The type of the managed action.
managedActionHistoryItem_actionType :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe ActionType)
managedActionHistoryItem_actionType = Lens.lens (\ManagedActionHistoryItem' {actionType} -> actionType) (\s@ManagedActionHistoryItem' {} a -> s {actionType = a} :: ManagedActionHistoryItem)

instance Core.FromXML ManagedActionHistoryItem where
  parseXML x =
    ManagedActionHistoryItem'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "FailureType")
      Prelude.<*> (x Core..@? "ActionId")
      Prelude.<*> (x Core..@? "FailureDescription")
      Prelude.<*> (x Core..@? "FinishedTime")
      Prelude.<*> (x Core..@? "ActionDescription")
      Prelude.<*> (x Core..@? "ExecutedTime")
      Prelude.<*> (x Core..@? "ActionType")

instance Prelude.Hashable ManagedActionHistoryItem

instance Prelude.NFData ManagedActionHistoryItem
