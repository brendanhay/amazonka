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
-- Module      : Amazonka.ElasticBeanstalk.Types.ManagedActionHistoryItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ManagedActionHistoryItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ActionHistoryStatus
import Amazonka.ElasticBeanstalk.Types.ActionType
import Amazonka.ElasticBeanstalk.Types.FailureType
import qualified Amazonka.Prelude as Prelude

-- | The record of a completed or failed managed action.
--
-- /See:/ 'newManagedActionHistoryItem' smart constructor.
data ManagedActionHistoryItem = ManagedActionHistoryItem'
  { -- | A description of the managed action.
    actionDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the managed action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The type of the managed action.
    actionType :: Prelude.Maybe ActionType,
    -- | The date and time that the action started executing.
    executedTime :: Prelude.Maybe Data.ISO8601,
    -- | If the action failed, a description of the failure.
    failureDescription :: Prelude.Maybe Prelude.Text,
    -- | If the action failed, the type of failure.
    failureType :: Prelude.Maybe FailureType,
    -- | The date and time that the action finished executing.
    finishedTime :: Prelude.Maybe Data.ISO8601,
    -- | The status of the action.
    status :: Prelude.Maybe ActionHistoryStatus
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
-- 'actionDescription', 'managedActionHistoryItem_actionDescription' - A description of the managed action.
--
-- 'actionId', 'managedActionHistoryItem_actionId' - A unique identifier for the managed action.
--
-- 'actionType', 'managedActionHistoryItem_actionType' - The type of the managed action.
--
-- 'executedTime', 'managedActionHistoryItem_executedTime' - The date and time that the action started executing.
--
-- 'failureDescription', 'managedActionHistoryItem_failureDescription' - If the action failed, a description of the failure.
--
-- 'failureType', 'managedActionHistoryItem_failureType' - If the action failed, the type of failure.
--
-- 'finishedTime', 'managedActionHistoryItem_finishedTime' - The date and time that the action finished executing.
--
-- 'status', 'managedActionHistoryItem_status' - The status of the action.
newManagedActionHistoryItem ::
  ManagedActionHistoryItem
newManagedActionHistoryItem =
  ManagedActionHistoryItem'
    { actionDescription =
        Prelude.Nothing,
      actionId = Prelude.Nothing,
      actionType = Prelude.Nothing,
      executedTime = Prelude.Nothing,
      failureDescription = Prelude.Nothing,
      failureType = Prelude.Nothing,
      finishedTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | A description of the managed action.
managedActionHistoryItem_actionDescription :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.Text)
managedActionHistoryItem_actionDescription = Lens.lens (\ManagedActionHistoryItem' {actionDescription} -> actionDescription) (\s@ManagedActionHistoryItem' {} a -> s {actionDescription = a} :: ManagedActionHistoryItem)

-- | A unique identifier for the managed action.
managedActionHistoryItem_actionId :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.Text)
managedActionHistoryItem_actionId = Lens.lens (\ManagedActionHistoryItem' {actionId} -> actionId) (\s@ManagedActionHistoryItem' {} a -> s {actionId = a} :: ManagedActionHistoryItem)

-- | The type of the managed action.
managedActionHistoryItem_actionType :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe ActionType)
managedActionHistoryItem_actionType = Lens.lens (\ManagedActionHistoryItem' {actionType} -> actionType) (\s@ManagedActionHistoryItem' {} a -> s {actionType = a} :: ManagedActionHistoryItem)

-- | The date and time that the action started executing.
managedActionHistoryItem_executedTime :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.UTCTime)
managedActionHistoryItem_executedTime = Lens.lens (\ManagedActionHistoryItem' {executedTime} -> executedTime) (\s@ManagedActionHistoryItem' {} a -> s {executedTime = a} :: ManagedActionHistoryItem) Prelude.. Lens.mapping Data._Time

-- | If the action failed, a description of the failure.
managedActionHistoryItem_failureDescription :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.Text)
managedActionHistoryItem_failureDescription = Lens.lens (\ManagedActionHistoryItem' {failureDescription} -> failureDescription) (\s@ManagedActionHistoryItem' {} a -> s {failureDescription = a} :: ManagedActionHistoryItem)

-- | If the action failed, the type of failure.
managedActionHistoryItem_failureType :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe FailureType)
managedActionHistoryItem_failureType = Lens.lens (\ManagedActionHistoryItem' {failureType} -> failureType) (\s@ManagedActionHistoryItem' {} a -> s {failureType = a} :: ManagedActionHistoryItem)

-- | The date and time that the action finished executing.
managedActionHistoryItem_finishedTime :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe Prelude.UTCTime)
managedActionHistoryItem_finishedTime = Lens.lens (\ManagedActionHistoryItem' {finishedTime} -> finishedTime) (\s@ManagedActionHistoryItem' {} a -> s {finishedTime = a} :: ManagedActionHistoryItem) Prelude.. Lens.mapping Data._Time

-- | The status of the action.
managedActionHistoryItem_status :: Lens.Lens' ManagedActionHistoryItem (Prelude.Maybe ActionHistoryStatus)
managedActionHistoryItem_status = Lens.lens (\ManagedActionHistoryItem' {status} -> status) (\s@ManagedActionHistoryItem' {} a -> s {status = a} :: ManagedActionHistoryItem)

instance Data.FromXML ManagedActionHistoryItem where
  parseXML x =
    ManagedActionHistoryItem'
      Prelude.<$> (x Data..@? "ActionDescription")
      Prelude.<*> (x Data..@? "ActionId")
      Prelude.<*> (x Data..@? "ActionType")
      Prelude.<*> (x Data..@? "ExecutedTime")
      Prelude.<*> (x Data..@? "FailureDescription")
      Prelude.<*> (x Data..@? "FailureType")
      Prelude.<*> (x Data..@? "FinishedTime")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable ManagedActionHistoryItem where
  hashWithSalt _salt ManagedActionHistoryItem' {..} =
    _salt
      `Prelude.hashWithSalt` actionDescription
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` executedTime
      `Prelude.hashWithSalt` failureDescription
      `Prelude.hashWithSalt` failureType
      `Prelude.hashWithSalt` finishedTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData ManagedActionHistoryItem where
  rnf ManagedActionHistoryItem' {..} =
    Prelude.rnf actionDescription `Prelude.seq`
      Prelude.rnf actionId `Prelude.seq`
        Prelude.rnf actionType `Prelude.seq`
          Prelude.rnf executedTime `Prelude.seq`
            Prelude.rnf failureDescription `Prelude.seq`
              Prelude.rnf failureType `Prelude.seq`
                Prelude.rnf finishedTime `Prelude.seq`
                  Prelude.rnf status
