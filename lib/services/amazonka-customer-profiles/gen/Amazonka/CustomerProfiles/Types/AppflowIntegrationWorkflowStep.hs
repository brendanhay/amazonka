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
-- Module      : Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | Workflow step details for @APPFLOW_INTEGRATION@ workflow.
--
-- /See:/ 'newAppflowIntegrationWorkflowStep' smart constructor.
data AppflowIntegrationWorkflowStep = AppflowIntegrationWorkflowStep'
  { -- | Name of the flow created during execution of workflow step.
    -- @APPFLOW_INTEGRATION@ workflow type creates an appflow flow during
    -- workflow step execution on the customers behalf.
    flowName :: Prelude.Text,
    -- | Workflow step status for @APPFLOW_INTEGRATION@ workflow.
    status :: Status,
    -- | Message indicating execution of workflow step for @APPFLOW_INTEGRATION@
    -- workflow.
    executionMessage :: Prelude.Text,
    -- | Total number of records processed during execution of workflow step for
    -- @APPFLOW_INTEGRATION@ workflow.
    recordsProcessed :: Prelude.Integer,
    -- | Start datetime of records pulled in batch during execution of workflow
    -- step for @APPFLOW_INTEGRATION@ workflow.
    batchRecordsStartTime :: Prelude.Text,
    -- | End datetime of records pulled in batch during execution of workflow
    -- step for @APPFLOW_INTEGRATION@ workflow.
    batchRecordsEndTime :: Prelude.Text,
    -- | Creation timestamp of workflow step for @APPFLOW_INTEGRATION@ workflow.
    createdAt :: Core.POSIX,
    -- | Last updated timestamp for workflow step for @APPFLOW_INTEGRATION@
    -- workflow.
    lastUpdatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppflowIntegrationWorkflowStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowName', 'appflowIntegrationWorkflowStep_flowName' - Name of the flow created during execution of workflow step.
-- @APPFLOW_INTEGRATION@ workflow type creates an appflow flow during
-- workflow step execution on the customers behalf.
--
-- 'status', 'appflowIntegrationWorkflowStep_status' - Workflow step status for @APPFLOW_INTEGRATION@ workflow.
--
-- 'executionMessage', 'appflowIntegrationWorkflowStep_executionMessage' - Message indicating execution of workflow step for @APPFLOW_INTEGRATION@
-- workflow.
--
-- 'recordsProcessed', 'appflowIntegrationWorkflowStep_recordsProcessed' - Total number of records processed during execution of workflow step for
-- @APPFLOW_INTEGRATION@ workflow.
--
-- 'batchRecordsStartTime', 'appflowIntegrationWorkflowStep_batchRecordsStartTime' - Start datetime of records pulled in batch during execution of workflow
-- step for @APPFLOW_INTEGRATION@ workflow.
--
-- 'batchRecordsEndTime', 'appflowIntegrationWorkflowStep_batchRecordsEndTime' - End datetime of records pulled in batch during execution of workflow
-- step for @APPFLOW_INTEGRATION@ workflow.
--
-- 'createdAt', 'appflowIntegrationWorkflowStep_createdAt' - Creation timestamp of workflow step for @APPFLOW_INTEGRATION@ workflow.
--
-- 'lastUpdatedAt', 'appflowIntegrationWorkflowStep_lastUpdatedAt' - Last updated timestamp for workflow step for @APPFLOW_INTEGRATION@
-- workflow.
newAppflowIntegrationWorkflowStep ::
  -- | 'flowName'
  Prelude.Text ->
  -- | 'status'
  Status ->
  -- | 'executionMessage'
  Prelude.Text ->
  -- | 'recordsProcessed'
  Prelude.Integer ->
  -- | 'batchRecordsStartTime'
  Prelude.Text ->
  -- | 'batchRecordsEndTime'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  AppflowIntegrationWorkflowStep
newAppflowIntegrationWorkflowStep
  pFlowName_
  pStatus_
  pExecutionMessage_
  pRecordsProcessed_
  pBatchRecordsStartTime_
  pBatchRecordsEndTime_
  pCreatedAt_
  pLastUpdatedAt_ =
    AppflowIntegrationWorkflowStep'
      { flowName =
          pFlowName_,
        status = pStatus_,
        executionMessage = pExecutionMessage_,
        recordsProcessed = pRecordsProcessed_,
        batchRecordsStartTime =
          pBatchRecordsStartTime_,
        batchRecordsEndTime = pBatchRecordsEndTime_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastUpdatedAt =
          Core._Time Lens.# pLastUpdatedAt_
      }

-- | Name of the flow created during execution of workflow step.
-- @APPFLOW_INTEGRATION@ workflow type creates an appflow flow during
-- workflow step execution on the customers behalf.
appflowIntegrationWorkflowStep_flowName :: Lens.Lens' AppflowIntegrationWorkflowStep Prelude.Text
appflowIntegrationWorkflowStep_flowName = Lens.lens (\AppflowIntegrationWorkflowStep' {flowName} -> flowName) (\s@AppflowIntegrationWorkflowStep' {} a -> s {flowName = a} :: AppflowIntegrationWorkflowStep)

-- | Workflow step status for @APPFLOW_INTEGRATION@ workflow.
appflowIntegrationWorkflowStep_status :: Lens.Lens' AppflowIntegrationWorkflowStep Status
appflowIntegrationWorkflowStep_status = Lens.lens (\AppflowIntegrationWorkflowStep' {status} -> status) (\s@AppflowIntegrationWorkflowStep' {} a -> s {status = a} :: AppflowIntegrationWorkflowStep)

-- | Message indicating execution of workflow step for @APPFLOW_INTEGRATION@
-- workflow.
appflowIntegrationWorkflowStep_executionMessage :: Lens.Lens' AppflowIntegrationWorkflowStep Prelude.Text
appflowIntegrationWorkflowStep_executionMessage = Lens.lens (\AppflowIntegrationWorkflowStep' {executionMessage} -> executionMessage) (\s@AppflowIntegrationWorkflowStep' {} a -> s {executionMessage = a} :: AppflowIntegrationWorkflowStep)

-- | Total number of records processed during execution of workflow step for
-- @APPFLOW_INTEGRATION@ workflow.
appflowIntegrationWorkflowStep_recordsProcessed :: Lens.Lens' AppflowIntegrationWorkflowStep Prelude.Integer
appflowIntegrationWorkflowStep_recordsProcessed = Lens.lens (\AppflowIntegrationWorkflowStep' {recordsProcessed} -> recordsProcessed) (\s@AppflowIntegrationWorkflowStep' {} a -> s {recordsProcessed = a} :: AppflowIntegrationWorkflowStep)

-- | Start datetime of records pulled in batch during execution of workflow
-- step for @APPFLOW_INTEGRATION@ workflow.
appflowIntegrationWorkflowStep_batchRecordsStartTime :: Lens.Lens' AppflowIntegrationWorkflowStep Prelude.Text
appflowIntegrationWorkflowStep_batchRecordsStartTime = Lens.lens (\AppflowIntegrationWorkflowStep' {batchRecordsStartTime} -> batchRecordsStartTime) (\s@AppflowIntegrationWorkflowStep' {} a -> s {batchRecordsStartTime = a} :: AppflowIntegrationWorkflowStep)

-- | End datetime of records pulled in batch during execution of workflow
-- step for @APPFLOW_INTEGRATION@ workflow.
appflowIntegrationWorkflowStep_batchRecordsEndTime :: Lens.Lens' AppflowIntegrationWorkflowStep Prelude.Text
appflowIntegrationWorkflowStep_batchRecordsEndTime = Lens.lens (\AppflowIntegrationWorkflowStep' {batchRecordsEndTime} -> batchRecordsEndTime) (\s@AppflowIntegrationWorkflowStep' {} a -> s {batchRecordsEndTime = a} :: AppflowIntegrationWorkflowStep)

-- | Creation timestamp of workflow step for @APPFLOW_INTEGRATION@ workflow.
appflowIntegrationWorkflowStep_createdAt :: Lens.Lens' AppflowIntegrationWorkflowStep Prelude.UTCTime
appflowIntegrationWorkflowStep_createdAt = Lens.lens (\AppflowIntegrationWorkflowStep' {createdAt} -> createdAt) (\s@AppflowIntegrationWorkflowStep' {} a -> s {createdAt = a} :: AppflowIntegrationWorkflowStep) Prelude.. Core._Time

-- | Last updated timestamp for workflow step for @APPFLOW_INTEGRATION@
-- workflow.
appflowIntegrationWorkflowStep_lastUpdatedAt :: Lens.Lens' AppflowIntegrationWorkflowStep Prelude.UTCTime
appflowIntegrationWorkflowStep_lastUpdatedAt = Lens.lens (\AppflowIntegrationWorkflowStep' {lastUpdatedAt} -> lastUpdatedAt) (\s@AppflowIntegrationWorkflowStep' {} a -> s {lastUpdatedAt = a} :: AppflowIntegrationWorkflowStep) Prelude.. Core._Time

instance Core.FromJSON AppflowIntegrationWorkflowStep where
  parseJSON =
    Core.withObject
      "AppflowIntegrationWorkflowStep"
      ( \x ->
          AppflowIntegrationWorkflowStep'
            Prelude.<$> (x Core..: "FlowName")
            Prelude.<*> (x Core..: "Status")
            Prelude.<*> (x Core..: "ExecutionMessage")
            Prelude.<*> (x Core..: "RecordsProcessed")
            Prelude.<*> (x Core..: "BatchRecordsStartTime")
            Prelude.<*> (x Core..: "BatchRecordsEndTime")
            Prelude.<*> (x Core..: "CreatedAt")
            Prelude.<*> (x Core..: "LastUpdatedAt")
      )

instance
  Prelude.Hashable
    AppflowIntegrationWorkflowStep
  where
  hashWithSalt
    _salt
    AppflowIntegrationWorkflowStep' {..} =
      _salt `Prelude.hashWithSalt` flowName
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` executionMessage
        `Prelude.hashWithSalt` recordsProcessed
        `Prelude.hashWithSalt` batchRecordsStartTime
        `Prelude.hashWithSalt` batchRecordsEndTime
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` lastUpdatedAt

instance
  Prelude.NFData
    AppflowIntegrationWorkflowStep
  where
  rnf AppflowIntegrationWorkflowStep' {..} =
    Prelude.rnf flowName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf executionMessage
      `Prelude.seq` Prelude.rnf recordsProcessed
      `Prelude.seq` Prelude.rnf batchRecordsStartTime
      `Prelude.seq` Prelude.rnf batchRecordsEndTime
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
