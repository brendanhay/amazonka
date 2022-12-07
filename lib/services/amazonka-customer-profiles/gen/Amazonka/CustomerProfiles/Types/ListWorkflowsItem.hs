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
-- Module      : Amazonka.CustomerProfiles.Types.ListWorkflowsItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ListWorkflowsItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.Status
import Amazonka.CustomerProfiles.Types.WorkflowType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A workflow in list of workflows.
--
-- /See:/ 'newListWorkflowsItem' smart constructor.
data ListWorkflowsItem = ListWorkflowsItem'
  { -- | The type of workflow. The only supported value is APPFLOW_INTEGRATION.
    workflowType :: WorkflowType,
    -- | Unique identifier for the workflow.
    workflowId :: Prelude.Text,
    -- | Status of workflow execution.
    status :: Status,
    -- | Description for workflow execution status.
    statusDescription :: Prelude.Text,
    -- | Creation timestamp for workflow.
    createdAt :: Data.POSIX,
    -- | Last updated timestamp for workflow.
    lastUpdatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowType', 'listWorkflowsItem_workflowType' - The type of workflow. The only supported value is APPFLOW_INTEGRATION.
--
-- 'workflowId', 'listWorkflowsItem_workflowId' - Unique identifier for the workflow.
--
-- 'status', 'listWorkflowsItem_status' - Status of workflow execution.
--
-- 'statusDescription', 'listWorkflowsItem_statusDescription' - Description for workflow execution status.
--
-- 'createdAt', 'listWorkflowsItem_createdAt' - Creation timestamp for workflow.
--
-- 'lastUpdatedAt', 'listWorkflowsItem_lastUpdatedAt' - Last updated timestamp for workflow.
newListWorkflowsItem ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'status'
  Status ->
  -- | 'statusDescription'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  ListWorkflowsItem
newListWorkflowsItem
  pWorkflowType_
  pWorkflowId_
  pStatus_
  pStatusDescription_
  pCreatedAt_
  pLastUpdatedAt_ =
    ListWorkflowsItem'
      { workflowType = pWorkflowType_,
        workflowId = pWorkflowId_,
        status = pStatus_,
        statusDescription = pStatusDescription_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_
      }

-- | The type of workflow. The only supported value is APPFLOW_INTEGRATION.
listWorkflowsItem_workflowType :: Lens.Lens' ListWorkflowsItem WorkflowType
listWorkflowsItem_workflowType = Lens.lens (\ListWorkflowsItem' {workflowType} -> workflowType) (\s@ListWorkflowsItem' {} a -> s {workflowType = a} :: ListWorkflowsItem)

-- | Unique identifier for the workflow.
listWorkflowsItem_workflowId :: Lens.Lens' ListWorkflowsItem Prelude.Text
listWorkflowsItem_workflowId = Lens.lens (\ListWorkflowsItem' {workflowId} -> workflowId) (\s@ListWorkflowsItem' {} a -> s {workflowId = a} :: ListWorkflowsItem)

-- | Status of workflow execution.
listWorkflowsItem_status :: Lens.Lens' ListWorkflowsItem Status
listWorkflowsItem_status = Lens.lens (\ListWorkflowsItem' {status} -> status) (\s@ListWorkflowsItem' {} a -> s {status = a} :: ListWorkflowsItem)

-- | Description for workflow execution status.
listWorkflowsItem_statusDescription :: Lens.Lens' ListWorkflowsItem Prelude.Text
listWorkflowsItem_statusDescription = Lens.lens (\ListWorkflowsItem' {statusDescription} -> statusDescription) (\s@ListWorkflowsItem' {} a -> s {statusDescription = a} :: ListWorkflowsItem)

-- | Creation timestamp for workflow.
listWorkflowsItem_createdAt :: Lens.Lens' ListWorkflowsItem Prelude.UTCTime
listWorkflowsItem_createdAt = Lens.lens (\ListWorkflowsItem' {createdAt} -> createdAt) (\s@ListWorkflowsItem' {} a -> s {createdAt = a} :: ListWorkflowsItem) Prelude.. Data._Time

-- | Last updated timestamp for workflow.
listWorkflowsItem_lastUpdatedAt :: Lens.Lens' ListWorkflowsItem Prelude.UTCTime
listWorkflowsItem_lastUpdatedAt = Lens.lens (\ListWorkflowsItem' {lastUpdatedAt} -> lastUpdatedAt) (\s@ListWorkflowsItem' {} a -> s {lastUpdatedAt = a} :: ListWorkflowsItem) Prelude.. Data._Time

instance Data.FromJSON ListWorkflowsItem where
  parseJSON =
    Data.withObject
      "ListWorkflowsItem"
      ( \x ->
          ListWorkflowsItem'
            Prelude.<$> (x Data..: "WorkflowType")
            Prelude.<*> (x Data..: "WorkflowId")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "StatusDescription")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "LastUpdatedAt")
      )

instance Prelude.Hashable ListWorkflowsItem where
  hashWithSalt _salt ListWorkflowsItem' {..} =
    _salt `Prelude.hashWithSalt` workflowType
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusDescription
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt

instance Prelude.NFData ListWorkflowsItem where
  rnf ListWorkflowsItem' {..} =
    Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDescription
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
