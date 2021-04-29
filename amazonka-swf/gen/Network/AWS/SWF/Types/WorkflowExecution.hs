{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SWF.Types.WorkflowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecution where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a workflow execution.
--
-- /See:/ 'newWorkflowExecution' smart constructor.
data WorkflowExecution = WorkflowExecution'
  { -- | The user defined identifier associated with the workflow execution.
    workflowId :: Prelude.Text,
    -- | A system-generated unique identifier for the workflow execution.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'workflowExecution_workflowId' - The user defined identifier associated with the workflow execution.
--
-- 'runId', 'workflowExecution_runId' - A system-generated unique identifier for the workflow execution.
newWorkflowExecution ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  WorkflowExecution
newWorkflowExecution pWorkflowId_ pRunId_ =
  WorkflowExecution'
    { workflowId = pWorkflowId_,
      runId = pRunId_
    }

-- | The user defined identifier associated with the workflow execution.
workflowExecution_workflowId :: Lens.Lens' WorkflowExecution Prelude.Text
workflowExecution_workflowId = Lens.lens (\WorkflowExecution' {workflowId} -> workflowId) (\s@WorkflowExecution' {} a -> s {workflowId = a} :: WorkflowExecution)

-- | A system-generated unique identifier for the workflow execution.
workflowExecution_runId :: Lens.Lens' WorkflowExecution Prelude.Text
workflowExecution_runId = Lens.lens (\WorkflowExecution' {runId} -> runId) (\s@WorkflowExecution' {} a -> s {runId = a} :: WorkflowExecution)

instance Prelude.FromJSON WorkflowExecution where
  parseJSON =
    Prelude.withObject
      "WorkflowExecution"
      ( \x ->
          WorkflowExecution'
            Prelude.<$> (x Prelude..: "workflowId")
            Prelude.<*> (x Prelude..: "runId")
      )

instance Prelude.Hashable WorkflowExecution

instance Prelude.NFData WorkflowExecution

instance Prelude.ToJSON WorkflowExecution where
  toJSON WorkflowExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("workflowId" Prelude..= workflowId),
            Prelude.Just ("runId" Prelude..= runId)
          ]
      )
