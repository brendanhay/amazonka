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
-- Module      : Amazonka.Transfer.Types.WorkflowDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.WorkflowDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the workflow ID for the workflow to assign and the execution
-- role that\'s used for executing the workflow.
--
-- In addition to a workflow to execute when a file is uploaded completely,
-- @WorkflowDetails@ can also contain a workflow ID (and execution role)
-- for a workflow to execute on partial upload. A partial upload occurs
-- when a file is open when the session disconnects.
--
-- /See:/ 'newWorkflowDetail' smart constructor.
data WorkflowDetail = WorkflowDetail'
  { -- | A unique identifier for the workflow.
    workflowId :: Prelude.Text,
    -- | Includes the necessary permissions for S3, EFS, and Lambda operations
    -- that Transfer can assume, so that all workflow steps can operate on the
    -- required resources
    executionRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'workflowDetail_workflowId' - A unique identifier for the workflow.
--
-- 'executionRole', 'workflowDetail_executionRole' - Includes the necessary permissions for S3, EFS, and Lambda operations
-- that Transfer can assume, so that all workflow steps can operate on the
-- required resources
newWorkflowDetail ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'executionRole'
  Prelude.Text ->
  WorkflowDetail
newWorkflowDetail pWorkflowId_ pExecutionRole_ =
  WorkflowDetail'
    { workflowId = pWorkflowId_,
      executionRole = pExecutionRole_
    }

-- | A unique identifier for the workflow.
workflowDetail_workflowId :: Lens.Lens' WorkflowDetail Prelude.Text
workflowDetail_workflowId = Lens.lens (\WorkflowDetail' {workflowId} -> workflowId) (\s@WorkflowDetail' {} a -> s {workflowId = a} :: WorkflowDetail)

-- | Includes the necessary permissions for S3, EFS, and Lambda operations
-- that Transfer can assume, so that all workflow steps can operate on the
-- required resources
workflowDetail_executionRole :: Lens.Lens' WorkflowDetail Prelude.Text
workflowDetail_executionRole = Lens.lens (\WorkflowDetail' {executionRole} -> executionRole) (\s@WorkflowDetail' {} a -> s {executionRole = a} :: WorkflowDetail)

instance Data.FromJSON WorkflowDetail where
  parseJSON =
    Data.withObject
      "WorkflowDetail"
      ( \x ->
          WorkflowDetail'
            Prelude.<$> (x Data..: "WorkflowId")
            Prelude.<*> (x Data..: "ExecutionRole")
      )

instance Prelude.Hashable WorkflowDetail where
  hashWithSalt _salt WorkflowDetail' {..} =
    _salt `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` executionRole

instance Prelude.NFData WorkflowDetail where
  rnf WorkflowDetail' {..} =
    Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf executionRole

instance Data.ToJSON WorkflowDetail where
  toJSON WorkflowDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WorkflowId" Data..= workflowId),
            Prelude.Just
              ("ExecutionRole" Data..= executionRole)
          ]
      )
