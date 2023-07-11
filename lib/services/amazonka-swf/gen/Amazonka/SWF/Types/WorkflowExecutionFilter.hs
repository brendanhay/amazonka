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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used to filter the workflow executions in visibility APIs by their
-- @workflowId@.
--
-- /See:/ 'newWorkflowExecutionFilter' smart constructor.
data WorkflowExecutionFilter = WorkflowExecutionFilter'
  { -- | The workflowId to pass of match the criteria of this filter.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'workflowExecutionFilter_workflowId' - The workflowId to pass of match the criteria of this filter.
newWorkflowExecutionFilter ::
  -- | 'workflowId'
  Prelude.Text ->
  WorkflowExecutionFilter
newWorkflowExecutionFilter pWorkflowId_ =
  WorkflowExecutionFilter' {workflowId = pWorkflowId_}

-- | The workflowId to pass of match the criteria of this filter.
workflowExecutionFilter_workflowId :: Lens.Lens' WorkflowExecutionFilter Prelude.Text
workflowExecutionFilter_workflowId = Lens.lens (\WorkflowExecutionFilter' {workflowId} -> workflowId) (\s@WorkflowExecutionFilter' {} a -> s {workflowId = a} :: WorkflowExecutionFilter)

instance Prelude.Hashable WorkflowExecutionFilter where
  hashWithSalt _salt WorkflowExecutionFilter' {..} =
    _salt `Prelude.hashWithSalt` workflowId

instance Prelude.NFData WorkflowExecutionFilter where
  rnf WorkflowExecutionFilter' {..} =
    Prelude.rnf workflowId

instance Data.ToJSON WorkflowExecutionFilter where
  toJSON WorkflowExecutionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("workflowId" Data..= workflowId)]
      )
