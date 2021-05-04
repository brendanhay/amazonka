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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to filter the workflow executions in visibility APIs by their
-- @workflowId@.
--
-- /See:/ 'newWorkflowExecutionFilter' smart constructor.
data WorkflowExecutionFilter = WorkflowExecutionFilter'
  { -- | The workflowId to pass of match the criteria of this filter.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable WorkflowExecutionFilter

instance Prelude.NFData WorkflowExecutionFilter

instance Prelude.ToJSON WorkflowExecutionFilter where
  toJSON WorkflowExecutionFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("workflowId" Prelude..= workflowId)]
      )
