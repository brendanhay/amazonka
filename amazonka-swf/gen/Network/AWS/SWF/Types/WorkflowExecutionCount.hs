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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCount where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the count of workflow executions returned from
-- CountOpenWorkflowExecutions or CountClosedWorkflowExecutions
--
-- /See:/ 'newWorkflowExecutionCount' smart constructor.
data WorkflowExecutionCount = WorkflowExecutionCount'
  { -- | If set to true, indicates that the actual count was more than the
    -- maximum supported by this API and the count returned is the truncated
    -- value.
    truncated :: Core.Maybe Core.Bool,
    -- | The number of workflow executions.
    count :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkflowExecutionCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truncated', 'workflowExecutionCount_truncated' - If set to true, indicates that the actual count was more than the
-- maximum supported by this API and the count returned is the truncated
-- value.
--
-- 'count', 'workflowExecutionCount_count' - The number of workflow executions.
newWorkflowExecutionCount ::
  -- | 'count'
  Core.Natural ->
  WorkflowExecutionCount
newWorkflowExecutionCount pCount_ =
  WorkflowExecutionCount'
    { truncated = Core.Nothing,
      count = pCount_
    }

-- | If set to true, indicates that the actual count was more than the
-- maximum supported by this API and the count returned is the truncated
-- value.
workflowExecutionCount_truncated :: Lens.Lens' WorkflowExecutionCount (Core.Maybe Core.Bool)
workflowExecutionCount_truncated = Lens.lens (\WorkflowExecutionCount' {truncated} -> truncated) (\s@WorkflowExecutionCount' {} a -> s {truncated = a} :: WorkflowExecutionCount)

-- | The number of workflow executions.
workflowExecutionCount_count :: Lens.Lens' WorkflowExecutionCount Core.Natural
workflowExecutionCount_count = Lens.lens (\WorkflowExecutionCount' {count} -> count) (\s@WorkflowExecutionCount' {} a -> s {count = a} :: WorkflowExecutionCount)

instance Core.FromJSON WorkflowExecutionCount where
  parseJSON =
    Core.withObject
      "WorkflowExecutionCount"
      ( \x ->
          WorkflowExecutionCount'
            Core.<$> (x Core..:? "truncated")
            Core.<*> (x Core..: "count")
      )

instance Core.Hashable WorkflowExecutionCount

instance Core.NFData WorkflowExecutionCount
