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
-- Module      : Amazonka.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ChildWorkflowExecutionStartedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.WorkflowExecution
import Amazonka.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionStarted@ event.
--
-- /See:/ 'newChildWorkflowExecutionStartedEventAttributes' smart constructor.
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes'
  { -- | The child workflow execution that was started.
    workflowExecution :: WorkflowExecution,
    -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
    -- to the @StartChildWorkflowExecution@ Decision to start this child
    -- workflow execution. This information can be useful for diagnosing
    -- problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChildWorkflowExecutionStartedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowExecution', 'childWorkflowExecutionStartedEventAttributes_workflowExecution' - The child workflow execution that was started.
--
-- 'workflowType', 'childWorkflowExecutionStartedEventAttributes_workflowType' - The type of the child workflow execution.
--
-- 'initiatedEventId', 'childWorkflowExecutionStartedEventAttributes_initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
newChildWorkflowExecutionStartedEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'initiatedEventId'
  Prelude.Integer ->
  ChildWorkflowExecutionStartedEventAttributes
newChildWorkflowExecutionStartedEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_ =
    ChildWorkflowExecutionStartedEventAttributes'
      { workflowExecution =
          pWorkflowExecution_,
        workflowType = pWorkflowType_,
        initiatedEventId =
          pInitiatedEventId_
      }

-- | The child workflow execution that was started.
childWorkflowExecutionStartedEventAttributes_workflowExecution :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowExecution
childWorkflowExecutionStartedEventAttributes_workflowExecution = Lens.lens (\ChildWorkflowExecutionStartedEventAttributes' {workflowExecution} -> workflowExecution) (\s@ChildWorkflowExecutionStartedEventAttributes' {} a -> s {workflowExecution = a} :: ChildWorkflowExecutionStartedEventAttributes)

-- | The type of the child workflow execution.
childWorkflowExecutionStartedEventAttributes_workflowType :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowType
childWorkflowExecutionStartedEventAttributes_workflowType = Lens.lens (\ChildWorkflowExecutionStartedEventAttributes' {workflowType} -> workflowType) (\s@ChildWorkflowExecutionStartedEventAttributes' {} a -> s {workflowType = a} :: ChildWorkflowExecutionStartedEventAttributes)

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionStartedEventAttributes_initiatedEventId :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes Prelude.Integer
childWorkflowExecutionStartedEventAttributes_initiatedEventId = Lens.lens (\ChildWorkflowExecutionStartedEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ChildWorkflowExecutionStartedEventAttributes' {} a -> s {initiatedEventId = a} :: ChildWorkflowExecutionStartedEventAttributes)

instance
  Data.FromJSON
    ChildWorkflowExecutionStartedEventAttributes
  where
  parseJSON =
    Data.withObject
      "ChildWorkflowExecutionStartedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionStartedEventAttributes'
            Prelude.<$> (x Data..: "workflowExecution")
              Prelude.<*> (x Data..: "workflowType")
              Prelude.<*> (x Data..: "initiatedEventId")
      )

instance
  Prelude.Hashable
    ChildWorkflowExecutionStartedEventAttributes
  where
  hashWithSalt
    _salt
    ChildWorkflowExecutionStartedEventAttributes' {..} =
      _salt `Prelude.hashWithSalt` workflowExecution
        `Prelude.hashWithSalt` workflowType
        `Prelude.hashWithSalt` initiatedEventId

instance
  Prelude.NFData
    ChildWorkflowExecutionStartedEventAttributes
  where
  rnf ChildWorkflowExecutionStartedEventAttributes' {..} =
    Prelude.rnf workflowExecution
      `Prelude.seq` Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf initiatedEventId
