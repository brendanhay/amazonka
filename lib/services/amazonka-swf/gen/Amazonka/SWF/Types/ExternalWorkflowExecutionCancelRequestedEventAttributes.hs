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
-- Module      : Amazonka.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.WorkflowExecution

-- | Provides the details of the @ExternalWorkflowExecutionCancelRequested@
-- event.
--
-- /See:/ 'newExternalWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes'
  { -- | The external workflow execution to which the cancellation request was
    -- delivered.
    workflowExecution :: WorkflowExecution,
    -- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event
    -- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
    -- to cancel this external workflow execution. This information can be
    -- useful for diagnosing problems by tracing back the chain of events
    -- leading up to this event.
    initiatedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalWorkflowExecutionCancelRequestedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowExecution', 'externalWorkflowExecutionCancelRequestedEventAttributes_workflowExecution' - The external workflow execution to which the cancellation request was
-- delivered.
--
-- 'initiatedEventId', 'externalWorkflowExecutionCancelRequestedEventAttributes_initiatedEventId' - The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event
-- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
-- to cancel this external workflow execution. This information can be
-- useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
newExternalWorkflowExecutionCancelRequestedEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'initiatedEventId'
  Prelude.Integer ->
  ExternalWorkflowExecutionCancelRequestedEventAttributes
newExternalWorkflowExecutionCancelRequestedEventAttributes
  pWorkflowExecution_
  pInitiatedEventId_ =
    ExternalWorkflowExecutionCancelRequestedEventAttributes'
      { workflowExecution =
          pWorkflowExecution_,
        initiatedEventId =
          pInitiatedEventId_
      }

-- | The external workflow execution to which the cancellation request was
-- delivered.
externalWorkflowExecutionCancelRequestedEventAttributes_workflowExecution :: Lens.Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes WorkflowExecution
externalWorkflowExecutionCancelRequestedEventAttributes_workflowExecution = Lens.lens (\ExternalWorkflowExecutionCancelRequestedEventAttributes' {workflowExecution} -> workflowExecution) (\s@ExternalWorkflowExecutionCancelRequestedEventAttributes' {} a -> s {workflowExecution = a} :: ExternalWorkflowExecutionCancelRequestedEventAttributes)

-- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event
-- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
-- to cancel this external workflow execution. This information can be
-- useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
externalWorkflowExecutionCancelRequestedEventAttributes_initiatedEventId :: Lens.Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes Prelude.Integer
externalWorkflowExecutionCancelRequestedEventAttributes_initiatedEventId = Lens.lens (\ExternalWorkflowExecutionCancelRequestedEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ExternalWorkflowExecutionCancelRequestedEventAttributes' {} a -> s {initiatedEventId = a} :: ExternalWorkflowExecutionCancelRequestedEventAttributes)

instance
  Data.FromJSON
    ExternalWorkflowExecutionCancelRequestedEventAttributes
  where
  parseJSON =
    Data.withObject
      "ExternalWorkflowExecutionCancelRequestedEventAttributes"
      ( \x ->
          ExternalWorkflowExecutionCancelRequestedEventAttributes'
            Prelude.<$> (x Data..: "workflowExecution")
            Prelude.<*> (x Data..: "initiatedEventId")
      )

instance
  Prelude.Hashable
    ExternalWorkflowExecutionCancelRequestedEventAttributes
  where
  hashWithSalt
    _salt
    ExternalWorkflowExecutionCancelRequestedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` workflowExecution
        `Prelude.hashWithSalt` initiatedEventId

instance
  Prelude.NFData
    ExternalWorkflowExecutionCancelRequestedEventAttributes
  where
  rnf
    ExternalWorkflowExecutionCancelRequestedEventAttributes' {..} =
      Prelude.rnf workflowExecution
        `Prelude.seq` Prelude.rnf initiatedEventId
