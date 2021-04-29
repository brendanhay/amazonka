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
-- Module      : Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.WorkflowExecution

-- | Provides the details of the @ExternalWorkflowExecutionSignaled@ event.
--
-- /See:/ 'newExternalWorkflowExecutionSignaledEventAttributes' smart constructor.
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes'
  { -- | The external workflow execution that the signal was delivered to.
    workflowExecution :: WorkflowExecution,
    -- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event
    -- corresponding to the @SignalExternalWorkflowExecution@ decision to
    -- request this signal. This information can be useful for diagnosing
    -- problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExternalWorkflowExecutionSignaledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowExecution', 'externalWorkflowExecutionSignaledEventAttributes_workflowExecution' - The external workflow execution that the signal was delivered to.
--
-- 'initiatedEventId', 'externalWorkflowExecutionSignaledEventAttributes_initiatedEventId' - The ID of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflowExecution@ decision to
-- request this signal. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
newExternalWorkflowExecutionSignaledEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'initiatedEventId'
  Prelude.Integer ->
  ExternalWorkflowExecutionSignaledEventAttributes
newExternalWorkflowExecutionSignaledEventAttributes
  pWorkflowExecution_
  pInitiatedEventId_ =
    ExternalWorkflowExecutionSignaledEventAttributes'
      { workflowExecution =
          pWorkflowExecution_,
        initiatedEventId =
          pInitiatedEventId_
      }

-- | The external workflow execution that the signal was delivered to.
externalWorkflowExecutionSignaledEventAttributes_workflowExecution :: Lens.Lens' ExternalWorkflowExecutionSignaledEventAttributes WorkflowExecution
externalWorkflowExecutionSignaledEventAttributes_workflowExecution = Lens.lens (\ExternalWorkflowExecutionSignaledEventAttributes' {workflowExecution} -> workflowExecution) (\s@ExternalWorkflowExecutionSignaledEventAttributes' {} a -> s {workflowExecution = a} :: ExternalWorkflowExecutionSignaledEventAttributes)

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflowExecution@ decision to
-- request this signal. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
externalWorkflowExecutionSignaledEventAttributes_initiatedEventId :: Lens.Lens' ExternalWorkflowExecutionSignaledEventAttributes Prelude.Integer
externalWorkflowExecutionSignaledEventAttributes_initiatedEventId = Lens.lens (\ExternalWorkflowExecutionSignaledEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ExternalWorkflowExecutionSignaledEventAttributes' {} a -> s {initiatedEventId = a} :: ExternalWorkflowExecutionSignaledEventAttributes)

instance
  Prelude.FromJSON
    ExternalWorkflowExecutionSignaledEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ExternalWorkflowExecutionSignaledEventAttributes"
      ( \x ->
          ExternalWorkflowExecutionSignaledEventAttributes'
            Prelude.<$> (x Prelude..: "workflowExecution")
              Prelude.<*> (x Prelude..: "initiatedEventId")
      )

instance
  Prelude.Hashable
    ExternalWorkflowExecutionSignaledEventAttributes

instance
  Prelude.NFData
    ExternalWorkflowExecutionSignaledEventAttributes
