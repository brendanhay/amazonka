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
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionFailed@ event.
--
-- /See:/ 'newChildWorkflowExecutionFailedEventAttributes' smart constructor.
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes'
  { -- | The details of the failure (if provided).
    details :: Prelude.Maybe Prelude.Text,
    -- | The reason for the failure (if provided).
    reason :: Prelude.Maybe Prelude.Text,
    -- | The child workflow execution that failed.
    workflowExecution :: WorkflowExecution,
    -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
    -- to the @StartChildWorkflowExecution@ Decision to start this child
    -- workflow execution. This information can be useful for diagnosing
    -- problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Prelude.Integer,
    -- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
    -- child workflow execution was started. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChildWorkflowExecutionFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'childWorkflowExecutionFailedEventAttributes_details' - The details of the failure (if provided).
--
-- 'reason', 'childWorkflowExecutionFailedEventAttributes_reason' - The reason for the failure (if provided).
--
-- 'workflowExecution', 'childWorkflowExecutionFailedEventAttributes_workflowExecution' - The child workflow execution that failed.
--
-- 'workflowType', 'childWorkflowExecutionFailedEventAttributes_workflowType' - The type of the child workflow execution.
--
-- 'initiatedEventId', 'childWorkflowExecutionFailedEventAttributes_initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
--
-- 'startedEventId', 'childWorkflowExecutionFailedEventAttributes_startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newChildWorkflowExecutionFailedEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'initiatedEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ChildWorkflowExecutionFailedEventAttributes
newChildWorkflowExecutionFailedEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionFailedEventAttributes'
      { details =
          Prelude.Nothing,
        reason = Prelude.Nothing,
        workflowExecution =
          pWorkflowExecution_,
        workflowType = pWorkflowType_,
        initiatedEventId =
          pInitiatedEventId_,
        startedEventId =
          pStartedEventId_
      }

-- | The details of the failure (if provided).
childWorkflowExecutionFailedEventAttributes_details :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes (Prelude.Maybe Prelude.Text)
childWorkflowExecutionFailedEventAttributes_details = Lens.lens (\ChildWorkflowExecutionFailedEventAttributes' {details} -> details) (\s@ChildWorkflowExecutionFailedEventAttributes' {} a -> s {details = a} :: ChildWorkflowExecutionFailedEventAttributes)

-- | The reason for the failure (if provided).
childWorkflowExecutionFailedEventAttributes_reason :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes (Prelude.Maybe Prelude.Text)
childWorkflowExecutionFailedEventAttributes_reason = Lens.lens (\ChildWorkflowExecutionFailedEventAttributes' {reason} -> reason) (\s@ChildWorkflowExecutionFailedEventAttributes' {} a -> s {reason = a} :: ChildWorkflowExecutionFailedEventAttributes)

-- | The child workflow execution that failed.
childWorkflowExecutionFailedEventAttributes_workflowExecution :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowExecution
childWorkflowExecutionFailedEventAttributes_workflowExecution = Lens.lens (\ChildWorkflowExecutionFailedEventAttributes' {workflowExecution} -> workflowExecution) (\s@ChildWorkflowExecutionFailedEventAttributes' {} a -> s {workflowExecution = a} :: ChildWorkflowExecutionFailedEventAttributes)

-- | The type of the child workflow execution.
childWorkflowExecutionFailedEventAttributes_workflowType :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowType
childWorkflowExecutionFailedEventAttributes_workflowType = Lens.lens (\ChildWorkflowExecutionFailedEventAttributes' {workflowType} -> workflowType) (\s@ChildWorkflowExecutionFailedEventAttributes' {} a -> s {workflowType = a} :: ChildWorkflowExecutionFailedEventAttributes)

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionFailedEventAttributes_initiatedEventId :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes Prelude.Integer
childWorkflowExecutionFailedEventAttributes_initiatedEventId = Lens.lens (\ChildWorkflowExecutionFailedEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ChildWorkflowExecutionFailedEventAttributes' {} a -> s {initiatedEventId = a} :: ChildWorkflowExecutionFailedEventAttributes)

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
childWorkflowExecutionFailedEventAttributes_startedEventId :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes Prelude.Integer
childWorkflowExecutionFailedEventAttributes_startedEventId = Lens.lens (\ChildWorkflowExecutionFailedEventAttributes' {startedEventId} -> startedEventId) (\s@ChildWorkflowExecutionFailedEventAttributes' {} a -> s {startedEventId = a} :: ChildWorkflowExecutionFailedEventAttributes)

instance
  Prelude.FromJSON
    ChildWorkflowExecutionFailedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ChildWorkflowExecutionFailedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionFailedEventAttributes'
            Prelude.<$> (x Prelude..:? "details")
              Prelude.<*> (x Prelude..:? "reason")
              Prelude.<*> (x Prelude..: "workflowExecution")
              Prelude.<*> (x Prelude..: "workflowType")
              Prelude.<*> (x Prelude..: "initiatedEventId")
              Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    ChildWorkflowExecutionFailedEventAttributes

instance
  Prelude.NFData
    ChildWorkflowExecutionFailedEventAttributes
