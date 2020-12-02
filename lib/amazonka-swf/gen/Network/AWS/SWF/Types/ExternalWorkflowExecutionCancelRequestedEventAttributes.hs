{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution

-- | Provides the details of the @ExternalWorkflowExecutionCancelRequested@ event.
--
--
--
-- /See:/ 'externalWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes'
  { _ewecreaWorkflowExecution ::
      !WorkflowExecution,
    _ewecreaInitiatedEventId ::
      !Integer
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ExternalWorkflowExecutionCancelRequestedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ewecreaWorkflowExecution' - The external workflow execution to which the cancellation request was delivered.
--
-- * 'ewecreaInitiatedEventId' - The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this external workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
externalWorkflowExecutionCancelRequestedEventAttributes ::
  -- | 'ewecreaWorkflowExecution'
  WorkflowExecution ->
  -- | 'ewecreaInitiatedEventId'
  Integer ->
  ExternalWorkflowExecutionCancelRequestedEventAttributes
externalWorkflowExecutionCancelRequestedEventAttributes
  pWorkflowExecution_
  pInitiatedEventId_ =
    ExternalWorkflowExecutionCancelRequestedEventAttributes'
      { _ewecreaWorkflowExecution =
          pWorkflowExecution_,
        _ewecreaInitiatedEventId =
          pInitiatedEventId_
      }

-- | The external workflow execution to which the cancellation request was delivered.
ewecreaWorkflowExecution :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes WorkflowExecution
ewecreaWorkflowExecution = lens _ewecreaWorkflowExecution (\s a -> s {_ewecreaWorkflowExecution = a})

-- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this external workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
ewecreaInitiatedEventId :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes Integer
ewecreaInitiatedEventId = lens _ewecreaInitiatedEventId (\s a -> s {_ewecreaInitiatedEventId = a})

instance
  FromJSON
    ExternalWorkflowExecutionCancelRequestedEventAttributes
  where
  parseJSON =
    withObject
      "ExternalWorkflowExecutionCancelRequestedEventAttributes"
      ( \x ->
          ExternalWorkflowExecutionCancelRequestedEventAttributes'
            <$> (x .: "workflowExecution") <*> (x .: "initiatedEventId")
      )

instance
  Hashable
    ExternalWorkflowExecutionCancelRequestedEventAttributes

instance
  NFData
    ExternalWorkflowExecutionCancelRequestedEventAttributes
