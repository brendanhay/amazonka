{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @RequestCancelExternalWorkflowExecutionInitiated@ event.
--
--
--
-- /See:/ 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
  { _rceweieaControl ::
      !( Maybe
           Text
       ),
    _rceweieaRunId ::
      !( Maybe
           Text
       ),
    _rceweieaWorkflowId ::
      !Text,
    _rceweieaDecisionTaskCompletedEventId ::
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

-- | Creates a value of 'RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rceweieaControl' - Data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- * 'rceweieaRunId' - The @runId@ of the external workflow execution to be canceled.
--
-- * 'rceweieaWorkflowId' - The @workflowId@ of the external workflow execution to be canceled.
--
-- * 'rceweieaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelExternalWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
requestCancelExternalWorkflowExecutionInitiatedEventAttributes ::
  -- | 'rceweieaWorkflowId'
  Text ->
  -- | 'rceweieaDecisionTaskCompletedEventId'
  Integer ->
  RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
requestCancelExternalWorkflowExecutionInitiatedEventAttributes
  pWorkflowId_
  pDecisionTaskCompletedEventId_ =
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
      { _rceweieaControl =
          Nothing,
        _rceweieaRunId = Nothing,
        _rceweieaWorkflowId =
          pWorkflowId_,
        _rceweieaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks.
rceweieaControl :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaControl = lens _rceweieaControl (\s a -> s {_rceweieaControl = a})

-- | The @runId@ of the external workflow execution to be canceled.
rceweieaRunId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaRunId = lens _rceweieaRunId (\s a -> s {_rceweieaRunId = a})

-- | The @workflowId@ of the external workflow execution to be canceled.
rceweieaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Text
rceweieaWorkflowId = lens _rceweieaWorkflowId (\s a -> s {_rceweieaWorkflowId = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelExternalWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
rceweieaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Integer
rceweieaDecisionTaskCompletedEventId = lens _rceweieaDecisionTaskCompletedEventId (\s a -> s {_rceweieaDecisionTaskCompletedEventId = a})

instance
  FromJSON
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    withObject
      "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
            <$> (x .:? "control")
            <*> (x .:? "runId")
            <*> (x .: "workflowId")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance
  Hashable
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes

instance
  NFData
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
