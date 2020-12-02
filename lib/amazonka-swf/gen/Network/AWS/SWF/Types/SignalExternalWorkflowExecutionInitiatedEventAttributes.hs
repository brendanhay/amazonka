{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @SignalExternalWorkflowExecutionInitiated@ event.
--
--
--
-- /See:/ 'signalExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes'
  { _seweieaControl ::
      !( Maybe
           Text
       ),
    _seweieaInput ::
      !( Maybe
           Text
       ),
    _seweieaRunId ::
      !( Maybe
           Text
       ),
    _seweieaWorkflowId ::
      !Text,
    _seweieaSignalName ::
      !Text,
    _seweieaDecisionTaskCompletedEventId ::
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

-- | Creates a value of 'SignalExternalWorkflowExecutionInitiatedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seweieaControl' - Data attached to the event that can be used by the decider in subsequent decision tasks.
--
-- * 'seweieaInput' - The input provided to the signal.
--
-- * 'seweieaRunId' - The @runId@ of the external workflow execution to send the signal to.
--
-- * 'seweieaWorkflowId' - The @workflowId@ of the external workflow execution.
--
-- * 'seweieaSignalName' - The name of the signal.
--
-- * 'seweieaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @SignalExternalWorkflowExecution@ decision for this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
signalExternalWorkflowExecutionInitiatedEventAttributes ::
  -- | 'seweieaWorkflowId'
  Text ->
  -- | 'seweieaSignalName'
  Text ->
  -- | 'seweieaDecisionTaskCompletedEventId'
  Integer ->
  SignalExternalWorkflowExecutionInitiatedEventAttributes
signalExternalWorkflowExecutionInitiatedEventAttributes
  pWorkflowId_
  pSignalName_
  pDecisionTaskCompletedEventId_ =
    SignalExternalWorkflowExecutionInitiatedEventAttributes'
      { _seweieaControl =
          Nothing,
        _seweieaInput = Nothing,
        _seweieaRunId = Nothing,
        _seweieaWorkflowId = pWorkflowId_,
        _seweieaSignalName = pSignalName_,
        _seweieaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent decision tasks.
seweieaControl :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaControl = lens _seweieaControl (\s a -> s {_seweieaControl = a})

-- | The input provided to the signal.
seweieaInput :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaInput = lens _seweieaInput (\s a -> s {_seweieaInput = a})

-- | The @runId@ of the external workflow execution to send the signal to.
seweieaRunId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaRunId = lens _seweieaRunId (\s a -> s {_seweieaRunId = a})

-- | The @workflowId@ of the external workflow execution.
seweieaWorkflowId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Text
seweieaWorkflowId = lens _seweieaWorkflowId (\s a -> s {_seweieaWorkflowId = a})

-- | The name of the signal.
seweieaSignalName :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Text
seweieaSignalName = lens _seweieaSignalName (\s a -> s {_seweieaSignalName = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @SignalExternalWorkflowExecution@ decision for this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
seweieaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Integer
seweieaDecisionTaskCompletedEventId = lens _seweieaDecisionTaskCompletedEventId (\s a -> s {_seweieaDecisionTaskCompletedEventId = a})

instance
  FromJSON
    SignalExternalWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    withObject
      "SignalExternalWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          SignalExternalWorkflowExecutionInitiatedEventAttributes'
            <$> (x .:? "control")
            <*> (x .:? "input")
            <*> (x .:? "runId")
            <*> (x .: "workflowId")
            <*> (x .: "signalName")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance
  Hashable
    SignalExternalWorkflowExecutionInitiatedEventAttributes

instance
  NFData
    SignalExternalWorkflowExecutionInitiatedEventAttributes
