{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution

-- | Provides the details of the @WorkflowExecutionSignaled@ event.
--
--
--
-- /See:/ 'workflowExecutionSignaledEventAttributes' smart constructor.
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes'
  { _wExternalWorkflowExecution ::
      !( Maybe
           WorkflowExecution
       ),
    _wExternalInitiatedEventId ::
      !( Maybe
           Integer
       ),
    _wInput ::
      !( Maybe
           Text
       ),
    _wSignalName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionSignaledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wExternalWorkflowExecution' - The workflow execution that sent the signal. This is set only of the signal was sent by another workflow execution.
--
-- * 'wExternalInitiatedEventId' - The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflow@ decision to signal this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event. This field is set only if the signal was initiated by another workflow execution.
--
-- * 'wInput' - The inputs provided with the signal. The decider can use the signal name and inputs to determine how to process the signal.
--
-- * 'wSignalName' - The name of the signal received. The decider can use the signal name and inputs to determine how to the process the signal.
workflowExecutionSignaledEventAttributes ::
  -- | 'wSignalName'
  Text ->
  WorkflowExecutionSignaledEventAttributes
workflowExecutionSignaledEventAttributes pSignalName_ =
  WorkflowExecutionSignaledEventAttributes'
    { _wExternalWorkflowExecution =
        Nothing,
      _wExternalInitiatedEventId = Nothing,
      _wInput = Nothing,
      _wSignalName = pSignalName_
    }

-- | The workflow execution that sent the signal. This is set only of the signal was sent by another workflow execution.
wExternalWorkflowExecution :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe WorkflowExecution)
wExternalWorkflowExecution = lens _wExternalWorkflowExecution (\s a -> s {_wExternalWorkflowExecution = a})

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflow@ decision to signal this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event. This field is set only if the signal was initiated by another workflow execution.
wExternalInitiatedEventId :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Integer)
wExternalInitiatedEventId = lens _wExternalInitiatedEventId (\s a -> s {_wExternalInitiatedEventId = a})

-- | The inputs provided with the signal. The decider can use the signal name and inputs to determine how to process the signal.
wInput :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Text)
wInput = lens _wInput (\s a -> s {_wInput = a})

-- | The name of the signal received. The decider can use the signal name and inputs to determine how to the process the signal.
wSignalName :: Lens' WorkflowExecutionSignaledEventAttributes Text
wSignalName = lens _wSignalName (\s a -> s {_wSignalName = a})

instance FromJSON WorkflowExecutionSignaledEventAttributes where
  parseJSON =
    withObject
      "WorkflowExecutionSignaledEventAttributes"
      ( \x ->
          WorkflowExecutionSignaledEventAttributes'
            <$> (x .:? "externalWorkflowExecution")
            <*> (x .:? "externalInitiatedEventId")
            <*> (x .:? "input")
            <*> (x .: "signalName")
      )

instance Hashable WorkflowExecutionSignaledEventAttributes

instance NFData WorkflowExecutionSignaledEventAttributes
