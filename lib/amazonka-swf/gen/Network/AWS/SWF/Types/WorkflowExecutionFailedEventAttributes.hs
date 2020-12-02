{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @WorkflowExecutionFailed@ event.
--
--
--
-- /See:/ 'workflowExecutionFailedEventAttributes' smart constructor.
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes'
  { _wefeaReason ::
      !(Maybe Text),
    _wefeaDetails ::
      !(Maybe Text),
    _wefeaDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wefeaReason' - The descriptive reason provided for the failure.
--
-- * 'wefeaDetails' - The details of the failure.
--
-- * 'wefeaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @FailWorkflowExecution@ decision to fail this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
workflowExecutionFailedEventAttributes ::
  -- | 'wefeaDecisionTaskCompletedEventId'
  Integer ->
  WorkflowExecutionFailedEventAttributes
workflowExecutionFailedEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionFailedEventAttributes'
      { _wefeaReason = Nothing,
        _wefeaDetails = Nothing,
        _wefeaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The descriptive reason provided for the failure.
wefeaReason :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaReason = lens _wefeaReason (\s a -> s {_wefeaReason = a})

-- | The details of the failure.
wefeaDetails :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaDetails = lens _wefeaDetails (\s a -> s {_wefeaDetails = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @FailWorkflowExecution@ decision to fail this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
wefeaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionFailedEventAttributes Integer
wefeaDecisionTaskCompletedEventId = lens _wefeaDecisionTaskCompletedEventId (\s a -> s {_wefeaDecisionTaskCompletedEventId = a})

instance FromJSON WorkflowExecutionFailedEventAttributes where
  parseJSON =
    withObject
      "WorkflowExecutionFailedEventAttributes"
      ( \x ->
          WorkflowExecutionFailedEventAttributes'
            <$> (x .:? "reason")
            <*> (x .:? "details")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable WorkflowExecutionFailedEventAttributes

instance NFData WorkflowExecutionFailedEventAttributes
