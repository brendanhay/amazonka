{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution

-- | Provides the details of the @ExternalWorkflowExecutionSignaled@ event.
--
--
--
-- /See:/ 'externalWorkflowExecutionSignaledEventAttributes' smart constructor.
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes'
  { _eweseaWorkflowExecution ::
      !WorkflowExecution,
    _eweseaInitiatedEventId ::
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

-- | Creates a value of 'ExternalWorkflowExecutionSignaledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eweseaWorkflowExecution' - The external workflow execution that the signal was delivered to.
--
-- * 'eweseaInitiatedEventId' - The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflowExecution@ decision to request this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
externalWorkflowExecutionSignaledEventAttributes ::
  -- | 'eweseaWorkflowExecution'
  WorkflowExecution ->
  -- | 'eweseaInitiatedEventId'
  Integer ->
  ExternalWorkflowExecutionSignaledEventAttributes
externalWorkflowExecutionSignaledEventAttributes
  pWorkflowExecution_
  pInitiatedEventId_ =
    ExternalWorkflowExecutionSignaledEventAttributes'
      { _eweseaWorkflowExecution =
          pWorkflowExecution_,
        _eweseaInitiatedEventId = pInitiatedEventId_
      }

-- | The external workflow execution that the signal was delivered to.
eweseaWorkflowExecution :: Lens' ExternalWorkflowExecutionSignaledEventAttributes WorkflowExecution
eweseaWorkflowExecution = lens _eweseaWorkflowExecution (\s a -> s {_eweseaWorkflowExecution = a})

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflowExecution@ decision to request this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
eweseaInitiatedEventId :: Lens' ExternalWorkflowExecutionSignaledEventAttributes Integer
eweseaInitiatedEventId = lens _eweseaInitiatedEventId (\s a -> s {_eweseaInitiatedEventId = a})

instance FromJSON ExternalWorkflowExecutionSignaledEventAttributes where
  parseJSON =
    withObject
      "ExternalWorkflowExecutionSignaledEventAttributes"
      ( \x ->
          ExternalWorkflowExecutionSignaledEventAttributes'
            <$> (x .: "workflowExecution") <*> (x .: "initiatedEventId")
      )

instance Hashable ExternalWorkflowExecutionSignaledEventAttributes

instance NFData ExternalWorkflowExecutionSignaledEventAttributes
