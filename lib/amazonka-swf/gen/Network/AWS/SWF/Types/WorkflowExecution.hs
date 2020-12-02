{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecution where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a workflow execution.
--
--
--
-- /See:/ 'workflowExecution' smart constructor.
data WorkflowExecution = WorkflowExecution'
  { _weWorkflowId :: !Text,
    _weRunId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'weWorkflowId' - The user defined identifier associated with the workflow execution.
--
-- * 'weRunId' - A system-generated unique identifier for the workflow execution.
workflowExecution ::
  -- | 'weWorkflowId'
  Text ->
  -- | 'weRunId'
  Text ->
  WorkflowExecution
workflowExecution pWorkflowId_ pRunId_ =
  WorkflowExecution'
    { _weWorkflowId = pWorkflowId_,
      _weRunId = pRunId_
    }

-- | The user defined identifier associated with the workflow execution.
weWorkflowId :: Lens' WorkflowExecution Text
weWorkflowId = lens _weWorkflowId (\s a -> s {_weWorkflowId = a})

-- | A system-generated unique identifier for the workflow execution.
weRunId :: Lens' WorkflowExecution Text
weRunId = lens _weRunId (\s a -> s {_weRunId = a})

instance FromJSON WorkflowExecution where
  parseJSON =
    withObject
      "WorkflowExecution"
      ( \x ->
          WorkflowExecution' <$> (x .: "workflowId") <*> (x .: "runId")
      )

instance Hashable WorkflowExecution

instance NFData WorkflowExecution

instance ToJSON WorkflowExecution where
  toJSON WorkflowExecution' {..} =
    object
      ( catMaybes
          [Just ("workflowId" .= _weWorkflowId), Just ("runId" .= _weRunId)]
      )
