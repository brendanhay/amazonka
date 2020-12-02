{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to filter the workflow executions in visibility APIs by their @workflowId@ .
--
--
--
-- /See:/ 'workflowExecutionFilter' smart constructor.
newtype WorkflowExecutionFilter = WorkflowExecutionFilter'
  { _wefWorkflowId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wefWorkflowId' - The workflowId to pass of match the criteria of this filter.
workflowExecutionFilter ::
  -- | 'wefWorkflowId'
  Text ->
  WorkflowExecutionFilter
workflowExecutionFilter pWorkflowId_ =
  WorkflowExecutionFilter' {_wefWorkflowId = pWorkflowId_}

-- | The workflowId to pass of match the criteria of this filter.
wefWorkflowId :: Lens' WorkflowExecutionFilter Text
wefWorkflowId = lens _wefWorkflowId (\s a -> s {_wefWorkflowId = a})

instance Hashable WorkflowExecutionFilter

instance NFData WorkflowExecutionFilter

instance ToJSON WorkflowExecutionFilter where
  toJSON WorkflowExecutionFilter' {..} =
    object (catMaybes [Just ("workflowId" .= _wefWorkflowId)])
