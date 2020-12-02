{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCount where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the count of workflow executions returned from 'CountOpenWorkflowExecutions' or 'CountClosedWorkflowExecutions'
--
--
--
-- /See:/ 'workflowExecutionCount' smart constructor.
data WorkflowExecutionCount = WorkflowExecutionCount'
  { _wecTruncated ::
      !(Maybe Bool),
    _wecCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wecTruncated' - If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
--
-- * 'wecCount' - The number of workflow executions.
workflowExecutionCount ::
  -- | 'wecCount'
  Natural ->
  WorkflowExecutionCount
workflowExecutionCount pCount_ =
  WorkflowExecutionCount'
    { _wecTruncated = Nothing,
      _wecCount = _Nat # pCount_
    }

-- | If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
wecTruncated :: Lens' WorkflowExecutionCount (Maybe Bool)
wecTruncated = lens _wecTruncated (\s a -> s {_wecTruncated = a})

-- | The number of workflow executions.
wecCount :: Lens' WorkflowExecutionCount Natural
wecCount = lens _wecCount (\s a -> s {_wecCount = a}) . _Nat

instance FromJSON WorkflowExecutionCount where
  parseJSON =
    withObject
      "WorkflowExecutionCount"
      ( \x ->
          WorkflowExecutionCount' <$> (x .:? "truncated") <*> (x .: "count")
      )

instance Hashable WorkflowExecutionCount

instance NFData WorkflowExecutionCount
