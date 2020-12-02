{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRunStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRunStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Workflow run statistics provides statistics about the workflow run.
--
--
--
-- /See:/ 'workflowRunStatistics' smart constructor.
data WorkflowRunStatistics = WorkflowRunStatistics'
  { _wrsRunningActions ::
      !(Maybe Int),
    _wrsStoppedActions :: !(Maybe Int),
    _wrsTotalActions :: !(Maybe Int),
    _wrsFailedActions :: !(Maybe Int),
    _wrsTimeoutActions :: !(Maybe Int),
    _wrsSucceededActions :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowRunStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wrsRunningActions' - Total number Actions in running state.
--
-- * 'wrsStoppedActions' - Total number of Actions that have stopped.
--
-- * 'wrsTotalActions' - Total number of Actions in the workflow run.
--
-- * 'wrsFailedActions' - Total number of Actions that have failed.
--
-- * 'wrsTimeoutActions' - Total number of Actions that timed out.
--
-- * 'wrsSucceededActions' - Total number of Actions that have succeeded.
workflowRunStatistics ::
  WorkflowRunStatistics
workflowRunStatistics =
  WorkflowRunStatistics'
    { _wrsRunningActions = Nothing,
      _wrsStoppedActions = Nothing,
      _wrsTotalActions = Nothing,
      _wrsFailedActions = Nothing,
      _wrsTimeoutActions = Nothing,
      _wrsSucceededActions = Nothing
    }

-- | Total number Actions in running state.
wrsRunningActions :: Lens' WorkflowRunStatistics (Maybe Int)
wrsRunningActions = lens _wrsRunningActions (\s a -> s {_wrsRunningActions = a})

-- | Total number of Actions that have stopped.
wrsStoppedActions :: Lens' WorkflowRunStatistics (Maybe Int)
wrsStoppedActions = lens _wrsStoppedActions (\s a -> s {_wrsStoppedActions = a})

-- | Total number of Actions in the workflow run.
wrsTotalActions :: Lens' WorkflowRunStatistics (Maybe Int)
wrsTotalActions = lens _wrsTotalActions (\s a -> s {_wrsTotalActions = a})

-- | Total number of Actions that have failed.
wrsFailedActions :: Lens' WorkflowRunStatistics (Maybe Int)
wrsFailedActions = lens _wrsFailedActions (\s a -> s {_wrsFailedActions = a})

-- | Total number of Actions that timed out.
wrsTimeoutActions :: Lens' WorkflowRunStatistics (Maybe Int)
wrsTimeoutActions = lens _wrsTimeoutActions (\s a -> s {_wrsTimeoutActions = a})

-- | Total number of Actions that have succeeded.
wrsSucceededActions :: Lens' WorkflowRunStatistics (Maybe Int)
wrsSucceededActions = lens _wrsSucceededActions (\s a -> s {_wrsSucceededActions = a})

instance FromJSON WorkflowRunStatistics where
  parseJSON =
    withObject
      "WorkflowRunStatistics"
      ( \x ->
          WorkflowRunStatistics'
            <$> (x .:? "RunningActions")
            <*> (x .:? "StoppedActions")
            <*> (x .:? "TotalActions")
            <*> (x .:? "FailedActions")
            <*> (x .:? "TimeoutActions")
            <*> (x .:? "SucceededActions")
      )

instance Hashable WorkflowRunStatistics

instance NFData WorkflowRunStatistics
