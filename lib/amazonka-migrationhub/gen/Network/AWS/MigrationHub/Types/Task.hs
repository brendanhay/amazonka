{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.Task
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.Task where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types.MigrationStatus
import Network.AWS.Prelude

-- | Task object encapsulating task information.
--
--
--
-- /See:/ 'task' smart constructor.
data Task = Task'
  { _tProgressPercent :: !(Maybe Nat),
    _tStatusDetail :: !(Maybe Text),
    _tStatus :: !MigrationStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Task' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tProgressPercent' - Indication of the percentage completion of the task.
--
-- * 'tStatusDetail' - Details of task status as notified by a migration tool. A tool might use this field to provide clarifying information about the status that is unique to that tool or that explains an error state.
--
-- * 'tStatus' - Status of the task - Not Started, In-Progress, Complete.
task ::
  -- | 'tStatus'
  MigrationStatus ->
  Task
task pStatus_ =
  Task'
    { _tProgressPercent = Nothing,
      _tStatusDetail = Nothing,
      _tStatus = pStatus_
    }

-- | Indication of the percentage completion of the task.
tProgressPercent :: Lens' Task (Maybe Natural)
tProgressPercent = lens _tProgressPercent (\s a -> s {_tProgressPercent = a}) . mapping _Nat

-- | Details of task status as notified by a migration tool. A tool might use this field to provide clarifying information about the status that is unique to that tool or that explains an error state.
tStatusDetail :: Lens' Task (Maybe Text)
tStatusDetail = lens _tStatusDetail (\s a -> s {_tStatusDetail = a})

-- | Status of the task - Not Started, In-Progress, Complete.
tStatus :: Lens' Task MigrationStatus
tStatus = lens _tStatus (\s a -> s {_tStatus = a})

instance FromJSON Task where
  parseJSON =
    withObject
      "Task"
      ( \x ->
          Task'
            <$> (x .:? "ProgressPercent")
            <*> (x .:? "StatusDetail")
            <*> (x .: "Status")
      )

instance Hashable Task

instance NFData Task

instance ToJSON Task where
  toJSON Task' {..} =
    object
      ( catMaybes
          [ ("ProgressPercent" .=) <$> _tProgressPercent,
            ("StatusDetail" .=) <$> _tStatusDetail,
            Just ("Status" .= _tStatus)
          ]
      )
