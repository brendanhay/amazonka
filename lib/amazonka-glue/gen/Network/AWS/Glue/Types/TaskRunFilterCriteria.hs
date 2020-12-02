{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunFilterCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunFilterCriteria where

import Network.AWS.Glue.Types.TaskStatusType
import Network.AWS.Glue.Types.TaskType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The criteria that are used to filter the task runs for the machine learning transform.
--
--
--
-- /See:/ 'taskRunFilterCriteria' smart constructor.
data TaskRunFilterCriteria = TaskRunFilterCriteria'
  { _trfcStatus ::
      !(Maybe TaskStatusType),
    _trfcStartedAfter :: !(Maybe POSIX),
    _trfcStartedBefore :: !(Maybe POSIX),
    _trfcTaskRunType :: !(Maybe TaskType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskRunFilterCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trfcStatus' - The current status of the task run.
--
-- * 'trfcStartedAfter' - Filter on task runs started after this date.
--
-- * 'trfcStartedBefore' - Filter on task runs started before this date.
--
-- * 'trfcTaskRunType' - The type of task run.
taskRunFilterCriteria ::
  TaskRunFilterCriteria
taskRunFilterCriteria =
  TaskRunFilterCriteria'
    { _trfcStatus = Nothing,
      _trfcStartedAfter = Nothing,
      _trfcStartedBefore = Nothing,
      _trfcTaskRunType = Nothing
    }

-- | The current status of the task run.
trfcStatus :: Lens' TaskRunFilterCriteria (Maybe TaskStatusType)
trfcStatus = lens _trfcStatus (\s a -> s {_trfcStatus = a})

-- | Filter on task runs started after this date.
trfcStartedAfter :: Lens' TaskRunFilterCriteria (Maybe UTCTime)
trfcStartedAfter = lens _trfcStartedAfter (\s a -> s {_trfcStartedAfter = a}) . mapping _Time

-- | Filter on task runs started before this date.
trfcStartedBefore :: Lens' TaskRunFilterCriteria (Maybe UTCTime)
trfcStartedBefore = lens _trfcStartedBefore (\s a -> s {_trfcStartedBefore = a}) . mapping _Time

-- | The type of task run.
trfcTaskRunType :: Lens' TaskRunFilterCriteria (Maybe TaskType)
trfcTaskRunType = lens _trfcTaskRunType (\s a -> s {_trfcTaskRunType = a})

instance Hashable TaskRunFilterCriteria

instance NFData TaskRunFilterCriteria

instance ToJSON TaskRunFilterCriteria where
  toJSON TaskRunFilterCriteria' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _trfcStatus,
            ("StartedAfter" .=) <$> _trfcStartedAfter,
            ("StartedBefore" .=) <$> _trfcStartedBefore,
            ("TaskRunType" .=) <$> _trfcTaskRunType
          ]
      )
