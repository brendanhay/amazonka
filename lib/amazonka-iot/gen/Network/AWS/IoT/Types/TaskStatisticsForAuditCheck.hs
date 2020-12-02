{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatisticsForAuditCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatisticsForAuditCheck where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides summary counts of how many tasks for findings are in a particular state. This information is included in the response from DescribeAuditMitigationActionsTask.
--
--
--
-- /See:/ 'taskStatisticsForAuditCheck' smart constructor.
data TaskStatisticsForAuditCheck = TaskStatisticsForAuditCheck'
  { _tsfacCanceledFindingsCount ::
      !(Maybe Integer),
    _tsfacSkippedFindingsCount ::
      !(Maybe Integer),
    _tsfacTotalFindingsCount ::
      !(Maybe Integer),
    _tsfacFailedFindingsCount ::
      !(Maybe Integer),
    _tsfacSucceededFindingsCount ::
      !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskStatisticsForAuditCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsfacCanceledFindingsCount' - The number of findings to which the mitigation action task was canceled when applied.
--
-- * 'tsfacSkippedFindingsCount' - The number of findings skipped because of filter conditions provided in the parameters to the command.
--
-- * 'tsfacTotalFindingsCount' - The total number of findings to which a task is being applied.
--
-- * 'tsfacFailedFindingsCount' - The number of findings for which at least one of the actions failed when applied.
--
-- * 'tsfacSucceededFindingsCount' - The number of findings for which all mitigation actions succeeded when applied.
taskStatisticsForAuditCheck ::
  TaskStatisticsForAuditCheck
taskStatisticsForAuditCheck =
  TaskStatisticsForAuditCheck'
    { _tsfacCanceledFindingsCount =
        Nothing,
      _tsfacSkippedFindingsCount = Nothing,
      _tsfacTotalFindingsCount = Nothing,
      _tsfacFailedFindingsCount = Nothing,
      _tsfacSucceededFindingsCount = Nothing
    }

-- | The number of findings to which the mitigation action task was canceled when applied.
tsfacCanceledFindingsCount :: Lens' TaskStatisticsForAuditCheck (Maybe Integer)
tsfacCanceledFindingsCount = lens _tsfacCanceledFindingsCount (\s a -> s {_tsfacCanceledFindingsCount = a})

-- | The number of findings skipped because of filter conditions provided in the parameters to the command.
tsfacSkippedFindingsCount :: Lens' TaskStatisticsForAuditCheck (Maybe Integer)
tsfacSkippedFindingsCount = lens _tsfacSkippedFindingsCount (\s a -> s {_tsfacSkippedFindingsCount = a})

-- | The total number of findings to which a task is being applied.
tsfacTotalFindingsCount :: Lens' TaskStatisticsForAuditCheck (Maybe Integer)
tsfacTotalFindingsCount = lens _tsfacTotalFindingsCount (\s a -> s {_tsfacTotalFindingsCount = a})

-- | The number of findings for which at least one of the actions failed when applied.
tsfacFailedFindingsCount :: Lens' TaskStatisticsForAuditCheck (Maybe Integer)
tsfacFailedFindingsCount = lens _tsfacFailedFindingsCount (\s a -> s {_tsfacFailedFindingsCount = a})

-- | The number of findings for which all mitigation actions succeeded when applied.
tsfacSucceededFindingsCount :: Lens' TaskStatisticsForAuditCheck (Maybe Integer)
tsfacSucceededFindingsCount = lens _tsfacSucceededFindingsCount (\s a -> s {_tsfacSucceededFindingsCount = a})

instance FromJSON TaskStatisticsForAuditCheck where
  parseJSON =
    withObject
      "TaskStatisticsForAuditCheck"
      ( \x ->
          TaskStatisticsForAuditCheck'
            <$> (x .:? "canceledFindingsCount")
            <*> (x .:? "skippedFindingsCount")
            <*> (x .:? "totalFindingsCount")
            <*> (x .:? "failedFindingsCount")
            <*> (x .:? "succeededFindingsCount")
      )

instance Hashable TaskStatisticsForAuditCheck

instance NFData TaskStatisticsForAuditCheck
