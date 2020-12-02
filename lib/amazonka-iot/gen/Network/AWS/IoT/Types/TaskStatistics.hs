{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Statistics for the checks performed during the audit.
--
--
--
-- /See:/ 'taskStatistics' smart constructor.
data TaskStatistics = TaskStatistics'
  { _tsNonCompliantChecks ::
      !(Maybe Int),
    _tsWaitingForDataCollectionChecks :: !(Maybe Int),
    _tsFailedChecks :: !(Maybe Int),
    _tsTotalChecks :: !(Maybe Int),
    _tsInProgressChecks :: !(Maybe Int),
    _tsCompliantChecks :: !(Maybe Int),
    _tsCanceledChecks :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsNonCompliantChecks' - The number of checks that found noncompliant resources.
--
-- * 'tsWaitingForDataCollectionChecks' - The number of checks waiting for data collection.
--
-- * 'tsFailedChecks' - The number of checks.
--
-- * 'tsTotalChecks' - The number of checks in this audit.
--
-- * 'tsInProgressChecks' - The number of checks in progress.
--
-- * 'tsCompliantChecks' - The number of checks that found compliant resources.
--
-- * 'tsCanceledChecks' - The number of checks that did not run because the audit was canceled.
taskStatistics ::
  TaskStatistics
taskStatistics =
  TaskStatistics'
    { _tsNonCompliantChecks = Nothing,
      _tsWaitingForDataCollectionChecks = Nothing,
      _tsFailedChecks = Nothing,
      _tsTotalChecks = Nothing,
      _tsInProgressChecks = Nothing,
      _tsCompliantChecks = Nothing,
      _tsCanceledChecks = Nothing
    }

-- | The number of checks that found noncompliant resources.
tsNonCompliantChecks :: Lens' TaskStatistics (Maybe Int)
tsNonCompliantChecks = lens _tsNonCompliantChecks (\s a -> s {_tsNonCompliantChecks = a})

-- | The number of checks waiting for data collection.
tsWaitingForDataCollectionChecks :: Lens' TaskStatistics (Maybe Int)
tsWaitingForDataCollectionChecks = lens _tsWaitingForDataCollectionChecks (\s a -> s {_tsWaitingForDataCollectionChecks = a})

-- | The number of checks.
tsFailedChecks :: Lens' TaskStatistics (Maybe Int)
tsFailedChecks = lens _tsFailedChecks (\s a -> s {_tsFailedChecks = a})

-- | The number of checks in this audit.
tsTotalChecks :: Lens' TaskStatistics (Maybe Int)
tsTotalChecks = lens _tsTotalChecks (\s a -> s {_tsTotalChecks = a})

-- | The number of checks in progress.
tsInProgressChecks :: Lens' TaskStatistics (Maybe Int)
tsInProgressChecks = lens _tsInProgressChecks (\s a -> s {_tsInProgressChecks = a})

-- | The number of checks that found compliant resources.
tsCompliantChecks :: Lens' TaskStatistics (Maybe Int)
tsCompliantChecks = lens _tsCompliantChecks (\s a -> s {_tsCompliantChecks = a})

-- | The number of checks that did not run because the audit was canceled.
tsCanceledChecks :: Lens' TaskStatistics (Maybe Int)
tsCanceledChecks = lens _tsCanceledChecks (\s a -> s {_tsCanceledChecks = a})

instance FromJSON TaskStatistics where
  parseJSON =
    withObject
      "TaskStatistics"
      ( \x ->
          TaskStatistics'
            <$> (x .:? "nonCompliantChecks")
            <*> (x .:? "waitingForDataCollectionChecks")
            <*> (x .:? "failedChecks")
            <*> (x .:? "totalChecks")
            <*> (x .:? "inProgressChecks")
            <*> (x .:? "compliantChecks")
            <*> (x .:? "canceledChecks")
      )

instance Hashable TaskStatistics

instance NFData TaskStatistics
