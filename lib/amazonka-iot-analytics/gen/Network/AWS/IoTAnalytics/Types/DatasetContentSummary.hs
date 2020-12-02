{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentSummary where

import Network.AWS.IoTAnalytics.Types.DatasetContentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information about dataset contents.
--
--
--
-- /See:/ 'datasetContentSummary' smart constructor.
data DatasetContentSummary = DatasetContentSummary'
  { _dcsCreationTime ::
      !(Maybe POSIX),
    _dcsStatus :: !(Maybe DatasetContentStatus),
    _dcsScheduleTime :: !(Maybe POSIX),
    _dcsCompletionTime :: !(Maybe POSIX),
    _dcsVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetContentSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsCreationTime' - The actual time the creation of the dataset contents was started.
--
-- * 'dcsStatus' - The status of the data set contents.
--
-- * 'dcsScheduleTime' - The time the creation of the dataset contents was scheduled to start.
--
-- * 'dcsCompletionTime' - The time the dataset content status was updated to SUCCEEDED or FAILED.
--
-- * 'dcsVersion' - The version of the dataset contents.
datasetContentSummary ::
  DatasetContentSummary
datasetContentSummary =
  DatasetContentSummary'
    { _dcsCreationTime = Nothing,
      _dcsStatus = Nothing,
      _dcsScheduleTime = Nothing,
      _dcsCompletionTime = Nothing,
      _dcsVersion = Nothing
    }

-- | The actual time the creation of the dataset contents was started.
dcsCreationTime :: Lens' DatasetContentSummary (Maybe UTCTime)
dcsCreationTime = lens _dcsCreationTime (\s a -> s {_dcsCreationTime = a}) . mapping _Time

-- | The status of the data set contents.
dcsStatus :: Lens' DatasetContentSummary (Maybe DatasetContentStatus)
dcsStatus = lens _dcsStatus (\s a -> s {_dcsStatus = a})

-- | The time the creation of the dataset contents was scheduled to start.
dcsScheduleTime :: Lens' DatasetContentSummary (Maybe UTCTime)
dcsScheduleTime = lens _dcsScheduleTime (\s a -> s {_dcsScheduleTime = a}) . mapping _Time

-- | The time the dataset content status was updated to SUCCEEDED or FAILED.
dcsCompletionTime :: Lens' DatasetContentSummary (Maybe UTCTime)
dcsCompletionTime = lens _dcsCompletionTime (\s a -> s {_dcsCompletionTime = a}) . mapping _Time

-- | The version of the dataset contents.
dcsVersion :: Lens' DatasetContentSummary (Maybe Text)
dcsVersion = lens _dcsVersion (\s a -> s {_dcsVersion = a})

instance FromJSON DatasetContentSummary where
  parseJSON =
    withObject
      "DatasetContentSummary"
      ( \x ->
          DatasetContentSummary'
            <$> (x .:? "creationTime")
            <*> (x .:? "status")
            <*> (x .:? "scheduleTime")
            <*> (x .:? "completionTime")
            <*> (x .:? "version")
      )

instance Hashable DatasetContentSummary

instance NFData DatasetContentSummary
