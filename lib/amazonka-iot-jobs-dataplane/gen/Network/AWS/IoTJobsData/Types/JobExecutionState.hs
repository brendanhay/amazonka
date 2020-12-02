{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.JobExecutionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types.JobExecutionState where

import Network.AWS.IoTJobsData.Types.JobExecutionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains data about the state of a job execution.
--
--
--
-- /See:/ 'jobExecutionState' smart constructor.
data JobExecutionState = JobExecutionState'
  { _jesStatus ::
      !(Maybe JobExecutionStatus),
    _jesStatusDetails :: !(Maybe (Map Text (Text))),
    _jesVersionNumber :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobExecutionState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesStatus' - The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
--
-- * 'jesStatusDetails' - A collection of name/value pairs that describe the status of the job execution.
--
-- * 'jesVersionNumber' - The version of the job execution. Job execution versions are incremented each time they are updated by a device.
jobExecutionState ::
  JobExecutionState
jobExecutionState =
  JobExecutionState'
    { _jesStatus = Nothing,
      _jesStatusDetails = Nothing,
      _jesVersionNumber = Nothing
    }

-- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
jesStatus :: Lens' JobExecutionState (Maybe JobExecutionStatus)
jesStatus = lens _jesStatus (\s a -> s {_jesStatus = a})

-- | A collection of name/value pairs that describe the status of the job execution.
jesStatusDetails :: Lens' JobExecutionState (HashMap Text (Text))
jesStatusDetails = lens _jesStatusDetails (\s a -> s {_jesStatusDetails = a}) . _Default . _Map

-- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
jesVersionNumber :: Lens' JobExecutionState (Maybe Integer)
jesVersionNumber = lens _jesVersionNumber (\s a -> s {_jesVersionNumber = a})

instance FromJSON JobExecutionState where
  parseJSON =
    withObject
      "JobExecutionState"
      ( \x ->
          JobExecutionState'
            <$> (x .:? "status")
            <*> (x .:? "statusDetails" .!= mempty)
            <*> (x .:? "versionNumber")
      )

instance Hashable JobExecutionState

instance NFData JobExecutionState
