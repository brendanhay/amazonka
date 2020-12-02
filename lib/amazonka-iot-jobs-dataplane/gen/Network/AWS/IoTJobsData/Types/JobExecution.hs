{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.JobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types.JobExecution where

import Network.AWS.IoTJobsData.Types.JobExecutionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains data about a job execution.
--
--
--
-- /See:/ 'jobExecution' smart constructor.
data JobExecution = JobExecution'
  { _jeStatus ::
      !(Maybe JobExecutionStatus),
    _jeJobId :: !(Maybe Text),
    _jeLastUpdatedAt :: !(Maybe Integer),
    _jeApproximateSecondsBeforeTimedOut :: !(Maybe Integer),
    _jeQueuedAt :: !(Maybe Integer),
    _jeJobDocument :: !(Maybe Text),
    _jeStatusDetails :: !(Maybe (Map Text (Text))),
    _jeExecutionNumber :: !(Maybe Integer),
    _jeVersionNumber :: !(Maybe Integer),
    _jeStartedAt :: !(Maybe Integer),
    _jeThingName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jeStatus' - The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
--
-- * 'jeJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'jeLastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was last updated.
--
-- * 'jeApproximateSecondsBeforeTimedOut' - The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ .
--
-- * 'jeQueuedAt' - The time, in milliseconds since the epoch, when the job execution was enqueued.
--
-- * 'jeJobDocument' - The content of the job document.
--
-- * 'jeStatusDetails' - A collection of name/value pairs that describe the status of the job execution.
--
-- * 'jeExecutionNumber' - A number that identifies a particular job execution on a particular device. It can be used later in commands that return or update job execution information.
--
-- * 'jeVersionNumber' - The version of the job execution. Job execution versions are incremented each time they are updated by a device.
--
-- * 'jeStartedAt' - The time, in milliseconds since the epoch, when the job execution was started.
--
-- * 'jeThingName' - The name of the thing that is executing the job.
jobExecution ::
  JobExecution
jobExecution =
  JobExecution'
    { _jeStatus = Nothing,
      _jeJobId = Nothing,
      _jeLastUpdatedAt = Nothing,
      _jeApproximateSecondsBeforeTimedOut = Nothing,
      _jeQueuedAt = Nothing,
      _jeJobDocument = Nothing,
      _jeStatusDetails = Nothing,
      _jeExecutionNumber = Nothing,
      _jeVersionNumber = Nothing,
      _jeStartedAt = Nothing,
      _jeThingName = Nothing
    }

-- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
jeStatus :: Lens' JobExecution (Maybe JobExecutionStatus)
jeStatus = lens _jeStatus (\s a -> s {_jeStatus = a})

-- | The unique identifier you assigned to this job when it was created.
jeJobId :: Lens' JobExecution (Maybe Text)
jeJobId = lens _jeJobId (\s a -> s {_jeJobId = a})

-- | The time, in milliseconds since the epoch, when the job execution was last updated.
jeLastUpdatedAt :: Lens' JobExecution (Maybe Integer)
jeLastUpdatedAt = lens _jeLastUpdatedAt (\s a -> s {_jeLastUpdatedAt = a})

-- | The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ .
jeApproximateSecondsBeforeTimedOut :: Lens' JobExecution (Maybe Integer)
jeApproximateSecondsBeforeTimedOut = lens _jeApproximateSecondsBeforeTimedOut (\s a -> s {_jeApproximateSecondsBeforeTimedOut = a})

-- | The time, in milliseconds since the epoch, when the job execution was enqueued.
jeQueuedAt :: Lens' JobExecution (Maybe Integer)
jeQueuedAt = lens _jeQueuedAt (\s a -> s {_jeQueuedAt = a})

-- | The content of the job document.
jeJobDocument :: Lens' JobExecution (Maybe Text)
jeJobDocument = lens _jeJobDocument (\s a -> s {_jeJobDocument = a})

-- | A collection of name/value pairs that describe the status of the job execution.
jeStatusDetails :: Lens' JobExecution (HashMap Text (Text))
jeStatusDetails = lens _jeStatusDetails (\s a -> s {_jeStatusDetails = a}) . _Default . _Map

-- | A number that identifies a particular job execution on a particular device. It can be used later in commands that return or update job execution information.
jeExecutionNumber :: Lens' JobExecution (Maybe Integer)
jeExecutionNumber = lens _jeExecutionNumber (\s a -> s {_jeExecutionNumber = a})

-- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
jeVersionNumber :: Lens' JobExecution (Maybe Integer)
jeVersionNumber = lens _jeVersionNumber (\s a -> s {_jeVersionNumber = a})

-- | The time, in milliseconds since the epoch, when the job execution was started.
jeStartedAt :: Lens' JobExecution (Maybe Integer)
jeStartedAt = lens _jeStartedAt (\s a -> s {_jeStartedAt = a})

-- | The name of the thing that is executing the job.
jeThingName :: Lens' JobExecution (Maybe Text)
jeThingName = lens _jeThingName (\s a -> s {_jeThingName = a})

instance FromJSON JobExecution where
  parseJSON =
    withObject
      "JobExecution"
      ( \x ->
          JobExecution'
            <$> (x .:? "status")
            <*> (x .:? "jobId")
            <*> (x .:? "lastUpdatedAt")
            <*> (x .:? "approximateSecondsBeforeTimedOut")
            <*> (x .:? "queuedAt")
            <*> (x .:? "jobDocument")
            <*> (x .:? "statusDetails" .!= mempty)
            <*> (x .:? "executionNumber")
            <*> (x .:? "versionNumber")
            <*> (x .:? "startedAt")
            <*> (x .:? "thingName")
      )

instance Hashable JobExecution

instance NFData JobExecution
