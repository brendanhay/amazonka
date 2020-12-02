{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTJobsData.Types.Product where

import Network.AWS.IoTJobsData.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains data about a job execution.
--
--
--
-- /See:/ 'jobExecution' smart constructor.
data JobExecution = JobExecution'
  { _jeStatus          :: !(Maybe JobExecutionStatus)
  , _jeJobId           :: !(Maybe Text)
  , _jeLastUpdatedAt   :: !(Maybe Integer)
  , _jeQueuedAt        :: !(Maybe Integer)
  , _jeJobDocument     :: !(Maybe Text)
  , _jeStatusDetails   :: !(Maybe (Map Text Text))
  , _jeExecutionNumber :: !(Maybe Integer)
  , _jeVersionNumber   :: !(Maybe Integer)
  , _jeStartedAt       :: !(Maybe Integer)
  , _jeThingName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
jobExecution
    :: JobExecution
jobExecution =
  JobExecution'
    { _jeStatus = Nothing
    , _jeJobId = Nothing
    , _jeLastUpdatedAt = Nothing
    , _jeQueuedAt = Nothing
    , _jeJobDocument = Nothing
    , _jeStatusDetails = Nothing
    , _jeExecutionNumber = Nothing
    , _jeVersionNumber = Nothing
    , _jeStartedAt = Nothing
    , _jeThingName = Nothing
    }


-- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
jeStatus :: Lens' JobExecution (Maybe JobExecutionStatus)
jeStatus = lens _jeStatus (\ s a -> s{_jeStatus = a})

-- | The unique identifier you assigned to this job when it was created.
jeJobId :: Lens' JobExecution (Maybe Text)
jeJobId = lens _jeJobId (\ s a -> s{_jeJobId = a})

-- | The time, in milliseconds since the epoch, when the job execution was last updated.
jeLastUpdatedAt :: Lens' JobExecution (Maybe Integer)
jeLastUpdatedAt = lens _jeLastUpdatedAt (\ s a -> s{_jeLastUpdatedAt = a})

-- | The time, in milliseconds since the epoch, when the job execution was enqueued.
jeQueuedAt :: Lens' JobExecution (Maybe Integer)
jeQueuedAt = lens _jeQueuedAt (\ s a -> s{_jeQueuedAt = a})

-- | The content of the job document.
jeJobDocument :: Lens' JobExecution (Maybe Text)
jeJobDocument = lens _jeJobDocument (\ s a -> s{_jeJobDocument = a})

-- | A collection of name/value pairs that describe the status of the job execution.
jeStatusDetails :: Lens' JobExecution (HashMap Text Text)
jeStatusDetails = lens _jeStatusDetails (\ s a -> s{_jeStatusDetails = a}) . _Default . _Map

-- | A number that identifies a particular job execution on a particular device. It can be used later in commands that return or update job execution information.
jeExecutionNumber :: Lens' JobExecution (Maybe Integer)
jeExecutionNumber = lens _jeExecutionNumber (\ s a -> s{_jeExecutionNumber = a})

-- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
jeVersionNumber :: Lens' JobExecution (Maybe Integer)
jeVersionNumber = lens _jeVersionNumber (\ s a -> s{_jeVersionNumber = a})

-- | The time, in milliseconds since the epoch, when the job execution was started.
jeStartedAt :: Lens' JobExecution (Maybe Integer)
jeStartedAt = lens _jeStartedAt (\ s a -> s{_jeStartedAt = a})

-- | The name of the thing that is executing the job.
jeThingName :: Lens' JobExecution (Maybe Text)
jeThingName = lens _jeThingName (\ s a -> s{_jeThingName = a})

instance FromJSON JobExecution where
        parseJSON
          = withObject "JobExecution"
              (\ x ->
                 JobExecution' <$>
                   (x .:? "status") <*> (x .:? "jobId") <*>
                     (x .:? "lastUpdatedAt")
                     <*> (x .:? "queuedAt")
                     <*> (x .:? "jobDocument")
                     <*> (x .:? "statusDetails" .!= mempty)
                     <*> (x .:? "executionNumber")
                     <*> (x .:? "versionNumber")
                     <*> (x .:? "startedAt")
                     <*> (x .:? "thingName"))

instance Hashable JobExecution where

instance NFData JobExecution where

-- | Contains data about the state of a job execution.
--
--
--
-- /See:/ 'jobExecutionState' smart constructor.
data JobExecutionState = JobExecutionState'
  { _jesStatus        :: !(Maybe JobExecutionStatus)
  , _jesStatusDetails :: !(Maybe (Map Text Text))
  , _jesVersionNumber :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobExecutionState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesStatus' - The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
--
-- * 'jesStatusDetails' - A collection of name/value pairs that describe the status of the job execution.
--
-- * 'jesVersionNumber' - The version of the job execution. Job execution versions are incremented each time they are updated by a device.
jobExecutionState
    :: JobExecutionState
jobExecutionState =
  JobExecutionState'
    { _jesStatus = Nothing
    , _jesStatusDetails = Nothing
    , _jesVersionNumber = Nothing
    }


-- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
jesStatus :: Lens' JobExecutionState (Maybe JobExecutionStatus)
jesStatus = lens _jesStatus (\ s a -> s{_jesStatus = a})

-- | A collection of name/value pairs that describe the status of the job execution.
jesStatusDetails :: Lens' JobExecutionState (HashMap Text Text)
jesStatusDetails = lens _jesStatusDetails (\ s a -> s{_jesStatusDetails = a}) . _Default . _Map

-- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
jesVersionNumber :: Lens' JobExecutionState (Maybe Integer)
jesVersionNumber = lens _jesVersionNumber (\ s a -> s{_jesVersionNumber = a})

instance FromJSON JobExecutionState where
        parseJSON
          = withObject "JobExecutionState"
              (\ x ->
                 JobExecutionState' <$>
                   (x .:? "status") <*>
                     (x .:? "statusDetails" .!= mempty)
                     <*> (x .:? "versionNumber"))

instance Hashable JobExecutionState where

instance NFData JobExecutionState where

-- | Contains a subset of information about a job execution.
--
--
--
-- /See:/ 'jobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { _jJobId           :: !(Maybe Text)
  , _jLastUpdatedAt   :: !(Maybe Integer)
  , _jQueuedAt        :: !(Maybe Integer)
  , _jExecutionNumber :: !(Maybe Integer)
  , _jVersionNumber   :: !(Maybe Integer)
  , _jStartedAt       :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobExecutionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'jLastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was last updated.
--
-- * 'jQueuedAt' - The time, in milliseconds since the epoch, when the job execution was enqueued.
--
-- * 'jExecutionNumber' - A number that identifies a particular job execution on a particular device.
--
-- * 'jVersionNumber' - The version of the job execution. Job execution versions are incremented each time AWS IoT Jobs receives an update from a device.
--
-- * 'jStartedAt' - The time, in milliseconds since the epoch, when the job execution started.
jobExecutionSummary
    :: JobExecutionSummary
jobExecutionSummary =
  JobExecutionSummary'
    { _jJobId = Nothing
    , _jLastUpdatedAt = Nothing
    , _jQueuedAt = Nothing
    , _jExecutionNumber = Nothing
    , _jVersionNumber = Nothing
    , _jStartedAt = Nothing
    }


-- | The unique identifier you assigned to this job when it was created.
jJobId :: Lens' JobExecutionSummary (Maybe Text)
jJobId = lens _jJobId (\ s a -> s{_jJobId = a})

-- | The time, in milliseconds since the epoch, when the job execution was last updated.
jLastUpdatedAt :: Lens' JobExecutionSummary (Maybe Integer)
jLastUpdatedAt = lens _jLastUpdatedAt (\ s a -> s{_jLastUpdatedAt = a})

-- | The time, in milliseconds since the epoch, when the job execution was enqueued.
jQueuedAt :: Lens' JobExecutionSummary (Maybe Integer)
jQueuedAt = lens _jQueuedAt (\ s a -> s{_jQueuedAt = a})

-- | A number that identifies a particular job execution on a particular device.
jExecutionNumber :: Lens' JobExecutionSummary (Maybe Integer)
jExecutionNumber = lens _jExecutionNumber (\ s a -> s{_jExecutionNumber = a})

-- | The version of the job execution. Job execution versions are incremented each time AWS IoT Jobs receives an update from a device.
jVersionNumber :: Lens' JobExecutionSummary (Maybe Integer)
jVersionNumber = lens _jVersionNumber (\ s a -> s{_jVersionNumber = a})

-- | The time, in milliseconds since the epoch, when the job execution started.
jStartedAt :: Lens' JobExecutionSummary (Maybe Integer)
jStartedAt = lens _jStartedAt (\ s a -> s{_jStartedAt = a})

instance FromJSON JobExecutionSummary where
        parseJSON
          = withObject "JobExecutionSummary"
              (\ x ->
                 JobExecutionSummary' <$>
                   (x .:? "jobId") <*> (x .:? "lastUpdatedAt") <*>
                     (x .:? "queuedAt")
                     <*> (x .:? "executionNumber")
                     <*> (x .:? "versionNumber")
                     <*> (x .:? "startedAt"))

instance Hashable JobExecutionSummary where

instance NFData JobExecutionSummary where
