{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobSummary where

import Network.AWS.IoT.Types.JobStatus
import Network.AWS.IoT.Types.TargetSelection
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The job summary.
--
--
--
-- /See:/ 'jobSummary' smart constructor.
data JobSummary = JobSummary'
  { _jsStatus :: !(Maybe JobStatus),
    _jsJobId :: !(Maybe Text),
    _jsLastUpdatedAt :: !(Maybe POSIX),
    _jsJobARN :: !(Maybe Text),
    _jsCreatedAt :: !(Maybe POSIX),
    _jsThingGroupId :: !(Maybe Text),
    _jsCompletedAt :: !(Maybe POSIX),
    _jsTargetSelection :: !(Maybe TargetSelection)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsStatus' - The job summary status.
--
-- * 'jsJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'jsLastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
--
-- * 'jsJobARN' - The job ARN.
--
-- * 'jsCreatedAt' - The time, in seconds since the epoch, when the job was created.
--
-- * 'jsThingGroupId' - The ID of the thing group.
--
-- * 'jsCompletedAt' - The time, in seconds since the epoch, when the job completed.
--
-- * 'jsTargetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
jobSummary ::
  JobSummary
jobSummary =
  JobSummary'
    { _jsStatus = Nothing,
      _jsJobId = Nothing,
      _jsLastUpdatedAt = Nothing,
      _jsJobARN = Nothing,
      _jsCreatedAt = Nothing,
      _jsThingGroupId = Nothing,
      _jsCompletedAt = Nothing,
      _jsTargetSelection = Nothing
    }

-- | The job summary status.
jsStatus :: Lens' JobSummary (Maybe JobStatus)
jsStatus = lens _jsStatus (\s a -> s {_jsStatus = a})

-- | The unique identifier you assigned to this job when it was created.
jsJobId :: Lens' JobSummary (Maybe Text)
jsJobId = lens _jsJobId (\s a -> s {_jsJobId = a})

-- | The time, in seconds since the epoch, when the job was last updated.
jsLastUpdatedAt :: Lens' JobSummary (Maybe UTCTime)
jsLastUpdatedAt = lens _jsLastUpdatedAt (\s a -> s {_jsLastUpdatedAt = a}) . mapping _Time

-- | The job ARN.
jsJobARN :: Lens' JobSummary (Maybe Text)
jsJobARN = lens _jsJobARN (\s a -> s {_jsJobARN = a})

-- | The time, in seconds since the epoch, when the job was created.
jsCreatedAt :: Lens' JobSummary (Maybe UTCTime)
jsCreatedAt = lens _jsCreatedAt (\s a -> s {_jsCreatedAt = a}) . mapping _Time

-- | The ID of the thing group.
jsThingGroupId :: Lens' JobSummary (Maybe Text)
jsThingGroupId = lens _jsThingGroupId (\s a -> s {_jsThingGroupId = a})

-- | The time, in seconds since the epoch, when the job completed.
jsCompletedAt :: Lens' JobSummary (Maybe UTCTime)
jsCompletedAt = lens _jsCompletedAt (\s a -> s {_jsCompletedAt = a}) . mapping _Time

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
jsTargetSelection :: Lens' JobSummary (Maybe TargetSelection)
jsTargetSelection = lens _jsTargetSelection (\s a -> s {_jsTargetSelection = a})

instance FromJSON JobSummary where
  parseJSON =
    withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            <$> (x .:? "status")
            <*> (x .:? "jobId")
            <*> (x .:? "lastUpdatedAt")
            <*> (x .:? "jobArn")
            <*> (x .:? "createdAt")
            <*> (x .:? "thingGroupId")
            <*> (x .:? "completedAt")
            <*> (x .:? "targetSelection")
      )

instance Hashable JobSummary

instance NFData JobSummary
