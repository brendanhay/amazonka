{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobListEntry where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.JobState
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.SnowballType

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of an export job.
--
--
--
-- /See:/ 'jobListEntry' smart constructor.
data JobListEntry = JobListEntry'
  { _jleJobType :: !(Maybe JobType),
    _jleJobId :: !(Maybe Text),
    _jleJobState :: !(Maybe JobState),
    _jleSnowballType :: !(Maybe SnowballType),
    _jleCreationDate :: !(Maybe POSIX),
    _jleDescription :: !(Maybe Text),
    _jleIsMaster :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jleJobType' - The type of job.
--
-- * 'jleJobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'jleJobState' - The current state of this job.
--
-- * 'jleSnowballType' - The type of device used with this job.
--
-- * 'jleCreationDate' - The creation date for this job.
--
-- * 'jleDescription' - The optional description of this specific job, for example @Important Photos 2016-08-11@ .
--
-- * 'jleIsMaster' - A value that indicates that this job is a main job. A main job represents a successful request to create an export job. Main jobs aren't associated with any Snowballs. Instead, each main job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular main job are listed, because they are created after the main job is created.
jobListEntry ::
  JobListEntry
jobListEntry =
  JobListEntry'
    { _jleJobType = Nothing,
      _jleJobId = Nothing,
      _jleJobState = Nothing,
      _jleSnowballType = Nothing,
      _jleCreationDate = Nothing,
      _jleDescription = Nothing,
      _jleIsMaster = Nothing
    }

-- | The type of job.
jleJobType :: Lens' JobListEntry (Maybe JobType)
jleJobType = lens _jleJobType (\s a -> s {_jleJobType = a})

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
jleJobId :: Lens' JobListEntry (Maybe Text)
jleJobId = lens _jleJobId (\s a -> s {_jleJobId = a})

-- | The current state of this job.
jleJobState :: Lens' JobListEntry (Maybe JobState)
jleJobState = lens _jleJobState (\s a -> s {_jleJobState = a})

-- | The type of device used with this job.
jleSnowballType :: Lens' JobListEntry (Maybe SnowballType)
jleSnowballType = lens _jleSnowballType (\s a -> s {_jleSnowballType = a})

-- | The creation date for this job.
jleCreationDate :: Lens' JobListEntry (Maybe UTCTime)
jleCreationDate = lens _jleCreationDate (\s a -> s {_jleCreationDate = a}) . mapping _Time

-- | The optional description of this specific job, for example @Important Photos 2016-08-11@ .
jleDescription :: Lens' JobListEntry (Maybe Text)
jleDescription = lens _jleDescription (\s a -> s {_jleDescription = a})

-- | A value that indicates that this job is a main job. A main job represents a successful request to create an export job. Main jobs aren't associated with any Snowballs. Instead, each main job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular main job are listed, because they are created after the main job is created.
jleIsMaster :: Lens' JobListEntry (Maybe Bool)
jleIsMaster = lens _jleIsMaster (\s a -> s {_jleIsMaster = a})

instance FromJSON JobListEntry where
  parseJSON =
    withObject
      "JobListEntry"
      ( \x ->
          JobListEntry'
            <$> (x .:? "JobType")
            <*> (x .:? "JobId")
            <*> (x .:? "JobState")
            <*> (x .:? "SnowballType")
            <*> (x .:? "CreationDate")
            <*> (x .:? "Description")
            <*> (x .:? "IsMaster")
      )

instance Hashable JobListEntry

instance NFData JobListEntry
