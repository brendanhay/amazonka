{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of PII entity detection jobs.
--
--
--
-- /See:/ 'piiEntitiesDetectionJobFilter' smart constructor.
data PiiEntitiesDetectionJobFilter = PiiEntitiesDetectionJobFilter'
  { _pedjfSubmitTimeAfter ::
      !(Maybe POSIX),
    _pedjfSubmitTimeBefore ::
      !(Maybe POSIX),
    _pedjfJobName :: !(Maybe Text),
    _pedjfJobStatus ::
      !(Maybe JobStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PiiEntitiesDetectionJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pedjfSubmitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'pedjfSubmitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'pedjfJobName' - Filters on the name of the job.
--
-- * 'pedjfJobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
piiEntitiesDetectionJobFilter ::
  PiiEntitiesDetectionJobFilter
piiEntitiesDetectionJobFilter =
  PiiEntitiesDetectionJobFilter'
    { _pedjfSubmitTimeAfter = Nothing,
      _pedjfSubmitTimeBefore = Nothing,
      _pedjfJobName = Nothing,
      _pedjfJobStatus = Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
pedjfSubmitTimeAfter :: Lens' PiiEntitiesDetectionJobFilter (Maybe UTCTime)
pedjfSubmitTimeAfter = lens _pedjfSubmitTimeAfter (\s a -> s {_pedjfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
pedjfSubmitTimeBefore :: Lens' PiiEntitiesDetectionJobFilter (Maybe UTCTime)
pedjfSubmitTimeBefore = lens _pedjfSubmitTimeBefore (\s a -> s {_pedjfSubmitTimeBefore = a}) . mapping _Time

-- | Filters on the name of the job.
pedjfJobName :: Lens' PiiEntitiesDetectionJobFilter (Maybe Text)
pedjfJobName = lens _pedjfJobName (\s a -> s {_pedjfJobName = a})

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
pedjfJobStatus :: Lens' PiiEntitiesDetectionJobFilter (Maybe JobStatus)
pedjfJobStatus = lens _pedjfJobStatus (\s a -> s {_pedjfJobStatus = a})

instance Hashable PiiEntitiesDetectionJobFilter

instance NFData PiiEntitiesDetectionJobFilter

instance ToJSON PiiEntitiesDetectionJobFilter where
  toJSON PiiEntitiesDetectionJobFilter' {..} =
    object
      ( catMaybes
          [ ("SubmitTimeAfter" .=) <$> _pedjfSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _pedjfSubmitTimeBefore,
            ("JobName" .=) <$> _pedjfJobName,
            ("JobStatus" .=) <$> _pedjfJobStatus
          ]
      )
