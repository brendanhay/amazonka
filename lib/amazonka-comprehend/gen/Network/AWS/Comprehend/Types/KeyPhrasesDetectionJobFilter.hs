{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
--
--
-- /See:/ 'keyPhrasesDetectionJobFilter' smart constructor.
data KeyPhrasesDetectionJobFilter = KeyPhrasesDetectionJobFilter'
  { _kpdjfSubmitTimeAfter ::
      !(Maybe POSIX),
    _kpdjfSubmitTimeBefore ::
      !(Maybe POSIX),
    _kpdjfJobName :: !(Maybe Text),
    _kpdjfJobStatus ::
      !(Maybe JobStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyPhrasesDetectionJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpdjfSubmitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'kpdjfSubmitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'kpdjfJobName' - Filters on the name of the job.
--
-- * 'kpdjfJobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
keyPhrasesDetectionJobFilter ::
  KeyPhrasesDetectionJobFilter
keyPhrasesDetectionJobFilter =
  KeyPhrasesDetectionJobFilter'
    { _kpdjfSubmitTimeAfter = Nothing,
      _kpdjfSubmitTimeBefore = Nothing,
      _kpdjfJobName = Nothing,
      _kpdjfJobStatus = Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
kpdjfSubmitTimeAfter :: Lens' KeyPhrasesDetectionJobFilter (Maybe UTCTime)
kpdjfSubmitTimeAfter = lens _kpdjfSubmitTimeAfter (\s a -> s {_kpdjfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
kpdjfSubmitTimeBefore :: Lens' KeyPhrasesDetectionJobFilter (Maybe UTCTime)
kpdjfSubmitTimeBefore = lens _kpdjfSubmitTimeBefore (\s a -> s {_kpdjfSubmitTimeBefore = a}) . mapping _Time

-- | Filters on the name of the job.
kpdjfJobName :: Lens' KeyPhrasesDetectionJobFilter (Maybe Text)
kpdjfJobName = lens _kpdjfJobName (\s a -> s {_kpdjfJobName = a})

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
kpdjfJobStatus :: Lens' KeyPhrasesDetectionJobFilter (Maybe JobStatus)
kpdjfJobStatus = lens _kpdjfJobStatus (\s a -> s {_kpdjfJobStatus = a})

instance Hashable KeyPhrasesDetectionJobFilter

instance NFData KeyPhrasesDetectionJobFilter

instance ToJSON KeyPhrasesDetectionJobFilter where
  toJSON KeyPhrasesDetectionJobFilter' {..} =
    object
      ( catMaybes
          [ ("SubmitTimeAfter" .=) <$> _kpdjfSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _kpdjfSubmitTimeBefore,
            ("JobName" .=) <$> _kpdjfJobName,
            ("JobStatus" .=) <$> _kpdjfJobStatus
          ]
      )
