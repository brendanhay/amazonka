{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
--
--
-- /See:/ 'dominantLanguageDetectionJobFilter' smart constructor.
data DominantLanguageDetectionJobFilter = DominantLanguageDetectionJobFilter'
  { _dldjfSubmitTimeAfter ::
      !(Maybe POSIX),
    _dldjfSubmitTimeBefore ::
      !(Maybe POSIX),
    _dldjfJobName ::
      !(Maybe Text),
    _dldjfJobStatus ::
      !(Maybe JobStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DominantLanguageDetectionJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dldjfSubmitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'dldjfSubmitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'dldjfJobName' - Filters on the name of the job.
--
-- * 'dldjfJobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
dominantLanguageDetectionJobFilter ::
  DominantLanguageDetectionJobFilter
dominantLanguageDetectionJobFilter =
  DominantLanguageDetectionJobFilter'
    { _dldjfSubmitTimeAfter =
        Nothing,
      _dldjfSubmitTimeBefore = Nothing,
      _dldjfJobName = Nothing,
      _dldjfJobStatus = Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
dldjfSubmitTimeAfter :: Lens' DominantLanguageDetectionJobFilter (Maybe UTCTime)
dldjfSubmitTimeAfter = lens _dldjfSubmitTimeAfter (\s a -> s {_dldjfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
dldjfSubmitTimeBefore :: Lens' DominantLanguageDetectionJobFilter (Maybe UTCTime)
dldjfSubmitTimeBefore = lens _dldjfSubmitTimeBefore (\s a -> s {_dldjfSubmitTimeBefore = a}) . mapping _Time

-- | Filters on the name of the job.
dldjfJobName :: Lens' DominantLanguageDetectionJobFilter (Maybe Text)
dldjfJobName = lens _dldjfJobName (\s a -> s {_dldjfJobName = a})

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
dldjfJobStatus :: Lens' DominantLanguageDetectionJobFilter (Maybe JobStatus)
dldjfJobStatus = lens _dldjfJobStatus (\s a -> s {_dldjfJobStatus = a})

instance Hashable DominantLanguageDetectionJobFilter

instance NFData DominantLanguageDetectionJobFilter

instance ToJSON DominantLanguageDetectionJobFilter where
  toJSON DominantLanguageDetectionJobFilter' {..} =
    object
      ( catMaybes
          [ ("SubmitTimeAfter" .=) <$> _dldjfSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _dldjfSubmitTimeBefore,
            ("JobName" .=) <$> _dldjfJobName,
            ("JobStatus" .=) <$> _dldjfJobStatus
          ]
      )
