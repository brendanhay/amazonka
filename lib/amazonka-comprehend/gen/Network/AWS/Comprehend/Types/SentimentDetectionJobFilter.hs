{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
--
--
-- /See:/ 'sentimentDetectionJobFilter' smart constructor.
data SentimentDetectionJobFilter = SentimentDetectionJobFilter'
  { _sdjfSubmitTimeAfter ::
      !(Maybe POSIX),
    _sdjfSubmitTimeBefore ::
      !(Maybe POSIX),
    _sdjfJobName :: !(Maybe Text),
    _sdjfJobStatus ::
      !(Maybe JobStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SentimentDetectionJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdjfSubmitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'sdjfSubmitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'sdjfJobName' - Filters on the name of the job.
--
-- * 'sdjfJobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
sentimentDetectionJobFilter ::
  SentimentDetectionJobFilter
sentimentDetectionJobFilter =
  SentimentDetectionJobFilter'
    { _sdjfSubmitTimeAfter = Nothing,
      _sdjfSubmitTimeBefore = Nothing,
      _sdjfJobName = Nothing,
      _sdjfJobStatus = Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
sdjfSubmitTimeAfter :: Lens' SentimentDetectionJobFilter (Maybe UTCTime)
sdjfSubmitTimeAfter = lens _sdjfSubmitTimeAfter (\s a -> s {_sdjfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
sdjfSubmitTimeBefore :: Lens' SentimentDetectionJobFilter (Maybe UTCTime)
sdjfSubmitTimeBefore = lens _sdjfSubmitTimeBefore (\s a -> s {_sdjfSubmitTimeBefore = a}) . mapping _Time

-- | Filters on the name of the job.
sdjfJobName :: Lens' SentimentDetectionJobFilter (Maybe Text)
sdjfJobName = lens _sdjfJobName (\s a -> s {_sdjfJobName = a})

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
sdjfJobStatus :: Lens' SentimentDetectionJobFilter (Maybe JobStatus)
sdjfJobStatus = lens _sdjfJobStatus (\s a -> s {_sdjfJobStatus = a})

instance Hashable SentimentDetectionJobFilter

instance NFData SentimentDetectionJobFilter

instance ToJSON SentimentDetectionJobFilter where
  toJSON SentimentDetectionJobFilter' {..} =
    object
      ( catMaybes
          [ ("SubmitTimeAfter" .=) <$> _sdjfSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _sdjfSubmitTimeBefore,
            ("JobName" .=) <$> _sdjfJobName,
            ("JobStatus" .=) <$> _sdjfJobStatus
          ]
      )
