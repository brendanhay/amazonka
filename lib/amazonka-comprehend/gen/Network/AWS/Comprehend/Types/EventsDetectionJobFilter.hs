{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EventsDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EventsDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of event detection jobs.
--
--
--
-- /See:/ 'eventsDetectionJobFilter' smart constructor.
data EventsDetectionJobFilter = EventsDetectionJobFilter'
  { _eSubmitTimeAfter ::
      !(Maybe POSIX),
    _eSubmitTimeBefore :: !(Maybe POSIX),
    _eJobName :: !(Maybe Text),
    _eJobStatus :: !(Maybe JobStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventsDetectionJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSubmitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'eSubmitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'eJobName' - Filters on the name of the events detection job.
--
-- * 'eJobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
eventsDetectionJobFilter ::
  EventsDetectionJobFilter
eventsDetectionJobFilter =
  EventsDetectionJobFilter'
    { _eSubmitTimeAfter = Nothing,
      _eSubmitTimeBefore = Nothing,
      _eJobName = Nothing,
      _eJobStatus = Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
eSubmitTimeAfter :: Lens' EventsDetectionJobFilter (Maybe UTCTime)
eSubmitTimeAfter = lens _eSubmitTimeAfter (\s a -> s {_eSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
eSubmitTimeBefore :: Lens' EventsDetectionJobFilter (Maybe UTCTime)
eSubmitTimeBefore = lens _eSubmitTimeBefore (\s a -> s {_eSubmitTimeBefore = a}) . mapping _Time

-- | Filters on the name of the events detection job.
eJobName :: Lens' EventsDetectionJobFilter (Maybe Text)
eJobName = lens _eJobName (\s a -> s {_eJobName = a})

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
eJobStatus :: Lens' EventsDetectionJobFilter (Maybe JobStatus)
eJobStatus = lens _eJobStatus (\s a -> s {_eJobStatus = a})

instance Hashable EventsDetectionJobFilter

instance NFData EventsDetectionJobFilter

instance ToJSON EventsDetectionJobFilter where
  toJSON EventsDetectionJobFilter' {..} =
    object
      ( catMaybes
          [ ("SubmitTimeAfter" .=) <$> _eSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _eSubmitTimeBefore,
            ("JobName" .=) <$> _eJobName,
            ("JobStatus" .=) <$> _eJobStatus
          ]
      )
