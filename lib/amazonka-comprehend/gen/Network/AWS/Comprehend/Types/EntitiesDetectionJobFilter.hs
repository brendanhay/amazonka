{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
--
--
-- /See:/ 'entitiesDetectionJobFilter' smart constructor.
data EntitiesDetectionJobFilter = EntitiesDetectionJobFilter'
  { _edjfSubmitTimeAfter ::
      !(Maybe POSIX),
    _edjfSubmitTimeBefore ::
      !(Maybe POSIX),
    _edjfJobName :: !(Maybe Text),
    _edjfJobStatus :: !(Maybe JobStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntitiesDetectionJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edjfSubmitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'edjfSubmitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'edjfJobName' - Filters on the name of the job.
--
-- * 'edjfJobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
entitiesDetectionJobFilter ::
  EntitiesDetectionJobFilter
entitiesDetectionJobFilter =
  EntitiesDetectionJobFilter'
    { _edjfSubmitTimeAfter = Nothing,
      _edjfSubmitTimeBefore = Nothing,
      _edjfJobName = Nothing,
      _edjfJobStatus = Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
edjfSubmitTimeAfter :: Lens' EntitiesDetectionJobFilter (Maybe UTCTime)
edjfSubmitTimeAfter = lens _edjfSubmitTimeAfter (\s a -> s {_edjfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
edjfSubmitTimeBefore :: Lens' EntitiesDetectionJobFilter (Maybe UTCTime)
edjfSubmitTimeBefore = lens _edjfSubmitTimeBefore (\s a -> s {_edjfSubmitTimeBefore = a}) . mapping _Time

-- | Filters on the name of the job.
edjfJobName :: Lens' EntitiesDetectionJobFilter (Maybe Text)
edjfJobName = lens _edjfJobName (\s a -> s {_edjfJobName = a})

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
edjfJobStatus :: Lens' EntitiesDetectionJobFilter (Maybe JobStatus)
edjfJobStatus = lens _edjfJobStatus (\s a -> s {_edjfJobStatus = a})

instance Hashable EntitiesDetectionJobFilter

instance NFData EntitiesDetectionJobFilter

instance ToJSON EntitiesDetectionJobFilter where
  toJSON EntitiesDetectionJobFilter' {..} =
    object
      ( catMaybes
          [ ("SubmitTimeAfter" .=) <$> _edjfSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _edjfSubmitTimeBefore,
            ("JobName" .=) <$> _edjfJobName,
            ("JobStatus" .=) <$> _edjfJobStatus
          ]
      )
