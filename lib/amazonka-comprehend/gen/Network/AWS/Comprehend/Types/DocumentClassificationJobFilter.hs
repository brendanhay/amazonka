{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassificationJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassificationJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information for filtering a list of document classification jobs. For more information, see the operation. You can provide only one filter parameter in each request.
--
--
--
-- /See:/ 'documentClassificationJobFilter' smart constructor.
data DocumentClassificationJobFilter = DocumentClassificationJobFilter'
  { _dcjfSubmitTimeAfter ::
      !(Maybe POSIX),
    _dcjfSubmitTimeBefore ::
      !(Maybe POSIX),
    _dcjfJobName ::
      !(Maybe Text),
    _dcjfJobStatus ::
      !(Maybe JobStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentClassificationJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcjfSubmitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'dcjfSubmitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'dcjfJobName' - Filters on the name of the job.
--
-- * 'dcjfJobStatus' - Filters the list based on job status. Returns only jobs with the specified status.
documentClassificationJobFilter ::
  DocumentClassificationJobFilter
documentClassificationJobFilter =
  DocumentClassificationJobFilter'
    { _dcjfSubmitTimeAfter = Nothing,
      _dcjfSubmitTimeBefore = Nothing,
      _dcjfJobName = Nothing,
      _dcjfJobStatus = Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
dcjfSubmitTimeAfter :: Lens' DocumentClassificationJobFilter (Maybe UTCTime)
dcjfSubmitTimeAfter = lens _dcjfSubmitTimeAfter (\s a -> s {_dcjfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
dcjfSubmitTimeBefore :: Lens' DocumentClassificationJobFilter (Maybe UTCTime)
dcjfSubmitTimeBefore = lens _dcjfSubmitTimeBefore (\s a -> s {_dcjfSubmitTimeBefore = a}) . mapping _Time

-- | Filters on the name of the job.
dcjfJobName :: Lens' DocumentClassificationJobFilter (Maybe Text)
dcjfJobName = lens _dcjfJobName (\s a -> s {_dcjfJobName = a})

-- | Filters the list based on job status. Returns only jobs with the specified status.
dcjfJobStatus :: Lens' DocumentClassificationJobFilter (Maybe JobStatus)
dcjfJobStatus = lens _dcjfJobStatus (\s a -> s {_dcjfJobStatus = a})

instance Hashable DocumentClassificationJobFilter

instance NFData DocumentClassificationJobFilter

instance ToJSON DocumentClassificationJobFilter where
  toJSON DocumentClassificationJobFilter' {..} =
    object
      ( catMaybes
          [ ("SubmitTimeAfter" .=) <$> _dcjfSubmitTimeAfter,
            ("SubmitTimeBefore" .=) <$> _dcjfSubmitTimeBefore,
            ("JobName" .=) <$> _dcjfJobName,
            ("JobStatus" .=) <$> _dcjfJobStatus
          ]
      )
