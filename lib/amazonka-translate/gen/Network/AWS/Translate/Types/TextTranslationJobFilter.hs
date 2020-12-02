{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TextTranslationJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TextTranslationJobFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Translate.Types.JobStatus

-- | Provides information for filtering a list of translation jobs. For more information, see 'ListTextTranslationJobs' .
--
--
--
-- /See:/ 'textTranslationJobFilter' smart constructor.
data TextTranslationJobFilter = TextTranslationJobFilter'
  { _ttjfSubmittedBeforeTime ::
      !(Maybe POSIX),
    _ttjfSubmittedAfterTime :: !(Maybe POSIX),
    _ttjfJobName :: !(Maybe Text),
    _ttjfJobStatus :: !(Maybe JobStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TextTranslationJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttjfSubmittedBeforeTime' - Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'ttjfSubmittedAfterTime' - Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'ttjfJobName' - Filters the list of jobs by name.
--
-- * 'ttjfJobStatus' - Filters the list of jobs based by job status.
textTranslationJobFilter ::
  TextTranslationJobFilter
textTranslationJobFilter =
  TextTranslationJobFilter'
    { _ttjfSubmittedBeforeTime = Nothing,
      _ttjfSubmittedAfterTime = Nothing,
      _ttjfJobName = Nothing,
      _ttjfJobStatus = Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
ttjfSubmittedBeforeTime :: Lens' TextTranslationJobFilter (Maybe UTCTime)
ttjfSubmittedBeforeTime = lens _ttjfSubmittedBeforeTime (\s a -> s {_ttjfSubmittedBeforeTime = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
ttjfSubmittedAfterTime :: Lens' TextTranslationJobFilter (Maybe UTCTime)
ttjfSubmittedAfterTime = lens _ttjfSubmittedAfterTime (\s a -> s {_ttjfSubmittedAfterTime = a}) . mapping _Time

-- | Filters the list of jobs by name.
ttjfJobName :: Lens' TextTranslationJobFilter (Maybe Text)
ttjfJobName = lens _ttjfJobName (\s a -> s {_ttjfJobName = a})

-- | Filters the list of jobs based by job status.
ttjfJobStatus :: Lens' TextTranslationJobFilter (Maybe JobStatus)
ttjfJobStatus = lens _ttjfJobStatus (\s a -> s {_ttjfJobStatus = a})

instance Hashable TextTranslationJobFilter

instance NFData TextTranslationJobFilter

instance ToJSON TextTranslationJobFilter where
  toJSON TextTranslationJobFilter' {..} =
    object
      ( catMaybes
          [ ("SubmittedBeforeTime" .=) <$> _ttjfSubmittedBeforeTime,
            ("SubmittedAfterTime" .=) <$> _ttjfSubmittedAfterTime,
            ("JobName" .=) <$> _ttjfJobName,
            ("JobStatus" .=) <$> _ttjfJobStatus
          ]
      )
