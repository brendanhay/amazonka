{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.JobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.JobDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The number of documents successfully and unsuccessfully processed during a translation job.
--
--
--
-- /See:/ 'jobDetails' smart constructor.
data JobDetails = JobDetails'
  { _jdTranslatedDocumentsCount ::
      !(Maybe Int),
    _jdDocumentsWithErrorsCount :: !(Maybe Int),
    _jdInputDocumentsCount :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdTranslatedDocumentsCount' - The number of documents successfully processed during a translation job.
--
-- * 'jdDocumentsWithErrorsCount' - The number of documents that could not be processed during a translation job.
--
-- * 'jdInputDocumentsCount' - The number of documents used as input in a translation job.
jobDetails ::
  JobDetails
jobDetails =
  JobDetails'
    { _jdTranslatedDocumentsCount = Nothing,
      _jdDocumentsWithErrorsCount = Nothing,
      _jdInputDocumentsCount = Nothing
    }

-- | The number of documents successfully processed during a translation job.
jdTranslatedDocumentsCount :: Lens' JobDetails (Maybe Int)
jdTranslatedDocumentsCount = lens _jdTranslatedDocumentsCount (\s a -> s {_jdTranslatedDocumentsCount = a})

-- | The number of documents that could not be processed during a translation job.
jdDocumentsWithErrorsCount :: Lens' JobDetails (Maybe Int)
jdDocumentsWithErrorsCount = lens _jdDocumentsWithErrorsCount (\s a -> s {_jdDocumentsWithErrorsCount = a})

-- | The number of documents used as input in a translation job.
jdInputDocumentsCount :: Lens' JobDetails (Maybe Int)
jdInputDocumentsCount = lens _jdInputDocumentsCount (\s a -> s {_jdInputDocumentsCount = a})

instance FromJSON JobDetails where
  parseJSON =
    withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            <$> (x .:? "TranslatedDocumentsCount")
            <*> (x .:? "DocumentsWithErrorsCount")
            <*> (x .:? "InputDocumentsCount")
      )

instance Hashable JobDetails

instance NFData JobDetails
