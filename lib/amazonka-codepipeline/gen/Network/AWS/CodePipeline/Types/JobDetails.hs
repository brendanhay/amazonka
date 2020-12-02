{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.JobDetails where

import Network.AWS.CodePipeline.Types.JobData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the details of a job.
--
--
--
-- /See:/ 'jobDetails' smart constructor.
data JobDetails = JobDetails'
  { _jdData :: !(Maybe JobData),
    _jdAccountId :: !(Maybe Text),
    _jdId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdData' - Represents other information about a job required for a job worker to complete the job.
--
-- * 'jdAccountId' - The AWS account ID associated with the job.
--
-- * 'jdId' - The unique system-generated ID of the job.
jobDetails ::
  JobDetails
jobDetails =
  JobDetails'
    { _jdData = Nothing,
      _jdAccountId = Nothing,
      _jdId = Nothing
    }

-- | Represents other information about a job required for a job worker to complete the job.
jdData :: Lens' JobDetails (Maybe JobData)
jdData = lens _jdData (\s a -> s {_jdData = a})

-- | The AWS account ID associated with the job.
jdAccountId :: Lens' JobDetails (Maybe Text)
jdAccountId = lens _jdAccountId (\s a -> s {_jdAccountId = a})

-- | The unique system-generated ID of the job.
jdId :: Lens' JobDetails (Maybe Text)
jdId = lens _jdId (\s a -> s {_jdId = a})

instance FromJSON JobDetails where
  parseJSON =
    withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            <$> (x .:? "data") <*> (x .:? "accountId") <*> (x .:? "id")
      )

instance Hashable JobDetails

instance NFData JobDetails
