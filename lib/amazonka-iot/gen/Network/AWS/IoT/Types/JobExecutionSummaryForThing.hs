{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionSummaryForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionSummaryForThing where

import Network.AWS.IoT.Types.JobExecutionSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The job execution summary for a thing.
--
--
--
-- /See:/ 'jobExecutionSummaryForThing' smart constructor.
data JobExecutionSummaryForThing = JobExecutionSummaryForThing'
  { _jesftJobId ::
      !(Maybe Text),
    _jesftJobExecutionSummary ::
      !(Maybe JobExecutionSummary)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobExecutionSummaryForThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesftJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'jesftJobExecutionSummary' - Contains a subset of information about a job execution.
jobExecutionSummaryForThing ::
  JobExecutionSummaryForThing
jobExecutionSummaryForThing =
  JobExecutionSummaryForThing'
    { _jesftJobId = Nothing,
      _jesftJobExecutionSummary = Nothing
    }

-- | The unique identifier you assigned to this job when it was created.
jesftJobId :: Lens' JobExecutionSummaryForThing (Maybe Text)
jesftJobId = lens _jesftJobId (\s a -> s {_jesftJobId = a})

-- | Contains a subset of information about a job execution.
jesftJobExecutionSummary :: Lens' JobExecutionSummaryForThing (Maybe JobExecutionSummary)
jesftJobExecutionSummary = lens _jesftJobExecutionSummary (\s a -> s {_jesftJobExecutionSummary = a})

instance FromJSON JobExecutionSummaryForThing where
  parseJSON =
    withObject
      "JobExecutionSummaryForThing"
      ( \x ->
          JobExecutionSummaryForThing'
            <$> (x .:? "jobId") <*> (x .:? "jobExecutionSummary")
      )

instance Hashable JobExecutionSummaryForThing

instance NFData JobExecutionSummaryForThing
