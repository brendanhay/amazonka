{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionSummaryForJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionSummaryForJob where

import Network.AWS.IoT.Types.JobExecutionSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a summary of information about job executions for a specific job.
--
--
--
-- /See:/ 'jobExecutionSummaryForJob' smart constructor.
data JobExecutionSummaryForJob = JobExecutionSummaryForJob'
  { _jesfjJobExecutionSummary ::
      !(Maybe JobExecutionSummary),
    _jesfjThingARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobExecutionSummaryForJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesfjJobExecutionSummary' - Contains a subset of information about a job execution.
--
-- * 'jesfjThingARN' - The ARN of the thing on which the job execution is running.
jobExecutionSummaryForJob ::
  JobExecutionSummaryForJob
jobExecutionSummaryForJob =
  JobExecutionSummaryForJob'
    { _jesfjJobExecutionSummary = Nothing,
      _jesfjThingARN = Nothing
    }

-- | Contains a subset of information about a job execution.
jesfjJobExecutionSummary :: Lens' JobExecutionSummaryForJob (Maybe JobExecutionSummary)
jesfjJobExecutionSummary = lens _jesfjJobExecutionSummary (\s a -> s {_jesfjJobExecutionSummary = a})

-- | The ARN of the thing on which the job execution is running.
jesfjThingARN :: Lens' JobExecutionSummaryForJob (Maybe Text)
jesfjThingARN = lens _jesfjThingARN (\s a -> s {_jesfjThingARN = a})

instance FromJSON JobExecutionSummaryForJob where
  parseJSON =
    withObject
      "JobExecutionSummaryForJob"
      ( \x ->
          JobExecutionSummaryForJob'
            <$> (x .:? "jobExecutionSummary") <*> (x .:? "thingArn")
      )

instance Hashable JobExecutionSummaryForJob

instance NFData JobExecutionSummaryForJob
