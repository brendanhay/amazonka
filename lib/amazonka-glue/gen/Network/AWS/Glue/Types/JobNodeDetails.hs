{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobNodeDetails where

import Network.AWS.Glue.Types.JobRun
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of a Job node present in the workflow.
--
--
--
-- /See:/ 'jobNodeDetails' smart constructor.
newtype JobNodeDetails = JobNodeDetails'
  { _jndJobRuns ::
      Maybe [JobRun]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobNodeDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jndJobRuns' - The information for the job runs represented by the job node.
jobNodeDetails ::
  JobNodeDetails
jobNodeDetails = JobNodeDetails' {_jndJobRuns = Nothing}

-- | The information for the job runs represented by the job node.
jndJobRuns :: Lens' JobNodeDetails [JobRun]
jndJobRuns = lens _jndJobRuns (\s a -> s {_jndJobRuns = a}) . _Default . _Coerce

instance FromJSON JobNodeDetails where
  parseJSON =
    withObject
      "JobNodeDetails"
      (\x -> JobNodeDetails' <$> (x .:? "JobRuns" .!= mempty))

instance Hashable JobNodeDetails

instance NFData JobNodeDetails
