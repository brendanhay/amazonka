{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDependency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDependency where

import Network.AWS.Batch.Types.ArrayJobDependency
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing an AWS Batch job dependency.
--
--
--
-- /See:/ 'jobDependency' smart constructor.
data JobDependency = JobDependency'
  { _jJobId :: !(Maybe Text),
    _jType :: !(Maybe ArrayJobDependency)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobDependency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jJobId' - The job ID of the AWS Batch job associated with this dependency.
--
-- * 'jType' - The type of the job dependency.
jobDependency ::
  JobDependency
jobDependency = JobDependency' {_jJobId = Nothing, _jType = Nothing}

-- | The job ID of the AWS Batch job associated with this dependency.
jJobId :: Lens' JobDependency (Maybe Text)
jJobId = lens _jJobId (\s a -> s {_jJobId = a})

-- | The type of the job dependency.
jType :: Lens' JobDependency (Maybe ArrayJobDependency)
jType = lens _jType (\s a -> s {_jType = a})

instance FromJSON JobDependency where
  parseJSON =
    withObject
      "JobDependency"
      (\x -> JobDependency' <$> (x .:? "jobId") <*> (x .:? "type"))

instance Hashable JobDependency

instance NFData JobDependency

instance ToJSON JobDependency where
  toJSON JobDependency' {..} =
    object
      (catMaybes [("jobId" .=) <$> _jJobId, ("type" .=) <$> _jType])
