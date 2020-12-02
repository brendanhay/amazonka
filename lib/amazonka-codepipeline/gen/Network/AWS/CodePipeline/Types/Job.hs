{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.Job where

import Network.AWS.CodePipeline.Types.JobData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about a job.
--
--
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jData :: !(Maybe JobData),
    _jAccountId :: !(Maybe Text),
    _jId :: !(Maybe Text),
    _jNonce :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jData' - Other data about a job.
--
-- * 'jAccountId' - The ID of the AWS account to use when performing the job.
--
-- * 'jId' - The unique system-generated ID of the job.
--
-- * 'jNonce' - A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeJob' request.
job ::
  Job
job =
  Job'
    { _jData = Nothing,
      _jAccountId = Nothing,
      _jId = Nothing,
      _jNonce = Nothing
    }

-- | Other data about a job.
jData :: Lens' Job (Maybe JobData)
jData = lens _jData (\s a -> s {_jData = a})

-- | The ID of the AWS account to use when performing the job.
jAccountId :: Lens' Job (Maybe Text)
jAccountId = lens _jAccountId (\s a -> s {_jAccountId = a})

-- | The unique system-generated ID of the job.
jId :: Lens' Job (Maybe Text)
jId = lens _jId (\s a -> s {_jId = a})

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeJob' request.
jNonce :: Lens' Job (Maybe Text)
jNonce = lens _jNonce (\s a -> s {_jNonce = a})

instance FromJSON Job where
  parseJSON =
    withObject
      "Job"
      ( \x ->
          Job'
            <$> (x .:? "data")
            <*> (x .:? "accountId")
            <*> (x .:? "id")
            <*> (x .:? "nonce")
      )

instance Hashable Job

instance NFData Job
