{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Records a successful request to stop a specified @JobRun@ .
--
--
--
-- /See:/ 'batchStopJobRunSuccessfulSubmission' smart constructor.
data BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission'
  { _bsjrssJobName ::
      !(Maybe Text),
    _bsjrssJobRunId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStopJobRunSuccessfulSubmission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsjrssJobName' - The name of the job definition used in the job run that was stopped.
--
-- * 'bsjrssJobRunId' - The @JobRunId@ of the job run that was stopped.
batchStopJobRunSuccessfulSubmission ::
  BatchStopJobRunSuccessfulSubmission
batchStopJobRunSuccessfulSubmission =
  BatchStopJobRunSuccessfulSubmission'
    { _bsjrssJobName = Nothing,
      _bsjrssJobRunId = Nothing
    }

-- | The name of the job definition used in the job run that was stopped.
bsjrssJobName :: Lens' BatchStopJobRunSuccessfulSubmission (Maybe Text)
bsjrssJobName = lens _bsjrssJobName (\s a -> s {_bsjrssJobName = a})

-- | The @JobRunId@ of the job run that was stopped.
bsjrssJobRunId :: Lens' BatchStopJobRunSuccessfulSubmission (Maybe Text)
bsjrssJobRunId = lens _bsjrssJobRunId (\s a -> s {_bsjrssJobRunId = a})

instance FromJSON BatchStopJobRunSuccessfulSubmission where
  parseJSON =
    withObject
      "BatchStopJobRunSuccessfulSubmission"
      ( \x ->
          BatchStopJobRunSuccessfulSubmission'
            <$> (x .:? "JobName") <*> (x .:? "JobRunId")
      )

instance Hashable BatchStopJobRunSuccessfulSubmission

instance NFData BatchStopJobRunSuccessfulSubmission
