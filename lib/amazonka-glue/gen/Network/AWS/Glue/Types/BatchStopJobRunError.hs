{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchStopJobRunError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchStopJobRunError where

import Network.AWS.Glue.Types.ErrorDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Records an error that occurred when attempting to stop a specified job run.
--
--
--
-- /See:/ 'batchStopJobRunError' smart constructor.
data BatchStopJobRunError = BatchStopJobRunError'
  { _bsjreJobName ::
      !(Maybe Text),
    _bsjreJobRunId :: !(Maybe Text),
    _bsjreErrorDetail :: !(Maybe ErrorDetail)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStopJobRunError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsjreJobName' - The name of the job definition that is used in the job run in question.
--
-- * 'bsjreJobRunId' - The @JobRunId@ of the job run in question.
--
-- * 'bsjreErrorDetail' - Specifies details about the error that was encountered.
batchStopJobRunError ::
  BatchStopJobRunError
batchStopJobRunError =
  BatchStopJobRunError'
    { _bsjreJobName = Nothing,
      _bsjreJobRunId = Nothing,
      _bsjreErrorDetail = Nothing
    }

-- | The name of the job definition that is used in the job run in question.
bsjreJobName :: Lens' BatchStopJobRunError (Maybe Text)
bsjreJobName = lens _bsjreJobName (\s a -> s {_bsjreJobName = a})

-- | The @JobRunId@ of the job run in question.
bsjreJobRunId :: Lens' BatchStopJobRunError (Maybe Text)
bsjreJobRunId = lens _bsjreJobRunId (\s a -> s {_bsjreJobRunId = a})

-- | Specifies details about the error that was encountered.
bsjreErrorDetail :: Lens' BatchStopJobRunError (Maybe ErrorDetail)
bsjreErrorDetail = lens _bsjreErrorDetail (\s a -> s {_bsjreErrorDetail = a})

instance FromJSON BatchStopJobRunError where
  parseJSON =
    withObject
      "BatchStopJobRunError"
      ( \x ->
          BatchStopJobRunError'
            <$> (x .:? "JobName") <*> (x .:? "JobRunId") <*> (x .:? "ErrorDetail")
      )

instance Hashable BatchStopJobRunError

instance NFData BatchStopJobRunError
