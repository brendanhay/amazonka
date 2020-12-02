{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.JobExecutionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.JobExecutionSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about when a transcription job should be executed.
--
--
--
-- /See:/ 'jobExecutionSettings' smart constructor.
data JobExecutionSettings = JobExecutionSettings'
  { _jesDataAccessRoleARN ::
      !(Maybe Text),
    _jesAllowDeferredExecution :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobExecutionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesDataAccessRoleARN' - The Amazon Resource Name (ARN) of a role that has access to the S3 bucket that contains the input files. Amazon Transcribe assumes this role to read queued media files. If you have specified an output S3 bucket for the transcription results, this role should have access to the output bucket as well. If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
--
-- * 'jesAllowDeferredExecution' - Indicates whether a job should be queued by Amazon Transcribe when the concurrent execution limit is exceeded. When the @AllowDeferredExecution@ field is true, jobs are queued and executed when the number of executing jobs falls below the concurrent execution limit. If the field is false, Amazon Transcribe returns a @LimitExceededException@ exception. If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
jobExecutionSettings ::
  JobExecutionSettings
jobExecutionSettings =
  JobExecutionSettings'
    { _jesDataAccessRoleARN = Nothing,
      _jesAllowDeferredExecution = Nothing
    }

-- | The Amazon Resource Name (ARN) of a role that has access to the S3 bucket that contains the input files. Amazon Transcribe assumes this role to read queued media files. If you have specified an output S3 bucket for the transcription results, this role should have access to the output bucket as well. If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
jesDataAccessRoleARN :: Lens' JobExecutionSettings (Maybe Text)
jesDataAccessRoleARN = lens _jesDataAccessRoleARN (\s a -> s {_jesDataAccessRoleARN = a})

-- | Indicates whether a job should be queued by Amazon Transcribe when the concurrent execution limit is exceeded. When the @AllowDeferredExecution@ field is true, jobs are queued and executed when the number of executing jobs falls below the concurrent execution limit. If the field is false, Amazon Transcribe returns a @LimitExceededException@ exception. If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
jesAllowDeferredExecution :: Lens' JobExecutionSettings (Maybe Bool)
jesAllowDeferredExecution = lens _jesAllowDeferredExecution (\s a -> s {_jesAllowDeferredExecution = a})

instance FromJSON JobExecutionSettings where
  parseJSON =
    withObject
      "JobExecutionSettings"
      ( \x ->
          JobExecutionSettings'
            <$> (x .:? "DataAccessRoleArn") <*> (x .:? "AllowDeferredExecution")
      )

instance Hashable JobExecutionSettings

instance NFData JobExecutionSettings

instance ToJSON JobExecutionSettings where
  toJSON JobExecutionSettings' {..} =
    object
      ( catMaybes
          [ ("DataAccessRoleArn" .=) <$> _jesDataAccessRoleARN,
            ("AllowDeferredExecution" .=) <$> _jesAllowDeferredExecution
          ]
      )
