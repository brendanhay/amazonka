{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3Output where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingS3UploadMode

-- | Information about where and how you want to store the results of an processing job.
--
--
--
-- /See:/ 'processingS3Output' smart constructor.
data ProcessingS3Output = ProcessingS3Output'
  { _psoS3URI :: !Text,
    _psoLocalPath :: !Text,
    _psoS3UploadMode :: !ProcessingS3UploadMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingS3Output' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psoS3URI' - A URI that identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of a processing job.
--
-- * 'psoLocalPath' - The local path to the Amazon S3 bucket where you want Amazon SageMaker to save the results of an processing job. @LocalPath@ is an absolute path to the input data.
--
-- * 'psoS3UploadMode' - Whether to upload the results of the processing job continuously or after the job completes.
processingS3Output ::
  -- | 'psoS3URI'
  Text ->
  -- | 'psoLocalPath'
  Text ->
  -- | 'psoS3UploadMode'
  ProcessingS3UploadMode ->
  ProcessingS3Output
processingS3Output pS3URI_ pLocalPath_ pS3UploadMode_ =
  ProcessingS3Output'
    { _psoS3URI = pS3URI_,
      _psoLocalPath = pLocalPath_,
      _psoS3UploadMode = pS3UploadMode_
    }

-- | A URI that identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of a processing job.
psoS3URI :: Lens' ProcessingS3Output Text
psoS3URI = lens _psoS3URI (\s a -> s {_psoS3URI = a})

-- | The local path to the Amazon S3 bucket where you want Amazon SageMaker to save the results of an processing job. @LocalPath@ is an absolute path to the input data.
psoLocalPath :: Lens' ProcessingS3Output Text
psoLocalPath = lens _psoLocalPath (\s a -> s {_psoLocalPath = a})

-- | Whether to upload the results of the processing job continuously or after the job completes.
psoS3UploadMode :: Lens' ProcessingS3Output ProcessingS3UploadMode
psoS3UploadMode = lens _psoS3UploadMode (\s a -> s {_psoS3UploadMode = a})

instance FromJSON ProcessingS3Output where
  parseJSON =
    withObject
      "ProcessingS3Output"
      ( \x ->
          ProcessingS3Output'
            <$> (x .: "S3Uri") <*> (x .: "LocalPath") <*> (x .: "S3UploadMode")
      )

instance Hashable ProcessingS3Output

instance NFData ProcessingS3Output

instance ToJSON ProcessingS3Output where
  toJSON ProcessingS3Output' {..} =
    object
      ( catMaybes
          [ Just ("S3Uri" .= _psoS3URI),
            Just ("LocalPath" .= _psoLocalPath),
            Just ("S3UploadMode" .= _psoS3UploadMode)
          ]
      )
