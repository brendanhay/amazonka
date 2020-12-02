{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringS3Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringS3Output where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingS3UploadMode

-- | Information about where and how you want to store the results of a monitoring job.
--
--
--
-- /See:/ 'monitoringS3Output' smart constructor.
data MonitoringS3Output = MonitoringS3Output'
  { _msoS3UploadMode ::
      !(Maybe ProcessingS3UploadMode),
    _msoS3URI :: !Text,
    _msoLocalPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringS3Output' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msoS3UploadMode' - Whether to upload the results of the monitoring job continuously or after the job completes.
--
-- * 'msoS3URI' - A URI that identifies the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job.
--
-- * 'msoLocalPath' - The local path to the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job. LocalPath is an absolute path for the output data.
monitoringS3Output ::
  -- | 'msoS3URI'
  Text ->
  -- | 'msoLocalPath'
  Text ->
  MonitoringS3Output
monitoringS3Output pS3URI_ pLocalPath_ =
  MonitoringS3Output'
    { _msoS3UploadMode = Nothing,
      _msoS3URI = pS3URI_,
      _msoLocalPath = pLocalPath_
    }

-- | Whether to upload the results of the monitoring job continuously or after the job completes.
msoS3UploadMode :: Lens' MonitoringS3Output (Maybe ProcessingS3UploadMode)
msoS3UploadMode = lens _msoS3UploadMode (\s a -> s {_msoS3UploadMode = a})

-- | A URI that identifies the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job.
msoS3URI :: Lens' MonitoringS3Output Text
msoS3URI = lens _msoS3URI (\s a -> s {_msoS3URI = a})

-- | The local path to the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job. LocalPath is an absolute path for the output data.
msoLocalPath :: Lens' MonitoringS3Output Text
msoLocalPath = lens _msoLocalPath (\s a -> s {_msoLocalPath = a})

instance FromJSON MonitoringS3Output where
  parseJSON =
    withObject
      "MonitoringS3Output"
      ( \x ->
          MonitoringS3Output'
            <$> (x .:? "S3UploadMode") <*> (x .: "S3Uri") <*> (x .: "LocalPath")
      )

instance Hashable MonitoringS3Output

instance NFData MonitoringS3Output

instance ToJSON MonitoringS3Output where
  toJSON MonitoringS3Output' {..} =
    object
      ( catMaybes
          [ ("S3UploadMode" .=) <$> _msoS3UploadMode,
            Just ("S3Uri" .= _msoS3URI),
            Just ("LocalPath" .= _msoLocalPath)
          ]
      )
