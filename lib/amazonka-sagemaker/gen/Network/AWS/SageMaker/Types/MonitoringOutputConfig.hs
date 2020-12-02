{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringOutputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MonitoringOutput

-- | The output configuration for monitoring jobs.
--
--
--
-- /See:/ 'monitoringOutputConfig' smart constructor.
data MonitoringOutputConfig = MonitoringOutputConfig'
  { _mocKMSKeyId ::
      !(Maybe Text),
    _mocMonitoringOutputs ::
      !(List1 MonitoringOutput)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mocKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption.
--
-- * 'mocMonitoringOutputs' - Monitoring outputs for monitoring jobs. This is where the output of the periodic monitoring jobs is uploaded.
monitoringOutputConfig ::
  -- | 'mocMonitoringOutputs'
  NonEmpty MonitoringOutput ->
  MonitoringOutputConfig
monitoringOutputConfig pMonitoringOutputs_ =
  MonitoringOutputConfig'
    { _mocKMSKeyId = Nothing,
      _mocMonitoringOutputs = _List1 # pMonitoringOutputs_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption.
mocKMSKeyId :: Lens' MonitoringOutputConfig (Maybe Text)
mocKMSKeyId = lens _mocKMSKeyId (\s a -> s {_mocKMSKeyId = a})

-- | Monitoring outputs for monitoring jobs. This is where the output of the periodic monitoring jobs is uploaded.
mocMonitoringOutputs :: Lens' MonitoringOutputConfig (NonEmpty MonitoringOutput)
mocMonitoringOutputs = lens _mocMonitoringOutputs (\s a -> s {_mocMonitoringOutputs = a}) . _List1

instance FromJSON MonitoringOutputConfig where
  parseJSON =
    withObject
      "MonitoringOutputConfig"
      ( \x ->
          MonitoringOutputConfig'
            <$> (x .:? "KmsKeyId") <*> (x .: "MonitoringOutputs")
      )

instance Hashable MonitoringOutputConfig

instance NFData MonitoringOutputConfig

instance ToJSON MonitoringOutputConfig where
  toJSON MonitoringOutputConfig' {..} =
    object
      ( catMaybes
          [ ("KmsKeyId" .=) <$> _mocKMSKeyId,
            Just ("MonitoringOutputs" .= _mocMonitoringOutputs)
          ]
      )
