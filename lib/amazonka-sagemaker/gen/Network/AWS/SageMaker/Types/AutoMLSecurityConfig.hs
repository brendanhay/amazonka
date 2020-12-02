{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLSecurityConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLSecurityConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.VPCConfig

-- | Security options.
--
--
--
-- /See:/ 'autoMLSecurityConfig' smart constructor.
data AutoMLSecurityConfig = AutoMLSecurityConfig'
  { _amlscVPCConfig ::
      !(Maybe VPCConfig),
    _amlscVolumeKMSKeyId :: !(Maybe Text),
    _amlscEnableInterContainerTrafficEncryption ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLSecurityConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlscVPCConfig' - VPC configuration.
--
-- * 'amlscVolumeKMSKeyId' - The key used to encrypt stored data.
--
-- * 'amlscEnableInterContainerTrafficEncryption' - Whether to use traffic encryption between the container layers.
autoMLSecurityConfig ::
  AutoMLSecurityConfig
autoMLSecurityConfig =
  AutoMLSecurityConfig'
    { _amlscVPCConfig = Nothing,
      _amlscVolumeKMSKeyId = Nothing,
      _amlscEnableInterContainerTrafficEncryption = Nothing
    }

-- | VPC configuration.
amlscVPCConfig :: Lens' AutoMLSecurityConfig (Maybe VPCConfig)
amlscVPCConfig = lens _amlscVPCConfig (\s a -> s {_amlscVPCConfig = a})

-- | The key used to encrypt stored data.
amlscVolumeKMSKeyId :: Lens' AutoMLSecurityConfig (Maybe Text)
amlscVolumeKMSKeyId = lens _amlscVolumeKMSKeyId (\s a -> s {_amlscVolumeKMSKeyId = a})

-- | Whether to use traffic encryption between the container layers.
amlscEnableInterContainerTrafficEncryption :: Lens' AutoMLSecurityConfig (Maybe Bool)
amlscEnableInterContainerTrafficEncryption = lens _amlscEnableInterContainerTrafficEncryption (\s a -> s {_amlscEnableInterContainerTrafficEncryption = a})

instance FromJSON AutoMLSecurityConfig where
  parseJSON =
    withObject
      "AutoMLSecurityConfig"
      ( \x ->
          AutoMLSecurityConfig'
            <$> (x .:? "VpcConfig")
            <*> (x .:? "VolumeKmsKeyId")
            <*> (x .:? "EnableInterContainerTrafficEncryption")
      )

instance Hashable AutoMLSecurityConfig

instance NFData AutoMLSecurityConfig

instance ToJSON AutoMLSecurityConfig where
  toJSON AutoMLSecurityConfig' {..} =
    object
      ( catMaybes
          [ ("VpcConfig" .=) <$> _amlscVPCConfig,
            ("VolumeKmsKeyId" .=) <$> _amlscVolumeKMSKeyId,
            ("EnableInterContainerTrafficEncryption" .=)
              <$> _amlscEnableInterContainerTrafficEncryption
          ]
      )
