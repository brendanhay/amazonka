{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceDataContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDataContainer where

import Network.AWS.Greengrass.Types.LocalDeviceResourceData
import Network.AWS.Greengrass.Types.LocalVolumeResourceData
import Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for resource data. The container takes only one of the following supported resource data types: ''LocalDeviceResourceData'', ''LocalVolumeResourceData'', ''SageMakerMachineLearningModelResourceData'', ''S3MachineLearningModelResourceData'', ''SecretsManagerSecretResourceData''.
--
-- /See:/ 'resourceDataContainer' smart constructor.
data ResourceDataContainer = ResourceDataContainer'
  { _rdcS3MachineLearningModelResourceData ::
      !(Maybe S3MachineLearningModelResourceData),
    _rdcSageMakerMachineLearningModelResourceData ::
      !( Maybe
           SageMakerMachineLearningModelResourceData
       ),
    _rdcLocalVolumeResourceData ::
      !(Maybe LocalVolumeResourceData),
    _rdcLocalDeviceResourceData ::
      !(Maybe LocalDeviceResourceData),
    _rdcSecretsManagerSecretResourceData ::
      !(Maybe SecretsManagerSecretResourceData)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDataContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcS3MachineLearningModelResourceData' - Attributes that define an Amazon S3 machine learning resource.
--
-- * 'rdcSageMakerMachineLearningModelResourceData' - Attributes that define an Amazon SageMaker machine learning resource.
--
-- * 'rdcLocalVolumeResourceData' - Attributes that define the local volume resource.
--
-- * 'rdcLocalDeviceResourceData' - Attributes that define the local device resource.
--
-- * 'rdcSecretsManagerSecretResourceData' - Attributes that define a secret resource, which references a secret from AWS Secrets Manager.
resourceDataContainer ::
  ResourceDataContainer
resourceDataContainer =
  ResourceDataContainer'
    { _rdcS3MachineLearningModelResourceData =
        Nothing,
      _rdcSageMakerMachineLearningModelResourceData = Nothing,
      _rdcLocalVolumeResourceData = Nothing,
      _rdcLocalDeviceResourceData = Nothing,
      _rdcSecretsManagerSecretResourceData = Nothing
    }

-- | Attributes that define an Amazon S3 machine learning resource.
rdcS3MachineLearningModelResourceData :: Lens' ResourceDataContainer (Maybe S3MachineLearningModelResourceData)
rdcS3MachineLearningModelResourceData = lens _rdcS3MachineLearningModelResourceData (\s a -> s {_rdcS3MachineLearningModelResourceData = a})

-- | Attributes that define an Amazon SageMaker machine learning resource.
rdcSageMakerMachineLearningModelResourceData :: Lens' ResourceDataContainer (Maybe SageMakerMachineLearningModelResourceData)
rdcSageMakerMachineLearningModelResourceData = lens _rdcSageMakerMachineLearningModelResourceData (\s a -> s {_rdcSageMakerMachineLearningModelResourceData = a})

-- | Attributes that define the local volume resource.
rdcLocalVolumeResourceData :: Lens' ResourceDataContainer (Maybe LocalVolumeResourceData)
rdcLocalVolumeResourceData = lens _rdcLocalVolumeResourceData (\s a -> s {_rdcLocalVolumeResourceData = a})

-- | Attributes that define the local device resource.
rdcLocalDeviceResourceData :: Lens' ResourceDataContainer (Maybe LocalDeviceResourceData)
rdcLocalDeviceResourceData = lens _rdcLocalDeviceResourceData (\s a -> s {_rdcLocalDeviceResourceData = a})

-- | Attributes that define a secret resource, which references a secret from AWS Secrets Manager.
rdcSecretsManagerSecretResourceData :: Lens' ResourceDataContainer (Maybe SecretsManagerSecretResourceData)
rdcSecretsManagerSecretResourceData = lens _rdcSecretsManagerSecretResourceData (\s a -> s {_rdcSecretsManagerSecretResourceData = a})

instance FromJSON ResourceDataContainer where
  parseJSON =
    withObject
      "ResourceDataContainer"
      ( \x ->
          ResourceDataContainer'
            <$> (x .:? "S3MachineLearningModelResourceData")
            <*> (x .:? "SageMakerMachineLearningModelResourceData")
            <*> (x .:? "LocalVolumeResourceData")
            <*> (x .:? "LocalDeviceResourceData")
            <*> (x .:? "SecretsManagerSecretResourceData")
      )

instance Hashable ResourceDataContainer

instance NFData ResourceDataContainer

instance ToJSON ResourceDataContainer where
  toJSON ResourceDataContainer' {..} =
    object
      ( catMaybes
          [ ("S3MachineLearningModelResourceData" .=)
              <$> _rdcS3MachineLearningModelResourceData,
            ("SageMakerMachineLearningModelResourceData" .=)
              <$> _rdcSageMakerMachineLearningModelResourceData,
            ("LocalVolumeResourceData" .=) <$> _rdcLocalVolumeResourceData,
            ("LocalDeviceResourceData" .=) <$> _rdcLocalDeviceResourceData,
            ("SecretsManagerSecretResourceData" .=)
              <$> _rdcSecretsManagerSecretResourceData
          ]
      )
