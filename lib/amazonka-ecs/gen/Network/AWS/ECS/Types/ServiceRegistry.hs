{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ServiceRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ServiceRegistry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the service registry.
--
--
--
-- /See:/ 'serviceRegistry' smart constructor.
data ServiceRegistry = ServiceRegistry'
  { _srRegistryARN ::
      !(Maybe Text),
    _srContainerName :: !(Maybe Text),
    _srContainerPort :: !(Maybe Int),
    _srPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceRegistry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srRegistryARN' - The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is AWS Cloud Map. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService> .
--
-- * 'srContainerName' - The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
--
-- * 'srContainerPort' - The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
--
-- * 'srPort' - The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
serviceRegistry ::
  ServiceRegistry
serviceRegistry =
  ServiceRegistry'
    { _srRegistryARN = Nothing,
      _srContainerName = Nothing,
      _srContainerPort = Nothing,
      _srPort = Nothing
    }

-- | The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is AWS Cloud Map. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService> .
srRegistryARN :: Lens' ServiceRegistry (Maybe Text)
srRegistryARN = lens _srRegistryARN (\s a -> s {_srRegistryARN = a})

-- | The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
srContainerName :: Lens' ServiceRegistry (Maybe Text)
srContainerName = lens _srContainerName (\s a -> s {_srContainerName = a})

-- | The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
srContainerPort :: Lens' ServiceRegistry (Maybe Int)
srContainerPort = lens _srContainerPort (\s a -> s {_srContainerPort = a})

-- | The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
srPort :: Lens' ServiceRegistry (Maybe Int)
srPort = lens _srPort (\s a -> s {_srPort = a})

instance FromJSON ServiceRegistry where
  parseJSON =
    withObject
      "ServiceRegistry"
      ( \x ->
          ServiceRegistry'
            <$> (x .:? "registryArn")
            <*> (x .:? "containerName")
            <*> (x .:? "containerPort")
            <*> (x .:? "port")
      )

instance Hashable ServiceRegistry

instance NFData ServiceRegistry

instance ToJSON ServiceRegistry where
  toJSON ServiceRegistry' {..} =
    object
      ( catMaybes
          [ ("registryArn" .=) <$> _srRegistryARN,
            ("containerName" .=) <$> _srContainerName,
            ("containerPort" .=) <$> _srContainerPort,
            ("port" .=) <$> _srPort
          ]
      )
