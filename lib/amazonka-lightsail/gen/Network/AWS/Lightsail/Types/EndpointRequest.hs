{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.EndpointRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.EndpointRequest where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
import Network.AWS.Prelude

-- | Describes the settings of a public endpoint for an Amazon Lightsail container service.
--
--
--
-- /See:/ 'endpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { _erHealthCheck ::
      !(Maybe ContainerServiceHealthCheckConfig),
    _erContainerName :: !Text,
    _erContainerPort :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erHealthCheck' - An object that describes the health check configuration of the container.
--
-- * 'erContainerName' - The name of the container for the endpoint.
--
-- * 'erContainerPort' - The port of the container to which traffic is forwarded to.
endpointRequest ::
  -- | 'erContainerName'
  Text ->
  -- | 'erContainerPort'
  Int ->
  EndpointRequest
endpointRequest pContainerName_ pContainerPort_ =
  EndpointRequest'
    { _erHealthCheck = Nothing,
      _erContainerName = pContainerName_,
      _erContainerPort = pContainerPort_
    }

-- | An object that describes the health check configuration of the container.
erHealthCheck :: Lens' EndpointRequest (Maybe ContainerServiceHealthCheckConfig)
erHealthCheck = lens _erHealthCheck (\s a -> s {_erHealthCheck = a})

-- | The name of the container for the endpoint.
erContainerName :: Lens' EndpointRequest Text
erContainerName = lens _erContainerName (\s a -> s {_erContainerName = a})

-- | The port of the container to which traffic is forwarded to.
erContainerPort :: Lens' EndpointRequest Int
erContainerPort = lens _erContainerPort (\s a -> s {_erContainerPort = a})

instance Hashable EndpointRequest

instance NFData EndpointRequest

instance ToJSON EndpointRequest where
  toJSON EndpointRequest' {..} =
    object
      ( catMaybes
          [ ("healthCheck" .=) <$> _erHealthCheck,
            Just ("containerName" .= _erContainerName),
            Just ("containerPort" .= _erContainerPort)
          ]
      )
