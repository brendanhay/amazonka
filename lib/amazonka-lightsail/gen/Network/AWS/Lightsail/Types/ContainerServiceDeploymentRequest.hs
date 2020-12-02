{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.EndpointRequest
import Network.AWS.Prelude

-- | Describes a container deployment configuration of an Amazon Lightsail container service.
--
--
-- A deployment specifies the settings, such as the ports and launch command, of containers that are deployed to your container service.
--
--
-- /See:/ 'containerServiceDeploymentRequest' smart constructor.
data ContainerServiceDeploymentRequest = ContainerServiceDeploymentRequest'
  { _csdrPublicEndpoint ::
      !( Maybe
           EndpointRequest
       ),
    _csdrContainers ::
      !( Maybe
           ( Map
               Text
               (Container)
           )
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerServiceDeploymentRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdrPublicEndpoint' - An object that describes the endpoint of the deployment.
--
-- * 'csdrContainers' - An object that describes the configuration for the containers of the deployment.
containerServiceDeploymentRequest ::
  ContainerServiceDeploymentRequest
containerServiceDeploymentRequest =
  ContainerServiceDeploymentRequest'
    { _csdrPublicEndpoint = Nothing,
      _csdrContainers = Nothing
    }

-- | An object that describes the endpoint of the deployment.
csdrPublicEndpoint :: Lens' ContainerServiceDeploymentRequest (Maybe EndpointRequest)
csdrPublicEndpoint = lens _csdrPublicEndpoint (\s a -> s {_csdrPublicEndpoint = a})

-- | An object that describes the configuration for the containers of the deployment.
csdrContainers :: Lens' ContainerServiceDeploymentRequest (HashMap Text (Container))
csdrContainers = lens _csdrContainers (\s a -> s {_csdrContainers = a}) . _Default . _Map

instance Hashable ContainerServiceDeploymentRequest

instance NFData ContainerServiceDeploymentRequest

instance ToJSON ContainerServiceDeploymentRequest where
  toJSON ContainerServiceDeploymentRequest' {..} =
    object
      ( catMaybes
          [ ("publicEndpoint" .=) <$> _csdrPublicEndpoint,
            ("containers" .=) <$> _csdrContainers
          ]
      )
