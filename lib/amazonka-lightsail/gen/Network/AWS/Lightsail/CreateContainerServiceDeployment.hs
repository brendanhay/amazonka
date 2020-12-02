{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateContainerServiceDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment for your Amazon Lightsail container service.
--
--
-- A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
--
-- You can deploy containers to your container service using container images from a public registry like Docker Hub, or from your local machine. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-creating-container-images Creating container images for your Amazon Lightsail container services> in the /Lightsail Dev Guide/ .
module Network.AWS.Lightsail.CreateContainerServiceDeployment
  ( -- * Creating a Request
    createContainerServiceDeployment,
    CreateContainerServiceDeployment,

    -- * Request Lenses
    ccsdPublicEndpoint,
    ccsdContainers,
    ccsdServiceName,

    -- * Destructuring the Response
    createContainerServiceDeploymentResponse,
    CreateContainerServiceDeploymentResponse,

    -- * Response Lenses
    ccsdrsContainerService,
    ccsdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createContainerServiceDeployment' smart constructor.
data CreateContainerServiceDeployment = CreateContainerServiceDeployment'
  { _ccsdPublicEndpoint ::
      !(Maybe EndpointRequest),
    _ccsdContainers ::
      !( Maybe
           ( Map
               Text
               (Container)
           )
       ),
    _ccsdServiceName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContainerServiceDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsdPublicEndpoint' - An object that describes the settings of the public endpoint for the container service.
--
-- * 'ccsdContainers' - An object that describes the settings of the containers that will be launched on the container service.
--
-- * 'ccsdServiceName' - The name of the container service for which to create the deployment.
createContainerServiceDeployment ::
  -- | 'ccsdServiceName'
  Text ->
  CreateContainerServiceDeployment
createContainerServiceDeployment pServiceName_ =
  CreateContainerServiceDeployment'
    { _ccsdPublicEndpoint = Nothing,
      _ccsdContainers = Nothing,
      _ccsdServiceName = pServiceName_
    }

-- | An object that describes the settings of the public endpoint for the container service.
ccsdPublicEndpoint :: Lens' CreateContainerServiceDeployment (Maybe EndpointRequest)
ccsdPublicEndpoint = lens _ccsdPublicEndpoint (\s a -> s {_ccsdPublicEndpoint = a})

-- | An object that describes the settings of the containers that will be launched on the container service.
ccsdContainers :: Lens' CreateContainerServiceDeployment (HashMap Text (Container))
ccsdContainers = lens _ccsdContainers (\s a -> s {_ccsdContainers = a}) . _Default . _Map

-- | The name of the container service for which to create the deployment.
ccsdServiceName :: Lens' CreateContainerServiceDeployment Text
ccsdServiceName = lens _ccsdServiceName (\s a -> s {_ccsdServiceName = a})

instance AWSRequest CreateContainerServiceDeployment where
  type
    Rs CreateContainerServiceDeployment =
      CreateContainerServiceDeploymentResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateContainerServiceDeploymentResponse'
            <$> (x .?> "containerService") <*> (pure (fromEnum s))
      )

instance Hashable CreateContainerServiceDeployment

instance NFData CreateContainerServiceDeployment

instance ToHeaders CreateContainerServiceDeployment where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Lightsail_20161128.CreateContainerServiceDeployment" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateContainerServiceDeployment where
  toJSON CreateContainerServiceDeployment' {..} =
    object
      ( catMaybes
          [ ("publicEndpoint" .=) <$> _ccsdPublicEndpoint,
            ("containers" .=) <$> _ccsdContainers,
            Just ("serviceName" .= _ccsdServiceName)
          ]
      )

instance ToPath CreateContainerServiceDeployment where
  toPath = const "/"

instance ToQuery CreateContainerServiceDeployment where
  toQuery = const mempty

-- | /See:/ 'createContainerServiceDeploymentResponse' smart constructor.
data CreateContainerServiceDeploymentResponse = CreateContainerServiceDeploymentResponse'
  { _ccsdrsContainerService ::
      !( Maybe
           ContainerService
       ),
    _ccsdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContainerServiceDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsdrsContainerService' - An object that describes a container service.
--
-- * 'ccsdrsResponseStatus' - -- | The response status code.
createContainerServiceDeploymentResponse ::
  -- | 'ccsdrsResponseStatus'
  Int ->
  CreateContainerServiceDeploymentResponse
createContainerServiceDeploymentResponse pResponseStatus_ =
  CreateContainerServiceDeploymentResponse'
    { _ccsdrsContainerService =
        Nothing,
      _ccsdrsResponseStatus = pResponseStatus_
    }

-- | An object that describes a container service.
ccsdrsContainerService :: Lens' CreateContainerServiceDeploymentResponse (Maybe ContainerService)
ccsdrsContainerService = lens _ccsdrsContainerService (\s a -> s {_ccsdrsContainerService = a})

-- | -- | The response status code.
ccsdrsResponseStatus :: Lens' CreateContainerServiceDeploymentResponse Int
ccsdrsResponseStatus = lens _ccsdrsResponseStatus (\s a -> s {_ccsdrsResponseStatus = a})

instance NFData CreateContainerServiceDeploymentResponse
