{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeployment where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
import Network.AWS.Lightsail.Types.ContainerServiceEndpoint
import Network.AWS.Prelude

-- | Describes a container deployment configuration of an Amazon Lightsail container service.
--
--
-- A deployment specifies the settings, such as the ports and launch command, of containers that are deployed to your container service.
--
--
-- /See:/ 'containerServiceDeployment' smart constructor.
data ContainerServiceDeployment = ContainerServiceDeployment'
  { _csdState ::
      !( Maybe
           ContainerServiceDeploymentState
       ),
    _csdPublicEndpoint ::
      !(Maybe ContainerServiceEndpoint),
    _csdCreatedAt :: !(Maybe POSIX),
    _csdContainers ::
      !(Maybe (Map Text (Container))),
    _csdVersion :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerServiceDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdState' - The state of the deployment. A deployment can be in one of the following states:     * @Activating@ - The deployment is being created.     * @Active@ - The deployment was successfully created, and it's currently running on the container service. The container service can have only one deployment in an active state at a time.     * @Inactive@ - The deployment was previously successfully created, but it is not currently running on the container service.     * @Failed@ - The deployment failed. Use the @GetContainerLog@ action to view the log events for the containers in the deployment to try to determine the reason for the failure.
--
-- * 'csdPublicEndpoint' - An object that describes the endpoint of the deployment.
--
-- * 'csdCreatedAt' - The timestamp when the deployment was created.
--
-- * 'csdContainers' - An object that describes the configuration for the containers of the deployment.
--
-- * 'csdVersion' - The version number of the deployment.
containerServiceDeployment ::
  ContainerServiceDeployment
containerServiceDeployment =
  ContainerServiceDeployment'
    { _csdState = Nothing,
      _csdPublicEndpoint = Nothing,
      _csdCreatedAt = Nothing,
      _csdContainers = Nothing,
      _csdVersion = Nothing
    }

-- | The state of the deployment. A deployment can be in one of the following states:     * @Activating@ - The deployment is being created.     * @Active@ - The deployment was successfully created, and it's currently running on the container service. The container service can have only one deployment in an active state at a time.     * @Inactive@ - The deployment was previously successfully created, but it is not currently running on the container service.     * @Failed@ - The deployment failed. Use the @GetContainerLog@ action to view the log events for the containers in the deployment to try to determine the reason for the failure.
csdState :: Lens' ContainerServiceDeployment (Maybe ContainerServiceDeploymentState)
csdState = lens _csdState (\s a -> s {_csdState = a})

-- | An object that describes the endpoint of the deployment.
csdPublicEndpoint :: Lens' ContainerServiceDeployment (Maybe ContainerServiceEndpoint)
csdPublicEndpoint = lens _csdPublicEndpoint (\s a -> s {_csdPublicEndpoint = a})

-- | The timestamp when the deployment was created.
csdCreatedAt :: Lens' ContainerServiceDeployment (Maybe UTCTime)
csdCreatedAt = lens _csdCreatedAt (\s a -> s {_csdCreatedAt = a}) . mapping _Time

-- | An object that describes the configuration for the containers of the deployment.
csdContainers :: Lens' ContainerServiceDeployment (HashMap Text (Container))
csdContainers = lens _csdContainers (\s a -> s {_csdContainers = a}) . _Default . _Map

-- | The version number of the deployment.
csdVersion :: Lens' ContainerServiceDeployment (Maybe Int)
csdVersion = lens _csdVersion (\s a -> s {_csdVersion = a})

instance FromJSON ContainerServiceDeployment where
  parseJSON =
    withObject
      "ContainerServiceDeployment"
      ( \x ->
          ContainerServiceDeployment'
            <$> (x .:? "state")
            <*> (x .:? "publicEndpoint")
            <*> (x .:? "createdAt")
            <*> (x .:? "containers" .!= mempty)
            <*> (x .:? "version")
      )

instance Hashable ContainerServiceDeployment

instance NFData ContainerServiceDeployment
