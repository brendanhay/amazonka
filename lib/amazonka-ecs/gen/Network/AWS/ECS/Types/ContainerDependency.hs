{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerDependency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerDependency where

import Network.AWS.ECS.Types.ContainerCondition
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed.
--
--
-- Your Amazon ECS container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
-- /See:/ 'containerDependency' smart constructor.
data ContainerDependency = ContainerDependency'
  { _cdContainerName ::
      !Text,
    _cdCondition :: !ContainerCondition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerDependency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdContainerName' - The name of a container.
--
-- * 'cdCondition' - The dependency condition of the container. The following are the available conditions and their behavior:     * @START@ - This condition emulates the behavior of links and volumes today. It validates that a dependent container is started before permitting other containers to start.     * @COMPLETE@ - This condition validates that a dependent container runs to completion (exits) before permitting other containers to start. This can be useful for nonessential containers that run a script and then exit. This condition cannot be set on an essential container.     * @SUCCESS@ - This condition is the same as @COMPLETE@ , but it also requires that the container exits with a @zero@ status. This condition cannot be set on an essential container.     * @HEALTHY@ - This condition validates that the dependent container passes its Docker health check before permitting other containers to start. This requires that the dependent container has health checks configured. This condition is confirmed only at task startup.
containerDependency ::
  -- | 'cdContainerName'
  Text ->
  -- | 'cdCondition'
  ContainerCondition ->
  ContainerDependency
containerDependency pContainerName_ pCondition_ =
  ContainerDependency'
    { _cdContainerName = pContainerName_,
      _cdCondition = pCondition_
    }

-- | The name of a container.
cdContainerName :: Lens' ContainerDependency Text
cdContainerName = lens _cdContainerName (\s a -> s {_cdContainerName = a})

-- | The dependency condition of the container. The following are the available conditions and their behavior:     * @START@ - This condition emulates the behavior of links and volumes today. It validates that a dependent container is started before permitting other containers to start.     * @COMPLETE@ - This condition validates that a dependent container runs to completion (exits) before permitting other containers to start. This can be useful for nonessential containers that run a script and then exit. This condition cannot be set on an essential container.     * @SUCCESS@ - This condition is the same as @COMPLETE@ , but it also requires that the container exits with a @zero@ status. This condition cannot be set on an essential container.     * @HEALTHY@ - This condition validates that the dependent container passes its Docker health check before permitting other containers to start. This requires that the dependent container has health checks configured. This condition is confirmed only at task startup.
cdCondition :: Lens' ContainerDependency ContainerCondition
cdCondition = lens _cdCondition (\s a -> s {_cdCondition = a})

instance FromJSON ContainerDependency where
  parseJSON =
    withObject
      "ContainerDependency"
      ( \x ->
          ContainerDependency'
            <$> (x .: "containerName") <*> (x .: "condition")
      )

instance Hashable ContainerDependency

instance NFData ContainerDependency

instance ToJSON ContainerDependency where
  toJSON ContainerDependency' {..} =
    object
      ( catMaybes
          [ Just ("containerName" .= _cdContainerName),
            Just ("condition" .= _cdCondition)
          ]
      )
