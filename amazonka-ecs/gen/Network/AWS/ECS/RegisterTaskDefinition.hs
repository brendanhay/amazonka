{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.RegisterTaskDefinition
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new task definition from the supplied @family@ and @containerDefinitions@ . Optionally, you can add data volumes to your containers with the @volumes@ parameter. For more information about task definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service Developer Guide/ .
--
--
-- You can specify an IAM role for your task with the @taskRoleArn@ parameter. When you specify an IAM role for a task, its containers can then use the latest versions of the AWS CLI or SDKs to make API requests to the AWS services that are specified in the IAM policy associated with the role. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon EC2 Container Service Developer Guide/ .
--
-- You can specify a Docker networking mode for the containers in your task definition with the @networkMode@ parameter. The available network modes correspond to those described in <https://docs.docker.com/engine/reference/run/#/network-settings Network settings> in the Docker run reference.
--
module Network.AWS.ECS.RegisterTaskDefinition
    (
    -- * Creating a Request
      registerTaskDefinition
    , RegisterTaskDefinition
    -- * Request Lenses
    , rtdTaskRoleARN
    , rtdPlacementConstraints
    , rtdNetworkMode
    , rtdVolumes
    , rtdFamily
    , rtdContainerDefinitions

    -- * Destructuring the Response
    , registerTaskDefinitionResponse
    , RegisterTaskDefinitionResponse
    -- * Response Lenses
    , rtdrsTaskDefinition
    , rtdrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerTaskDefinition' smart constructor.
data RegisterTaskDefinition = RegisterTaskDefinition'
  { _rtdTaskRoleARN          :: !(Maybe Text)
  , _rtdPlacementConstraints :: !(Maybe [TaskDefinitionPlacementConstraint])
  , _rtdNetworkMode          :: !(Maybe NetworkMode)
  , _rtdVolumes              :: !(Maybe [Volume])
  , _rtdFamily               :: !Text
  , _rtdContainerDefinitions :: ![ContainerDefinition]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterTaskDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdTaskRoleARN' - The short name or full Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'rtdPlacementConstraints' - An array of placement constraint objects to use for the task. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at run time).
--
-- * 'rtdNetworkMode' - The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , and @host@ .  The default Docker network mode is @bridge@ . If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the task's containers do not have external connectivity. The @host@ network mode offers the highest networking performance for containers because they use the host network stack instead of the virtualized network stack provided by the @bridge@ mode; however, exposed container ports are mapped directly to the corresponding host port, so you cannot take advantage of dynamic host port mappings or run multiple instantiations of the same task on a single container instance if port mappings are used. For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
--
-- * 'rtdVolumes' - A list of volume definitions in JSON format that containers in your task may use.
--
-- * 'rtdFamily' - You must specify a @family@ for a task definition, which allows you to track multiple versions of the same task definition. The @family@ is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- * 'rtdContainerDefinitions' - A list of container definitions in JSON format that describe the different containers that make up your task.
registerTaskDefinition
    :: Text -- ^ 'rtdFamily'
    -> RegisterTaskDefinition
registerTaskDefinition pFamily_ =
  RegisterTaskDefinition'
  { _rtdTaskRoleARN = Nothing
  , _rtdPlacementConstraints = Nothing
  , _rtdNetworkMode = Nothing
  , _rtdVolumes = Nothing
  , _rtdFamily = pFamily_
  , _rtdContainerDefinitions = mempty
  }


-- | The short name or full Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon EC2 Container Service Developer Guide/ .
rtdTaskRoleARN :: Lens' RegisterTaskDefinition (Maybe Text)
rtdTaskRoleARN = lens _rtdTaskRoleARN (\ s a -> s{_rtdTaskRoleARN = a});

-- | An array of placement constraint objects to use for the task. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at run time).
rtdPlacementConstraints :: Lens' RegisterTaskDefinition [TaskDefinitionPlacementConstraint]
rtdPlacementConstraints = lens _rtdPlacementConstraints (\ s a -> s{_rtdPlacementConstraints = a}) . _Default . _Coerce;

-- | The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , and @host@ .  The default Docker network mode is @bridge@ . If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the task's containers do not have external connectivity. The @host@ network mode offers the highest networking performance for containers because they use the host network stack instead of the virtualized network stack provided by the @bridge@ mode; however, exposed container ports are mapped directly to the corresponding host port, so you cannot take advantage of dynamic host port mappings or run multiple instantiations of the same task on a single container instance if port mappings are used. For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
rtdNetworkMode :: Lens' RegisterTaskDefinition (Maybe NetworkMode)
rtdNetworkMode = lens _rtdNetworkMode (\ s a -> s{_rtdNetworkMode = a});

-- | A list of volume definitions in JSON format that containers in your task may use.
rtdVolumes :: Lens' RegisterTaskDefinition [Volume]
rtdVolumes = lens _rtdVolumes (\ s a -> s{_rtdVolumes = a}) . _Default . _Coerce;

-- | You must specify a @family@ for a task definition, which allows you to track multiple versions of the same task definition. The @family@ is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
rtdFamily :: Lens' RegisterTaskDefinition Text
rtdFamily = lens _rtdFamily (\ s a -> s{_rtdFamily = a});

-- | A list of container definitions in JSON format that describe the different containers that make up your task.
rtdContainerDefinitions :: Lens' RegisterTaskDefinition [ContainerDefinition]
rtdContainerDefinitions = lens _rtdContainerDefinitions (\ s a -> s{_rtdContainerDefinitions = a}) . _Coerce;

instance AWSRequest RegisterTaskDefinition where
        type Rs RegisterTaskDefinition =
             RegisterTaskDefinitionResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 RegisterTaskDefinitionResponse' <$>
                   (x .?> "taskDefinition") <*> (pure (fromEnum s)))

instance Hashable RegisterTaskDefinition where

instance NFData RegisterTaskDefinition where

instance ToHeaders RegisterTaskDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.RegisterTaskDefinition"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterTaskDefinition where
        toJSON RegisterTaskDefinition'{..}
          = object
              (catMaybes
                 [("taskRoleArn" .=) <$> _rtdTaskRoleARN,
                  ("placementConstraints" .=) <$>
                    _rtdPlacementConstraints,
                  ("networkMode" .=) <$> _rtdNetworkMode,
                  ("volumes" .=) <$> _rtdVolumes,
                  Just ("family" .= _rtdFamily),
                  Just
                    ("containerDefinitions" .=
                       _rtdContainerDefinitions)])

instance ToPath RegisterTaskDefinition where
        toPath = const "/"

instance ToQuery RegisterTaskDefinition where
        toQuery = const mempty

-- | /See:/ 'registerTaskDefinitionResponse' smart constructor.
data RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse'
  { _rtdrsTaskDefinition :: !(Maybe TaskDefinition)
  , _rtdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterTaskDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdrsTaskDefinition' - The full description of the registered task definition.
--
-- * 'rtdrsResponseStatus' - -- | The response status code.
registerTaskDefinitionResponse
    :: Int -- ^ 'rtdrsResponseStatus'
    -> RegisterTaskDefinitionResponse
registerTaskDefinitionResponse pResponseStatus_ =
  RegisterTaskDefinitionResponse'
  {_rtdrsTaskDefinition = Nothing, _rtdrsResponseStatus = pResponseStatus_}


-- | The full description of the registered task definition.
rtdrsTaskDefinition :: Lens' RegisterTaskDefinitionResponse (Maybe TaskDefinition)
rtdrsTaskDefinition = lens _rtdrsTaskDefinition (\ s a -> s{_rtdrsTaskDefinition = a});

-- | -- | The response status code.
rtdrsResponseStatus :: Lens' RegisterTaskDefinitionResponse Int
rtdrsResponseStatus = lens _rtdrsResponseStatus (\ s a -> s{_rtdrsResponseStatus = a});

instance NFData RegisterTaskDefinitionResponse where
