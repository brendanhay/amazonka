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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new task definition from the supplied @family@ and @containerDefinitions@ . Optionally, you can add data volumes to your containers with the @volumes@ parameter. For more information about task definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
-- You can specify an IAM role for your task with the @taskRoleArn@ parameter. When you specify an IAM role for a task, its containers can then use the latest versions of the AWS CLI or SDKs to make API requests to the AWS services that are specified in the IAM policy associated with the role. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- You can specify a Docker networking mode for the containers in your task definition with the @networkMode@ parameter. The available network modes correspond to those described in <https://docs.docker.com/engine/reference/run/#/network-settings Network settings> in the Docker run reference. If you specify the @awsvpc@ network mode, the task is allocated an Elastic Network Interface, and you must specify a 'NetworkConfiguration' when you create a service or run a task with the task definition. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
--
module Network.AWS.ECS.RegisterTaskDefinition
    (
    -- * Creating a Request
      registerTaskDefinition
    , RegisterTaskDefinition
    -- * Request Lenses
    , rtdExecutionRoleARN
    , rtdRequiresCompatibilities
    , rtdMemory
    , rtdTaskRoleARN
    , rtdPlacementConstraints
    , rtdNetworkMode
    , rtdVolumes
    , rtdCpu
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
  { _rtdExecutionRoleARN        :: !(Maybe Text)
  , _rtdRequiresCompatibilities :: !(Maybe [Compatibility])
  , _rtdMemory                  :: !(Maybe Text)
  , _rtdTaskRoleARN             :: !(Maybe Text)
  , _rtdPlacementConstraints    :: !(Maybe [TaskDefinitionPlacementConstraint])
  , _rtdNetworkMode             :: !(Maybe NetworkMode)
  , _rtdVolumes                 :: !(Maybe [Volume])
  , _rtdCpu                     :: !(Maybe Text)
  , _rtdFamily                  :: !Text
  , _rtdContainerDefinitions    :: ![ContainerDefinition]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterTaskDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdExecutionRoleARN' - The Amazon Resource Name (ARN) of the task execution role that the Amazon ECS container agent and the Docker daemon can assume.
--
-- * 'rtdRequiresCompatibilities' - The launch type required by the task. If no value is specified, it defaults to @EC2@ .
--
-- * 'rtdMemory' - The amount of memory (in MiB) used by the task. It can be expressed as an integer using MiB, for example @1024@ , or as a string using GB, for example @1GB@ or @1 GB@ , in a task definition but will be converted to an integer indicating the MiB when the task definition is registered. If using the EC2 launch type, this field is optional. If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
--
-- * 'rtdTaskRoleARN' - The short name or full Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'rtdPlacementConstraints' - An array of placement constraint objects to use for the task. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at run time).
--
-- * 'rtdNetworkMode' - The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . The default Docker network mode is @bridge@ . If using the Fargate launch type, the @awsvpc@ network mode is required. If using the EC2 launch type, any network mode can be used. If the network mode is set to @none@ , you can't specify port mappings in your container definitions, and the task's containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode. With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings.  If the network mode is @awsvpc@ , the task is allocated an Elastic Network Interface, and you must specify a 'NetworkConfiguration' when you create a service or run a task with the task definition. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ . If the network mode is @host@ , you can't run multiple instantiations of the same task on a single container instance when port mappings are used. Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
--
-- * 'rtdVolumes' - A list of volume definitions in JSON format that containers in your task may use.
--
-- * 'rtdCpu' - The number of CPU units used by the task. It can be expressed as an integer using CPU units, for example @1024@ , or as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ , in a task definition but will be converted to an integer indicating the CPU units when the task definition is registered. If using the EC2 launch type, this field is optional. Supported values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units (@10@ vCPUs). If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @memory@ parameter:     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
--
-- * 'rtdFamily' - You must specify a @family@ for a task definition, which allows you to track multiple versions of the same task definition. The @family@ is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- * 'rtdContainerDefinitions' - A list of container definitions in JSON format that describe the different containers that make up your task.
registerTaskDefinition
    :: Text -- ^ 'rtdFamily'
    -> RegisterTaskDefinition
registerTaskDefinition pFamily_ =
  RegisterTaskDefinition'
    { _rtdExecutionRoleARN = Nothing
    , _rtdRequiresCompatibilities = Nothing
    , _rtdMemory = Nothing
    , _rtdTaskRoleARN = Nothing
    , _rtdPlacementConstraints = Nothing
    , _rtdNetworkMode = Nothing
    , _rtdVolumes = Nothing
    , _rtdCpu = Nothing
    , _rtdFamily = pFamily_
    , _rtdContainerDefinitions = mempty
    }


-- | The Amazon Resource Name (ARN) of the task execution role that the Amazon ECS container agent and the Docker daemon can assume.
rtdExecutionRoleARN :: Lens' RegisterTaskDefinition (Maybe Text)
rtdExecutionRoleARN = lens _rtdExecutionRoleARN (\ s a -> s{_rtdExecutionRoleARN = a})

-- | The launch type required by the task. If no value is specified, it defaults to @EC2@ .
rtdRequiresCompatibilities :: Lens' RegisterTaskDefinition [Compatibility]
rtdRequiresCompatibilities = lens _rtdRequiresCompatibilities (\ s a -> s{_rtdRequiresCompatibilities = a}) . _Default . _Coerce

-- | The amount of memory (in MiB) used by the task. It can be expressed as an integer using MiB, for example @1024@ , or as a string using GB, for example @1GB@ or @1 GB@ , in a task definition but will be converted to an integer indicating the MiB when the task definition is registered. If using the EC2 launch type, this field is optional. If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
rtdMemory :: Lens' RegisterTaskDefinition (Maybe Text)
rtdMemory = lens _rtdMemory (\ s a -> s{_rtdMemory = a})

-- | The short name or full Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
rtdTaskRoleARN :: Lens' RegisterTaskDefinition (Maybe Text)
rtdTaskRoleARN = lens _rtdTaskRoleARN (\ s a -> s{_rtdTaskRoleARN = a})

-- | An array of placement constraint objects to use for the task. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at run time).
rtdPlacementConstraints :: Lens' RegisterTaskDefinition [TaskDefinitionPlacementConstraint]
rtdPlacementConstraints = lens _rtdPlacementConstraints (\ s a -> s{_rtdPlacementConstraints = a}) . _Default . _Coerce

-- | The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . The default Docker network mode is @bridge@ . If using the Fargate launch type, the @awsvpc@ network mode is required. If using the EC2 launch type, any network mode can be used. If the network mode is set to @none@ , you can't specify port mappings in your container definitions, and the task's containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode. With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings.  If the network mode is @awsvpc@ , the task is allocated an Elastic Network Interface, and you must specify a 'NetworkConfiguration' when you create a service or run a task with the task definition. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ . If the network mode is @host@ , you can't run multiple instantiations of the same task on a single container instance when port mappings are used. Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
rtdNetworkMode :: Lens' RegisterTaskDefinition (Maybe NetworkMode)
rtdNetworkMode = lens _rtdNetworkMode (\ s a -> s{_rtdNetworkMode = a})

-- | A list of volume definitions in JSON format that containers in your task may use.
rtdVolumes :: Lens' RegisterTaskDefinition [Volume]
rtdVolumes = lens _rtdVolumes (\ s a -> s{_rtdVolumes = a}) . _Default . _Coerce

-- | The number of CPU units used by the task. It can be expressed as an integer using CPU units, for example @1024@ , or as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ , in a task definition but will be converted to an integer indicating the CPU units when the task definition is registered. If using the EC2 launch type, this field is optional. Supported values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units (@10@ vCPUs). If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @memory@ parameter:     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
rtdCpu :: Lens' RegisterTaskDefinition (Maybe Text)
rtdCpu = lens _rtdCpu (\ s a -> s{_rtdCpu = a})

-- | You must specify a @family@ for a task definition, which allows you to track multiple versions of the same task definition. The @family@ is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
rtdFamily :: Lens' RegisterTaskDefinition Text
rtdFamily = lens _rtdFamily (\ s a -> s{_rtdFamily = a})

-- | A list of container definitions in JSON format that describe the different containers that make up your task.
rtdContainerDefinitions :: Lens' RegisterTaskDefinition [ContainerDefinition]
rtdContainerDefinitions = lens _rtdContainerDefinitions (\ s a -> s{_rtdContainerDefinitions = a}) . _Coerce

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
                 [("executionRoleArn" .=) <$> _rtdExecutionRoleARN,
                  ("requiresCompatibilities" .=) <$>
                    _rtdRequiresCompatibilities,
                  ("memory" .=) <$> _rtdMemory,
                  ("taskRoleArn" .=) <$> _rtdTaskRoleARN,
                  ("placementConstraints" .=) <$>
                    _rtdPlacementConstraints,
                  ("networkMode" .=) <$> _rtdNetworkMode,
                  ("volumes" .=) <$> _rtdVolumes,
                  ("cpu" .=) <$> _rtdCpu,
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
rtdrsTaskDefinition = lens _rtdrsTaskDefinition (\ s a -> s{_rtdrsTaskDefinition = a})

-- | -- | The response status code.
rtdrsResponseStatus :: Lens' RegisterTaskDefinitionResponse Int
rtdrsResponseStatus = lens _rtdrsResponseStatus (\ s a -> s{_rtdrsResponseStatus = a})

instance NFData RegisterTaskDefinitionResponse where
