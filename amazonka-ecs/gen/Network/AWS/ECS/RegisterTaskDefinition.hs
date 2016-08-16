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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new task definition from the supplied 'family' and 'containerDefinitions'. Optionally, you can add data volumes to your containers with the 'volumes' parameter. For more information about task definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service Developer Guide/.
--
-- You may also specify an IAM role for your task with the 'taskRoleArn' parameter. When you specify an IAM role for a task, its containers can then use the latest versions of the AWS CLI or SDKs to make API requests to the AWS services that are specified in the IAM policy associated with the role. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon EC2 Container Service Developer Guide/.
module Network.AWS.ECS.RegisterTaskDefinition
    (
    -- * Creating a Request
      registerTaskDefinition
    , RegisterTaskDefinition
    -- * Request Lenses
    , rtdTaskRoleARN
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

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerTaskDefinition' smart constructor.
data RegisterTaskDefinition = RegisterTaskDefinition'
    { _rtdTaskRoleARN          :: !(Maybe Text)
    , _rtdVolumes              :: !(Maybe [Volume])
    , _rtdFamily               :: !Text
    , _rtdContainerDefinitions :: ![ContainerDefinition]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterTaskDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdTaskRoleARN'
--
-- * 'rtdVolumes'
--
-- * 'rtdFamily'
--
-- * 'rtdContainerDefinitions'
registerTaskDefinition
    :: Text -- ^ 'rtdFamily'
    -> RegisterTaskDefinition
registerTaskDefinition pFamily_ =
    RegisterTaskDefinition'
    { _rtdTaskRoleARN = Nothing
    , _rtdVolumes = Nothing
    , _rtdFamily = pFamily_
    , _rtdContainerDefinitions = mempty
    }

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
rtdTaskRoleARN :: Lens' RegisterTaskDefinition (Maybe Text)
rtdTaskRoleARN = lens _rtdTaskRoleARN (\ s a -> s{_rtdTaskRoleARN = a});

-- | A list of volume definitions in JSON format that containers in your task may use.
rtdVolumes :: Lens' RegisterTaskDefinition [Volume]
rtdVolumes = lens _rtdVolumes (\ s a -> s{_rtdVolumes = a}) . _Default . _Coerce;

-- | You must specify a 'family' for a task definition, which allows you to track multiple versions of the same task definition. The 'family' is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
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

instance Hashable RegisterTaskDefinition

instance NFData RegisterTaskDefinition

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterTaskDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdrsTaskDefinition'
--
-- * 'rtdrsResponseStatus'
registerTaskDefinitionResponse
    :: Int -- ^ 'rtdrsResponseStatus'
    -> RegisterTaskDefinitionResponse
registerTaskDefinitionResponse pResponseStatus_ =
    RegisterTaskDefinitionResponse'
    { _rtdrsTaskDefinition = Nothing
    , _rtdrsResponseStatus = pResponseStatus_
    }

-- | The full description of the registered task definition.
rtdrsTaskDefinition :: Lens' RegisterTaskDefinitionResponse (Maybe TaskDefinition)
rtdrsTaskDefinition = lens _rtdrsTaskDefinition (\ s a -> s{_rtdrsTaskDefinition = a});

-- | The response status code.
rtdrsResponseStatus :: Lens' RegisterTaskDefinitionResponse Int
rtdrsResponseStatus = lens _rtdrsResponseStatus (\ s a -> s{_rtdrsResponseStatus = a});

instance NFData RegisterTaskDefinitionResponse
