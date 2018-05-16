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
-- Module      : Network.AWS.ECS.DeregisterContainerInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon ECS container instance from the specified cluster. This instance is no longer available to run tasks.
--
--
-- If you intend to use the container instance for some other purpose after deregistration, you should stop all of the tasks running on the container instance before deregistration. That prevents any orphaned tasks from consuming resources.
--
-- Deregistering a container instance removes the instance from a cluster, but it does not terminate the EC2 instance; if you are finished using the instance, be sure to terminate it in the Amazon EC2 console to stop billing.
--
module Network.AWS.ECS.DeregisterContainerInstance
    (
    -- * Creating a Request
      deregisterContainerInstance
    , DeregisterContainerInstance
    -- * Request Lenses
    , derCluster
    , derForce
    , derContainerInstance

    -- * Destructuring the Response
    , deregisterContainerInstanceResponse
    , DeregisterContainerInstanceResponse
    -- * Response Lenses
    , dcirsContainerInstance
    , dcirsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterContainerInstance' smart constructor.
data DeregisterContainerInstance = DeregisterContainerInstance'
  { _derCluster           :: !(Maybe Text)
  , _derForce             :: !(Maybe Bool)
  , _derContainerInstance :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterContainerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to deregister. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'derForce' - Forces the deregistration of the container instance. If you have tasks running on the container instance when you deregister it with the @force@ option, these tasks remain running until you terminate the instance or the tasks stop through some other means, but they are orphaned (no longer monitored or accounted for by Amazon ECS). If an orphaned task on your container instance is part of an Amazon ECS service, then the service scheduler starts another copy of that task, on a different container instance if possible.  Any containers in orphaned service tasks that are registered with a Classic Load Balancer or an Application Load Balancer target group are deregistered. They begin connection draining according to the settings on the load balancer or target group.
--
-- * 'derContainerInstance' - The container instance ID or full ARN of the container instance to deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
deregisterContainerInstance
    :: Text -- ^ 'derContainerInstance'
    -> DeregisterContainerInstance
deregisterContainerInstance pContainerInstance_ =
  DeregisterContainerInstance'
    { _derCluster = Nothing
    , _derForce = Nothing
    , _derContainerInstance = pContainerInstance_
    }


-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to deregister. If you do not specify a cluster, the default cluster is assumed.
derCluster :: Lens' DeregisterContainerInstance (Maybe Text)
derCluster = lens _derCluster (\ s a -> s{_derCluster = a})

-- | Forces the deregistration of the container instance. If you have tasks running on the container instance when you deregister it with the @force@ option, these tasks remain running until you terminate the instance or the tasks stop through some other means, but they are orphaned (no longer monitored or accounted for by Amazon ECS). If an orphaned task on your container instance is part of an Amazon ECS service, then the service scheduler starts another copy of that task, on a different container instance if possible.  Any containers in orphaned service tasks that are registered with a Classic Load Balancer or an Application Load Balancer target group are deregistered. They begin connection draining according to the settings on the load balancer or target group.
derForce :: Lens' DeregisterContainerInstance (Maybe Bool)
derForce = lens _derForce (\ s a -> s{_derForce = a})

-- | The container instance ID or full ARN of the container instance to deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
derContainerInstance :: Lens' DeregisterContainerInstance Text
derContainerInstance = lens _derContainerInstance (\ s a -> s{_derContainerInstance = a})

instance AWSRequest DeregisterContainerInstance where
        type Rs DeregisterContainerInstance =
             DeregisterContainerInstanceResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 DeregisterContainerInstanceResponse' <$>
                   (x .?> "containerInstance") <*> (pure (fromEnum s)))

instance Hashable DeregisterContainerInstance where

instance NFData DeregisterContainerInstance where

instance ToHeaders DeregisterContainerInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DeregisterContainerInstance"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterContainerInstance where
        toJSON DeregisterContainerInstance'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _derCluster,
                  ("force" .=) <$> _derForce,
                  Just ("containerInstance" .= _derContainerInstance)])

instance ToPath DeregisterContainerInstance where
        toPath = const "/"

instance ToQuery DeregisterContainerInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterContainerInstanceResponse' smart constructor.
data DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse'
  { _dcirsContainerInstance :: !(Maybe ContainerInstance)
  , _dcirsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterContainerInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcirsContainerInstance' - The container instance that was deregistered.
--
-- * 'dcirsResponseStatus' - -- | The response status code.
deregisterContainerInstanceResponse
    :: Int -- ^ 'dcirsResponseStatus'
    -> DeregisterContainerInstanceResponse
deregisterContainerInstanceResponse pResponseStatus_ =
  DeregisterContainerInstanceResponse'
    {_dcirsContainerInstance = Nothing, _dcirsResponseStatus = pResponseStatus_}


-- | The container instance that was deregistered.
dcirsContainerInstance :: Lens' DeregisterContainerInstanceResponse (Maybe ContainerInstance)
dcirsContainerInstance = lens _dcirsContainerInstance (\ s a -> s{_dcirsContainerInstance = a})

-- | -- | The response status code.
dcirsResponseStatus :: Lens' DeregisterContainerInstanceResponse Int
dcirsResponseStatus = lens _dcirsResponseStatus (\ s a -> s{_dcirsResponseStatus = a})

instance NFData DeregisterContainerInstanceResponse
         where
