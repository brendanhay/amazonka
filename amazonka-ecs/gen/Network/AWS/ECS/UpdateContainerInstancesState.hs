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
-- Module      : Network.AWS.ECS.UpdateContainerInstancesState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the status of an Amazon ECS container instance.
--
--
-- You can change the status of a container instance to @DRAINING@ to manually remove an instance from a cluster, for example to perform system updates, update the Docker daemon, or scale down the cluster size.
--
-- When you set a container instance to @DRAINING@ , Amazon ECS prevents new tasks from being scheduled for placement on the container instance and replacement service tasks are started on other container instances in the cluster if the resources are available. Service tasks on the container instance that are in the @PENDING@ state are stopped immediately.
--
-- Service tasks on the container instance that are in the @RUNNING@ state are stopped and replaced according to the service's deployment configuration parameters, @minimumHealthyPercent@ and @maximumPercent@ . You can change the deployment configuration of your service using 'UpdateService' .
--
--     * If @minimumHealthyPercent@ is below 100%, the scheduler can ignore @desiredCount@ temporarily during task replacement. For example, @desiredCount@ is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. If the minimum is 100%, the service scheduler can't remove existing tasks until the replacement tasks are considered healthy. Tasks for services that do not use a load balancer are considered healthy if they are in the @RUNNING@ state. Tasks for services that use a load balancer are considered healthy if they are in the @RUNNING@ state and the container instance they are hosted on is reported as healthy by the load balancer.
--
--     * The @maximumPercent@ parameter represents an upper limit on the number of running tasks during task replacement, which enables you to define the replacement batch size. For example, if @desiredCount@ of four tasks, a maximum of 200% starts four new tasks before stopping the four tasks to be drained (provided that the cluster resources required to do this are available). If the maximum is 100%, then replacement tasks can't start until the draining tasks have stopped.
--
--
--
-- Any @PENDING@ or @RUNNING@ tasks that do not belong to a service are not affected; you must wait for them to finish or stop them manually.
--
-- A container instance has completed draining when it has no more @RUNNING@ tasks. You can verify this using 'ListTasks' .
--
-- When you set a container instance to @ACTIVE@ , the Amazon ECS scheduler can begin scheduling tasks on the instance again.
--
module Network.AWS.ECS.UpdateContainerInstancesState
    (
    -- * Creating a Request
      updateContainerInstancesState
    , UpdateContainerInstancesState
    -- * Request Lenses
    , ucisCluster
    , ucisContainerInstances
    , ucisStatus

    -- * Destructuring the Response
    , updateContainerInstancesStateResponse
    , UpdateContainerInstancesStateResponse
    -- * Response Lenses
    , ucisrsFailures
    , ucisrsContainerInstances
    , ucisrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateContainerInstancesState' smart constructor.
data UpdateContainerInstancesState = UpdateContainerInstancesState'
  { _ucisCluster            :: !(Maybe Text)
  , _ucisContainerInstances :: ![Text]
  , _ucisStatus             :: !ContainerInstanceStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContainerInstancesState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucisCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to update. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'ucisContainerInstances' - A list of container instance IDs or full ARN entries.
--
-- * 'ucisStatus' - The container instance state with which to update the container instance.
updateContainerInstancesState
    :: ContainerInstanceStatus -- ^ 'ucisStatus'
    -> UpdateContainerInstancesState
updateContainerInstancesState pStatus_ =
  UpdateContainerInstancesState'
    { _ucisCluster = Nothing
    , _ucisContainerInstances = mempty
    , _ucisStatus = pStatus_
    }


-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to update. If you do not specify a cluster, the default cluster is assumed.
ucisCluster :: Lens' UpdateContainerInstancesState (Maybe Text)
ucisCluster = lens _ucisCluster (\ s a -> s{_ucisCluster = a})

-- | A list of container instance IDs or full ARN entries.
ucisContainerInstances :: Lens' UpdateContainerInstancesState [Text]
ucisContainerInstances = lens _ucisContainerInstances (\ s a -> s{_ucisContainerInstances = a}) . _Coerce

-- | The container instance state with which to update the container instance.
ucisStatus :: Lens' UpdateContainerInstancesState ContainerInstanceStatus
ucisStatus = lens _ucisStatus (\ s a -> s{_ucisStatus = a})

instance AWSRequest UpdateContainerInstancesState
         where
        type Rs UpdateContainerInstancesState =
             UpdateContainerInstancesStateResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 UpdateContainerInstancesStateResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "containerInstances" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable UpdateContainerInstancesState where

instance NFData UpdateContainerInstancesState where

instance ToHeaders UpdateContainerInstancesState
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.UpdateContainerInstancesState"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateContainerInstancesState where
        toJSON UpdateContainerInstancesState'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _ucisCluster,
                  Just
                    ("containerInstances" .= _ucisContainerInstances),
                  Just ("status" .= _ucisStatus)])

instance ToPath UpdateContainerInstancesState where
        toPath = const "/"

instance ToQuery UpdateContainerInstancesState where
        toQuery = const mempty

-- | /See:/ 'updateContainerInstancesStateResponse' smart constructor.
data UpdateContainerInstancesStateResponse = UpdateContainerInstancesStateResponse'
  { _ucisrsFailures           :: !(Maybe [Failure])
  , _ucisrsContainerInstances :: !(Maybe [ContainerInstance])
  , _ucisrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContainerInstancesStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucisrsFailures' - Any failures associated with the call.
--
-- * 'ucisrsContainerInstances' - The list of container instances.
--
-- * 'ucisrsResponseStatus' - -- | The response status code.
updateContainerInstancesStateResponse
    :: Int -- ^ 'ucisrsResponseStatus'
    -> UpdateContainerInstancesStateResponse
updateContainerInstancesStateResponse pResponseStatus_ =
  UpdateContainerInstancesStateResponse'
    { _ucisrsFailures = Nothing
    , _ucisrsContainerInstances = Nothing
    , _ucisrsResponseStatus = pResponseStatus_
    }


-- | Any failures associated with the call.
ucisrsFailures :: Lens' UpdateContainerInstancesStateResponse [Failure]
ucisrsFailures = lens _ucisrsFailures (\ s a -> s{_ucisrsFailures = a}) . _Default . _Coerce

-- | The list of container instances.
ucisrsContainerInstances :: Lens' UpdateContainerInstancesStateResponse [ContainerInstance]
ucisrsContainerInstances = lens _ucisrsContainerInstances (\ s a -> s{_ucisrsContainerInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
ucisrsResponseStatus :: Lens' UpdateContainerInstancesStateResponse Int
ucisrsResponseStatus = lens _ucisrsResponseStatus (\ s a -> s{_ucisrsResponseStatus = a})

instance NFData UpdateContainerInstancesStateResponse
         where
