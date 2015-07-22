{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeregisterContainerInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon ECS container instance from the specified cluster.
-- This instance will no longer be available to run tasks.
--
-- If you intend to use the container instance for some other purpose after
-- deregistration, you should stop all of the tasks running on the
-- container instance before deregistration to avoid any orphaned tasks
-- from consuming resources.
--
-- Deregistering a container instance removes the instance from a cluster,
-- but it does not terminate the EC2 instance; if you are finished using
-- the instance, be sure to terminate it in the Amazon EC2 console to stop
-- billing.
--
-- When you terminate a container instance, it is automatically
-- deregistered from your cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterContainerInstance.html>
module Network.AWS.ECS.DeregisterContainerInstance
    (
    -- * Request
      DeregisterContainerInstance
    -- ** Request constructor
    , deregisterContainerInstance
    -- ** Request lenses
    , derrqCluster
    , derrqForce
    , derrqContainerInstance

    -- * Response
    , DeregisterContainerInstanceResponse
    -- ** Response constructor
    , deregisterContainerInstanceResponse
    -- ** Response lenses
    , dcirsContainerInstance
    , dcirsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterContainerInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derrqCluster'
--
-- * 'derrqForce'
--
-- * 'derrqContainerInstance'
data DeregisterContainerInstance = DeregisterContainerInstance'
    { _derrqCluster           :: !(Maybe Text)
    , _derrqForce             :: !(Maybe Bool)
    , _derrqContainerInstance :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterContainerInstance' smart constructor.
deregisterContainerInstance :: Text -> DeregisterContainerInstance
deregisterContainerInstance pContainerInstance =
    DeregisterContainerInstance'
    { _derrqCluster = Nothing
    , _derrqForce = Nothing
    , _derrqContainerInstance = pContainerInstance
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instance you want to deregister. If you do not
-- specify a cluster, the default cluster is assumed.
derrqCluster :: Lens' DeregisterContainerInstance (Maybe Text)
derrqCluster = lens _derrqCluster (\ s a -> s{_derrqCluster = a});

-- | Force the deregistration of the container instance. If you have tasks
-- running on the container instance when you deregister it with the
-- @force@ option, these tasks remain running and they will continue to
-- pass Elastic Load Balancing load balancer health checks until you
-- terminate the instance or the tasks stop through some other means, but
-- they are orphaned (no longer monitored or accounted for by Amazon ECS).
-- If an orphaned task on your container instance is part of an Amazon ECS
-- service, then the service scheduler will start another copy of that task
-- on a different container instance if possible.
derrqForce :: Lens' DeregisterContainerInstance (Maybe Bool)
derrqForce = lens _derrqForce (\ s a -> s{_derrqForce = a});

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance you want to deregister. The ARN contains the
-- @arn:aws:ecs@ namespace, followed by the region of the container
-- instance, the AWS account ID of the container instance owner, the
-- @container-instance@ namespace, and then the container instance UUID.
-- For example,
-- arn:aws:ecs:/region/:/aws_account_id/:container-instance\//container_instance_UUID/.
derrqContainerInstance :: Lens' DeregisterContainerInstance Text
derrqContainerInstance = lens _derrqContainerInstance (\ s a -> s{_derrqContainerInstance = a});

instance AWSRequest DeregisterContainerInstance where
        type Sv DeregisterContainerInstance = ECS
        type Rs DeregisterContainerInstance =
             DeregisterContainerInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeregisterContainerInstanceResponse' <$>
                   (x .?> "containerInstance") <*> (pure (fromEnum s)))

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
              ["cluster" .= _derrqCluster, "force" .= _derrqForce,
               "containerInstance" .= _derrqContainerInstance]

instance ToPath DeregisterContainerInstance where
        toPath = const "/"

instance ToQuery DeregisterContainerInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterContainerInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcirsContainerInstance'
--
-- * 'dcirsStatus'
data DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse'
    { _dcirsContainerInstance :: !(Maybe ContainerInstance)
    , _dcirsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterContainerInstanceResponse' smart constructor.
deregisterContainerInstanceResponse :: Int -> DeregisterContainerInstanceResponse
deregisterContainerInstanceResponse pStatus =
    DeregisterContainerInstanceResponse'
    { _dcirsContainerInstance = Nothing
    , _dcirsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dcirsContainerInstance :: Lens' DeregisterContainerInstanceResponse (Maybe ContainerInstance)
dcirsContainerInstance = lens _dcirsContainerInstance (\ s a -> s{_dcirsContainerInstance = a});

-- | FIXME: Undocumented member.
dcirsStatus :: Lens' DeregisterContainerInstanceResponse Int
dcirsStatus = lens _dcirsStatus (\ s a -> s{_dcirsStatus = a});
