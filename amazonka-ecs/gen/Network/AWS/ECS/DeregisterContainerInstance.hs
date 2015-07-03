{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ECS.DeregisterContainerInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deregisters an Amazon ECS container instance from the specified cluster.
-- This instance will no longer be available to run tasks.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterContainerInstance.html>
module Network.AWS.ECS.DeregisterContainerInstance
    (
    -- * Request
      DeregisterContainerInstance
    -- ** Request constructor
    , deregisterContainerInstance
    -- ** Request lenses
    , derCluster
    , derForce
    , derContainerInstance

    -- * Response
    , DeregisterContainerInstanceResponse
    -- ** Response constructor
    , deregisterContainerInstanceResponse
    -- ** Response lenses
    , dcirContainerInstance
    , dcirStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterContainerInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derCluster'
--
-- * 'derForce'
--
-- * 'derContainerInstance'
data DeregisterContainerInstance = DeregisterContainerInstance'
    { _derCluster           :: !(Maybe Text)
    , _derForce             :: !(Maybe Bool)
    , _derContainerInstance :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeregisterContainerInstance' smart constructor.
deregisterContainerInstance :: Text -> DeregisterContainerInstance
deregisterContainerInstance pContainerInstance =
    DeregisterContainerInstance'
    { _derCluster = Nothing
    , _derForce = Nothing
    , _derContainerInstance = pContainerInstance
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instance you want to deregister. If you do not
-- specify a cluster, the default cluster is assumed.
derCluster :: Lens' DeregisterContainerInstance (Maybe Text)
derCluster = lens _derCluster (\ s a -> s{_derCluster = a});

-- | Force the deregistration of the container instance. You can use the
-- @force@ parameter if you have several tasks running on a container
-- instance and you don\'t want to run @StopTask@ for each task before
-- deregistering the container instance.
derForce :: Lens' DeregisterContainerInstance (Maybe Bool)
derForce = lens _derForce (\ s a -> s{_derForce = a});

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance you want to deregister. The ARN contains the
-- @arn:aws:ecs@ namespace, followed by the region of the container
-- instance, the AWS account ID of the container instance owner, the
-- @container-instance@ namespace, and then the container instance UUID.
-- For example,
-- arn:aws:ecs:/region/:/aws_account_id/:container-instance\//container_instance_UUID/.
derContainerInstance :: Lens' DeregisterContainerInstance Text
derContainerInstance = lens _derContainerInstance (\ s a -> s{_derContainerInstance = a});

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
              ["cluster" .= _derCluster, "force" .= _derForce,
               "containerInstance" .= _derContainerInstance]

instance ToPath DeregisterContainerInstance where
        toPath = const "/"

instance ToQuery DeregisterContainerInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterContainerInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcirContainerInstance'
--
-- * 'dcirStatus'
data DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse'
    { _dcirContainerInstance :: !(Maybe ContainerInstance)
    , _dcirStatus            :: !Int
    } deriving (Eq,Read,Show)

-- | 'DeregisterContainerInstanceResponse' smart constructor.
deregisterContainerInstanceResponse :: Int -> DeregisterContainerInstanceResponse
deregisterContainerInstanceResponse pStatus =
    DeregisterContainerInstanceResponse'
    { _dcirContainerInstance = Nothing
    , _dcirStatus = pStatus
    }

-- | FIXME: Undocumented member.
dcirContainerInstance :: Lens' DeregisterContainerInstanceResponse (Maybe ContainerInstance)
dcirContainerInstance = lens _dcirContainerInstance (\ s a -> s{_dcirContainerInstance = a});

-- | FIXME: Undocumented member.
dcirStatus :: Lens' DeregisterContainerInstanceResponse Int
dcirStatus = lens _dcirStatus (\ s a -> s{_dcirStatus = a});
