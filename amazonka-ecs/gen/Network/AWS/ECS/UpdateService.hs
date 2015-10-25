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
-- Module      : Network.AWS.ECS.UpdateService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the desired count or task definition used in a service.
--
-- You can add to or subtract from the number of instantiations of a task
-- definition in a service by specifying the cluster that the service is
-- running in and a new 'desiredCount' parameter.
--
-- You can use 'UpdateService' to modify your task definition and deploy a
-- new version of your service, one task at a time. If you modify the task
-- definition with 'UpdateService', Amazon ECS spawns a task with the new
-- version of the task definition and then stops an old task after the new
-- version is running. Because 'UpdateService' starts a new version of the
-- task before stopping an old version, your cluster must have capacity to
-- support one more instantiation of the task when 'UpdateService' is run.
-- If your cluster cannot support another instantiation of the task used in
-- your service, you can reduce the desired count of your service by one
-- before modifying the task definition.
--
-- When UpdateService replaces a task during an update, the equivalent of
-- 'docker stop' is issued to the containers running in the task. This
-- results in a 'SIGTERM' and a 30-second timeout, after which 'SIGKILL' is
-- sent and the containers are forcibly stopped. If the container handles
-- the 'SIGTERM' gracefully and exits within 30 seconds from receiving it,
-- no 'SIGKILL' is sent.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_UpdateService.html AWS API Reference> for UpdateService.
module Network.AWS.ECS.UpdateService
    (
    -- * Creating a Request
      updateService
    , UpdateService
    -- * Request Lenses
    , usCluster
    , usDesiredCount
    , usTaskDefinition
    , usService

    -- * Destructuring the Response
    , updateServiceResponse
    , UpdateServiceResponse
    -- * Response Lenses
    , usrsService
    , usrsResponseStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateService' smart constructor.
data UpdateService = UpdateService'
    { _usCluster        :: !(Maybe Text)
    , _usDesiredCount   :: !(Maybe Int)
    , _usTaskDefinition :: !(Maybe Text)
    , _usService        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usCluster'
--
-- * 'usDesiredCount'
--
-- * 'usTaskDefinition'
--
-- * 'usService'
updateService
    :: Text -- ^ 'usService'
    -> UpdateService
updateService pService_ =
    UpdateService'
    { _usCluster = Nothing
    , _usDesiredCount = Nothing
    , _usTaskDefinition = Nothing
    , _usService = pService_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- your service is running on. If you do not specify a cluster, the default
-- cluster is assumed.
usCluster :: Lens' UpdateService (Maybe Text)
usCluster = lens _usCluster (\ s a -> s{_usCluster = a});

-- | The number of instantiations of the task to place and keep running in
-- your service.
usDesiredCount :: Lens' UpdateService (Maybe Int)
usDesiredCount = lens _usDesiredCount (\ s a -> s{_usDesiredCount = a});

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource
-- Name (ARN) of the task definition to run in your service. If a
-- 'revision' is not specified, the latest 'ACTIVE' revision is used. If
-- you modify the task definition with 'UpdateService', Amazon ECS spawns a
-- task with the new version of the task definition and then stops an old
-- task after the new version is running.
usTaskDefinition :: Lens' UpdateService (Maybe Text)
usTaskDefinition = lens _usTaskDefinition (\ s a -> s{_usTaskDefinition = a});

-- | The name of the service to update.
usService :: Lens' UpdateService Text
usService = lens _usService (\ s a -> s{_usService = a});

instance AWSRequest UpdateService where
        type Rs UpdateService = UpdateServiceResponse
        request = postJSON eCS
        response
          = receiveJSON
              (\ s h x ->
                 UpdateServiceResponse' <$>
                   (x .?> "service") <*> (pure (fromEnum s)))

instance ToHeaders UpdateService where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.UpdateService"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateService where
        toJSON UpdateService'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _usCluster,
                  ("desiredCount" .=) <$> _usDesiredCount,
                  ("taskDefinition" .=) <$> _usTaskDefinition,
                  Just ("service" .= _usService)])

instance ToPath UpdateService where
        toPath = const "/"

instance ToQuery UpdateService where
        toQuery = const mempty

-- | /See:/ 'updateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
    { _usrsService        :: !(Maybe ContainerService)
    , _usrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsService'
--
-- * 'usrsResponseStatus'
updateServiceResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateServiceResponse
updateServiceResponse pResponseStatus_ =
    UpdateServiceResponse'
    { _usrsService = Nothing
    , _usrsResponseStatus = pResponseStatus_
    }

-- | The full description of your service following the update call.
usrsService :: Lens' UpdateServiceResponse (Maybe ContainerService)
usrsService = lens _usrsService (\ s a -> s{_usrsService = a});

-- | The response status code.
usrsResponseStatus :: Lens' UpdateServiceResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a});
