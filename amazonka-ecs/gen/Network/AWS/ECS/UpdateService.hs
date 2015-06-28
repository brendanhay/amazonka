{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ECS.UpdateService
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modify the desired count or task definition used in a service.
--
-- You can add to or subtract from the number of instantiations of a task
-- definition in a service by specifying the cluster that the service is
-- running in and a new @desiredCount@ parameter.
--
-- You can use @UpdateService@ to modify your task definition and deploy a
-- new version of your service, one task at a time. If you modify the task
-- definition with @UpdateService@, Amazon ECS spawns a task with the new
-- version of the task definition and then stops an old task after the new
-- version is running. Because @UpdateService@ starts a new version of the
-- task before stopping an old version, your cluster must have capacity to
-- support one more instantiation of the task when @UpdateService@ is run.
-- If your cluster cannot support another instantiation of the task used in
-- your service, you can reduce the desired count of your service by one
-- before modifying the task definition.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_UpdateService.html>
module Network.AWS.ECS.UpdateService
    (
    -- * Request
      UpdateService
    -- ** Request constructor
    , updateService
    -- ** Request lenses
    , usCluster
    , usDesiredCount
    , usTaskDefinition
    , usService

    -- * Response
    , UpdateServiceResponse
    -- ** Response constructor
    , updateServiceResponse
    -- ** Response lenses
    , usrService
    , usrStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateService' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usCluster'
--
-- * 'usDesiredCount'
--
-- * 'usTaskDefinition'
--
-- * 'usService'
data UpdateService = UpdateService'
    { _usCluster        :: !(Maybe Text)
    , _usDesiredCount   :: !(Maybe Int)
    , _usTaskDefinition :: !(Maybe Text)
    , _usService        :: !Text
    } deriving (Eq,Read,Show)

-- | 'UpdateService' smart constructor.
updateService :: Text -> UpdateService
updateService pService =
    UpdateService'
    { _usCluster = Nothing
    , _usDesiredCount = Nothing
    , _usTaskDefinition = Nothing
    , _usService = pService
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- your service is running on. If you do not specify a cluster, the default
-- cluster is assumed.
usCluster :: Lens' UpdateService (Maybe Text)
usCluster = lens _usCluster (\ s a -> s{_usCluster = a});

-- | The number of instantiations of the task that you would like to place
-- and keep running in your service.
usDesiredCount :: Lens' UpdateService (Maybe Int)
usDesiredCount = lens _usDesiredCount (\ s a -> s{_usDesiredCount = a});

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to run in your service.
-- If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
-- If you modify the task definition with @UpdateService@, Amazon ECS
-- spawns a task with the new version of the task definition and then stops
-- an old task after the new version is running.
usTaskDefinition :: Lens' UpdateService (Maybe Text)
usTaskDefinition = lens _usTaskDefinition (\ s a -> s{_usTaskDefinition = a});

-- | The name of the service that you want to update.
usService :: Lens' UpdateService Text
usService = lens _usService (\ s a -> s{_usService = a});

instance AWSRequest UpdateService where
        type Sv UpdateService = ECS
        type Rs UpdateService = UpdateServiceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateServiceResponse' <$>
                   (x .?> "service") <*> (pure s))

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
              ["cluster" .= _usCluster,
               "desiredCount" .= _usDesiredCount,
               "taskDefinition" .= _usTaskDefinition,
               "service" .= _usService]

instance ToPath UpdateService where
        toPath = const "/"

instance ToQuery UpdateService where
        toQuery = const mempty

-- | /See:/ 'updateServiceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usrService'
--
-- * 'usrStatus'
data UpdateServiceResponse = UpdateServiceResponse'
    { _usrService :: !(Maybe ContainerService)
    , _usrStatus  :: !Status
    } deriving (Eq,Read,Show)

-- | 'UpdateServiceResponse' smart constructor.
updateServiceResponse :: Status -> UpdateServiceResponse
updateServiceResponse pStatus =
    UpdateServiceResponse'
    { _usrService = Nothing
    , _usrStatus = pStatus
    }

-- | The full description of your service following the update call.
usrService :: Lens' UpdateServiceResponse (Maybe ContainerService)
usrService = lens _usrService (\ s a -> s{_usrService = a});

-- | FIXME: Undocumented member.
usrStatus :: Lens' UpdateServiceResponse Status
usrStatus = lens _usrStatus (\ s a -> s{_usrStatus = a});
