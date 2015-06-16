{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.DeleteService
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

-- | Deletes a specified service within a cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteService.html>
module Network.AWS.ECS.DeleteService
    (
    -- * Request
      DeleteService
    -- ** Request constructor
    , deleteService
    -- ** Request lenses
    , dsCluster
    , dsContainerService

    -- * Response
    , DeleteServiceResponse
    -- ** Response constructor
    , deleteServiceResponse
    -- ** Response lenses
    , dsrContainerService
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'deleteService' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsCluster'
--
-- * 'dsContainerService'
data DeleteService = DeleteService'{_dsCluster :: Maybe Text, _dsContainerService :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DeleteService' smart constructor.
deleteService :: DeleteService
deleteService = DeleteService'{_dsCluster = Nothing, _dsContainerService = Nothing};

-- | The name of the cluster that hosts the service you want to delete.
dsCluster :: Lens' DeleteService (Maybe Text)
dsCluster = lens _dsCluster (\ s a -> s{_dsCluster = a});

-- | The name of the service you want to delete.
dsContainerService :: Lens' DeleteService (Maybe Text)
dsContainerService = lens _dsContainerService (\ s a -> s{_dsContainerService = a});

instance AWSRequest DeleteService where
        type Sv DeleteService = ECS
        type Rs DeleteService = DeleteServiceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteServiceResponse' <$>
                   (x .?> "ContainerService"))

instance ToHeaders DeleteService where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DeleteService"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteService where
        toJSON DeleteService'{..}
          = object
              ["cluster" .= _dsCluster,
               "ContainerService" .= _dsContainerService]

instance ToPath DeleteService where
        toPath = const "/"

instance ToQuery DeleteService where
        toQuery = const mempty

-- | /See:/ 'deleteServiceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrContainerService'
newtype DeleteServiceResponse = DeleteServiceResponse'{_dsrContainerService :: Maybe ContainerService} deriving (Eq, Read, Show)

-- | 'DeleteServiceResponse' smart constructor.
deleteServiceResponse :: DeleteServiceResponse
deleteServiceResponse = DeleteServiceResponse'{_dsrContainerService = Nothing};

-- | FIXME: Undocumented member.
dsrContainerService :: Lens' DeleteServiceResponse (Maybe ContainerService)
dsrContainerService = lens _dsrContainerService (\ s a -> s{_dsrContainerService = a});
