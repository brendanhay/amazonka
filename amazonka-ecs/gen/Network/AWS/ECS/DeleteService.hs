{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , dsService

    -- * Response
    , DeleteServiceResponse
    -- ** Response constructor
    , deleteServiceResponse
    -- ** Response lenses
    , dsrService
    , dsrStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteService' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsCluster'
--
-- * 'dsService'
data DeleteService = DeleteService'
    { _dsCluster :: !(Maybe Text)
    , _dsService :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteService' smart constructor.
deleteService :: Text -> DeleteService
deleteService pService =
    DeleteService'
    { _dsCluster = Nothing
    , _dsService = pService
    }

-- | The name of the cluster that hosts the service you want to delete.
dsCluster :: Lens' DeleteService (Maybe Text)
dsCluster = lens _dsCluster (\ s a -> s{_dsCluster = a});

-- | The name of the service you want to delete.
dsService :: Lens' DeleteService Text
dsService = lens _dsService (\ s a -> s{_dsService = a});

instance AWSRequest DeleteService where
        type Sv DeleteService = ECS
        type Rs DeleteService = DeleteServiceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteServiceResponse' <$>
                   (x .?> "service") <*> (pure (fromEnum s)))

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
              ["cluster" .= _dsCluster, "service" .= _dsService]

instance ToPath DeleteService where
        toPath = const "/"

instance ToQuery DeleteService where
        toQuery = const mempty

-- | /See:/ 'deleteServiceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrService'
--
-- * 'dsrStatus'
data DeleteServiceResponse = DeleteServiceResponse'
    { _dsrService :: !(Maybe ContainerService)
    , _dsrStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteServiceResponse' smart constructor.
deleteServiceResponse :: Int -> DeleteServiceResponse
deleteServiceResponse pStatus =
    DeleteServiceResponse'
    { _dsrService = Nothing
    , _dsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dsrService :: Lens' DeleteServiceResponse (Maybe ContainerService)
dsrService = lens _dsrService (\ s a -> s{_dsrService = a});

-- | FIXME: Undocumented member.
dsrStatus :: Lens' DeleteServiceResponse Int
dsrStatus = lens _dsrStatus (\ s a -> s{_dsrStatus = a});
