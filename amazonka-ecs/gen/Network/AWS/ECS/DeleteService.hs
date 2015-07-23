{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service within a cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteService.html>
module Network.AWS.ECS.DeleteService
    (
    -- * Request
      DeleteService
    -- ** Request constructor
    , deleteService
    -- ** Request lenses
    , dsrqCluster
    , dsrqService

    -- * Response
    , DeleteServiceResponse
    -- ** Response constructor
    , deleteServiceResponse
    -- ** Response lenses
    , dsrsService
    , dsrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteService' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrqCluster'
--
-- * 'dsrqService'
data DeleteService = DeleteService'
    { _dsrqCluster :: !(Maybe Text)
    , _dsrqService :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteService' smart constructor.
deleteService :: Text -> DeleteService
deleteService pService_ =
    DeleteService'
    { _dsrqCluster = Nothing
    , _dsrqService = pService_
    }

-- | The name of the cluster that hosts the service you want to delete.
dsrqCluster :: Lens' DeleteService (Maybe Text)
dsrqCluster = lens _dsrqCluster (\ s a -> s{_dsrqCluster = a});

-- | The name of the service you want to delete.
dsrqService :: Lens' DeleteService Text
dsrqService = lens _dsrqService (\ s a -> s{_dsrqService = a});

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
              ["cluster" .= _dsrqCluster,
               "service" .= _dsrqService]

instance ToPath DeleteService where
        toPath = const "/"

instance ToQuery DeleteService where
        toQuery = const mempty

-- | /See:/ 'deleteServiceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrsService'
--
-- * 'dsrsStatus'
data DeleteServiceResponse = DeleteServiceResponse'
    { _dsrsService :: !(Maybe ContainerService)
    , _dsrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteServiceResponse' smart constructor.
deleteServiceResponse :: Int -> DeleteServiceResponse
deleteServiceResponse pStatus_ =
    DeleteServiceResponse'
    { _dsrsService = Nothing
    , _dsrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dsrsService :: Lens' DeleteServiceResponse (Maybe ContainerService)
dsrsService = lens _dsrsService (\ s a -> s{_dsrsService = a});

-- | FIXME: Undocumented member.
dsrsStatus :: Lens' DeleteServiceResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
