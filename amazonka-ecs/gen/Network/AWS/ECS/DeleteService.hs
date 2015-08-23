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
-- Module      : Network.AWS.ECS.DeleteService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service within a cluster.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteService.html AWS API Reference> for DeleteService.
module Network.AWS.ECS.DeleteService
    (
    -- * Creating a Request
      deleteService
    , DeleteService
    -- * Request Lenses
    , dsCluster
    , dsService

    -- * Destructuring the Response
    , deleteServiceResponse
    , DeleteServiceResponse
    -- * Response Lenses
    , dsrsService
    , dsrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteService' smart constructor.
data DeleteService = DeleteService'
    { _dsCluster :: !(Maybe Text)
    , _dsService :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsCluster'
--
-- * 'dsService'
deleteService
    :: Text -- ^ 'dsService'
    -> DeleteService
deleteService pService_ =
    DeleteService'
    { _dsCluster = Nothing
    , _dsService = pService_
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
              (catMaybes
                 [("cluster" .=) <$> _dsCluster,
                  Just ("service" .= _dsService)])

instance ToPath DeleteService where
        toPath = const "/"

instance ToQuery DeleteService where
        toQuery = const mempty

-- | /See:/ 'deleteServiceResponse' smart constructor.
data DeleteServiceResponse = DeleteServiceResponse'
    { _dsrsService :: !(Maybe ContainerService)
    , _dsrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsService'
--
-- * 'dsrsStatus'
deleteServiceResponse
    :: Int -- ^ 'dsrsStatus'
    -> DeleteServiceResponse
deleteServiceResponse pStatus_ =
    DeleteServiceResponse'
    { _dsrsService = Nothing
    , _dsrsStatus = pStatus_
    }

-- | Undocumented member.
dsrsService :: Lens' DeleteServiceResponse (Maybe ContainerService)
dsrsService = lens _dsrsService (\ s a -> s{_dsrsService = a});

-- | The response status code.
dsrsStatus :: Lens' DeleteServiceResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
