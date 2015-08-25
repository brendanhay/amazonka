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
-- Module      : Network.AWS.ECS.DeleteCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster. You must deregister all container
-- instances from this cluster before you may delete it. You can list the
-- container instances in a cluster with ListContainerInstances and
-- deregister them with DeregisterContainerInstance.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteCluster.html AWS API Reference> for DeleteCluster.
module Network.AWS.ECS.DeleteCluster
    (
    -- * Creating a Request
      deleteCluster
    , DeleteCluster
    -- * Request Lenses
    , dcCluster

    -- * Destructuring the Response
    , deleteClusterResponse
    , DeleteClusterResponse
    -- * Response Lenses
    , drsCluster
    , drsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster'
    { _dcCluster :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCluster'
deleteCluster
    :: Text -- ^ 'dcCluster'
    -> DeleteCluster
deleteCluster pCluster_ =
    DeleteCluster'
    { _dcCluster = pCluster_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to delete.
dcCluster :: Lens' DeleteCluster Text
dcCluster = lens _dcCluster (\ s a -> s{_dcCluster = a});

instance AWSRequest DeleteCluster where
        type Rs DeleteCluster = DeleteClusterResponse
        request = postJSON eCS
        response
          = receiveJSON
              (\ s h x ->
                 DeleteClusterResponse' <$>
                   (x .?> "cluster") <*> (pure (fromEnum s)))

instance ToHeaders DeleteCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DeleteCluster"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCluster where
        toJSON DeleteCluster'{..}
          = object (catMaybes [Just ("cluster" .= _dcCluster)])

instance ToPath DeleteCluster where
        toPath = const "/"

instance ToQuery DeleteCluster where
        toQuery = const mempty

-- | /See:/ 'deleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
    { _drsCluster :: !(Maybe Cluster)
    , _drsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCluster'
--
-- * 'drsStatus'
deleteClusterResponse
    :: Int -- ^ 'drsStatus'
    -> DeleteClusterResponse
deleteClusterResponse pStatus_ =
    DeleteClusterResponse'
    { _drsCluster = Nothing
    , _drsStatus = pStatus_
    }

-- | The full description of the deleted cluster.
drsCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
drsCluster = lens _drsCluster (\ s a -> s{_drsCluster = a});

-- | The response status code.
drsStatus :: Lens' DeleteClusterResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
