{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster. You must deregister all container
-- instances from this cluster before you may delete it. You can list the
-- container instances in a cluster with ListContainerInstances and
-- deregister them with DeregisterContainerInstance.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteCluster.html>
module Network.AWS.ECS.DeleteCluster
    (
    -- * Request
      DeleteCluster
    -- ** Request constructor
    , deleteCluster
    -- ** Request lenses
    , dcrqCluster

    -- * Response
    , DeleteClusterResponse
    -- ** Response constructor
    , deleteClusterResponse
    -- ** Response lenses
    , dcrsCluster
    , dcrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrqCluster'
newtype DeleteCluster = DeleteCluster'
    { _dcrqCluster :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCluster' smart constructor.
deleteCluster :: Text -> DeleteCluster
deleteCluster pCluster =
    DeleteCluster'
    { _dcrqCluster = pCluster
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to delete.
dcrqCluster :: Lens' DeleteCluster Text
dcrqCluster = lens _dcrqCluster (\ s a -> s{_dcrqCluster = a});

instance AWSRequest DeleteCluster where
        type Sv DeleteCluster = ECS
        type Rs DeleteCluster = DeleteClusterResponse
        request = postJSON
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
          = object ["cluster" .= _dcrqCluster]

instance ToPath DeleteCluster where
        toPath = const "/"

instance ToQuery DeleteCluster where
        toQuery = const mempty

-- | /See:/ 'deleteClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsCluster'
--
-- * 'dcrsStatus'
data DeleteClusterResponse = DeleteClusterResponse'
    { _dcrsCluster :: !(Maybe Cluster)
    , _dcrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterResponse' smart constructor.
deleteClusterResponse :: Int -> DeleteClusterResponse
deleteClusterResponse pStatus =
    DeleteClusterResponse'
    { _dcrsCluster = Nothing
    , _dcrsStatus = pStatus
    }

-- | The full description of the deleted cluster.
dcrsCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
dcrsCluster = lens _dcrsCluster (\ s a -> s{_dcrsCluster = a});

-- | FIXME: Undocumented member.
dcrsStatus :: Lens' DeleteClusterResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});
