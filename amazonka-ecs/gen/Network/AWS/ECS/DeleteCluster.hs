{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.DeleteCluster
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

-- | Deletes the specified cluster. You must deregister all container
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
    , dcCluster

    -- * Response
    , DeleteClusterResponse
    -- ** Response constructor
    , deleteClusterResponse
    -- ** Response lenses
    , dcrCluster
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'deleteCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcCluster'
newtype DeleteCluster = DeleteCluster'{_dcCluster :: Text} deriving (Eq, Read, Show)

-- | 'DeleteCluster' smart constructor.
deleteCluster :: Text -> DeleteCluster
deleteCluster pCluster = DeleteCluster'{_dcCluster = pCluster};

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to delete.
dcCluster :: Lens' DeleteCluster Text
dcCluster = lens _dcCluster (\ s a -> s{_dcCluster = a});

instance AWSRequest DeleteCluster where
        type Sv DeleteCluster = ECS
        type Rs DeleteCluster = DeleteClusterResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteClusterResponse' <$> x .?> "cluster")

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
          = object ["cluster" .= _dcCluster]

instance ToPath DeleteCluster where
        toPath = const "/"

instance ToQuery DeleteCluster where
        toQuery = const mempty

-- | /See:/ 'deleteClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrCluster'
newtype DeleteClusterResponse = DeleteClusterResponse'{_dcrCluster :: Maybe Cluster} deriving (Eq, Read, Show)

-- | 'DeleteClusterResponse' smart constructor.
deleteClusterResponse :: DeleteClusterResponse
deleteClusterResponse = DeleteClusterResponse'{_dcrCluster = Nothing};

-- | The full description of the deleted cluster.
dcrCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
dcrCluster = lens _dcrCluster (\ s a -> s{_dcrCluster = a});
