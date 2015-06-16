{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.CreateCluster
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

-- | Creates a new Amazon ECS cluster. By default, your account will receive
-- a @default@ cluster when you launch your first container instance.
-- However, you can create your own cluster with a unique name with the
-- @CreateCluster@ action.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateCluster.html>
module Network.AWS.ECS.CreateCluster
    (
    -- * Request
      CreateCluster
    -- ** Request constructor
    , createCluster
    -- ** Request lenses
    , ccClusterName

    -- * Response
    , CreateClusterResponse
    -- ** Response constructor
    , createClusterResponse
    -- ** Response lenses
    , ccrCluster
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'createCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccClusterName'
newtype CreateCluster = CreateCluster'{_ccClusterName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateCluster' smart constructor.
createCluster :: CreateCluster
createCluster = CreateCluster'{_ccClusterName = Nothing};

-- | The name of your cluster. If you do not specify a name for your cluster,
-- you will create a cluster named @default@. Up to 255 letters (uppercase
-- and lowercase), numbers, hyphens, and underscores are allowed.
ccClusterName :: Lens' CreateCluster (Maybe Text)
ccClusterName = lens _ccClusterName (\ s a -> s{_ccClusterName = a});

instance AWSRequest CreateCluster where
        type Sv CreateCluster = ECS
        type Rs CreateCluster = CreateClusterResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateClusterResponse' <$> (x .?> "cluster"))

instance ToHeaders CreateCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.CreateCluster"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCluster where
        toJSON CreateCluster'{..}
          = object ["clusterName" .= _ccClusterName]

instance ToPath CreateCluster where
        toPath = const "/"

instance ToQuery CreateCluster where
        toQuery = const mempty

-- | /See:/ 'createClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrCluster'
newtype CreateClusterResponse = CreateClusterResponse'{_ccrCluster :: Maybe Cluster} deriving (Eq, Read, Show)

-- | 'CreateClusterResponse' smart constructor.
createClusterResponse :: CreateClusterResponse
createClusterResponse = CreateClusterResponse'{_ccrCluster = Nothing};

-- | The full description of your new cluster.
ccrCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrCluster = lens _ccrCluster (\ s a -> s{_ccrCluster = a});
