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
-- Module      : Network.AWS.ECS.CreateCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon ECS cluster. By default, your account will receive
-- a @default@ cluster when you launch your first container instance.
-- However, you can create your own cluster with a unique name with the
-- @CreateCluster@ action.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateCluster.html AWS API Reference> for CreateCluster.
module Network.AWS.ECS.CreateCluster
    (
    -- * Creating a Request
      CreateCluster
    , createCluster
    -- * Request Lenses
    , ccClusterName

    -- * Destructuring the Response
    , CreateClusterResponse
    , createClusterResponse
    -- * Response Lenses
    , ccrsCluster
    , ccrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccClusterName'
newtype CreateCluster = CreateCluster'
    { _ccClusterName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCluster' smart constructor.
createCluster :: CreateCluster
createCluster =
    CreateCluster'
    { _ccClusterName = Nothing
    }

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
                 CreateClusterResponse' <$>
                   (x .?> "cluster") <*> (pure (fromEnum s)))

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
-- * 'ccrsCluster'
--
-- * 'ccrsStatus'
data CreateClusterResponse = CreateClusterResponse'
    { _ccrsCluster :: !(Maybe Cluster)
    , _ccrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterResponse' smart constructor.
createClusterResponse :: Int -> CreateClusterResponse
createClusterResponse pStatus_ =
    CreateClusterResponse'
    { _ccrsCluster = Nothing
    , _ccrsStatus = pStatus_
    }

-- | The full description of your new cluster.
ccrsCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrsCluster = lens _ccrsCluster (\ s a -> s{_ccrsCluster = a});

-- | Undocumented member.
ccrsStatus :: Lens' CreateClusterResponse Int
ccrsStatus = lens _ccrsStatus (\ s a -> s{_ccrsStatus = a});
