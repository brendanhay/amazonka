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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon ECS cluster. By default, your account receives a @default@ cluster when you launch your first container instance. However, you can create your own cluster with a unique name with the @CreateCluster@ action.
--
--
module Network.AWS.ECS.CreateCluster
    (
    -- * Creating a Request
      createCluster
    , CreateCluster
    -- * Request Lenses
    , ccClusterName

    -- * Destructuring the Response
    , createClusterResponse
    , CreateClusterResponse
    -- * Response Lenses
    , ccrsCluster
    , ccrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCluster' smart constructor.
newtype CreateCluster = CreateCluster'
  { _ccClusterName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccClusterName' - The name of your cluster. If you do not specify a name for your cluster, you create a cluster named @default@ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
createCluster
    :: CreateCluster
createCluster = CreateCluster' {_ccClusterName = Nothing}


-- | The name of your cluster. If you do not specify a name for your cluster, you create a cluster named @default@ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
ccClusterName :: Lens' CreateCluster (Maybe Text)
ccClusterName = lens _ccClusterName (\ s a -> s{_ccClusterName = a})

instance AWSRequest CreateCluster where
        type Rs CreateCluster = CreateClusterResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 CreateClusterResponse' <$>
                   (x .?> "cluster") <*> (pure (fromEnum s)))

instance Hashable CreateCluster where

instance NFData CreateCluster where

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
          = object
              (catMaybes [("clusterName" .=) <$> _ccClusterName])

instance ToPath CreateCluster where
        toPath = const "/"

instance ToQuery CreateCluster where
        toQuery = const mempty

-- | /See:/ 'createClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { _ccrsCluster        :: !(Maybe Cluster)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsCluster' - The full description of your new cluster.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createClusterResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateClusterResponse
createClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    {_ccrsCluster = Nothing, _ccrsResponseStatus = pResponseStatus_}


-- | The full description of your new cluster.
ccrsCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrsCluster = lens _ccrsCluster (\ s a -> s{_ccrsCluster = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateClusterResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateClusterResponse where
