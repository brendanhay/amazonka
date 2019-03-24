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
-- Module      : Network.AWS.Redshift.ResizeCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the size of the cluster. You can change the cluster's type, or change the number or type of nodes. The default behavior is to use the elastic resize method. With an elastic resize, your cluster is available for read and write operations more quickly than with the classic resize method.
--
--
-- Elastic resize operations have the following restrictions:
--
--     * You can only resize clusters of the following types:
--
--     * dc2.large
--
--     * dc2.8xlarge
--
--     * ds2.xlarge
--
--     * ds2.8xlarge
--
--
--
--     * The type of nodes that you add must match the node type for the cluster.
--
--
--
module Network.AWS.Redshift.ResizeCluster
    (
    -- * Creating a Request
      resizeCluster
    , ResizeCluster
    -- * Request Lenses
    , rcClassic
    , rcClusterType
    , rcNodeType
    , rcClusterIdentifier
    , rcNumberOfNodes

    -- * Destructuring the Response
    , resizeClusterResponse
    , ResizeClusterResponse
    -- * Response Lenses
    , rrsCluster
    , rrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resizeCluster' smart constructor.
data ResizeCluster = ResizeCluster'
  { _rcClassic           :: !(Maybe Bool)
  , _rcClusterType       :: !(Maybe Text)
  , _rcNodeType          :: !(Maybe Text)
  , _rcClusterIdentifier :: !Text
  , _rcNumberOfNodes     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResizeCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcClassic' - A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
--
-- * 'rcClusterType' - The new cluster type for the specified cluster.
--
-- * 'rcNodeType' - The new node type for the nodes you are adding.
--
-- * 'rcClusterIdentifier' - The unique identifier for the cluster to resize.
--
-- * 'rcNumberOfNodes' - The new number of nodes for the cluster.
resizeCluster
    :: Text -- ^ 'rcClusterIdentifier'
    -> Int -- ^ 'rcNumberOfNodes'
    -> ResizeCluster
resizeCluster pClusterIdentifier_ pNumberOfNodes_ =
  ResizeCluster'
    { _rcClassic = Nothing
    , _rcClusterType = Nothing
    , _rcNodeType = Nothing
    , _rcClusterIdentifier = pClusterIdentifier_
    , _rcNumberOfNodes = pNumberOfNodes_
    }


-- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
rcClassic :: Lens' ResizeCluster (Maybe Bool)
rcClassic = lens _rcClassic (\ s a -> s{_rcClassic = a})

-- | The new cluster type for the specified cluster.
rcClusterType :: Lens' ResizeCluster (Maybe Text)
rcClusterType = lens _rcClusterType (\ s a -> s{_rcClusterType = a})

-- | The new node type for the nodes you are adding.
rcNodeType :: Lens' ResizeCluster (Maybe Text)
rcNodeType = lens _rcNodeType (\ s a -> s{_rcNodeType = a})

-- | The unique identifier for the cluster to resize.
rcClusterIdentifier :: Lens' ResizeCluster Text
rcClusterIdentifier = lens _rcClusterIdentifier (\ s a -> s{_rcClusterIdentifier = a})

-- | The new number of nodes for the cluster.
rcNumberOfNodes :: Lens' ResizeCluster Int
rcNumberOfNodes = lens _rcNumberOfNodes (\ s a -> s{_rcNumberOfNodes = a})

instance AWSRequest ResizeCluster where
        type Rs ResizeCluster = ResizeClusterResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "ResizeClusterResult"
              (\ s h x ->
                 ResizeClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable ResizeCluster where

instance NFData ResizeCluster where

instance ToHeaders ResizeCluster where
        toHeaders = const mempty

instance ToPath ResizeCluster where
        toPath = const "/"

instance ToQuery ResizeCluster where
        toQuery ResizeCluster'{..}
          = mconcat
              ["Action" =: ("ResizeCluster" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Classic" =: _rcClassic,
               "ClusterType" =: _rcClusterType,
               "NodeType" =: _rcNodeType,
               "ClusterIdentifier" =: _rcClusterIdentifier,
               "NumberOfNodes" =: _rcNumberOfNodes]

-- | /See:/ 'resizeClusterResponse' smart constructor.
data ResizeClusterResponse = ResizeClusterResponse'
  { _rrsCluster        :: !(Maybe Cluster)
  , _rrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResizeClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsCluster' - Undocumented member.
--
-- * 'rrsResponseStatus' - -- | The response status code.
resizeClusterResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> ResizeClusterResponse
resizeClusterResponse pResponseStatus_ =
  ResizeClusterResponse'
    {_rrsCluster = Nothing, _rrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rrsCluster :: Lens' ResizeClusterResponse (Maybe Cluster)
rrsCluster = lens _rrsCluster (\ s a -> s{_rrsCluster = a})

-- | -- | The response status code.
rrsResponseStatus :: Lens' ResizeClusterResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a})

instance NFData ResizeClusterResponse where
