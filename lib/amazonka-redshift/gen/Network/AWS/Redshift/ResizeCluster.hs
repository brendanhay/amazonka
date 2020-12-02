{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ResizeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--     * dc1.large (if your cluster is in a VPC)
--
--     * dc1.8xlarge (if your cluster is in a VPC)
--
--     * dc2.large
--
--     * dc2.8xlarge
--
--     * ds2.xlarge
--
--     * ds2.8xlarge
--
--     * ra3.4xlarge
--
--     * ra3.16xlarge
--
--
--
--     * The type of nodes that you add must match the node type for the cluster.
module Network.AWS.Redshift.ResizeCluster
  ( -- * Creating a Request
    resizeCluster,
    ResizeCluster,

    -- * Request Lenses
    rcNumberOfNodes,
    rcClassic,
    rcClusterType,
    rcNodeType,
    rcClusterIdentifier,

    -- * Destructuring the Response
    resizeClusterResponse,
    ResizeClusterResponse,

    -- * Response Lenses
    resrsCluster,
    resrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | Describes a resize cluster operation. For example, a scheduled action to run the @ResizeCluster@ API operation.
--
--
--
-- /See:/ 'resizeCluster' smart constructor.
data ResizeCluster = ResizeCluster'
  { _rcNumberOfNodes ::
      !(Maybe Int),
    _rcClassic :: !(Maybe Bool),
    _rcClusterType :: !(Maybe Text),
    _rcNodeType :: !(Maybe Text),
    _rcClusterIdentifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResizeCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcNumberOfNodes' - The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
--
-- * 'rcClassic' - A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
--
-- * 'rcClusterType' - The new cluster type for the specified cluster.
--
-- * 'rcNodeType' - The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
--
-- * 'rcClusterIdentifier' - The unique identifier for the cluster to resize.
resizeCluster ::
  -- | 'rcClusterIdentifier'
  Text ->
  ResizeCluster
resizeCluster pClusterIdentifier_ =
  ResizeCluster'
    { _rcNumberOfNodes = Nothing,
      _rcClassic = Nothing,
      _rcClusterType = Nothing,
      _rcNodeType = Nothing,
      _rcClusterIdentifier = pClusterIdentifier_
    }

-- | The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
rcNumberOfNodes :: Lens' ResizeCluster (Maybe Int)
rcNumberOfNodes = lens _rcNumberOfNodes (\s a -> s {_rcNumberOfNodes = a})

-- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
rcClassic :: Lens' ResizeCluster (Maybe Bool)
rcClassic = lens _rcClassic (\s a -> s {_rcClassic = a})

-- | The new cluster type for the specified cluster.
rcClusterType :: Lens' ResizeCluster (Maybe Text)
rcClusterType = lens _rcClusterType (\s a -> s {_rcClusterType = a})

-- | The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
rcNodeType :: Lens' ResizeCluster (Maybe Text)
rcNodeType = lens _rcNodeType (\s a -> s {_rcNodeType = a})

-- | The unique identifier for the cluster to resize.
rcClusterIdentifier :: Lens' ResizeCluster Text
rcClusterIdentifier = lens _rcClusterIdentifier (\s a -> s {_rcClusterIdentifier = a})

instance AWSRequest ResizeCluster where
  type Rs ResizeCluster = ResizeClusterResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "ResizeClusterResult"
      ( \s h x ->
          ResizeClusterResponse'
            <$> (x .@? "Cluster") <*> (pure (fromEnum s))
      )

instance Hashable ResizeCluster

instance NFData ResizeCluster

instance ToHeaders ResizeCluster where
  toHeaders = const mempty

instance ToPath ResizeCluster where
  toPath = const "/"

instance ToQuery ResizeCluster where
  toQuery ResizeCluster' {..} =
    mconcat
      [ "Action" =: ("ResizeCluster" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "NumberOfNodes" =: _rcNumberOfNodes,
        "Classic" =: _rcClassic,
        "ClusterType" =: _rcClusterType,
        "NodeType" =: _rcNodeType,
        "ClusterIdentifier" =: _rcClusterIdentifier
      ]

-- | /See:/ 'resizeClusterResponse' smart constructor.
data ResizeClusterResponse = ResizeClusterResponse'
  { _resrsCluster ::
      !(Maybe Cluster),
    _resrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResizeClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'resrsCluster' - Undocumented member.
--
-- * 'resrsResponseStatus' - -- | The response status code.
resizeClusterResponse ::
  -- | 'resrsResponseStatus'
  Int ->
  ResizeClusterResponse
resizeClusterResponse pResponseStatus_ =
  ResizeClusterResponse'
    { _resrsCluster = Nothing,
      _resrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
resrsCluster :: Lens' ResizeClusterResponse (Maybe Cluster)
resrsCluster = lens _resrsCluster (\s a -> s {_resrsCluster = a})

-- | -- | The response status code.
resrsResponseStatus :: Lens' ResizeClusterResponse Int
resrsResponseStatus = lens _resrsResponseStatus (\s a -> s {_resrsResponseStatus = a})

instance NFData ResizeClusterResponse
