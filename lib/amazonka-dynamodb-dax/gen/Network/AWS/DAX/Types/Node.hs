{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Node
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Node where

import Network.AWS.DAX.Types.Endpoint
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an individual node within a DAX cluster.
--
--
--
-- /See:/ 'node' smart constructor.
data Node = Node'
  { _nNodeStatus :: !(Maybe Text),
    _nParameterGroupStatus :: !(Maybe Text),
    _nAvailabilityZone :: !(Maybe Text),
    _nNodeId :: !(Maybe Text),
    _nEndpoint :: !(Maybe Endpoint),
    _nNodeCreateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Node' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nNodeStatus' - The current status of the node. For example: @available@ .
--
-- * 'nParameterGroupStatus' - The status of the parameter group associated with this node. For example, @in-sync@ .
--
-- * 'nAvailabilityZone' - The Availability Zone (AZ) in which the node has been deployed.
--
-- * 'nNodeId' - A system-generated identifier for the node.
--
-- * 'nEndpoint' - The endpoint for the node, consisting of a DNS name and a port number. Client applications can connect directly to a node endpoint, if desired (as an alternative to allowing DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
--
-- * 'nNodeCreateTime' - The date and time (in UNIX epoch format) when the node was launched.
node ::
  Node
node =
  Node'
    { _nNodeStatus = Nothing,
      _nParameterGroupStatus = Nothing,
      _nAvailabilityZone = Nothing,
      _nNodeId = Nothing,
      _nEndpoint = Nothing,
      _nNodeCreateTime = Nothing
    }

-- | The current status of the node. For example: @available@ .
nNodeStatus :: Lens' Node (Maybe Text)
nNodeStatus = lens _nNodeStatus (\s a -> s {_nNodeStatus = a})

-- | The status of the parameter group associated with this node. For example, @in-sync@ .
nParameterGroupStatus :: Lens' Node (Maybe Text)
nParameterGroupStatus = lens _nParameterGroupStatus (\s a -> s {_nParameterGroupStatus = a})

-- | The Availability Zone (AZ) in which the node has been deployed.
nAvailabilityZone :: Lens' Node (Maybe Text)
nAvailabilityZone = lens _nAvailabilityZone (\s a -> s {_nAvailabilityZone = a})

-- | A system-generated identifier for the node.
nNodeId :: Lens' Node (Maybe Text)
nNodeId = lens _nNodeId (\s a -> s {_nNodeId = a})

-- | The endpoint for the node, consisting of a DNS name and a port number. Client applications can connect directly to a node endpoint, if desired (as an alternative to allowing DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
nEndpoint :: Lens' Node (Maybe Endpoint)
nEndpoint = lens _nEndpoint (\s a -> s {_nEndpoint = a})

-- | The date and time (in UNIX epoch format) when the node was launched.
nNodeCreateTime :: Lens' Node (Maybe UTCTime)
nNodeCreateTime = lens _nNodeCreateTime (\s a -> s {_nNodeCreateTime = a}) . mapping _Time

instance FromJSON Node where
  parseJSON =
    withObject
      "Node"
      ( \x ->
          Node'
            <$> (x .:? "NodeStatus")
            <*> (x .:? "ParameterGroupStatus")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "NodeId")
            <*> (x .:? "Endpoint")
            <*> (x .:? "NodeCreateTime")
      )

instance Hashable Node

instance NFData Node
