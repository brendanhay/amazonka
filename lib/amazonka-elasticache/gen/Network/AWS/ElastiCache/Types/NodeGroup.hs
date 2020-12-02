{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroup where

import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.NodeGroupMember
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a collection of cache nodes in a replication group. One node in the node group is the read/write primary node. All the other nodes are read-only Replica nodes.
--
--
--
-- /See:/ 'nodeGroup' smart constructor.
data NodeGroup = NodeGroup'
  { _ngStatus :: !(Maybe Text),
    _ngPrimaryEndpoint :: !(Maybe Endpoint),
    _ngSlots :: !(Maybe Text),
    _ngNodeGroupMembers :: !(Maybe [NodeGroupMember]),
    _ngNodeGroupId :: !(Maybe Text),
    _ngReaderEndpoint :: !(Maybe Endpoint)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngStatus' - The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ .
--
-- * 'ngPrimaryEndpoint' - The endpoint of the primary node in this node group (shard).
--
-- * 'ngSlots' - The keyspace for this node group (shard).
--
-- * 'ngNodeGroupMembers' - A list containing information about individual nodes within the node group (shard).
--
-- * 'ngNodeGroupId' - The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user can provide the id for a node group.
--
-- * 'ngReaderEndpoint' - The endpoint of the replica nodes in this node group (shard).
nodeGroup ::
  NodeGroup
nodeGroup =
  NodeGroup'
    { _ngStatus = Nothing,
      _ngPrimaryEndpoint = Nothing,
      _ngSlots = Nothing,
      _ngNodeGroupMembers = Nothing,
      _ngNodeGroupId = Nothing,
      _ngReaderEndpoint = Nothing
    }

-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ .
ngStatus :: Lens' NodeGroup (Maybe Text)
ngStatus = lens _ngStatus (\s a -> s {_ngStatus = a})

-- | The endpoint of the primary node in this node group (shard).
ngPrimaryEndpoint :: Lens' NodeGroup (Maybe Endpoint)
ngPrimaryEndpoint = lens _ngPrimaryEndpoint (\s a -> s {_ngPrimaryEndpoint = a})

-- | The keyspace for this node group (shard).
ngSlots :: Lens' NodeGroup (Maybe Text)
ngSlots = lens _ngSlots (\s a -> s {_ngSlots = a})

-- | A list containing information about individual nodes within the node group (shard).
ngNodeGroupMembers :: Lens' NodeGroup [NodeGroupMember]
ngNodeGroupMembers = lens _ngNodeGroupMembers (\s a -> s {_ngNodeGroupMembers = a}) . _Default . _Coerce

-- | The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user can provide the id for a node group.
ngNodeGroupId :: Lens' NodeGroup (Maybe Text)
ngNodeGroupId = lens _ngNodeGroupId (\s a -> s {_ngNodeGroupId = a})

-- | The endpoint of the replica nodes in this node group (shard).
ngReaderEndpoint :: Lens' NodeGroup (Maybe Endpoint)
ngReaderEndpoint = lens _ngReaderEndpoint (\s a -> s {_ngReaderEndpoint = a})

instance FromXML NodeGroup where
  parseXML x =
    NodeGroup'
      <$> (x .@? "Status")
      <*> (x .@? "PrimaryEndpoint")
      <*> (x .@? "Slots")
      <*> ( x .@? "NodeGroupMembers" .!@ mempty
              >>= may (parseXMLList "NodeGroupMember")
          )
      <*> (x .@? "NodeGroupId")
      <*> (x .@? "ReaderEndpoint")

instance Hashable NodeGroup

instance NFData NodeGroup
