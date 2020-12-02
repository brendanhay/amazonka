{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Node group (shard) configuration options. Each node group (shard) configuration has the following: @Slots@ , @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ .
--
--
--
-- /See:/ 'nodeGroupConfiguration' smart constructor.
data NodeGroupConfiguration = NodeGroupConfiguration'
  { _ngcSlots ::
      !(Maybe Text),
    _ngcReplicaOutpostARNs :: !(Maybe [Text]),
    _ngcReplicaCount :: !(Maybe Int),
    _ngcPrimaryAvailabilityZone :: !(Maybe Text),
    _ngcReplicaAvailabilityZones ::
      !(Maybe [Text]),
    _ngcPrimaryOutpostARN :: !(Maybe Text),
    _ngcNodeGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeGroupConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngcSlots' - A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ . Example: @"0-3999"@
--
-- * 'ngcReplicaOutpostARNs' - The outpost ARN of the node replicas.
--
-- * 'ngcReplicaCount' - The number of read replica nodes in this node group (shard).
--
-- * 'ngcPrimaryAvailabilityZone' - The Availability Zone where the primary node of this node group (shard) is launched.
--
-- * 'ngcReplicaAvailabilityZones' - A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
--
-- * 'ngcPrimaryOutpostARN' - The output ARN of the primary node.
--
-- * 'ngcNodeGroupId' - Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
nodeGroupConfiguration ::
  NodeGroupConfiguration
nodeGroupConfiguration =
  NodeGroupConfiguration'
    { _ngcSlots = Nothing,
      _ngcReplicaOutpostARNs = Nothing,
      _ngcReplicaCount = Nothing,
      _ngcPrimaryAvailabilityZone = Nothing,
      _ngcReplicaAvailabilityZones = Nothing,
      _ngcPrimaryOutpostARN = Nothing,
      _ngcNodeGroupId = Nothing
    }

-- | A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ . Example: @"0-3999"@
ngcSlots :: Lens' NodeGroupConfiguration (Maybe Text)
ngcSlots = lens _ngcSlots (\s a -> s {_ngcSlots = a})

-- | The outpost ARN of the node replicas.
ngcReplicaOutpostARNs :: Lens' NodeGroupConfiguration [Text]
ngcReplicaOutpostARNs = lens _ngcReplicaOutpostARNs (\s a -> s {_ngcReplicaOutpostARNs = a}) . _Default . _Coerce

-- | The number of read replica nodes in this node group (shard).
ngcReplicaCount :: Lens' NodeGroupConfiguration (Maybe Int)
ngcReplicaCount = lens _ngcReplicaCount (\s a -> s {_ngcReplicaCount = a})

-- | The Availability Zone where the primary node of this node group (shard) is launched.
ngcPrimaryAvailabilityZone :: Lens' NodeGroupConfiguration (Maybe Text)
ngcPrimaryAvailabilityZone = lens _ngcPrimaryAvailabilityZone (\s a -> s {_ngcPrimaryAvailabilityZone = a})

-- | A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
ngcReplicaAvailabilityZones :: Lens' NodeGroupConfiguration [Text]
ngcReplicaAvailabilityZones = lens _ngcReplicaAvailabilityZones (\s a -> s {_ngcReplicaAvailabilityZones = a}) . _Default . _Coerce

-- | The output ARN of the primary node.
ngcPrimaryOutpostARN :: Lens' NodeGroupConfiguration (Maybe Text)
ngcPrimaryOutpostARN = lens _ngcPrimaryOutpostARN (\s a -> s {_ngcPrimaryOutpostARN = a})

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
ngcNodeGroupId :: Lens' NodeGroupConfiguration (Maybe Text)
ngcNodeGroupId = lens _ngcNodeGroupId (\s a -> s {_ngcNodeGroupId = a})

instance FromXML NodeGroupConfiguration where
  parseXML x =
    NodeGroupConfiguration'
      <$> (x .@? "Slots")
      <*> ( x .@? "ReplicaOutpostArns" .!@ mempty
              >>= may (parseXMLList "OutpostArn")
          )
      <*> (x .@? "ReplicaCount")
      <*> (x .@? "PrimaryAvailabilityZone")
      <*> ( x .@? "ReplicaAvailabilityZones" .!@ mempty
              >>= may (parseXMLList "AvailabilityZone")
          )
      <*> (x .@? "PrimaryOutpostArn")
      <*> (x .@? "NodeGroupId")

instance Hashable NodeGroupConfiguration

instance NFData NodeGroupConfiguration

instance ToQuery NodeGroupConfiguration where
  toQuery NodeGroupConfiguration' {..} =
    mconcat
      [ "Slots" =: _ngcSlots,
        "ReplicaOutpostArns"
          =: toQuery (toQueryList "OutpostArn" <$> _ngcReplicaOutpostARNs),
        "ReplicaCount" =: _ngcReplicaCount,
        "PrimaryAvailabilityZone" =: _ngcPrimaryAvailabilityZone,
        "ReplicaAvailabilityZones"
          =: toQuery
            (toQueryList "AvailabilityZone" <$> _ngcReplicaAvailabilityZones),
        "PrimaryOutpostArn" =: _ngcPrimaryOutpostARN,
        "NodeGroupId" =: _ngcNodeGroupId
      ]
