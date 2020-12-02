{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.RegionalConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.RegionalConfiguration where

import Network.AWS.ElastiCache.Types.ReshardingConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of the replication groups
--
--
--
-- /See:/ 'regionalConfiguration' smart constructor.
data RegionalConfiguration = RegionalConfiguration'
  { _rcReplicationGroupId ::
      !Text,
    _rcReplicationGroupRegion :: !Text,
    _rcReshardingConfiguration ::
      ![ReshardingConfiguration]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegionalConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcReplicationGroupId' - The name of the secondary cluster
--
-- * 'rcReplicationGroupRegion' - The AWS region where the cluster is stored
--
-- * 'rcReshardingConfiguration' - A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
regionalConfiguration ::
  -- | 'rcReplicationGroupId'
  Text ->
  -- | 'rcReplicationGroupRegion'
  Text ->
  RegionalConfiguration
regionalConfiguration pReplicationGroupId_ pReplicationGroupRegion_ =
  RegionalConfiguration'
    { _rcReplicationGroupId =
        pReplicationGroupId_,
      _rcReplicationGroupRegion = pReplicationGroupRegion_,
      _rcReshardingConfiguration = mempty
    }

-- | The name of the secondary cluster
rcReplicationGroupId :: Lens' RegionalConfiguration Text
rcReplicationGroupId = lens _rcReplicationGroupId (\s a -> s {_rcReplicationGroupId = a})

-- | The AWS region where the cluster is stored
rcReplicationGroupRegion :: Lens' RegionalConfiguration Text
rcReplicationGroupRegion = lens _rcReplicationGroupRegion (\s a -> s {_rcReplicationGroupRegion = a})

-- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
rcReshardingConfiguration :: Lens' RegionalConfiguration [ReshardingConfiguration]
rcReshardingConfiguration = lens _rcReshardingConfiguration (\s a -> s {_rcReshardingConfiguration = a}) . _Coerce

instance Hashable RegionalConfiguration

instance NFData RegionalConfiguration

instance ToQuery RegionalConfiguration where
  toQuery RegionalConfiguration' {..} =
    mconcat
      [ "ReplicationGroupId" =: _rcReplicationGroupId,
        "ReplicationGroupRegion" =: _rcReplicationGroupRegion,
        "ReshardingConfiguration"
          =: toQueryList "ReshardingConfiguration" _rcReshardingConfiguration
      ]
