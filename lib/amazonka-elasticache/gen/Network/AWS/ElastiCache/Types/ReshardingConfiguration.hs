{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReshardingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReshardingConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
--
--
--
-- /See:/ 'reshardingConfiguration' smart constructor.
data ReshardingConfiguration = ReshardingConfiguration'
  { _rcPreferredAvailabilityZones ::
      !(Maybe [Text]),
    _rcNodeGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReshardingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcPreferredAvailabilityZones' - A list of preferred availability zones for the nodes in this cluster.
--
-- * 'rcNodeGroupId' - Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
reshardingConfiguration ::
  ReshardingConfiguration
reshardingConfiguration =
  ReshardingConfiguration'
    { _rcPreferredAvailabilityZones = Nothing,
      _rcNodeGroupId = Nothing
    }

-- | A list of preferred availability zones for the nodes in this cluster.
rcPreferredAvailabilityZones :: Lens' ReshardingConfiguration [Text]
rcPreferredAvailabilityZones = lens _rcPreferredAvailabilityZones (\s a -> s {_rcPreferredAvailabilityZones = a}) . _Default . _Coerce

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
rcNodeGroupId :: Lens' ReshardingConfiguration (Maybe Text)
rcNodeGroupId = lens _rcNodeGroupId (\s a -> s {_rcNodeGroupId = a})

instance Hashable ReshardingConfiguration

instance NFData ReshardingConfiguration

instance ToQuery ReshardingConfiguration where
  toQuery ReshardingConfiguration' {..} =
    mconcat
      [ "PreferredAvailabilityZones"
          =: toQuery
            (toQueryList "AvailabilityZone" <$> _rcPreferredAvailabilityZones),
        "NodeGroupId" =: _rcNodeGroupId
      ]
