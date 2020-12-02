{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the auto scaling settings of a replica that will be modified.
--
--
--
-- /See:/ 'replicaAutoScalingUpdate' smart constructor.
data ReplicaAutoScalingUpdate = ReplicaAutoScalingUpdate'
  { _rasuReplicaProvisionedReadCapacityAutoScalingUpdate ::
      !(Maybe AutoScalingSettingsUpdate),
    _rasuReplicaGlobalSecondaryIndexUpdates ::
      !( Maybe
           [ReplicaGlobalSecondaryIndexAutoScalingUpdate]
       ),
    _rasuRegionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicaAutoScalingUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rasuReplicaProvisionedReadCapacityAutoScalingUpdate' - Undocumented member.
--
-- * 'rasuReplicaGlobalSecondaryIndexUpdates' - Represents the auto scaling settings of global secondary indexes that will be modified.
--
-- * 'rasuRegionName' - The Region where the replica exists.
replicaAutoScalingUpdate ::
  -- | 'rasuRegionName'
  Text ->
  ReplicaAutoScalingUpdate
replicaAutoScalingUpdate pRegionName_ =
  ReplicaAutoScalingUpdate'
    { _rasuReplicaProvisionedReadCapacityAutoScalingUpdate =
        Nothing,
      _rasuReplicaGlobalSecondaryIndexUpdates = Nothing,
      _rasuRegionName = pRegionName_
    }

-- | Undocumented member.
rasuReplicaProvisionedReadCapacityAutoScalingUpdate :: Lens' ReplicaAutoScalingUpdate (Maybe AutoScalingSettingsUpdate)
rasuReplicaProvisionedReadCapacityAutoScalingUpdate = lens _rasuReplicaProvisionedReadCapacityAutoScalingUpdate (\s a -> s {_rasuReplicaProvisionedReadCapacityAutoScalingUpdate = a})

-- | Represents the auto scaling settings of global secondary indexes that will be modified.
rasuReplicaGlobalSecondaryIndexUpdates :: Lens' ReplicaAutoScalingUpdate [ReplicaGlobalSecondaryIndexAutoScalingUpdate]
rasuReplicaGlobalSecondaryIndexUpdates = lens _rasuReplicaGlobalSecondaryIndexUpdates (\s a -> s {_rasuReplicaGlobalSecondaryIndexUpdates = a}) . _Default . _Coerce

-- | The Region where the replica exists.
rasuRegionName :: Lens' ReplicaAutoScalingUpdate Text
rasuRegionName = lens _rasuRegionName (\s a -> s {_rasuRegionName = a})

instance Hashable ReplicaAutoScalingUpdate

instance NFData ReplicaAutoScalingUpdate

instance ToJSON ReplicaAutoScalingUpdate where
  toJSON ReplicaAutoScalingUpdate' {..} =
    object
      ( catMaybes
          [ ("ReplicaProvisionedReadCapacityAutoScalingUpdate" .=)
              <$> _rasuReplicaProvisionedReadCapacityAutoScalingUpdate,
            ("ReplicaGlobalSecondaryIndexUpdates" .=)
              <$> _rasuReplicaGlobalSecondaryIndexUpdates,
            Just ("RegionName" .= _rasuRegionName)
          ]
      )
