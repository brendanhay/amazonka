{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the auto scaling settings of the replica.
--
--
--
-- /See:/ 'replicaAutoScalingDescription' smart constructor.
data ReplicaAutoScalingDescription = ReplicaAutoScalingDescription'
  { _rasdReplicaStatus ::
      !(Maybe ReplicaStatus),
    _rasdRegionName ::
      !(Maybe Text),
    _rasdGlobalSecondaryIndexes ::
      !( Maybe
           [ReplicaGlobalSecondaryIndexAutoScalingDescription]
       ),
    _rasdReplicaProvisionedWriteCapacityAutoScalingSettings ::
      !( Maybe
           AutoScalingSettingsDescription
       ),
    _rasdReplicaProvisionedReadCapacityAutoScalingSettings ::
      !( Maybe
           AutoScalingSettingsDescription
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicaAutoScalingDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rasdReplicaStatus' - The current state of the replica:     * @CREATING@ - The replica is being created.     * @UPDATING@ - The replica is being updated.     * @DELETING@ - The replica is being deleted.     * @ACTIVE@ - The replica is ready for use.
--
-- * 'rasdRegionName' - The Region where the replica exists.
--
-- * 'rasdGlobalSecondaryIndexes' - Replica-specific global secondary index auto scaling settings.
--
-- * 'rasdReplicaProvisionedWriteCapacityAutoScalingSettings' - Undocumented member.
--
-- * 'rasdReplicaProvisionedReadCapacityAutoScalingSettings' - Undocumented member.
replicaAutoScalingDescription ::
  ReplicaAutoScalingDescription
replicaAutoScalingDescription =
  ReplicaAutoScalingDescription'
    { _rasdReplicaStatus = Nothing,
      _rasdRegionName = Nothing,
      _rasdGlobalSecondaryIndexes = Nothing,
      _rasdReplicaProvisionedWriteCapacityAutoScalingSettings =
        Nothing,
      _rasdReplicaProvisionedReadCapacityAutoScalingSettings = Nothing
    }

-- | The current state of the replica:     * @CREATING@ - The replica is being created.     * @UPDATING@ - The replica is being updated.     * @DELETING@ - The replica is being deleted.     * @ACTIVE@ - The replica is ready for use.
rasdReplicaStatus :: Lens' ReplicaAutoScalingDescription (Maybe ReplicaStatus)
rasdReplicaStatus = lens _rasdReplicaStatus (\s a -> s {_rasdReplicaStatus = a})

-- | The Region where the replica exists.
rasdRegionName :: Lens' ReplicaAutoScalingDescription (Maybe Text)
rasdRegionName = lens _rasdRegionName (\s a -> s {_rasdRegionName = a})

-- | Replica-specific global secondary index auto scaling settings.
rasdGlobalSecondaryIndexes :: Lens' ReplicaAutoScalingDescription [ReplicaGlobalSecondaryIndexAutoScalingDescription]
rasdGlobalSecondaryIndexes = lens _rasdGlobalSecondaryIndexes (\s a -> s {_rasdGlobalSecondaryIndexes = a}) . _Default . _Coerce

-- | Undocumented member.
rasdReplicaProvisionedWriteCapacityAutoScalingSettings :: Lens' ReplicaAutoScalingDescription (Maybe AutoScalingSettingsDescription)
rasdReplicaProvisionedWriteCapacityAutoScalingSettings = lens _rasdReplicaProvisionedWriteCapacityAutoScalingSettings (\s a -> s {_rasdReplicaProvisionedWriteCapacityAutoScalingSettings = a})

-- | Undocumented member.
rasdReplicaProvisionedReadCapacityAutoScalingSettings :: Lens' ReplicaAutoScalingDescription (Maybe AutoScalingSettingsDescription)
rasdReplicaProvisionedReadCapacityAutoScalingSettings = lens _rasdReplicaProvisionedReadCapacityAutoScalingSettings (\s a -> s {_rasdReplicaProvisionedReadCapacityAutoScalingSettings = a})

instance FromJSON ReplicaAutoScalingDescription where
  parseJSON =
    withObject
      "ReplicaAutoScalingDescription"
      ( \x ->
          ReplicaAutoScalingDescription'
            <$> (x .:? "ReplicaStatus")
            <*> (x .:? "RegionName")
            <*> (x .:? "GlobalSecondaryIndexes" .!= mempty)
            <*> (x .:? "ReplicaProvisionedWriteCapacityAutoScalingSettings")
            <*> (x .:? "ReplicaProvisionedReadCapacityAutoScalingSettings")
      )

instance Hashable ReplicaAutoScalingDescription

instance NFData ReplicaAutoScalingDescription
