{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the settings for a global table in a Region that will be modified.
--
--
--
-- /See:/ 'replicaSettingsUpdate' smart constructor.
data ReplicaSettingsUpdate = ReplicaSettingsUpdate'
  { _rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate ::
      !(Maybe AutoScalingSettingsUpdate),
    _rsuReplicaProvisionedReadCapacityUnits ::
      !(Maybe Nat),
    _rsuReplicaGlobalSecondaryIndexSettingsUpdate ::
      !( Maybe
           ( List1
               ReplicaGlobalSecondaryIndexSettingsUpdate
           )
       ),
    _rsuRegionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicaSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global table replica's read capacity units.
--
-- * 'rsuReplicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'rsuReplicaGlobalSecondaryIndexSettingsUpdate' - Represents the settings of a global secondary index for a global table that will be modified.
--
-- * 'rsuRegionName' - The Region of the replica to be added.
replicaSettingsUpdate ::
  -- | 'rsuRegionName'
  Text ->
  ReplicaSettingsUpdate
replicaSettingsUpdate pRegionName_ =
  ReplicaSettingsUpdate'
    { _rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate =
        Nothing,
      _rsuReplicaProvisionedReadCapacityUnits = Nothing,
      _rsuReplicaGlobalSecondaryIndexSettingsUpdate = Nothing,
      _rsuRegionName = pRegionName_
    }

-- | Auto scaling settings for managing a global table replica's read capacity units.
rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens' ReplicaSettingsUpdate (Maybe AutoScalingSettingsUpdate)
rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate = lens _rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate (\s a -> s {_rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate = a})

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
rsuReplicaProvisionedReadCapacityUnits :: Lens' ReplicaSettingsUpdate (Maybe Natural)
rsuReplicaProvisionedReadCapacityUnits = lens _rsuReplicaProvisionedReadCapacityUnits (\s a -> s {_rsuReplicaProvisionedReadCapacityUnits = a}) . mapping _Nat

-- | Represents the settings of a global secondary index for a global table that will be modified.
rsuReplicaGlobalSecondaryIndexSettingsUpdate :: Lens' ReplicaSettingsUpdate (Maybe (NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate))
rsuReplicaGlobalSecondaryIndexSettingsUpdate = lens _rsuReplicaGlobalSecondaryIndexSettingsUpdate (\s a -> s {_rsuReplicaGlobalSecondaryIndexSettingsUpdate = a}) . mapping _List1

-- | The Region of the replica to be added.
rsuRegionName :: Lens' ReplicaSettingsUpdate Text
rsuRegionName = lens _rsuRegionName (\s a -> s {_rsuRegionName = a})

instance Hashable ReplicaSettingsUpdate

instance NFData ReplicaSettingsUpdate

instance ToJSON ReplicaSettingsUpdate where
  toJSON ReplicaSettingsUpdate' {..} =
    object
      ( catMaybes
          [ ("ReplicaProvisionedReadCapacityAutoScalingSettingsUpdate" .=)
              <$> _rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate,
            ("ReplicaProvisionedReadCapacityUnits" .=)
              <$> _rsuReplicaProvisionedReadCapacityUnits,
            ("ReplicaGlobalSecondaryIndexSettingsUpdate" .=)
              <$> _rsuReplicaGlobalSecondaryIndexSettingsUpdate,
            Just ("RegionName" .= _rsuRegionName)
          ]
      )
