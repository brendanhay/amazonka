{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaSettingsDescription where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.BillingModeSummary
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a replica.
--
--
--
-- /See:/ 'replicaSettingsDescription' smart constructor.
data ReplicaSettingsDescription = ReplicaSettingsDescription'
  { _rsdReplicaStatus ::
      !(Maybe ReplicaStatus),
    _rsdReplicaProvisionedReadCapacityUnits ::
      !(Maybe Nat),
    _rsdReplicaProvisionedWriteCapacityUnits ::
      !(Maybe Nat),
    _rsdReplicaBillingModeSummary ::
      !(Maybe BillingModeSummary),
    _rsdReplicaGlobalSecondaryIndexSettings ::
      !( Maybe
           [ReplicaGlobalSecondaryIndexSettingsDescription]
       ),
    _rsdReplicaProvisionedWriteCapacityAutoScalingSettings ::
      !( Maybe
           AutoScalingSettingsDescription
       ),
    _rsdReplicaProvisionedReadCapacityAutoScalingSettings ::
      !( Maybe
           AutoScalingSettingsDescription
       ),
    _rsdRegionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicaSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsdReplicaStatus' - The current state of the Region:     * @CREATING@ - The Region is being created.     * @UPDATING@ - The Region is being updated.     * @DELETING@ - The Region is being deleted.     * @ACTIVE@ - The Region is ready for use.
--
-- * 'rsdReplicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'rsdReplicaProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'rsdReplicaBillingModeSummary' - The read/write capacity mode of the replica.
--
-- * 'rsdReplicaGlobalSecondaryIndexSettings' - Replica global secondary index settings for the global table.
--
-- * 'rsdReplicaProvisionedWriteCapacityAutoScalingSettings' - Auto scaling settings for a global table replica's write capacity units.
--
-- * 'rsdReplicaProvisionedReadCapacityAutoScalingSettings' - Auto scaling settings for a global table replica's read capacity units.
--
-- * 'rsdRegionName' - The Region name of the replica.
replicaSettingsDescription ::
  -- | 'rsdRegionName'
  Text ->
  ReplicaSettingsDescription
replicaSettingsDescription pRegionName_ =
  ReplicaSettingsDescription'
    { _rsdReplicaStatus = Nothing,
      _rsdReplicaProvisionedReadCapacityUnits = Nothing,
      _rsdReplicaProvisionedWriteCapacityUnits = Nothing,
      _rsdReplicaBillingModeSummary = Nothing,
      _rsdReplicaGlobalSecondaryIndexSettings = Nothing,
      _rsdReplicaProvisionedWriteCapacityAutoScalingSettings = Nothing,
      _rsdReplicaProvisionedReadCapacityAutoScalingSettings = Nothing,
      _rsdRegionName = pRegionName_
    }

-- | The current state of the Region:     * @CREATING@ - The Region is being created.     * @UPDATING@ - The Region is being updated.     * @DELETING@ - The Region is being deleted.     * @ACTIVE@ - The Region is ready for use.
rsdReplicaStatus :: Lens' ReplicaSettingsDescription (Maybe ReplicaStatus)
rsdReplicaStatus = lens _rsdReplicaStatus (\s a -> s {_rsdReplicaStatus = a})

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
rsdReplicaProvisionedReadCapacityUnits :: Lens' ReplicaSettingsDescription (Maybe Natural)
rsdReplicaProvisionedReadCapacityUnits = lens _rsdReplicaProvisionedReadCapacityUnits (\s a -> s {_rsdReplicaProvisionedReadCapacityUnits = a}) . mapping _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
rsdReplicaProvisionedWriteCapacityUnits :: Lens' ReplicaSettingsDescription (Maybe Natural)
rsdReplicaProvisionedWriteCapacityUnits = lens _rsdReplicaProvisionedWriteCapacityUnits (\s a -> s {_rsdReplicaProvisionedWriteCapacityUnits = a}) . mapping _Nat

-- | The read/write capacity mode of the replica.
rsdReplicaBillingModeSummary :: Lens' ReplicaSettingsDescription (Maybe BillingModeSummary)
rsdReplicaBillingModeSummary = lens _rsdReplicaBillingModeSummary (\s a -> s {_rsdReplicaBillingModeSummary = a})

-- | Replica global secondary index settings for the global table.
rsdReplicaGlobalSecondaryIndexSettings :: Lens' ReplicaSettingsDescription [ReplicaGlobalSecondaryIndexSettingsDescription]
rsdReplicaGlobalSecondaryIndexSettings = lens _rsdReplicaGlobalSecondaryIndexSettings (\s a -> s {_rsdReplicaGlobalSecondaryIndexSettings = a}) . _Default . _Coerce

-- | Auto scaling settings for a global table replica's write capacity units.
rsdReplicaProvisionedWriteCapacityAutoScalingSettings :: Lens' ReplicaSettingsDescription (Maybe AutoScalingSettingsDescription)
rsdReplicaProvisionedWriteCapacityAutoScalingSettings = lens _rsdReplicaProvisionedWriteCapacityAutoScalingSettings (\s a -> s {_rsdReplicaProvisionedWriteCapacityAutoScalingSettings = a})

-- | Auto scaling settings for a global table replica's read capacity units.
rsdReplicaProvisionedReadCapacityAutoScalingSettings :: Lens' ReplicaSettingsDescription (Maybe AutoScalingSettingsDescription)
rsdReplicaProvisionedReadCapacityAutoScalingSettings = lens _rsdReplicaProvisionedReadCapacityAutoScalingSettings (\s a -> s {_rsdReplicaProvisionedReadCapacityAutoScalingSettings = a})

-- | The Region name of the replica.
rsdRegionName :: Lens' ReplicaSettingsDescription Text
rsdRegionName = lens _rsdRegionName (\s a -> s {_rsdRegionName = a})

instance FromJSON ReplicaSettingsDescription where
  parseJSON =
    withObject
      "ReplicaSettingsDescription"
      ( \x ->
          ReplicaSettingsDescription'
            <$> (x .:? "ReplicaStatus")
            <*> (x .:? "ReplicaProvisionedReadCapacityUnits")
            <*> (x .:? "ReplicaProvisionedWriteCapacityUnits")
            <*> (x .:? "ReplicaBillingModeSummary")
            <*> (x .:? "ReplicaGlobalSecondaryIndexSettings" .!= mempty)
            <*> (x .:? "ReplicaProvisionedWriteCapacityAutoScalingSettings")
            <*> (x .:? "ReplicaProvisionedReadCapacityAutoScalingSettings")
            <*> (x .: "RegionName")
      )

instance Hashable ReplicaSettingsDescription

instance NFData ReplicaSettingsDescription
