{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.IndexStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a global secondary index.
--
--
--
-- /See:/ 'replicaGlobalSecondaryIndexSettingsDescription' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsDescription = ReplicaGlobalSecondaryIndexSettingsDescription'
  { _rgsisdIndexStatus ::
      !( Maybe
           IndexStatus
       ),
    _rgsisdProvisionedReadCapacityUnits ::
      !( Maybe
           Nat
       ),
    _rgsisdProvisionedWriteCapacityUnits ::
      !( Maybe
           Nat
       ),
    _rgsisdProvisionedWriteCapacityAutoScalingSettings ::
      !( Maybe
           AutoScalingSettingsDescription
       ),
    _rgsisdProvisionedReadCapacityAutoScalingSettings ::
      !( Maybe
           AutoScalingSettingsDescription
       ),
    _rgsisdIndexName ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ReplicaGlobalSecondaryIndexSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsisdIndexStatus' - The current status of the global secondary index:     * @CREATING@ - The global secondary index is being created.     * @UPDATING@ - The global secondary index is being updated.     * @DELETING@ - The global secondary index is being deleted.     * @ACTIVE@ - The global secondary index is ready for use.
--
-- * 'rgsisdProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'rgsisdProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'rgsisdProvisionedWriteCapacityAutoScalingSettings' - Auto scaling settings for a global secondary index replica's write capacity units.
--
-- * 'rgsisdProvisionedReadCapacityAutoScalingSettings' - Auto scaling settings for a global secondary index replica's read capacity units.
--
-- * 'rgsisdIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
replicaGlobalSecondaryIndexSettingsDescription ::
  -- | 'rgsisdIndexName'
  Text ->
  ReplicaGlobalSecondaryIndexSettingsDescription
replicaGlobalSecondaryIndexSettingsDescription pIndexName_ =
  ReplicaGlobalSecondaryIndexSettingsDescription'
    { _rgsisdIndexStatus =
        Nothing,
      _rgsisdProvisionedReadCapacityUnits = Nothing,
      _rgsisdProvisionedWriteCapacityUnits = Nothing,
      _rgsisdProvisionedWriteCapacityAutoScalingSettings =
        Nothing,
      _rgsisdProvisionedReadCapacityAutoScalingSettings =
        Nothing,
      _rgsisdIndexName = pIndexName_
    }

-- | The current status of the global secondary index:     * @CREATING@ - The global secondary index is being created.     * @UPDATING@ - The global secondary index is being updated.     * @DELETING@ - The global secondary index is being deleted.     * @ACTIVE@ - The global secondary index is ready for use.
rgsisdIndexStatus :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe IndexStatus)
rgsisdIndexStatus = lens _rgsisdIndexStatus (\s a -> s {_rgsisdIndexStatus = a})

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
rgsisdProvisionedReadCapacityUnits :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe Natural)
rgsisdProvisionedReadCapacityUnits = lens _rgsisdProvisionedReadCapacityUnits (\s a -> s {_rgsisdProvisionedReadCapacityUnits = a}) . mapping _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
rgsisdProvisionedWriteCapacityUnits :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe Natural)
rgsisdProvisionedWriteCapacityUnits = lens _rgsisdProvisionedWriteCapacityUnits (\s a -> s {_rgsisdProvisionedWriteCapacityUnits = a}) . mapping _Nat

-- | Auto scaling settings for a global secondary index replica's write capacity units.
rgsisdProvisionedWriteCapacityAutoScalingSettings :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe AutoScalingSettingsDescription)
rgsisdProvisionedWriteCapacityAutoScalingSettings = lens _rgsisdProvisionedWriteCapacityAutoScalingSettings (\s a -> s {_rgsisdProvisionedWriteCapacityAutoScalingSettings = a})

-- | Auto scaling settings for a global secondary index replica's read capacity units.
rgsisdProvisionedReadCapacityAutoScalingSettings :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe AutoScalingSettingsDescription)
rgsisdProvisionedReadCapacityAutoScalingSettings = lens _rgsisdProvisionedReadCapacityAutoScalingSettings (\s a -> s {_rgsisdProvisionedReadCapacityAutoScalingSettings = a})

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
rgsisdIndexName :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription Text
rgsisdIndexName = lens _rgsisdIndexName (\s a -> s {_rgsisdIndexName = a})

instance FromJSON ReplicaGlobalSecondaryIndexSettingsDescription where
  parseJSON =
    withObject
      "ReplicaGlobalSecondaryIndexSettingsDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexSettingsDescription'
            <$> (x .:? "IndexStatus")
            <*> (x .:? "ProvisionedReadCapacityUnits")
            <*> (x .:? "ProvisionedWriteCapacityUnits")
            <*> (x .:? "ProvisionedWriteCapacityAutoScalingSettings")
            <*> (x .:? "ProvisionedReadCapacityAutoScalingSettings")
            <*> (x .: "IndexName")
      )

instance Hashable ReplicaGlobalSecondaryIndexSettingsDescription

instance NFData ReplicaGlobalSecondaryIndexSettingsDescription
