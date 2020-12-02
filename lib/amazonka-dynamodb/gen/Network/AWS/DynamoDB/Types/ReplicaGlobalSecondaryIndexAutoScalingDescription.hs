{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.IndexStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the auto scaling configuration for a replica global secondary index.
--
--
--
-- /See:/ 'replicaGlobalSecondaryIndexAutoScalingDescription' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingDescription = ReplicaGlobalSecondaryIndexAutoScalingDescription'
  { _rgsiasdIndexStatus ::
      !( Maybe
           IndexStatus
       ),
    _rgsiasdProvisionedWriteCapacityAutoScalingSettings ::
      !( Maybe
           AutoScalingSettingsDescription
       ),
    _rgsiasdProvisionedReadCapacityAutoScalingSettings ::
      !( Maybe
           AutoScalingSettingsDescription
       ),
    _rgsiasdIndexName ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ReplicaGlobalSecondaryIndexAutoScalingDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsiasdIndexStatus' - The current state of the replica global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.
--
-- * 'rgsiasdProvisionedWriteCapacityAutoScalingSettings' - Undocumented member.
--
-- * 'rgsiasdProvisionedReadCapacityAutoScalingSettings' - Undocumented member.
--
-- * 'rgsiasdIndexName' - The name of the global secondary index.
replicaGlobalSecondaryIndexAutoScalingDescription ::
  ReplicaGlobalSecondaryIndexAutoScalingDescription
replicaGlobalSecondaryIndexAutoScalingDescription =
  ReplicaGlobalSecondaryIndexAutoScalingDescription'
    { _rgsiasdIndexStatus =
        Nothing,
      _rgsiasdProvisionedWriteCapacityAutoScalingSettings =
        Nothing,
      _rgsiasdProvisionedReadCapacityAutoScalingSettings =
        Nothing,
      _rgsiasdIndexName = Nothing
    }

-- | The current state of the replica global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.
rgsiasdIndexStatus :: Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Maybe IndexStatus)
rgsiasdIndexStatus = lens _rgsiasdIndexStatus (\s a -> s {_rgsiasdIndexStatus = a})

-- | Undocumented member.
rgsiasdProvisionedWriteCapacityAutoScalingSettings :: Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Maybe AutoScalingSettingsDescription)
rgsiasdProvisionedWriteCapacityAutoScalingSettings = lens _rgsiasdProvisionedWriteCapacityAutoScalingSettings (\s a -> s {_rgsiasdProvisionedWriteCapacityAutoScalingSettings = a})

-- | Undocumented member.
rgsiasdProvisionedReadCapacityAutoScalingSettings :: Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Maybe AutoScalingSettingsDescription)
rgsiasdProvisionedReadCapacityAutoScalingSettings = lens _rgsiasdProvisionedReadCapacityAutoScalingSettings (\s a -> s {_rgsiasdProvisionedReadCapacityAutoScalingSettings = a})

-- | The name of the global secondary index.
rgsiasdIndexName :: Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Maybe Text)
rgsiasdIndexName = lens _rgsiasdIndexName (\s a -> s {_rgsiasdIndexName = a})

instance FromJSON ReplicaGlobalSecondaryIndexAutoScalingDescription where
  parseJSON =
    withObject
      "ReplicaGlobalSecondaryIndexAutoScalingDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexAutoScalingDescription'
            <$> (x .:? "IndexStatus")
            <*> (x .:? "ProvisionedWriteCapacityAutoScalingSettings")
            <*> (x .:? "ProvisionedReadCapacityAutoScalingSettings")
            <*> (x .:? "IndexName")
      )

instance Hashable ReplicaGlobalSecondaryIndexAutoScalingDescription

instance NFData ReplicaGlobalSecondaryIndexAutoScalingDescription
