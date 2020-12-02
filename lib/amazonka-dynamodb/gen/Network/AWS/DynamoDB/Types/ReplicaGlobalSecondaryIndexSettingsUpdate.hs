{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
--
--
-- /See:/ 'replicaGlobalSecondaryIndexSettingsUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsUpdate = ReplicaGlobalSecondaryIndexSettingsUpdate'
  { _rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate ::
      !( Maybe
           AutoScalingSettingsUpdate
       ),
    _rgsisuProvisionedReadCapacityUnits ::
      !( Maybe
           Nat
       ),
    _rgsisuIndexName ::
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

-- | Creates a value of 'ReplicaGlobalSecondaryIndexSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global secondary index replica's read capacity units.
--
-- * 'rgsisuProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'rgsisuIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
replicaGlobalSecondaryIndexSettingsUpdate ::
  -- | 'rgsisuIndexName'
  Text ->
  ReplicaGlobalSecondaryIndexSettingsUpdate
replicaGlobalSecondaryIndexSettingsUpdate pIndexName_ =
  ReplicaGlobalSecondaryIndexSettingsUpdate'
    { _rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate =
        Nothing,
      _rgsisuProvisionedReadCapacityUnits = Nothing,
      _rgsisuIndexName = pIndexName_
    }

-- | Auto scaling settings for managing a global secondary index replica's read capacity units.
rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens' ReplicaGlobalSecondaryIndexSettingsUpdate (Maybe AutoScalingSettingsUpdate)
rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate = lens _rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate (\s a -> s {_rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate = a})

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
rgsisuProvisionedReadCapacityUnits :: Lens' ReplicaGlobalSecondaryIndexSettingsUpdate (Maybe Natural)
rgsisuProvisionedReadCapacityUnits = lens _rgsisuProvisionedReadCapacityUnits (\s a -> s {_rgsisuProvisionedReadCapacityUnits = a}) . mapping _Nat

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
rgsisuIndexName :: Lens' ReplicaGlobalSecondaryIndexSettingsUpdate Text
rgsisuIndexName = lens _rgsisuIndexName (\s a -> s {_rgsisuIndexName = a})

instance Hashable ReplicaGlobalSecondaryIndexSettingsUpdate

instance NFData ReplicaGlobalSecondaryIndexSettingsUpdate

instance ToJSON ReplicaGlobalSecondaryIndexSettingsUpdate where
  toJSON ReplicaGlobalSecondaryIndexSettingsUpdate' {..} =
    object
      ( catMaybes
          [ ("ProvisionedReadCapacityAutoScalingSettingsUpdate" .=)
              <$> _rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate,
            ("ProvisionedReadCapacityUnits" .=)
              <$> _rgsisuProvisionedReadCapacityUnits,
            Just ("IndexName" .= _rgsisuIndexName)
          ]
      )
