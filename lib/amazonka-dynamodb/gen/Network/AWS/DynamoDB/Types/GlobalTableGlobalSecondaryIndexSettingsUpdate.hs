{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
--
--
-- /See:/ 'globalTableGlobalSecondaryIndexSettingsUpdate' smart constructor.
data GlobalTableGlobalSecondaryIndexSettingsUpdate = GlobalTableGlobalSecondaryIndexSettingsUpdate'
  { _gtgsisuProvisionedWriteCapacityUnits ::
      !( Maybe
           Nat
       ),
    _gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate ::
      !( Maybe
           AutoScalingSettingsUpdate
       ),
    _gtgsisuIndexName ::
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

-- | Creates a value of 'GlobalTableGlobalSecondaryIndexSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgsisuProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
--
-- * 'gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global secondary index's write capacity units.
--
-- * 'gtgsisuIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
globalTableGlobalSecondaryIndexSettingsUpdate ::
  -- | 'gtgsisuIndexName'
  Text ->
  GlobalTableGlobalSecondaryIndexSettingsUpdate
globalTableGlobalSecondaryIndexSettingsUpdate pIndexName_ =
  GlobalTableGlobalSecondaryIndexSettingsUpdate'
    { _gtgsisuProvisionedWriteCapacityUnits =
        Nothing,
      _gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate =
        Nothing,
      _gtgsisuIndexName = pIndexName_
    }

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
gtgsisuProvisionedWriteCapacityUnits :: Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Maybe Natural)
gtgsisuProvisionedWriteCapacityUnits = lens _gtgsisuProvisionedWriteCapacityUnits (\s a -> s {_gtgsisuProvisionedWriteCapacityUnits = a}) . mapping _Nat

-- | Auto scaling settings for managing a global secondary index's write capacity units.
gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Maybe AutoScalingSettingsUpdate)
gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate = lens _gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate (\s a -> s {_gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate = a})

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
gtgsisuIndexName :: Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate Text
gtgsisuIndexName = lens _gtgsisuIndexName (\s a -> s {_gtgsisuIndexName = a})

instance Hashable GlobalTableGlobalSecondaryIndexSettingsUpdate

instance NFData GlobalTableGlobalSecondaryIndexSettingsUpdate

instance ToJSON GlobalTableGlobalSecondaryIndexSettingsUpdate where
  toJSON GlobalTableGlobalSecondaryIndexSettingsUpdate' {..} =
    object
      ( catMaybes
          [ ("ProvisionedWriteCapacityUnits" .=)
              <$> _gtgsisuProvisionedWriteCapacityUnits,
            ("ProvisionedWriteCapacityAutoScalingSettingsUpdate" .=)
              <$> _gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate,
            Just ("IndexName" .= _gtgsisuIndexName)
          ]
      )
