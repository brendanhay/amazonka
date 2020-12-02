{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the auto scaling settings of a global secondary index for a global table that will be modified.
--
--
--
-- /See:/ 'globalSecondaryIndexAutoScalingUpdate' smart constructor.
data GlobalSecondaryIndexAutoScalingUpdate = GlobalSecondaryIndexAutoScalingUpdate'
  { _gsiasuProvisionedWriteCapacityAutoScalingUpdate ::
      !( Maybe
           AutoScalingSettingsUpdate
       ),
    _gsiasuIndexName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalSecondaryIndexAutoScalingUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiasuProvisionedWriteCapacityAutoScalingUpdate' - Undocumented member.
--
-- * 'gsiasuIndexName' - The name of the global secondary index.
globalSecondaryIndexAutoScalingUpdate ::
  GlobalSecondaryIndexAutoScalingUpdate
globalSecondaryIndexAutoScalingUpdate =
  GlobalSecondaryIndexAutoScalingUpdate'
    { _gsiasuProvisionedWriteCapacityAutoScalingUpdate =
        Nothing,
      _gsiasuIndexName = Nothing
    }

-- | Undocumented member.
gsiasuProvisionedWriteCapacityAutoScalingUpdate :: Lens' GlobalSecondaryIndexAutoScalingUpdate (Maybe AutoScalingSettingsUpdate)
gsiasuProvisionedWriteCapacityAutoScalingUpdate = lens _gsiasuProvisionedWriteCapacityAutoScalingUpdate (\s a -> s {_gsiasuProvisionedWriteCapacityAutoScalingUpdate = a})

-- | The name of the global secondary index.
gsiasuIndexName :: Lens' GlobalSecondaryIndexAutoScalingUpdate (Maybe Text)
gsiasuIndexName = lens _gsiasuIndexName (\s a -> s {_gsiasuIndexName = a})

instance Hashable GlobalSecondaryIndexAutoScalingUpdate

instance NFData GlobalSecondaryIndexAutoScalingUpdate

instance ToJSON GlobalSecondaryIndexAutoScalingUpdate where
  toJSON GlobalSecondaryIndexAutoScalingUpdate' {..} =
    object
      ( catMaybes
          [ ("ProvisionedWriteCapacityAutoScalingUpdate" .=)
              <$> _gsiasuProvisionedWriteCapacityAutoScalingUpdate,
            ("IndexName" .=) <$> _gsiasuIndexName
          ]
      )
