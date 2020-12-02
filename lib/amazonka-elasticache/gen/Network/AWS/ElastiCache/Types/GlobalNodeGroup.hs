{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalNodeGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalNodeGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates the slot configuration and global identifier for a slice group.
--
--
--
-- /See:/ 'globalNodeGroup' smart constructor.
data GlobalNodeGroup = GlobalNodeGroup'
  { _gngSlots :: !(Maybe Text),
    _gngGlobalNodeGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalNodeGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gngSlots' - The keyspace for this node group
--
-- * 'gngGlobalNodeGroupId' - The name of the global node group
globalNodeGroup ::
  GlobalNodeGroup
globalNodeGroup =
  GlobalNodeGroup'
    { _gngSlots = Nothing,
      _gngGlobalNodeGroupId = Nothing
    }

-- | The keyspace for this node group
gngSlots :: Lens' GlobalNodeGroup (Maybe Text)
gngSlots = lens _gngSlots (\s a -> s {_gngSlots = a})

-- | The name of the global node group
gngGlobalNodeGroupId :: Lens' GlobalNodeGroup (Maybe Text)
gngGlobalNodeGroupId = lens _gngGlobalNodeGroupId (\s a -> s {_gngGlobalNodeGroupId = a})

instance FromXML GlobalNodeGroup where
  parseXML x =
    GlobalNodeGroup'
      <$> (x .@? "Slots") <*> (x .@? "GlobalNodeGroupId")

instance Hashable GlobalNodeGroup

instance NFData GlobalNodeGroup
