{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The name of the Global Datastore and role of this replication group in the Global Datastore.
--
--
--
-- /See:/ 'globalReplicationGroupInfo' smart constructor.
data GlobalReplicationGroupInfo = GlobalReplicationGroupInfo'
  { _grgiGlobalReplicationGroupMemberRole ::
      !(Maybe Text),
    _grgiGlobalReplicationGroupId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalReplicationGroupInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grgiGlobalReplicationGroupMemberRole' - The role of the replication group in a Global Datastore. Can be primary or secondary.
--
-- * 'grgiGlobalReplicationGroupId' - The name of the Global Datastore
globalReplicationGroupInfo ::
  GlobalReplicationGroupInfo
globalReplicationGroupInfo =
  GlobalReplicationGroupInfo'
    { _grgiGlobalReplicationGroupMemberRole =
        Nothing,
      _grgiGlobalReplicationGroupId = Nothing
    }

-- | The role of the replication group in a Global Datastore. Can be primary or secondary.
grgiGlobalReplicationGroupMemberRole :: Lens' GlobalReplicationGroupInfo (Maybe Text)
grgiGlobalReplicationGroupMemberRole = lens _grgiGlobalReplicationGroupMemberRole (\s a -> s {_grgiGlobalReplicationGroupMemberRole = a})

-- | The name of the Global Datastore
grgiGlobalReplicationGroupId :: Lens' GlobalReplicationGroupInfo (Maybe Text)
grgiGlobalReplicationGroupId = lens _grgiGlobalReplicationGroupId (\s a -> s {_grgiGlobalReplicationGroupId = a})

instance FromXML GlobalReplicationGroupInfo where
  parseXML x =
    GlobalReplicationGroupInfo'
      <$> (x .@? "GlobalReplicationGroupMemberRole")
      <*> (x .@? "GlobalReplicationGroupId")

instance Hashable GlobalReplicationGroupInfo

instance NFData GlobalReplicationGroupInfo
