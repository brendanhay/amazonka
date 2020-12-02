{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember where

import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A member of a Global Datastore. It contains the Replication Group Id, the AWS region and the role of the replication group.
--
--
--
-- /See:/ 'globalReplicationGroupMember' smart constructor.
data GlobalReplicationGroupMember = GlobalReplicationGroupMember'
  { _grgmStatus ::
      !(Maybe Text),
    _grgmReplicationGroupRegion ::
      !(Maybe Text),
    _grgmRole :: !(Maybe Text),
    _grgmReplicationGroupId ::
      !(Maybe Text),
    _grgmAutomaticFailover ::
      !(Maybe AutomaticFailoverStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalReplicationGroupMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grgmStatus' - The status of the membership of the replication group.
--
-- * 'grgmReplicationGroupRegion' - The AWS region of the Global Datastore member.
--
-- * 'grgmRole' - Indicates the role of the replication group, primary or secondary.
--
-- * 'grgmReplicationGroupId' - The replication group id of the Global Datastore member.
--
-- * 'grgmAutomaticFailover' - Indicates whether automatic failover is enabled for the replication group.
globalReplicationGroupMember ::
  GlobalReplicationGroupMember
globalReplicationGroupMember =
  GlobalReplicationGroupMember'
    { _grgmStatus = Nothing,
      _grgmReplicationGroupRegion = Nothing,
      _grgmRole = Nothing,
      _grgmReplicationGroupId = Nothing,
      _grgmAutomaticFailover = Nothing
    }

-- | The status of the membership of the replication group.
grgmStatus :: Lens' GlobalReplicationGroupMember (Maybe Text)
grgmStatus = lens _grgmStatus (\s a -> s {_grgmStatus = a})

-- | The AWS region of the Global Datastore member.
grgmReplicationGroupRegion :: Lens' GlobalReplicationGroupMember (Maybe Text)
grgmReplicationGroupRegion = lens _grgmReplicationGroupRegion (\s a -> s {_grgmReplicationGroupRegion = a})

-- | Indicates the role of the replication group, primary or secondary.
grgmRole :: Lens' GlobalReplicationGroupMember (Maybe Text)
grgmRole = lens _grgmRole (\s a -> s {_grgmRole = a})

-- | The replication group id of the Global Datastore member.
grgmReplicationGroupId :: Lens' GlobalReplicationGroupMember (Maybe Text)
grgmReplicationGroupId = lens _grgmReplicationGroupId (\s a -> s {_grgmReplicationGroupId = a})

-- | Indicates whether automatic failover is enabled for the replication group.
grgmAutomaticFailover :: Lens' GlobalReplicationGroupMember (Maybe AutomaticFailoverStatus)
grgmAutomaticFailover = lens _grgmAutomaticFailover (\s a -> s {_grgmAutomaticFailover = a})

instance FromXML GlobalReplicationGroupMember where
  parseXML x =
    GlobalReplicationGroupMember'
      <$> (x .@? "Status")
      <*> (x .@? "ReplicationGroupRegion")
      <*> (x .@? "Role")
      <*> (x .@? "ReplicationGroupId")
      <*> (x .@? "AutomaticFailover")

instance Hashable GlobalReplicationGroupMember

instance NFData GlobalReplicationGroupMember
