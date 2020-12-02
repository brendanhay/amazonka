{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues where

import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
import Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.ReshardingStatus
import Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The settings to be applied to the Redis replication group, either immediately or during the next maintenance window.
--
--
--
-- /See:/ 'replicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
  { _rgpmvAuthTokenStatus ::
      !( Maybe
           AuthTokenUpdateStatus
       ),
    _rgpmvUserGroups ::
      !( Maybe
           UserGroupsUpdateStatus
       ),
    _rgpmvResharding ::
      !( Maybe
           ReshardingStatus
       ),
    _rgpmvPrimaryClusterId ::
      !(Maybe Text),
    _rgpmvAutomaticFailoverStatus ::
      !( Maybe
           PendingAutomaticFailoverStatus
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationGroupPendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgpmvAuthTokenStatus' - The auth token status
--
-- * 'rgpmvUserGroups' - The user groups being modified.
--
-- * 'rgpmvResharding' - The status of an online resharding operation.
--
-- * 'rgpmvPrimaryClusterId' - The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
--
-- * 'rgpmvAutomaticFailoverStatus' - Indicates the status of automatic failover for this Redis replication group.
replicationGroupPendingModifiedValues ::
  ReplicationGroupPendingModifiedValues
replicationGroupPendingModifiedValues =
  ReplicationGroupPendingModifiedValues'
    { _rgpmvAuthTokenStatus =
        Nothing,
      _rgpmvUserGroups = Nothing,
      _rgpmvResharding = Nothing,
      _rgpmvPrimaryClusterId = Nothing,
      _rgpmvAutomaticFailoverStatus = Nothing
    }

-- | The auth token status
rgpmvAuthTokenStatus :: Lens' ReplicationGroupPendingModifiedValues (Maybe AuthTokenUpdateStatus)
rgpmvAuthTokenStatus = lens _rgpmvAuthTokenStatus (\s a -> s {_rgpmvAuthTokenStatus = a})

-- | The user groups being modified.
rgpmvUserGroups :: Lens' ReplicationGroupPendingModifiedValues (Maybe UserGroupsUpdateStatus)
rgpmvUserGroups = lens _rgpmvUserGroups (\s a -> s {_rgpmvUserGroups = a})

-- | The status of an online resharding operation.
rgpmvResharding :: Lens' ReplicationGroupPendingModifiedValues (Maybe ReshardingStatus)
rgpmvResharding = lens _rgpmvResharding (\s a -> s {_rgpmvResharding = a})

-- | The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
rgpmvPrimaryClusterId :: Lens' ReplicationGroupPendingModifiedValues (Maybe Text)
rgpmvPrimaryClusterId = lens _rgpmvPrimaryClusterId (\s a -> s {_rgpmvPrimaryClusterId = a})

-- | Indicates the status of automatic failover for this Redis replication group.
rgpmvAutomaticFailoverStatus :: Lens' ReplicationGroupPendingModifiedValues (Maybe PendingAutomaticFailoverStatus)
rgpmvAutomaticFailoverStatus = lens _rgpmvAutomaticFailoverStatus (\s a -> s {_rgpmvAutomaticFailoverStatus = a})

instance FromXML ReplicationGroupPendingModifiedValues where
  parseXML x =
    ReplicationGroupPendingModifiedValues'
      <$> (x .@? "AuthTokenStatus")
      <*> (x .@? "UserGroups")
      <*> (x .@? "Resharding")
      <*> (x .@? "PrimaryClusterId")
      <*> (x .@? "AutomaticFailoverStatus")

instance Hashable ReplicationGroupPendingModifiedValues

instance NFData ReplicationGroupPendingModifiedValues
