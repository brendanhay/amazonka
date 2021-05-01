{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues where

import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
import Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.ReshardingStatus
import Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The settings to be applied to the Redis replication group, either
-- immediately or during the next maintenance window.
--
-- /See:/ 'newReplicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
  { -- | The status of an online resharding operation.
    resharding :: Prelude.Maybe ReshardingStatus,
    -- | The primary cluster ID that is applied immediately (if
    -- @--apply-immediately@ was specified), or during the next maintenance
    -- window.
    primaryClusterId :: Prelude.Maybe Prelude.Text,
    -- | The auth token status
    authTokenStatus :: Prelude.Maybe AuthTokenUpdateStatus,
    -- | The user groups being modified.
    userGroups :: Prelude.Maybe UserGroupsUpdateStatus,
    -- | Indicates the status of automatic failover for this Redis replication
    -- group.
    automaticFailoverStatus :: Prelude.Maybe PendingAutomaticFailoverStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicationGroupPendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resharding', 'replicationGroupPendingModifiedValues_resharding' - The status of an online resharding operation.
--
-- 'primaryClusterId', 'replicationGroupPendingModifiedValues_primaryClusterId' - The primary cluster ID that is applied immediately (if
-- @--apply-immediately@ was specified), or during the next maintenance
-- window.
--
-- 'authTokenStatus', 'replicationGroupPendingModifiedValues_authTokenStatus' - The auth token status
--
-- 'userGroups', 'replicationGroupPendingModifiedValues_userGroups' - The user groups being modified.
--
-- 'automaticFailoverStatus', 'replicationGroupPendingModifiedValues_automaticFailoverStatus' - Indicates the status of automatic failover for this Redis replication
-- group.
newReplicationGroupPendingModifiedValues ::
  ReplicationGroupPendingModifiedValues
newReplicationGroupPendingModifiedValues =
  ReplicationGroupPendingModifiedValues'
    { resharding =
        Prelude.Nothing,
      primaryClusterId = Prelude.Nothing,
      authTokenStatus = Prelude.Nothing,
      userGroups = Prelude.Nothing,
      automaticFailoverStatus =
        Prelude.Nothing
    }

-- | The status of an online resharding operation.
replicationGroupPendingModifiedValues_resharding :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe ReshardingStatus)
replicationGroupPendingModifiedValues_resharding = Lens.lens (\ReplicationGroupPendingModifiedValues' {resharding} -> resharding) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {resharding = a} :: ReplicationGroupPendingModifiedValues)

-- | The primary cluster ID that is applied immediately (if
-- @--apply-immediately@ was specified), or during the next maintenance
-- window.
replicationGroupPendingModifiedValues_primaryClusterId :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe Prelude.Text)
replicationGroupPendingModifiedValues_primaryClusterId = Lens.lens (\ReplicationGroupPendingModifiedValues' {primaryClusterId} -> primaryClusterId) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {primaryClusterId = a} :: ReplicationGroupPendingModifiedValues)

-- | The auth token status
replicationGroupPendingModifiedValues_authTokenStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe AuthTokenUpdateStatus)
replicationGroupPendingModifiedValues_authTokenStatus = Lens.lens (\ReplicationGroupPendingModifiedValues' {authTokenStatus} -> authTokenStatus) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {authTokenStatus = a} :: ReplicationGroupPendingModifiedValues)

-- | The user groups being modified.
replicationGroupPendingModifiedValues_userGroups :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe UserGroupsUpdateStatus)
replicationGroupPendingModifiedValues_userGroups = Lens.lens (\ReplicationGroupPendingModifiedValues' {userGroups} -> userGroups) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {userGroups = a} :: ReplicationGroupPendingModifiedValues)

-- | Indicates the status of automatic failover for this Redis replication
-- group.
replicationGroupPendingModifiedValues_automaticFailoverStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe PendingAutomaticFailoverStatus)
replicationGroupPendingModifiedValues_automaticFailoverStatus = Lens.lens (\ReplicationGroupPendingModifiedValues' {automaticFailoverStatus} -> automaticFailoverStatus) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {automaticFailoverStatus = a} :: ReplicationGroupPendingModifiedValues)

instance
  Prelude.FromXML
    ReplicationGroupPendingModifiedValues
  where
  parseXML x =
    ReplicationGroupPendingModifiedValues'
      Prelude.<$> (x Prelude..@? "Resharding")
      Prelude.<*> (x Prelude..@? "PrimaryClusterId")
      Prelude.<*> (x Prelude..@? "AuthTokenStatus")
      Prelude.<*> (x Prelude..@? "UserGroups")
      Prelude.<*> (x Prelude..@? "AutomaticFailoverStatus")

instance
  Prelude.Hashable
    ReplicationGroupPendingModifiedValues

instance
  Prelude.NFData
    ReplicationGroupPendingModifiedValues
