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

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
import Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.PendingLogDeliveryConfiguration
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
    -- | The log delivery configurations being modified
    logDeliveryConfigurations :: Prelude.Maybe [PendingLogDeliveryConfiguration],
    -- | Indicates the status of automatic failover for this Redis replication
    -- group.
    automaticFailoverStatus :: Prelude.Maybe PendingAutomaticFailoverStatus,
    -- | The user group being modified.
    userGroups :: Prelude.Maybe UserGroupsUpdateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'logDeliveryConfigurations', 'replicationGroupPendingModifiedValues_logDeliveryConfigurations' - The log delivery configurations being modified
--
-- 'automaticFailoverStatus', 'replicationGroupPendingModifiedValues_automaticFailoverStatus' - Indicates the status of automatic failover for this Redis replication
-- group.
--
-- 'userGroups', 'replicationGroupPendingModifiedValues_userGroups' - The user group being modified.
newReplicationGroupPendingModifiedValues ::
  ReplicationGroupPendingModifiedValues
newReplicationGroupPendingModifiedValues =
  ReplicationGroupPendingModifiedValues'
    { resharding =
        Prelude.Nothing,
      primaryClusterId = Prelude.Nothing,
      authTokenStatus = Prelude.Nothing,
      logDeliveryConfigurations =
        Prelude.Nothing,
      automaticFailoverStatus =
        Prelude.Nothing,
      userGroups = Prelude.Nothing
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

-- | The log delivery configurations being modified
replicationGroupPendingModifiedValues_logDeliveryConfigurations :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe [PendingLogDeliveryConfiguration])
replicationGroupPendingModifiedValues_logDeliveryConfigurations = Lens.lens (\ReplicationGroupPendingModifiedValues' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {logDeliveryConfigurations = a} :: ReplicationGroupPendingModifiedValues) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates the status of automatic failover for this Redis replication
-- group.
replicationGroupPendingModifiedValues_automaticFailoverStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe PendingAutomaticFailoverStatus)
replicationGroupPendingModifiedValues_automaticFailoverStatus = Lens.lens (\ReplicationGroupPendingModifiedValues' {automaticFailoverStatus} -> automaticFailoverStatus) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {automaticFailoverStatus = a} :: ReplicationGroupPendingModifiedValues)

-- | The user group being modified.
replicationGroupPendingModifiedValues_userGroups :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe UserGroupsUpdateStatus)
replicationGroupPendingModifiedValues_userGroups = Lens.lens (\ReplicationGroupPendingModifiedValues' {userGroups} -> userGroups) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {userGroups = a} :: ReplicationGroupPendingModifiedValues)

instance
  Core.FromXML
    ReplicationGroupPendingModifiedValues
  where
  parseXML x =
    ReplicationGroupPendingModifiedValues'
      Prelude.<$> (x Core..@? "Resharding")
      Prelude.<*> (x Core..@? "PrimaryClusterId")
      Prelude.<*> (x Core..@? "AuthTokenStatus")
      Prelude.<*> ( x Core..@? "LogDeliveryConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "AutomaticFailoverStatus")
      Prelude.<*> (x Core..@? "UserGroups")

instance
  Prelude.Hashable
    ReplicationGroupPendingModifiedValues

instance
  Prelude.NFData
    ReplicationGroupPendingModifiedValues
