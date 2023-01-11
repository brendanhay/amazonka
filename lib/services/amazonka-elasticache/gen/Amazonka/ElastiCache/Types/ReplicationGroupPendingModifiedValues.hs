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
-- Module      : Amazonka.ElastiCache.Types.ReplicationGroupPendingModifiedValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ReplicationGroupPendingModifiedValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.AuthTokenUpdateStatus
import Amazonka.ElastiCache.Types.PendingAutomaticFailoverStatus
import Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration
import Amazonka.ElastiCache.Types.ReshardingStatus
import Amazonka.ElastiCache.Types.TransitEncryptionMode
import Amazonka.ElastiCache.Types.UserGroupsUpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | The settings to be applied to the Redis replication group, either
-- immediately or during the next maintenance window.
--
-- /See:/ 'newReplicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
  { -- | The auth token status
    authTokenStatus :: Prelude.Maybe AuthTokenUpdateStatus,
    -- | Indicates the status of automatic failover for this Redis replication
    -- group.
    automaticFailoverStatus :: Prelude.Maybe PendingAutomaticFailoverStatus,
    -- | The log delivery configurations being modified
    logDeliveryConfigurations :: Prelude.Maybe [PendingLogDeliveryConfiguration],
    -- | The primary cluster ID that is applied immediately (if
    -- @--apply-immediately@ was specified), or during the next maintenance
    -- window.
    primaryClusterId :: Prelude.Maybe Prelude.Text,
    -- | The status of an online resharding operation.
    resharding :: Prelude.Maybe ReshardingStatus,
    -- | A flag that enables in-transit encryption when set to true.
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A setting that allows you to migrate your clients to use in-transit
    -- encryption, with no downtime.
    transitEncryptionMode :: Prelude.Maybe TransitEncryptionMode,
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
-- 'authTokenStatus', 'replicationGroupPendingModifiedValues_authTokenStatus' - The auth token status
--
-- 'automaticFailoverStatus', 'replicationGroupPendingModifiedValues_automaticFailoverStatus' - Indicates the status of automatic failover for this Redis replication
-- group.
--
-- 'logDeliveryConfigurations', 'replicationGroupPendingModifiedValues_logDeliveryConfigurations' - The log delivery configurations being modified
--
-- 'primaryClusterId', 'replicationGroupPendingModifiedValues_primaryClusterId' - The primary cluster ID that is applied immediately (if
-- @--apply-immediately@ was specified), or during the next maintenance
-- window.
--
-- 'resharding', 'replicationGroupPendingModifiedValues_resharding' - The status of an online resharding operation.
--
-- 'transitEncryptionEnabled', 'replicationGroupPendingModifiedValues_transitEncryptionEnabled' - A flag that enables in-transit encryption when set to true.
--
-- 'transitEncryptionMode', 'replicationGroupPendingModifiedValues_transitEncryptionMode' - A setting that allows you to migrate your clients to use in-transit
-- encryption, with no downtime.
--
-- 'userGroups', 'replicationGroupPendingModifiedValues_userGroups' - The user group being modified.
newReplicationGroupPendingModifiedValues ::
  ReplicationGroupPendingModifiedValues
newReplicationGroupPendingModifiedValues =
  ReplicationGroupPendingModifiedValues'
    { authTokenStatus =
        Prelude.Nothing,
      automaticFailoverStatus =
        Prelude.Nothing,
      logDeliveryConfigurations =
        Prelude.Nothing,
      primaryClusterId = Prelude.Nothing,
      resharding = Prelude.Nothing,
      transitEncryptionEnabled =
        Prelude.Nothing,
      transitEncryptionMode =
        Prelude.Nothing,
      userGroups = Prelude.Nothing
    }

-- | The auth token status
replicationGroupPendingModifiedValues_authTokenStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe AuthTokenUpdateStatus)
replicationGroupPendingModifiedValues_authTokenStatus = Lens.lens (\ReplicationGroupPendingModifiedValues' {authTokenStatus} -> authTokenStatus) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {authTokenStatus = a} :: ReplicationGroupPendingModifiedValues)

-- | Indicates the status of automatic failover for this Redis replication
-- group.
replicationGroupPendingModifiedValues_automaticFailoverStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe PendingAutomaticFailoverStatus)
replicationGroupPendingModifiedValues_automaticFailoverStatus = Lens.lens (\ReplicationGroupPendingModifiedValues' {automaticFailoverStatus} -> automaticFailoverStatus) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {automaticFailoverStatus = a} :: ReplicationGroupPendingModifiedValues)

-- | The log delivery configurations being modified
replicationGroupPendingModifiedValues_logDeliveryConfigurations :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe [PendingLogDeliveryConfiguration])
replicationGroupPendingModifiedValues_logDeliveryConfigurations = Lens.lens (\ReplicationGroupPendingModifiedValues' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {logDeliveryConfigurations = a} :: ReplicationGroupPendingModifiedValues) Prelude.. Lens.mapping Lens.coerced

-- | The primary cluster ID that is applied immediately (if
-- @--apply-immediately@ was specified), or during the next maintenance
-- window.
replicationGroupPendingModifiedValues_primaryClusterId :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe Prelude.Text)
replicationGroupPendingModifiedValues_primaryClusterId = Lens.lens (\ReplicationGroupPendingModifiedValues' {primaryClusterId} -> primaryClusterId) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {primaryClusterId = a} :: ReplicationGroupPendingModifiedValues)

-- | The status of an online resharding operation.
replicationGroupPendingModifiedValues_resharding :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe ReshardingStatus)
replicationGroupPendingModifiedValues_resharding = Lens.lens (\ReplicationGroupPendingModifiedValues' {resharding} -> resharding) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {resharding = a} :: ReplicationGroupPendingModifiedValues)

-- | A flag that enables in-transit encryption when set to true.
replicationGroupPendingModifiedValues_transitEncryptionEnabled :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe Prelude.Bool)
replicationGroupPendingModifiedValues_transitEncryptionEnabled = Lens.lens (\ReplicationGroupPendingModifiedValues' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {transitEncryptionEnabled = a} :: ReplicationGroupPendingModifiedValues)

-- | A setting that allows you to migrate your clients to use in-transit
-- encryption, with no downtime.
replicationGroupPendingModifiedValues_transitEncryptionMode :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe TransitEncryptionMode)
replicationGroupPendingModifiedValues_transitEncryptionMode = Lens.lens (\ReplicationGroupPendingModifiedValues' {transitEncryptionMode} -> transitEncryptionMode) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {transitEncryptionMode = a} :: ReplicationGroupPendingModifiedValues)

-- | The user group being modified.
replicationGroupPendingModifiedValues_userGroups :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe UserGroupsUpdateStatus)
replicationGroupPendingModifiedValues_userGroups = Lens.lens (\ReplicationGroupPendingModifiedValues' {userGroups} -> userGroups) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {userGroups = a} :: ReplicationGroupPendingModifiedValues)

instance
  Data.FromXML
    ReplicationGroupPendingModifiedValues
  where
  parseXML x =
    ReplicationGroupPendingModifiedValues'
      Prelude.<$> (x Data..@? "AuthTokenStatus")
      Prelude.<*> (x Data..@? "AutomaticFailoverStatus")
      Prelude.<*> ( x Data..@? "LogDeliveryConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "PrimaryClusterId")
      Prelude.<*> (x Data..@? "Resharding")
      Prelude.<*> (x Data..@? "TransitEncryptionEnabled")
      Prelude.<*> (x Data..@? "TransitEncryptionMode")
      Prelude.<*> (x Data..@? "UserGroups")

instance
  Prelude.Hashable
    ReplicationGroupPendingModifiedValues
  where
  hashWithSalt
    _salt
    ReplicationGroupPendingModifiedValues' {..} =
      _salt `Prelude.hashWithSalt` authTokenStatus
        `Prelude.hashWithSalt` automaticFailoverStatus
        `Prelude.hashWithSalt` logDeliveryConfigurations
        `Prelude.hashWithSalt` primaryClusterId
        `Prelude.hashWithSalt` resharding
        `Prelude.hashWithSalt` transitEncryptionEnabled
        `Prelude.hashWithSalt` transitEncryptionMode
        `Prelude.hashWithSalt` userGroups

instance
  Prelude.NFData
    ReplicationGroupPendingModifiedValues
  where
  rnf ReplicationGroupPendingModifiedValues' {..} =
    Prelude.rnf authTokenStatus
      `Prelude.seq` Prelude.rnf automaticFailoverStatus
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf primaryClusterId
      `Prelude.seq` Prelude.rnf resharding
      `Prelude.seq` Prelude.rnf transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf transitEncryptionMode
      `Prelude.seq` Prelude.rnf userGroups
