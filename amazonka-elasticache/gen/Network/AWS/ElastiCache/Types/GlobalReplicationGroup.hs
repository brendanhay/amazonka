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
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.GlobalNodeGroup
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Consists of a primary cluster that accepts writes and an associated
-- secondary cluster that resides in a different Amazon region. The
-- secondary cluster accepts only reads. The primary cluster automatically
-- replicates updates to the secondary cluster.
--
-- -   The __GlobalReplicationGroupIdSuffix__ represents the name of the
--     Global datastore, which is what you use to associate a secondary
--     cluster.
--
-- /See:/ 'newGlobalReplicationGroup' smart constructor.
data GlobalReplicationGroup = GlobalReplicationGroup'
  { -- | A flag that indicates whether the Global datastore is cluster enabled.
    clusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The status of the Global datastore
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the global replication group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the slot configuration and global identifier for each slice
    -- group.
    globalNodeGroups :: Prelude.Maybe [GlobalNodeGroup],
    -- | A flag that enables encryption at rest when set to @true@.
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the
    -- replication group is created. To enable encryption at rest on a
    -- replication group you must set @AtRestEncryptionEnabled@ to @true@ when
    -- you create the replication group.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    atRestEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Elasticache Redis engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The cache node type of the Global datastore
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The optional description of the Global datastore
    globalReplicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The Elasticache engine. For Redis only.
    engine :: Prelude.Maybe Prelude.Text,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The replication groups that comprise the Global datastore.
    members :: Prelude.Maybe [GlobalReplicationGroupMember],
    -- | A flag that enables in-transit encryption when set to true. You cannot
    -- modify the value of @TransitEncryptionEnabled@ after the cluster is
    -- created. To enable in-transit encryption on a cluster you must set
    -- @TransitEncryptionEnabled@ to true when you create a cluster.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterEnabled', 'globalReplicationGroup_clusterEnabled' - A flag that indicates whether the Global datastore is cluster enabled.
--
-- 'status', 'globalReplicationGroup_status' - The status of the Global datastore
--
-- 'globalReplicationGroupId', 'globalReplicationGroup_globalReplicationGroupId' - The name of the Global datastore
--
-- 'arn', 'globalReplicationGroup_arn' - The ARN (Amazon Resource Name) of the global replication group.
--
-- 'globalNodeGroups', 'globalReplicationGroup_globalNodeGroups' - Indicates the slot configuration and global identifier for each slice
-- group.
--
-- 'atRestEncryptionEnabled', 'globalReplicationGroup_atRestEncryptionEnabled' - A flag that enables encryption at rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- replication group is created. To enable encryption at rest on a
-- replication group you must set @AtRestEncryptionEnabled@ to @true@ when
-- you create the replication group.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- 'engineVersion', 'globalReplicationGroup_engineVersion' - The Elasticache Redis engine version.
--
-- 'cacheNodeType', 'globalReplicationGroup_cacheNodeType' - The cache node type of the Global datastore
--
-- 'globalReplicationGroupDescription', 'globalReplicationGroup_globalReplicationGroupDescription' - The optional description of the Global datastore
--
-- 'engine', 'globalReplicationGroup_engine' - The Elasticache engine. For Redis only.
--
-- 'authTokenEnabled', 'globalReplicationGroup_authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
--
-- 'members', 'globalReplicationGroup_members' - The replication groups that comprise the Global datastore.
--
-- 'transitEncryptionEnabled', 'globalReplicationGroup_transitEncryptionEnabled' - A flag that enables in-transit encryption when set to true. You cannot
-- modify the value of @TransitEncryptionEnabled@ after the cluster is
-- created. To enable in-transit encryption on a cluster you must set
-- @TransitEncryptionEnabled@ to true when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
newGlobalReplicationGroup ::
  GlobalReplicationGroup
newGlobalReplicationGroup =
  GlobalReplicationGroup'
    { clusterEnabled =
        Prelude.Nothing,
      status = Prelude.Nothing,
      globalReplicationGroupId = Prelude.Nothing,
      arn = Prelude.Nothing,
      globalNodeGroups = Prelude.Nothing,
      atRestEncryptionEnabled = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      globalReplicationGroupDescription = Prelude.Nothing,
      engine = Prelude.Nothing,
      authTokenEnabled = Prelude.Nothing,
      members = Prelude.Nothing,
      transitEncryptionEnabled = Prelude.Nothing
    }

-- | A flag that indicates whether the Global datastore is cluster enabled.
globalReplicationGroup_clusterEnabled :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
globalReplicationGroup_clusterEnabled = Lens.lens (\GlobalReplicationGroup' {clusterEnabled} -> clusterEnabled) (\s@GlobalReplicationGroup' {} a -> s {clusterEnabled = a} :: GlobalReplicationGroup)

-- | The status of the Global datastore
globalReplicationGroup_status :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_status = Lens.lens (\GlobalReplicationGroup' {status} -> status) (\s@GlobalReplicationGroup' {} a -> s {status = a} :: GlobalReplicationGroup)

-- | The name of the Global datastore
globalReplicationGroup_globalReplicationGroupId :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_globalReplicationGroupId = Lens.lens (\GlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@GlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: GlobalReplicationGroup)

-- | The ARN (Amazon Resource Name) of the global replication group.
globalReplicationGroup_arn :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_arn = Lens.lens (\GlobalReplicationGroup' {arn} -> arn) (\s@GlobalReplicationGroup' {} a -> s {arn = a} :: GlobalReplicationGroup)

-- | Indicates the slot configuration and global identifier for each slice
-- group.
globalReplicationGroup_globalNodeGroups :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe [GlobalNodeGroup])
globalReplicationGroup_globalNodeGroups = Lens.lens (\GlobalReplicationGroup' {globalNodeGroups} -> globalNodeGroups) (\s@GlobalReplicationGroup' {} a -> s {globalNodeGroups = a} :: GlobalReplicationGroup) Prelude.. Lens.mapping Lens._Coerce

-- | A flag that enables encryption at rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- replication group is created. To enable encryption at rest on a
-- replication group you must set @AtRestEncryptionEnabled@ to @true@ when
-- you create the replication group.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
globalReplicationGroup_atRestEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
globalReplicationGroup_atRestEncryptionEnabled = Lens.lens (\GlobalReplicationGroup' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@GlobalReplicationGroup' {} a -> s {atRestEncryptionEnabled = a} :: GlobalReplicationGroup)

-- | The Elasticache Redis engine version.
globalReplicationGroup_engineVersion :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_engineVersion = Lens.lens (\GlobalReplicationGroup' {engineVersion} -> engineVersion) (\s@GlobalReplicationGroup' {} a -> s {engineVersion = a} :: GlobalReplicationGroup)

-- | The cache node type of the Global datastore
globalReplicationGroup_cacheNodeType :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_cacheNodeType = Lens.lens (\GlobalReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@GlobalReplicationGroup' {} a -> s {cacheNodeType = a} :: GlobalReplicationGroup)

-- | The optional description of the Global datastore
globalReplicationGroup_globalReplicationGroupDescription :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_globalReplicationGroupDescription = Lens.lens (\GlobalReplicationGroup' {globalReplicationGroupDescription} -> globalReplicationGroupDescription) (\s@GlobalReplicationGroup' {} a -> s {globalReplicationGroupDescription = a} :: GlobalReplicationGroup)

-- | The Elasticache engine. For Redis only.
globalReplicationGroup_engine :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_engine = Lens.lens (\GlobalReplicationGroup' {engine} -> engine) (\s@GlobalReplicationGroup' {} a -> s {engine = a} :: GlobalReplicationGroup)

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
globalReplicationGroup_authTokenEnabled :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
globalReplicationGroup_authTokenEnabled = Lens.lens (\GlobalReplicationGroup' {authTokenEnabled} -> authTokenEnabled) (\s@GlobalReplicationGroup' {} a -> s {authTokenEnabled = a} :: GlobalReplicationGroup)

-- | The replication groups that comprise the Global datastore.
globalReplicationGroup_members :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe [GlobalReplicationGroupMember])
globalReplicationGroup_members = Lens.lens (\GlobalReplicationGroup' {members} -> members) (\s@GlobalReplicationGroup' {} a -> s {members = a} :: GlobalReplicationGroup) Prelude.. Lens.mapping Lens._Coerce

-- | A flag that enables in-transit encryption when set to true. You cannot
-- modify the value of @TransitEncryptionEnabled@ after the cluster is
-- created. To enable in-transit encryption on a cluster you must set
-- @TransitEncryptionEnabled@ to true when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
globalReplicationGroup_transitEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
globalReplicationGroup_transitEncryptionEnabled = Lens.lens (\GlobalReplicationGroup' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@GlobalReplicationGroup' {} a -> s {transitEncryptionEnabled = a} :: GlobalReplicationGroup)

instance Core.FromXML GlobalReplicationGroup where
  parseXML x =
    GlobalReplicationGroup'
      Prelude.<$> (x Core..@? "ClusterEnabled")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "GlobalReplicationGroupId")
      Prelude.<*> (x Core..@? "ARN")
      Prelude.<*> ( x Core..@? "GlobalNodeGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "GlobalNodeGroup")
                  )
      Prelude.<*> (x Core..@? "AtRestEncryptionEnabled")
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "CacheNodeType")
      Prelude.<*> (x Core..@? "GlobalReplicationGroupDescription")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "AuthTokenEnabled")
      Prelude.<*> ( x Core..@? "Members" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "GlobalReplicationGroupMember")
                  )
      Prelude.<*> (x Core..@? "TransitEncryptionEnabled")

instance Prelude.Hashable GlobalReplicationGroup

instance Prelude.NFData GlobalReplicationGroup
