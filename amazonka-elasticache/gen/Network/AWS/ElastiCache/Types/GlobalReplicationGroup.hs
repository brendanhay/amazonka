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

-- | Consists of a primary cluster that accepts writes and an associated
-- secondary cluster that resides in a different AWS region. The secondary
-- cluster accepts only reads. The primary cluster automatically replicates
-- updates to the secondary cluster.
--
-- -   The __GlobalReplicationGroupIdSuffix__ represents the name of the
--     Global Datastore, which is what you use to associate a secondary
--     cluster.
--
-- /See:/ 'newGlobalReplicationGroup' smart constructor.
data GlobalReplicationGroup = GlobalReplicationGroup'
  { -- | A flag that indicates whether the Global Datastore is cluster enabled.
    clusterEnabled :: Core.Maybe Core.Bool,
    -- | The status of the Global Datastore
    status :: Core.Maybe Core.Text,
    -- | The ARN (Amazon Resource Name) of the global replication group.
    arn :: Core.Maybe Core.Text,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Maybe Core.Text,
    -- | A flag that enables encryption at rest when set to @true@.
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the
    -- replication group is created. To enable encryption at rest on a
    -- replication group you must set @AtRestEncryptionEnabled@ to @true@ when
    -- you create the replication group.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    atRestEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | Indicates the slot configuration and global identifier for each slice
    -- group.
    globalNodeGroups :: Core.Maybe [GlobalNodeGroup],
    -- | The Elasticache Redis engine version.
    engineVersion :: Core.Maybe Core.Text,
    -- | The cache node type of the Global Datastore
    cacheNodeType :: Core.Maybe Core.Text,
    -- | The optional description of the Global Datastore
    globalReplicationGroupDescription :: Core.Maybe Core.Text,
    -- | The Elasticache engine. For Redis only.
    engine :: Core.Maybe Core.Text,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Core.Maybe Core.Bool,
    -- | The replication groups that comprise the Global Datastore.
    members :: Core.Maybe [GlobalReplicationGroupMember],
    -- | A flag that enables in-transit encryption when set to true. You cannot
    -- modify the value of @TransitEncryptionEnabled@ after the cluster is
    -- created. To enable in-transit encryption on a cluster you must set
    -- @TransitEncryptionEnabled@ to true when you create a cluster.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    transitEncryptionEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterEnabled', 'globalReplicationGroup_clusterEnabled' - A flag that indicates whether the Global Datastore is cluster enabled.
--
-- 'status', 'globalReplicationGroup_status' - The status of the Global Datastore
--
-- 'arn', 'globalReplicationGroup_arn' - The ARN (Amazon Resource Name) of the global replication group.
--
-- 'globalReplicationGroupId', 'globalReplicationGroup_globalReplicationGroupId' - The name of the Global Datastore
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
-- 'globalNodeGroups', 'globalReplicationGroup_globalNodeGroups' - Indicates the slot configuration and global identifier for each slice
-- group.
--
-- 'engineVersion', 'globalReplicationGroup_engineVersion' - The Elasticache Redis engine version.
--
-- 'cacheNodeType', 'globalReplicationGroup_cacheNodeType' - The cache node type of the Global Datastore
--
-- 'globalReplicationGroupDescription', 'globalReplicationGroup_globalReplicationGroupDescription' - The optional description of the Global Datastore
--
-- 'engine', 'globalReplicationGroup_engine' - The Elasticache engine. For Redis only.
--
-- 'authTokenEnabled', 'globalReplicationGroup_authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
--
-- 'members', 'globalReplicationGroup_members' - The replication groups that comprise the Global Datastore.
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
        Core.Nothing,
      status = Core.Nothing,
      arn = Core.Nothing,
      globalReplicationGroupId = Core.Nothing,
      atRestEncryptionEnabled = Core.Nothing,
      globalNodeGroups = Core.Nothing,
      engineVersion = Core.Nothing,
      cacheNodeType = Core.Nothing,
      globalReplicationGroupDescription = Core.Nothing,
      engine = Core.Nothing,
      authTokenEnabled = Core.Nothing,
      members = Core.Nothing,
      transitEncryptionEnabled = Core.Nothing
    }

-- | A flag that indicates whether the Global Datastore is cluster enabled.
globalReplicationGroup_clusterEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
globalReplicationGroup_clusterEnabled = Lens.lens (\GlobalReplicationGroup' {clusterEnabled} -> clusterEnabled) (\s@GlobalReplicationGroup' {} a -> s {clusterEnabled = a} :: GlobalReplicationGroup)

-- | The status of the Global Datastore
globalReplicationGroup_status :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
globalReplicationGroup_status = Lens.lens (\GlobalReplicationGroup' {status} -> status) (\s@GlobalReplicationGroup' {} a -> s {status = a} :: GlobalReplicationGroup)

-- | The ARN (Amazon Resource Name) of the global replication group.
globalReplicationGroup_arn :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
globalReplicationGroup_arn = Lens.lens (\GlobalReplicationGroup' {arn} -> arn) (\s@GlobalReplicationGroup' {} a -> s {arn = a} :: GlobalReplicationGroup)

-- | The name of the Global Datastore
globalReplicationGroup_globalReplicationGroupId :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
globalReplicationGroup_globalReplicationGroupId = Lens.lens (\GlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@GlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: GlobalReplicationGroup)

-- | A flag that enables encryption at rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- replication group is created. To enable encryption at rest on a
-- replication group you must set @AtRestEncryptionEnabled@ to @true@ when
-- you create the replication group.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
globalReplicationGroup_atRestEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
globalReplicationGroup_atRestEncryptionEnabled = Lens.lens (\GlobalReplicationGroup' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@GlobalReplicationGroup' {} a -> s {atRestEncryptionEnabled = a} :: GlobalReplicationGroup)

-- | Indicates the slot configuration and global identifier for each slice
-- group.
globalReplicationGroup_globalNodeGroups :: Lens.Lens' GlobalReplicationGroup (Core.Maybe [GlobalNodeGroup])
globalReplicationGroup_globalNodeGroups = Lens.lens (\GlobalReplicationGroup' {globalNodeGroups} -> globalNodeGroups) (\s@GlobalReplicationGroup' {} a -> s {globalNodeGroups = a} :: GlobalReplicationGroup) Core.. Lens.mapping Lens._Coerce

-- | The Elasticache Redis engine version.
globalReplicationGroup_engineVersion :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
globalReplicationGroup_engineVersion = Lens.lens (\GlobalReplicationGroup' {engineVersion} -> engineVersion) (\s@GlobalReplicationGroup' {} a -> s {engineVersion = a} :: GlobalReplicationGroup)

-- | The cache node type of the Global Datastore
globalReplicationGroup_cacheNodeType :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
globalReplicationGroup_cacheNodeType = Lens.lens (\GlobalReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@GlobalReplicationGroup' {} a -> s {cacheNodeType = a} :: GlobalReplicationGroup)

-- | The optional description of the Global Datastore
globalReplicationGroup_globalReplicationGroupDescription :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
globalReplicationGroup_globalReplicationGroupDescription = Lens.lens (\GlobalReplicationGroup' {globalReplicationGroupDescription} -> globalReplicationGroupDescription) (\s@GlobalReplicationGroup' {} a -> s {globalReplicationGroupDescription = a} :: GlobalReplicationGroup)

-- | The Elasticache engine. For Redis only.
globalReplicationGroup_engine :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
globalReplicationGroup_engine = Lens.lens (\GlobalReplicationGroup' {engine} -> engine) (\s@GlobalReplicationGroup' {} a -> s {engine = a} :: GlobalReplicationGroup)

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
globalReplicationGroup_authTokenEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
globalReplicationGroup_authTokenEnabled = Lens.lens (\GlobalReplicationGroup' {authTokenEnabled} -> authTokenEnabled) (\s@GlobalReplicationGroup' {} a -> s {authTokenEnabled = a} :: GlobalReplicationGroup)

-- | The replication groups that comprise the Global Datastore.
globalReplicationGroup_members :: Lens.Lens' GlobalReplicationGroup (Core.Maybe [GlobalReplicationGroupMember])
globalReplicationGroup_members = Lens.lens (\GlobalReplicationGroup' {members} -> members) (\s@GlobalReplicationGroup' {} a -> s {members = a} :: GlobalReplicationGroup) Core.. Lens.mapping Lens._Coerce

-- | A flag that enables in-transit encryption when set to true. You cannot
-- modify the value of @TransitEncryptionEnabled@ after the cluster is
-- created. To enable in-transit encryption on a cluster you must set
-- @TransitEncryptionEnabled@ to true when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
globalReplicationGroup_transitEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
globalReplicationGroup_transitEncryptionEnabled = Lens.lens (\GlobalReplicationGroup' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@GlobalReplicationGroup' {} a -> s {transitEncryptionEnabled = a} :: GlobalReplicationGroup)

instance Core.FromXML GlobalReplicationGroup where
  parseXML x =
    GlobalReplicationGroup'
      Core.<$> (x Core..@? "ClusterEnabled")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "ARN")
      Core.<*> (x Core..@? "GlobalReplicationGroupId")
      Core.<*> (x Core..@? "AtRestEncryptionEnabled")
      Core.<*> ( x Core..@? "GlobalNodeGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "GlobalNodeGroup")
               )
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "GlobalReplicationGroupDescription")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "AuthTokenEnabled")
      Core.<*> ( x Core..@? "Members" Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "GlobalReplicationGroupMember")
               )
      Core.<*> (x Core..@? "TransitEncryptionEnabled")

instance Core.Hashable GlobalReplicationGroup

instance Core.NFData GlobalReplicationGroup
