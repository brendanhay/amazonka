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
-- Module      : Amazonka.ElastiCache.Types.GlobalReplicationGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.GlobalReplicationGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.GlobalNodeGroup
import Amazonka.ElastiCache.Types.GlobalReplicationGroupMember
import qualified Amazonka.Prelude as Prelude

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
  { -- | The ARN (Amazon Resource Name) of the global replication group.
    arn :: Prelude.Maybe Prelude.Text,
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
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The cache node type of the Global datastore
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether the Global datastore is cluster enabled.
    clusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Elasticache engine. For Redis only.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The Elasticache Redis engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Indicates the slot configuration and global identifier for each slice
    -- group.
    globalNodeGroups :: Prelude.Maybe [GlobalNodeGroup],
    -- | The optional description of the Global datastore
    globalReplicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The replication groups that comprise the Global datastore.
    members :: Prelude.Maybe [GlobalReplicationGroupMember],
    -- | The status of the Global datastore
    status :: Prelude.Maybe Prelude.Text,
    -- | A flag that enables in-transit encryption when set to true.
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
-- 'arn', 'globalReplicationGroup_arn' - The ARN (Amazon Resource Name) of the global replication group.
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
-- 'authTokenEnabled', 'globalReplicationGroup_authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
--
-- 'cacheNodeType', 'globalReplicationGroup_cacheNodeType' - The cache node type of the Global datastore
--
-- 'clusterEnabled', 'globalReplicationGroup_clusterEnabled' - A flag that indicates whether the Global datastore is cluster enabled.
--
-- 'engine', 'globalReplicationGroup_engine' - The Elasticache engine. For Redis only.
--
-- 'engineVersion', 'globalReplicationGroup_engineVersion' - The Elasticache Redis engine version.
--
-- 'globalNodeGroups', 'globalReplicationGroup_globalNodeGroups' - Indicates the slot configuration and global identifier for each slice
-- group.
--
-- 'globalReplicationGroupDescription', 'globalReplicationGroup_globalReplicationGroupDescription' - The optional description of the Global datastore
--
-- 'globalReplicationGroupId', 'globalReplicationGroup_globalReplicationGroupId' - The name of the Global datastore
--
-- 'members', 'globalReplicationGroup_members' - The replication groups that comprise the Global datastore.
--
-- 'status', 'globalReplicationGroup_status' - The status of the Global datastore
--
-- 'transitEncryptionEnabled', 'globalReplicationGroup_transitEncryptionEnabled' - A flag that enables in-transit encryption when set to true.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
newGlobalReplicationGroup ::
  GlobalReplicationGroup
newGlobalReplicationGroup =
  GlobalReplicationGroup'
    { arn = Prelude.Nothing,
      atRestEncryptionEnabled = Prelude.Nothing,
      authTokenEnabled = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      clusterEnabled = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalNodeGroups = Prelude.Nothing,
      globalReplicationGroupDescription = Prelude.Nothing,
      globalReplicationGroupId = Prelude.Nothing,
      members = Prelude.Nothing,
      status = Prelude.Nothing,
      transitEncryptionEnabled = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the global replication group.
globalReplicationGroup_arn :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_arn = Lens.lens (\GlobalReplicationGroup' {arn} -> arn) (\s@GlobalReplicationGroup' {} a -> s {arn = a} :: GlobalReplicationGroup)

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

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
globalReplicationGroup_authTokenEnabled :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
globalReplicationGroup_authTokenEnabled = Lens.lens (\GlobalReplicationGroup' {authTokenEnabled} -> authTokenEnabled) (\s@GlobalReplicationGroup' {} a -> s {authTokenEnabled = a} :: GlobalReplicationGroup)

-- | The cache node type of the Global datastore
globalReplicationGroup_cacheNodeType :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_cacheNodeType = Lens.lens (\GlobalReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@GlobalReplicationGroup' {} a -> s {cacheNodeType = a} :: GlobalReplicationGroup)

-- | A flag that indicates whether the Global datastore is cluster enabled.
globalReplicationGroup_clusterEnabled :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
globalReplicationGroup_clusterEnabled = Lens.lens (\GlobalReplicationGroup' {clusterEnabled} -> clusterEnabled) (\s@GlobalReplicationGroup' {} a -> s {clusterEnabled = a} :: GlobalReplicationGroup)

-- | The Elasticache engine. For Redis only.
globalReplicationGroup_engine :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_engine = Lens.lens (\GlobalReplicationGroup' {engine} -> engine) (\s@GlobalReplicationGroup' {} a -> s {engine = a} :: GlobalReplicationGroup)

-- | The Elasticache Redis engine version.
globalReplicationGroup_engineVersion :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_engineVersion = Lens.lens (\GlobalReplicationGroup' {engineVersion} -> engineVersion) (\s@GlobalReplicationGroup' {} a -> s {engineVersion = a} :: GlobalReplicationGroup)

-- | Indicates the slot configuration and global identifier for each slice
-- group.
globalReplicationGroup_globalNodeGroups :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe [GlobalNodeGroup])
globalReplicationGroup_globalNodeGroups = Lens.lens (\GlobalReplicationGroup' {globalNodeGroups} -> globalNodeGroups) (\s@GlobalReplicationGroup' {} a -> s {globalNodeGroups = a} :: GlobalReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The optional description of the Global datastore
globalReplicationGroup_globalReplicationGroupDescription :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_globalReplicationGroupDescription = Lens.lens (\GlobalReplicationGroup' {globalReplicationGroupDescription} -> globalReplicationGroupDescription) (\s@GlobalReplicationGroup' {} a -> s {globalReplicationGroupDescription = a} :: GlobalReplicationGroup)

-- | The name of the Global datastore
globalReplicationGroup_globalReplicationGroupId :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_globalReplicationGroupId = Lens.lens (\GlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@GlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: GlobalReplicationGroup)

-- | The replication groups that comprise the Global datastore.
globalReplicationGroup_members :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe [GlobalReplicationGroupMember])
globalReplicationGroup_members = Lens.lens (\GlobalReplicationGroup' {members} -> members) (\s@GlobalReplicationGroup' {} a -> s {members = a} :: GlobalReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The status of the Global datastore
globalReplicationGroup_status :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Text)
globalReplicationGroup_status = Lens.lens (\GlobalReplicationGroup' {status} -> status) (\s@GlobalReplicationGroup' {} a -> s {status = a} :: GlobalReplicationGroup)

-- | A flag that enables in-transit encryption when set to true.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
globalReplicationGroup_transitEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
globalReplicationGroup_transitEncryptionEnabled = Lens.lens (\GlobalReplicationGroup' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@GlobalReplicationGroup' {} a -> s {transitEncryptionEnabled = a} :: GlobalReplicationGroup)

instance Data.FromXML GlobalReplicationGroup where
  parseXML x =
    GlobalReplicationGroup'
      Prelude.<$> (x Data..@? "ARN")
      Prelude.<*> (x Data..@? "AtRestEncryptionEnabled")
      Prelude.<*> (x Data..@? "AuthTokenEnabled")
      Prelude.<*> (x Data..@? "CacheNodeType")
      Prelude.<*> (x Data..@? "ClusterEnabled")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> ( x
                      Data..@? "GlobalNodeGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "GlobalNodeGroup")
                  )
      Prelude.<*> (x Data..@? "GlobalReplicationGroupDescription")
      Prelude.<*> (x Data..@? "GlobalReplicationGroupId")
      Prelude.<*> ( x
                      Data..@? "Members"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "GlobalReplicationGroupMember")
                  )
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "TransitEncryptionEnabled")

instance Prelude.Hashable GlobalReplicationGroup where
  hashWithSalt _salt GlobalReplicationGroup' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` atRestEncryptionEnabled
      `Prelude.hashWithSalt` authTokenEnabled
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` clusterEnabled
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` globalNodeGroups
      `Prelude.hashWithSalt` globalReplicationGroupDescription
      `Prelude.hashWithSalt` globalReplicationGroupId
      `Prelude.hashWithSalt` members
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` transitEncryptionEnabled

instance Prelude.NFData GlobalReplicationGroup where
  rnf GlobalReplicationGroup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf atRestEncryptionEnabled
      `Prelude.seq` Prelude.rnf authTokenEnabled
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf clusterEnabled
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf globalNodeGroups
      `Prelude.seq` Prelude.rnf globalReplicationGroupDescription
      `Prelude.seq` Prelude.rnf globalReplicationGroupId
      `Prelude.seq` Prelude.rnf members
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf transitEncryptionEnabled
