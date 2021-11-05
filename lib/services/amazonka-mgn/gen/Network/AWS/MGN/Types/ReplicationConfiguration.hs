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
-- Module      : Amazonka.MGN.Types.ReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ReplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MGN.Types.ReplicationConfigurationDataPlaneRouting
import Amazonka.MGN.Types.ReplicationConfigurationDefaultLargeStagingDiskType
import Amazonka.MGN.Types.ReplicationConfigurationEbsEncryption
import Amazonka.MGN.Types.ReplicationConfigurationReplicatedDisk
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newReplicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { -- | Replication Configuration create Public IP.
    createPublicIP :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration Staging Area tags.
    stagingAreaTags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Replication Configuration Staging Area subnet ID.
    stagingAreaSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration Replication Server instance type.
    replicationServerInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration EBS encryption.
    ebsEncryption :: Prelude.Maybe ReplicationConfigurationEbsEncryption,
    -- | Replication Configuration associate default Application Migration
    -- Service Security Group.
    associateDefaultSecurityGroup :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration Replication Server Security Group IDs.
    replicationServersSecurityGroupsIDs :: Prelude.Maybe [Prelude.Text],
    -- | Replication Configuration EBS encryption key ARN.
    ebsEncryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration use default large Staging Disks.
    defaultLargeStagingDiskType :: Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType,
    -- | Replication Configuration set bandwidth throttling.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | Replication Configuration data plane routing.
    dataPlaneRouting :: Prelude.Maybe ReplicationConfigurationDataPlaneRouting,
    -- | Replication Configuration Source Server ID.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration replicated disks.
    replicatedDisks :: Prelude.Maybe [ReplicationConfigurationReplicatedDisk],
    -- | Replication Configuration use Dedicated Replication Server.
    useDedicatedReplicationServer :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createPublicIP', 'replicationConfiguration_createPublicIP' - Replication Configuration create Public IP.
--
-- 'stagingAreaTags', 'replicationConfiguration_stagingAreaTags' - Replication Configuration Staging Area tags.
--
-- 'stagingAreaSubnetId', 'replicationConfiguration_stagingAreaSubnetId' - Replication Configuration Staging Area subnet ID.
--
-- 'replicationServerInstanceType', 'replicationConfiguration_replicationServerInstanceType' - Replication Configuration Replication Server instance type.
--
-- 'ebsEncryption', 'replicationConfiguration_ebsEncryption' - Replication Configuration EBS encryption.
--
-- 'associateDefaultSecurityGroup', 'replicationConfiguration_associateDefaultSecurityGroup' - Replication Configuration associate default Application Migration
-- Service Security Group.
--
-- 'replicationServersSecurityGroupsIDs', 'replicationConfiguration_replicationServersSecurityGroupsIDs' - Replication Configuration Replication Server Security Group IDs.
--
-- 'ebsEncryptionKeyArn', 'replicationConfiguration_ebsEncryptionKeyArn' - Replication Configuration EBS encryption key ARN.
--
-- 'name', 'replicationConfiguration_name' - Replication Configuration name.
--
-- 'defaultLargeStagingDiskType', 'replicationConfiguration_defaultLargeStagingDiskType' - Replication Configuration use default large Staging Disks.
--
-- 'bandwidthThrottling', 'replicationConfiguration_bandwidthThrottling' - Replication Configuration set bandwidth throttling.
--
-- 'dataPlaneRouting', 'replicationConfiguration_dataPlaneRouting' - Replication Configuration data plane routing.
--
-- 'sourceServerID', 'replicationConfiguration_sourceServerID' - Replication Configuration Source Server ID.
--
-- 'replicatedDisks', 'replicationConfiguration_replicatedDisks' - Replication Configuration replicated disks.
--
-- 'useDedicatedReplicationServer', 'replicationConfiguration_useDedicatedReplicationServer' - Replication Configuration use Dedicated Replication Server.
newReplicationConfiguration ::
  ReplicationConfiguration
newReplicationConfiguration =
  ReplicationConfiguration'
    { createPublicIP =
        Prelude.Nothing,
      stagingAreaTags = Prelude.Nothing,
      stagingAreaSubnetId = Prelude.Nothing,
      replicationServerInstanceType = Prelude.Nothing,
      ebsEncryption = Prelude.Nothing,
      associateDefaultSecurityGroup = Prelude.Nothing,
      replicationServersSecurityGroupsIDs =
        Prelude.Nothing,
      ebsEncryptionKeyArn = Prelude.Nothing,
      name = Prelude.Nothing,
      defaultLargeStagingDiskType = Prelude.Nothing,
      bandwidthThrottling = Prelude.Nothing,
      dataPlaneRouting = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      replicatedDisks = Prelude.Nothing,
      useDedicatedReplicationServer = Prelude.Nothing
    }

-- | Replication Configuration create Public IP.
replicationConfiguration_createPublicIP :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Bool)
replicationConfiguration_createPublicIP = Lens.lens (\ReplicationConfiguration' {createPublicIP} -> createPublicIP) (\s@ReplicationConfiguration' {} a -> s {createPublicIP = a} :: ReplicationConfiguration)

-- | Replication Configuration Staging Area tags.
replicationConfiguration_stagingAreaTags :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
replicationConfiguration_stagingAreaTags = Lens.lens (\ReplicationConfiguration' {stagingAreaTags} -> stagingAreaTags) (\s@ReplicationConfiguration' {} a -> s {stagingAreaTags = a} :: ReplicationConfiguration) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Replication Configuration Staging Area subnet ID.
replicationConfiguration_stagingAreaSubnetId :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_stagingAreaSubnetId = Lens.lens (\ReplicationConfiguration' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@ReplicationConfiguration' {} a -> s {stagingAreaSubnetId = a} :: ReplicationConfiguration)

-- | Replication Configuration Replication Server instance type.
replicationConfiguration_replicationServerInstanceType :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_replicationServerInstanceType = Lens.lens (\ReplicationConfiguration' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@ReplicationConfiguration' {} a -> s {replicationServerInstanceType = a} :: ReplicationConfiguration)

-- | Replication Configuration EBS encryption.
replicationConfiguration_ebsEncryption :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe ReplicationConfigurationEbsEncryption)
replicationConfiguration_ebsEncryption = Lens.lens (\ReplicationConfiguration' {ebsEncryption} -> ebsEncryption) (\s@ReplicationConfiguration' {} a -> s {ebsEncryption = a} :: ReplicationConfiguration)

-- | Replication Configuration associate default Application Migration
-- Service Security Group.
replicationConfiguration_associateDefaultSecurityGroup :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Bool)
replicationConfiguration_associateDefaultSecurityGroup = Lens.lens (\ReplicationConfiguration' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@ReplicationConfiguration' {} a -> s {associateDefaultSecurityGroup = a} :: ReplicationConfiguration)

-- | Replication Configuration Replication Server Security Group IDs.
replicationConfiguration_replicationServersSecurityGroupsIDs :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe [Prelude.Text])
replicationConfiguration_replicationServersSecurityGroupsIDs = Lens.lens (\ReplicationConfiguration' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@ReplicationConfiguration' {} a -> s {replicationServersSecurityGroupsIDs = a} :: ReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Replication Configuration EBS encryption key ARN.
replicationConfiguration_ebsEncryptionKeyArn :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_ebsEncryptionKeyArn = Lens.lens (\ReplicationConfiguration' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@ReplicationConfiguration' {} a -> s {ebsEncryptionKeyArn = a} :: ReplicationConfiguration)

-- | Replication Configuration name.
replicationConfiguration_name :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_name = Lens.lens (\ReplicationConfiguration' {name} -> name) (\s@ReplicationConfiguration' {} a -> s {name = a} :: ReplicationConfiguration)

-- | Replication Configuration use default large Staging Disks.
replicationConfiguration_defaultLargeStagingDiskType :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType)
replicationConfiguration_defaultLargeStagingDiskType = Lens.lens (\ReplicationConfiguration' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@ReplicationConfiguration' {} a -> s {defaultLargeStagingDiskType = a} :: ReplicationConfiguration)

-- | Replication Configuration set bandwidth throttling.
replicationConfiguration_bandwidthThrottling :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Natural)
replicationConfiguration_bandwidthThrottling = Lens.lens (\ReplicationConfiguration' {bandwidthThrottling} -> bandwidthThrottling) (\s@ReplicationConfiguration' {} a -> s {bandwidthThrottling = a} :: ReplicationConfiguration)

-- | Replication Configuration data plane routing.
replicationConfiguration_dataPlaneRouting :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe ReplicationConfigurationDataPlaneRouting)
replicationConfiguration_dataPlaneRouting = Lens.lens (\ReplicationConfiguration' {dataPlaneRouting} -> dataPlaneRouting) (\s@ReplicationConfiguration' {} a -> s {dataPlaneRouting = a} :: ReplicationConfiguration)

-- | Replication Configuration Source Server ID.
replicationConfiguration_sourceServerID :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_sourceServerID = Lens.lens (\ReplicationConfiguration' {sourceServerID} -> sourceServerID) (\s@ReplicationConfiguration' {} a -> s {sourceServerID = a} :: ReplicationConfiguration)

-- | Replication Configuration replicated disks.
replicationConfiguration_replicatedDisks :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe [ReplicationConfigurationReplicatedDisk])
replicationConfiguration_replicatedDisks = Lens.lens (\ReplicationConfiguration' {replicatedDisks} -> replicatedDisks) (\s@ReplicationConfiguration' {} a -> s {replicatedDisks = a} :: ReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Replication Configuration use Dedicated Replication Server.
replicationConfiguration_useDedicatedReplicationServer :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Bool)
replicationConfiguration_useDedicatedReplicationServer = Lens.lens (\ReplicationConfiguration' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@ReplicationConfiguration' {} a -> s {useDedicatedReplicationServer = a} :: ReplicationConfiguration)

instance Core.FromJSON ReplicationConfiguration where
  parseJSON =
    Core.withObject
      "ReplicationConfiguration"
      ( \x ->
          ReplicationConfiguration'
            Prelude.<$> (x Core..:? "createPublicIP")
            Prelude.<*> ( x Core..:? "stagingAreaTags"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "stagingAreaSubnetId")
            Prelude.<*> (x Core..:? "replicationServerInstanceType")
            Prelude.<*> (x Core..:? "ebsEncryption")
            Prelude.<*> (x Core..:? "associateDefaultSecurityGroup")
            Prelude.<*> ( x Core..:? "replicationServersSecurityGroupsIDs"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ebsEncryptionKeyArn")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "defaultLargeStagingDiskType")
            Prelude.<*> (x Core..:? "bandwidthThrottling")
            Prelude.<*> (x Core..:? "dataPlaneRouting")
            Prelude.<*> (x Core..:? "sourceServerID")
            Prelude.<*> ( x Core..:? "replicatedDisks"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "useDedicatedReplicationServer")
      )

instance Prelude.Hashable ReplicationConfiguration

instance Prelude.NFData ReplicationConfiguration
