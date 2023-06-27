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
-- Module      : Amazonka.DrS.Types.ReplicationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ReplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.PITPolicyRule
import Amazonka.DrS.Types.ReplicationConfigurationDataPlaneRouting
import Amazonka.DrS.Types.ReplicationConfigurationDefaultLargeStagingDiskType
import Amazonka.DrS.Types.ReplicationConfigurationEbsEncryption
import Amazonka.DrS.Types.ReplicationConfigurationReplicatedDisk
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newReplicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { -- | Whether to associate the default Elastic Disaster Recovery Security
    -- group with the Replication Configuration.
    associateDefaultSecurityGroup :: Prelude.Maybe Prelude.Bool,
    -- | Whether to allow the AWS replication agent to automatically replicate
    -- newly added disks.
    autoReplicateNewDisks :: Prelude.Maybe Prelude.Bool,
    -- | Configure bandwidth throttling for the outbound data transfer rate of
    -- the Source Server in Mbps.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | Whether to create a Public IP for the Recovery Instance by default.
    createPublicIP :: Prelude.Maybe Prelude.Bool,
    -- | The data plane routing mechanism that will be used for replication.
    dataPlaneRouting :: Prelude.Maybe ReplicationConfigurationDataPlaneRouting,
    -- | The Staging Disk EBS volume type to be used during replication.
    defaultLargeStagingDiskType :: Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType,
    -- | The type of EBS encryption to be used during replication.
    ebsEncryption :: Prelude.Maybe ReplicationConfigurationEbsEncryption,
    -- | The ARN of the EBS encryption key to be used during replication.
    ebsEncryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Replication Configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Point in time (PIT) policy to manage snapshots taken during
    -- replication.
    pitPolicy :: Prelude.Maybe (Prelude.NonEmpty PITPolicyRule),
    -- | The configuration of the disks of the Source Server to be replicated.
    replicatedDisks :: Prelude.Maybe [ReplicationConfigurationReplicatedDisk],
    -- | The instance type to be used for the replication server.
    replicationServerInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The security group IDs that will be used by the replication server.
    replicationServersSecurityGroupsIDs :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Source Server for this Replication Configuration.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | The subnet to be used by the replication staging area.
    stagingAreaSubnetId :: Prelude.Maybe Prelude.Text,
    -- | A set of tags to be associated with all resources created in the
    -- replication staging area: EC2 replication server, EBS volumes, EBS
    -- snapshots, etc.
    stagingAreaTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Whether to use a dedicated Replication Server in the replication staging
    -- area.
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
-- 'associateDefaultSecurityGroup', 'replicationConfiguration_associateDefaultSecurityGroup' - Whether to associate the default Elastic Disaster Recovery Security
-- group with the Replication Configuration.
--
-- 'autoReplicateNewDisks', 'replicationConfiguration_autoReplicateNewDisks' - Whether to allow the AWS replication agent to automatically replicate
-- newly added disks.
--
-- 'bandwidthThrottling', 'replicationConfiguration_bandwidthThrottling' - Configure bandwidth throttling for the outbound data transfer rate of
-- the Source Server in Mbps.
--
-- 'createPublicIP', 'replicationConfiguration_createPublicIP' - Whether to create a Public IP for the Recovery Instance by default.
--
-- 'dataPlaneRouting', 'replicationConfiguration_dataPlaneRouting' - The data plane routing mechanism that will be used for replication.
--
-- 'defaultLargeStagingDiskType', 'replicationConfiguration_defaultLargeStagingDiskType' - The Staging Disk EBS volume type to be used during replication.
--
-- 'ebsEncryption', 'replicationConfiguration_ebsEncryption' - The type of EBS encryption to be used during replication.
--
-- 'ebsEncryptionKeyArn', 'replicationConfiguration_ebsEncryptionKeyArn' - The ARN of the EBS encryption key to be used during replication.
--
-- 'name', 'replicationConfiguration_name' - The name of the Replication Configuration.
--
-- 'pitPolicy', 'replicationConfiguration_pitPolicy' - The Point in time (PIT) policy to manage snapshots taken during
-- replication.
--
-- 'replicatedDisks', 'replicationConfiguration_replicatedDisks' - The configuration of the disks of the Source Server to be replicated.
--
-- 'replicationServerInstanceType', 'replicationConfiguration_replicationServerInstanceType' - The instance type to be used for the replication server.
--
-- 'replicationServersSecurityGroupsIDs', 'replicationConfiguration_replicationServersSecurityGroupsIDs' - The security group IDs that will be used by the replication server.
--
-- 'sourceServerID', 'replicationConfiguration_sourceServerID' - The ID of the Source Server for this Replication Configuration.
--
-- 'stagingAreaSubnetId', 'replicationConfiguration_stagingAreaSubnetId' - The subnet to be used by the replication staging area.
--
-- 'stagingAreaTags', 'replicationConfiguration_stagingAreaTags' - A set of tags to be associated with all resources created in the
-- replication staging area: EC2 replication server, EBS volumes, EBS
-- snapshots, etc.
--
-- 'useDedicatedReplicationServer', 'replicationConfiguration_useDedicatedReplicationServer' - Whether to use a dedicated Replication Server in the replication staging
-- area.
newReplicationConfiguration ::
  ReplicationConfiguration
newReplicationConfiguration =
  ReplicationConfiguration'
    { associateDefaultSecurityGroup =
        Prelude.Nothing,
      autoReplicateNewDisks = Prelude.Nothing,
      bandwidthThrottling = Prelude.Nothing,
      createPublicIP = Prelude.Nothing,
      dataPlaneRouting = Prelude.Nothing,
      defaultLargeStagingDiskType = Prelude.Nothing,
      ebsEncryption = Prelude.Nothing,
      ebsEncryptionKeyArn = Prelude.Nothing,
      name = Prelude.Nothing,
      pitPolicy = Prelude.Nothing,
      replicatedDisks = Prelude.Nothing,
      replicationServerInstanceType = Prelude.Nothing,
      replicationServersSecurityGroupsIDs =
        Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      stagingAreaSubnetId = Prelude.Nothing,
      stagingAreaTags = Prelude.Nothing,
      useDedicatedReplicationServer = Prelude.Nothing
    }

-- | Whether to associate the default Elastic Disaster Recovery Security
-- group with the Replication Configuration.
replicationConfiguration_associateDefaultSecurityGroup :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Bool)
replicationConfiguration_associateDefaultSecurityGroup = Lens.lens (\ReplicationConfiguration' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@ReplicationConfiguration' {} a -> s {associateDefaultSecurityGroup = a} :: ReplicationConfiguration)

-- | Whether to allow the AWS replication agent to automatically replicate
-- newly added disks.
replicationConfiguration_autoReplicateNewDisks :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Bool)
replicationConfiguration_autoReplicateNewDisks = Lens.lens (\ReplicationConfiguration' {autoReplicateNewDisks} -> autoReplicateNewDisks) (\s@ReplicationConfiguration' {} a -> s {autoReplicateNewDisks = a} :: ReplicationConfiguration)

-- | Configure bandwidth throttling for the outbound data transfer rate of
-- the Source Server in Mbps.
replicationConfiguration_bandwidthThrottling :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Natural)
replicationConfiguration_bandwidthThrottling = Lens.lens (\ReplicationConfiguration' {bandwidthThrottling} -> bandwidthThrottling) (\s@ReplicationConfiguration' {} a -> s {bandwidthThrottling = a} :: ReplicationConfiguration)

-- | Whether to create a Public IP for the Recovery Instance by default.
replicationConfiguration_createPublicIP :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Bool)
replicationConfiguration_createPublicIP = Lens.lens (\ReplicationConfiguration' {createPublicIP} -> createPublicIP) (\s@ReplicationConfiguration' {} a -> s {createPublicIP = a} :: ReplicationConfiguration)

-- | The data plane routing mechanism that will be used for replication.
replicationConfiguration_dataPlaneRouting :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe ReplicationConfigurationDataPlaneRouting)
replicationConfiguration_dataPlaneRouting = Lens.lens (\ReplicationConfiguration' {dataPlaneRouting} -> dataPlaneRouting) (\s@ReplicationConfiguration' {} a -> s {dataPlaneRouting = a} :: ReplicationConfiguration)

-- | The Staging Disk EBS volume type to be used during replication.
replicationConfiguration_defaultLargeStagingDiskType :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType)
replicationConfiguration_defaultLargeStagingDiskType = Lens.lens (\ReplicationConfiguration' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@ReplicationConfiguration' {} a -> s {defaultLargeStagingDiskType = a} :: ReplicationConfiguration)

-- | The type of EBS encryption to be used during replication.
replicationConfiguration_ebsEncryption :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe ReplicationConfigurationEbsEncryption)
replicationConfiguration_ebsEncryption = Lens.lens (\ReplicationConfiguration' {ebsEncryption} -> ebsEncryption) (\s@ReplicationConfiguration' {} a -> s {ebsEncryption = a} :: ReplicationConfiguration)

-- | The ARN of the EBS encryption key to be used during replication.
replicationConfiguration_ebsEncryptionKeyArn :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_ebsEncryptionKeyArn = Lens.lens (\ReplicationConfiguration' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@ReplicationConfiguration' {} a -> s {ebsEncryptionKeyArn = a} :: ReplicationConfiguration)

-- | The name of the Replication Configuration.
replicationConfiguration_name :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_name = Lens.lens (\ReplicationConfiguration' {name} -> name) (\s@ReplicationConfiguration' {} a -> s {name = a} :: ReplicationConfiguration)

-- | The Point in time (PIT) policy to manage snapshots taken during
-- replication.
replicationConfiguration_pitPolicy :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe (Prelude.NonEmpty PITPolicyRule))
replicationConfiguration_pitPolicy = Lens.lens (\ReplicationConfiguration' {pitPolicy} -> pitPolicy) (\s@ReplicationConfiguration' {} a -> s {pitPolicy = a} :: ReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of the disks of the Source Server to be replicated.
replicationConfiguration_replicatedDisks :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe [ReplicationConfigurationReplicatedDisk])
replicationConfiguration_replicatedDisks = Lens.lens (\ReplicationConfiguration' {replicatedDisks} -> replicatedDisks) (\s@ReplicationConfiguration' {} a -> s {replicatedDisks = a} :: ReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The instance type to be used for the replication server.
replicationConfiguration_replicationServerInstanceType :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_replicationServerInstanceType = Lens.lens (\ReplicationConfiguration' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@ReplicationConfiguration' {} a -> s {replicationServerInstanceType = a} :: ReplicationConfiguration)

-- | The security group IDs that will be used by the replication server.
replicationConfiguration_replicationServersSecurityGroupsIDs :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe [Prelude.Text])
replicationConfiguration_replicationServersSecurityGroupsIDs = Lens.lens (\ReplicationConfiguration' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@ReplicationConfiguration' {} a -> s {replicationServersSecurityGroupsIDs = a} :: ReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Source Server for this Replication Configuration.
replicationConfiguration_sourceServerID :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_sourceServerID = Lens.lens (\ReplicationConfiguration' {sourceServerID} -> sourceServerID) (\s@ReplicationConfiguration' {} a -> s {sourceServerID = a} :: ReplicationConfiguration)

-- | The subnet to be used by the replication staging area.
replicationConfiguration_stagingAreaSubnetId :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Text)
replicationConfiguration_stagingAreaSubnetId = Lens.lens (\ReplicationConfiguration' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@ReplicationConfiguration' {} a -> s {stagingAreaSubnetId = a} :: ReplicationConfiguration)

-- | A set of tags to be associated with all resources created in the
-- replication staging area: EC2 replication server, EBS volumes, EBS
-- snapshots, etc.
replicationConfiguration_stagingAreaTags :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
replicationConfiguration_stagingAreaTags = Lens.lens (\ReplicationConfiguration' {stagingAreaTags} -> stagingAreaTags) (\s@ReplicationConfiguration' {} a -> s {stagingAreaTags = a} :: ReplicationConfiguration) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Whether to use a dedicated Replication Server in the replication staging
-- area.
replicationConfiguration_useDedicatedReplicationServer :: Lens.Lens' ReplicationConfiguration (Prelude.Maybe Prelude.Bool)
replicationConfiguration_useDedicatedReplicationServer = Lens.lens (\ReplicationConfiguration' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@ReplicationConfiguration' {} a -> s {useDedicatedReplicationServer = a} :: ReplicationConfiguration)

instance Data.FromJSON ReplicationConfiguration where
  parseJSON =
    Data.withObject
      "ReplicationConfiguration"
      ( \x ->
          ReplicationConfiguration'
            Prelude.<$> (x Data..:? "associateDefaultSecurityGroup")
            Prelude.<*> (x Data..:? "autoReplicateNewDisks")
            Prelude.<*> (x Data..:? "bandwidthThrottling")
            Prelude.<*> (x Data..:? "createPublicIP")
            Prelude.<*> (x Data..:? "dataPlaneRouting")
            Prelude.<*> (x Data..:? "defaultLargeStagingDiskType")
            Prelude.<*> (x Data..:? "ebsEncryption")
            Prelude.<*> (x Data..:? "ebsEncryptionKeyArn")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "pitPolicy")
            Prelude.<*> ( x
                            Data..:? "replicatedDisks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "replicationServerInstanceType")
            Prelude.<*> ( x
                            Data..:? "replicationServersSecurityGroupsIDs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "sourceServerID")
            Prelude.<*> (x Data..:? "stagingAreaSubnetId")
            Prelude.<*> ( x
                            Data..:? "stagingAreaTags"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "useDedicatedReplicationServer")
      )

instance Prelude.Hashable ReplicationConfiguration where
  hashWithSalt _salt ReplicationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` associateDefaultSecurityGroup
      `Prelude.hashWithSalt` autoReplicateNewDisks
      `Prelude.hashWithSalt` bandwidthThrottling
      `Prelude.hashWithSalt` createPublicIP
      `Prelude.hashWithSalt` dataPlaneRouting
      `Prelude.hashWithSalt` defaultLargeStagingDiskType
      `Prelude.hashWithSalt` ebsEncryption
      `Prelude.hashWithSalt` ebsEncryptionKeyArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pitPolicy
      `Prelude.hashWithSalt` replicatedDisks
      `Prelude.hashWithSalt` replicationServerInstanceType
      `Prelude.hashWithSalt` replicationServersSecurityGroupsIDs
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` stagingAreaSubnetId
      `Prelude.hashWithSalt` stagingAreaTags
      `Prelude.hashWithSalt` useDedicatedReplicationServer

instance Prelude.NFData ReplicationConfiguration where
  rnf ReplicationConfiguration' {..} =
    Prelude.rnf associateDefaultSecurityGroup
      `Prelude.seq` Prelude.rnf autoReplicateNewDisks
      `Prelude.seq` Prelude.rnf bandwidthThrottling
      `Prelude.seq` Prelude.rnf createPublicIP
      `Prelude.seq` Prelude.rnf dataPlaneRouting
      `Prelude.seq` Prelude.rnf defaultLargeStagingDiskType
      `Prelude.seq` Prelude.rnf ebsEncryption
      `Prelude.seq` Prelude.rnf ebsEncryptionKeyArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pitPolicy
      `Prelude.seq` Prelude.rnf replicatedDisks
      `Prelude.seq` Prelude.rnf replicationServerInstanceType
      `Prelude.seq` Prelude.rnf
        replicationServersSecurityGroupsIDs
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf stagingAreaSubnetId
      `Prelude.seq` Prelude.rnf stagingAreaTags
      `Prelude.seq` Prelude.rnf
        useDedicatedReplicationServer
