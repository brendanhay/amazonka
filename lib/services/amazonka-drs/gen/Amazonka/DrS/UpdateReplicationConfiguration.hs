{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DrS.UpdateReplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to update a ReplicationConfiguration by Source Server ID.
module Amazonka.DrS.UpdateReplicationConfiguration
  ( -- * Creating a Request
    UpdateReplicationConfiguration (..),
    newUpdateReplicationConfiguration,

    -- * Request Lenses
    updateReplicationConfiguration_bandwidthThrottling,
    updateReplicationConfiguration_name,
    updateReplicationConfiguration_replicationServerInstanceType,
    updateReplicationConfiguration_stagingAreaTags,
    updateReplicationConfiguration_associateDefaultSecurityGroup,
    updateReplicationConfiguration_defaultLargeStagingDiskType,
    updateReplicationConfiguration_stagingAreaSubnetId,
    updateReplicationConfiguration_createPublicIP,
    updateReplicationConfiguration_dataPlaneRouting,
    updateReplicationConfiguration_ebsEncryption,
    updateReplicationConfiguration_replicatedDisks,
    updateReplicationConfiguration_pitPolicy,
    updateReplicationConfiguration_useDedicatedReplicationServer,
    updateReplicationConfiguration_replicationServersSecurityGroupsIDs,
    updateReplicationConfiguration_ebsEncryptionKeyArn,
    updateReplicationConfiguration_sourceServerID,

    -- * Destructuring the Response
    ReplicationConfiguration (..),
    newReplicationConfiguration,

    -- * Response Lenses
    replicationConfiguration_bandwidthThrottling,
    replicationConfiguration_name,
    replicationConfiguration_replicationServerInstanceType,
    replicationConfiguration_stagingAreaTags,
    replicationConfiguration_associateDefaultSecurityGroup,
    replicationConfiguration_defaultLargeStagingDiskType,
    replicationConfiguration_stagingAreaSubnetId,
    replicationConfiguration_createPublicIP,
    replicationConfiguration_dataPlaneRouting,
    replicationConfiguration_ebsEncryption,
    replicationConfiguration_replicatedDisks,
    replicationConfiguration_sourceServerID,
    replicationConfiguration_pitPolicy,
    replicationConfiguration_useDedicatedReplicationServer,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_ebsEncryptionKeyArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateReplicationConfiguration' smart constructor.
data UpdateReplicationConfiguration = UpdateReplicationConfiguration'
  { -- | Configure bandwidth throttling for the outbound data transfer rate of
    -- the Source Server in Mbps.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Replication Configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The instance type to be used for the replication server.
    replicationServerInstanceType :: Prelude.Maybe Prelude.Text,
    -- | A set of tags to be associated with all resources created in the
    -- replication staging area: EC2 replication server, EBS volumes, EBS
    -- snapshots, etc.
    stagingAreaTags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Whether to associate the default Elastic Disaster Recovery Security
    -- group with the Replication Configuration.
    associateDefaultSecurityGroup :: Prelude.Maybe Prelude.Bool,
    -- | The Staging Disk EBS volume type to be used during replication.
    defaultLargeStagingDiskType :: Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType,
    -- | The subnet to be used by the replication staging area.
    stagingAreaSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Whether to create a Public IP for the Recovery Instance by default.
    createPublicIP :: Prelude.Maybe Prelude.Bool,
    -- | The data plane routing mechanism that will be used for replication.
    dataPlaneRouting :: Prelude.Maybe ReplicationConfigurationDataPlaneRouting,
    -- | The type of EBS encryption to be used during replication.
    ebsEncryption :: Prelude.Maybe ReplicationConfigurationEbsEncryption,
    -- | The configuration of the disks of the Source Server to be replicated.
    replicatedDisks :: Prelude.Maybe [ReplicationConfigurationReplicatedDisk],
    -- | The Point in time (PIT) policy to manage snapshots taken during
    -- replication.
    pitPolicy :: Prelude.Maybe (Prelude.NonEmpty PITPolicyRule),
    -- | Whether to use a dedicated Replication Server in the replication staging
    -- area.
    useDedicatedReplicationServer :: Prelude.Maybe Prelude.Bool,
    -- | The security group IDs that will be used by the replication server.
    replicationServersSecurityGroupsIDs :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the EBS encryption key to be used during replication.
    ebsEncryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Source Server for this Replication Configuration.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidthThrottling', 'updateReplicationConfiguration_bandwidthThrottling' - Configure bandwidth throttling for the outbound data transfer rate of
-- the Source Server in Mbps.
--
-- 'name', 'updateReplicationConfiguration_name' - The name of the Replication Configuration.
--
-- 'replicationServerInstanceType', 'updateReplicationConfiguration_replicationServerInstanceType' - The instance type to be used for the replication server.
--
-- 'stagingAreaTags', 'updateReplicationConfiguration_stagingAreaTags' - A set of tags to be associated with all resources created in the
-- replication staging area: EC2 replication server, EBS volumes, EBS
-- snapshots, etc.
--
-- 'associateDefaultSecurityGroup', 'updateReplicationConfiguration_associateDefaultSecurityGroup' - Whether to associate the default Elastic Disaster Recovery Security
-- group with the Replication Configuration.
--
-- 'defaultLargeStagingDiskType', 'updateReplicationConfiguration_defaultLargeStagingDiskType' - The Staging Disk EBS volume type to be used during replication.
--
-- 'stagingAreaSubnetId', 'updateReplicationConfiguration_stagingAreaSubnetId' - The subnet to be used by the replication staging area.
--
-- 'createPublicIP', 'updateReplicationConfiguration_createPublicIP' - Whether to create a Public IP for the Recovery Instance by default.
--
-- 'dataPlaneRouting', 'updateReplicationConfiguration_dataPlaneRouting' - The data plane routing mechanism that will be used for replication.
--
-- 'ebsEncryption', 'updateReplicationConfiguration_ebsEncryption' - The type of EBS encryption to be used during replication.
--
-- 'replicatedDisks', 'updateReplicationConfiguration_replicatedDisks' - The configuration of the disks of the Source Server to be replicated.
--
-- 'pitPolicy', 'updateReplicationConfiguration_pitPolicy' - The Point in time (PIT) policy to manage snapshots taken during
-- replication.
--
-- 'useDedicatedReplicationServer', 'updateReplicationConfiguration_useDedicatedReplicationServer' - Whether to use a dedicated Replication Server in the replication staging
-- area.
--
-- 'replicationServersSecurityGroupsIDs', 'updateReplicationConfiguration_replicationServersSecurityGroupsIDs' - The security group IDs that will be used by the replication server.
--
-- 'ebsEncryptionKeyArn', 'updateReplicationConfiguration_ebsEncryptionKeyArn' - The ARN of the EBS encryption key to be used during replication.
--
-- 'sourceServerID', 'updateReplicationConfiguration_sourceServerID' - The ID of the Source Server for this Replication Configuration.
newUpdateReplicationConfiguration ::
  -- | 'sourceServerID'
  Prelude.Text ->
  UpdateReplicationConfiguration
newUpdateReplicationConfiguration pSourceServerID_ =
  UpdateReplicationConfiguration'
    { bandwidthThrottling =
        Prelude.Nothing,
      name = Prelude.Nothing,
      replicationServerInstanceType =
        Prelude.Nothing,
      stagingAreaTags = Prelude.Nothing,
      associateDefaultSecurityGroup =
        Prelude.Nothing,
      defaultLargeStagingDiskType =
        Prelude.Nothing,
      stagingAreaSubnetId = Prelude.Nothing,
      createPublicIP = Prelude.Nothing,
      dataPlaneRouting = Prelude.Nothing,
      ebsEncryption = Prelude.Nothing,
      replicatedDisks = Prelude.Nothing,
      pitPolicy = Prelude.Nothing,
      useDedicatedReplicationServer =
        Prelude.Nothing,
      replicationServersSecurityGroupsIDs =
        Prelude.Nothing,
      ebsEncryptionKeyArn = Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | Configure bandwidth throttling for the outbound data transfer rate of
-- the Source Server in Mbps.
updateReplicationConfiguration_bandwidthThrottling :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Natural)
updateReplicationConfiguration_bandwidthThrottling = Lens.lens (\UpdateReplicationConfiguration' {bandwidthThrottling} -> bandwidthThrottling) (\s@UpdateReplicationConfiguration' {} a -> s {bandwidthThrottling = a} :: UpdateReplicationConfiguration)

-- | The name of the Replication Configuration.
updateReplicationConfiguration_name :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateReplicationConfiguration_name = Lens.lens (\UpdateReplicationConfiguration' {name} -> name) (\s@UpdateReplicationConfiguration' {} a -> s {name = a} :: UpdateReplicationConfiguration)

-- | The instance type to be used for the replication server.
updateReplicationConfiguration_replicationServerInstanceType :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateReplicationConfiguration_replicationServerInstanceType = Lens.lens (\UpdateReplicationConfiguration' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@UpdateReplicationConfiguration' {} a -> s {replicationServerInstanceType = a} :: UpdateReplicationConfiguration)

-- | A set of tags to be associated with all resources created in the
-- replication staging area: EC2 replication server, EBS volumes, EBS
-- snapshots, etc.
updateReplicationConfiguration_stagingAreaTags :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateReplicationConfiguration_stagingAreaTags = Lens.lens (\UpdateReplicationConfiguration' {stagingAreaTags} -> stagingAreaTags) (\s@UpdateReplicationConfiguration' {} a -> s {stagingAreaTags = a} :: UpdateReplicationConfiguration) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Whether to associate the default Elastic Disaster Recovery Security
-- group with the Replication Configuration.
updateReplicationConfiguration_associateDefaultSecurityGroup :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Bool)
updateReplicationConfiguration_associateDefaultSecurityGroup = Lens.lens (\UpdateReplicationConfiguration' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@UpdateReplicationConfiguration' {} a -> s {associateDefaultSecurityGroup = a} :: UpdateReplicationConfiguration)

-- | The Staging Disk EBS volume type to be used during replication.
updateReplicationConfiguration_defaultLargeStagingDiskType :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType)
updateReplicationConfiguration_defaultLargeStagingDiskType = Lens.lens (\UpdateReplicationConfiguration' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@UpdateReplicationConfiguration' {} a -> s {defaultLargeStagingDiskType = a} :: UpdateReplicationConfiguration)

-- | The subnet to be used by the replication staging area.
updateReplicationConfiguration_stagingAreaSubnetId :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateReplicationConfiguration_stagingAreaSubnetId = Lens.lens (\UpdateReplicationConfiguration' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@UpdateReplicationConfiguration' {} a -> s {stagingAreaSubnetId = a} :: UpdateReplicationConfiguration)

-- | Whether to create a Public IP for the Recovery Instance by default.
updateReplicationConfiguration_createPublicIP :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Bool)
updateReplicationConfiguration_createPublicIP = Lens.lens (\UpdateReplicationConfiguration' {createPublicIP} -> createPublicIP) (\s@UpdateReplicationConfiguration' {} a -> s {createPublicIP = a} :: UpdateReplicationConfiguration)

-- | The data plane routing mechanism that will be used for replication.
updateReplicationConfiguration_dataPlaneRouting :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe ReplicationConfigurationDataPlaneRouting)
updateReplicationConfiguration_dataPlaneRouting = Lens.lens (\UpdateReplicationConfiguration' {dataPlaneRouting} -> dataPlaneRouting) (\s@UpdateReplicationConfiguration' {} a -> s {dataPlaneRouting = a} :: UpdateReplicationConfiguration)

-- | The type of EBS encryption to be used during replication.
updateReplicationConfiguration_ebsEncryption :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe ReplicationConfigurationEbsEncryption)
updateReplicationConfiguration_ebsEncryption = Lens.lens (\UpdateReplicationConfiguration' {ebsEncryption} -> ebsEncryption) (\s@UpdateReplicationConfiguration' {} a -> s {ebsEncryption = a} :: UpdateReplicationConfiguration)

-- | The configuration of the disks of the Source Server to be replicated.
updateReplicationConfiguration_replicatedDisks :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe [ReplicationConfigurationReplicatedDisk])
updateReplicationConfiguration_replicatedDisks = Lens.lens (\UpdateReplicationConfiguration' {replicatedDisks} -> replicatedDisks) (\s@UpdateReplicationConfiguration' {} a -> s {replicatedDisks = a} :: UpdateReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Point in time (PIT) policy to manage snapshots taken during
-- replication.
updateReplicationConfiguration_pitPolicy :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe (Prelude.NonEmpty PITPolicyRule))
updateReplicationConfiguration_pitPolicy = Lens.lens (\UpdateReplicationConfiguration' {pitPolicy} -> pitPolicy) (\s@UpdateReplicationConfiguration' {} a -> s {pitPolicy = a} :: UpdateReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Whether to use a dedicated Replication Server in the replication staging
-- area.
updateReplicationConfiguration_useDedicatedReplicationServer :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Bool)
updateReplicationConfiguration_useDedicatedReplicationServer = Lens.lens (\UpdateReplicationConfiguration' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@UpdateReplicationConfiguration' {} a -> s {useDedicatedReplicationServer = a} :: UpdateReplicationConfiguration)

-- | The security group IDs that will be used by the replication server.
updateReplicationConfiguration_replicationServersSecurityGroupsIDs :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe [Prelude.Text])
updateReplicationConfiguration_replicationServersSecurityGroupsIDs = Lens.lens (\UpdateReplicationConfiguration' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@UpdateReplicationConfiguration' {} a -> s {replicationServersSecurityGroupsIDs = a} :: UpdateReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the EBS encryption key to be used during replication.
updateReplicationConfiguration_ebsEncryptionKeyArn :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateReplicationConfiguration_ebsEncryptionKeyArn = Lens.lens (\UpdateReplicationConfiguration' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@UpdateReplicationConfiguration' {} a -> s {ebsEncryptionKeyArn = a} :: UpdateReplicationConfiguration)

-- | The ID of the Source Server for this Replication Configuration.
updateReplicationConfiguration_sourceServerID :: Lens.Lens' UpdateReplicationConfiguration Prelude.Text
updateReplicationConfiguration_sourceServerID = Lens.lens (\UpdateReplicationConfiguration' {sourceServerID} -> sourceServerID) (\s@UpdateReplicationConfiguration' {} a -> s {sourceServerID = a} :: UpdateReplicationConfiguration)

instance
  Core.AWSRequest
    UpdateReplicationConfiguration
  where
  type
    AWSResponse UpdateReplicationConfiguration =
      ReplicationConfiguration
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    UpdateReplicationConfiguration
  where
  hashWithSalt
    _salt
    UpdateReplicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` bandwidthThrottling
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` replicationServerInstanceType
        `Prelude.hashWithSalt` stagingAreaTags
        `Prelude.hashWithSalt` associateDefaultSecurityGroup
        `Prelude.hashWithSalt` defaultLargeStagingDiskType
        `Prelude.hashWithSalt` stagingAreaSubnetId
        `Prelude.hashWithSalt` createPublicIP
        `Prelude.hashWithSalt` dataPlaneRouting
        `Prelude.hashWithSalt` ebsEncryption
        `Prelude.hashWithSalt` replicatedDisks
        `Prelude.hashWithSalt` pitPolicy
        `Prelude.hashWithSalt` useDedicatedReplicationServer
        `Prelude.hashWithSalt` replicationServersSecurityGroupsIDs
        `Prelude.hashWithSalt` ebsEncryptionKeyArn
        `Prelude.hashWithSalt` sourceServerID

instance
  Prelude.NFData
    UpdateReplicationConfiguration
  where
  rnf UpdateReplicationConfiguration' {..} =
    Prelude.rnf bandwidthThrottling
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf replicationServerInstanceType
      `Prelude.seq` Prelude.rnf stagingAreaTags
      `Prelude.seq` Prelude.rnf associateDefaultSecurityGroup
      `Prelude.seq` Prelude.rnf defaultLargeStagingDiskType
      `Prelude.seq` Prelude.rnf stagingAreaSubnetId
      `Prelude.seq` Prelude.rnf createPublicIP
      `Prelude.seq` Prelude.rnf dataPlaneRouting
      `Prelude.seq` Prelude.rnf ebsEncryption
      `Prelude.seq` Prelude.rnf replicatedDisks
      `Prelude.seq` Prelude.rnf pitPolicy
      `Prelude.seq` Prelude.rnf useDedicatedReplicationServer
      `Prelude.seq` Prelude.rnf
        replicationServersSecurityGroupsIDs
      `Prelude.seq` Prelude.rnf ebsEncryptionKeyArn
      `Prelude.seq` Prelude.rnf sourceServerID

instance
  Core.ToHeaders
    UpdateReplicationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateReplicationConfiguration where
  toJSON UpdateReplicationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bandwidthThrottling" Core..=)
              Prelude.<$> bandwidthThrottling,
            ("name" Core..=) Prelude.<$> name,
            ("replicationServerInstanceType" Core..=)
              Prelude.<$> replicationServerInstanceType,
            ("stagingAreaTags" Core..=)
              Prelude.<$> stagingAreaTags,
            ("associateDefaultSecurityGroup" Core..=)
              Prelude.<$> associateDefaultSecurityGroup,
            ("defaultLargeStagingDiskType" Core..=)
              Prelude.<$> defaultLargeStagingDiskType,
            ("stagingAreaSubnetId" Core..=)
              Prelude.<$> stagingAreaSubnetId,
            ("createPublicIP" Core..=)
              Prelude.<$> createPublicIP,
            ("dataPlaneRouting" Core..=)
              Prelude.<$> dataPlaneRouting,
            ("ebsEncryption" Core..=) Prelude.<$> ebsEncryption,
            ("replicatedDisks" Core..=)
              Prelude.<$> replicatedDisks,
            ("pitPolicy" Core..=) Prelude.<$> pitPolicy,
            ("useDedicatedReplicationServer" Core..=)
              Prelude.<$> useDedicatedReplicationServer,
            ("replicationServersSecurityGroupsIDs" Core..=)
              Prelude.<$> replicationServersSecurityGroupsIDs,
            ("ebsEncryptionKeyArn" Core..=)
              Prelude.<$> ebsEncryptionKeyArn,
            Prelude.Just
              ("sourceServerID" Core..= sourceServerID)
          ]
      )

instance Core.ToPath UpdateReplicationConfiguration where
  toPath =
    Prelude.const "/UpdateReplicationConfiguration"

instance Core.ToQuery UpdateReplicationConfiguration where
  toQuery = Prelude.const Prelude.mempty
