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
-- Module      : Amazonka.MGN.UpdateReplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to update multiple ReplicationConfigurations by Source Server
-- ID.
module Amazonka.MGN.UpdateReplicationConfiguration
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
    replicationConfiguration_useDedicatedReplicationServer,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_ebsEncryptionKeyArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateReplicationConfiguration' smart constructor.
data UpdateReplicationConfiguration = UpdateReplicationConfiguration'
  { -- | Update replication configuration bandwidth throttling request.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | Update replication configuration name request.
    name :: Prelude.Maybe Prelude.Text,
    -- | Update replication configuration Replication Server instance type
    -- request.
    replicationServerInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Update replication configuration Staging Area Tags request.
    stagingAreaTags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Update replication configuration associate default Application Migration
    -- Service Security group request.
    associateDefaultSecurityGroup :: Prelude.Maybe Prelude.Bool,
    -- | Update replication configuration use default large Staging Disk type
    -- request.
    defaultLargeStagingDiskType :: Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType,
    -- | Update replication configuration Staging Area subnet request.
    stagingAreaSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Update replication configuration create Public IP request.
    createPublicIP :: Prelude.Maybe Prelude.Bool,
    -- | Update replication configuration data plane routing request.
    dataPlaneRouting :: Prelude.Maybe ReplicationConfigurationDataPlaneRouting,
    -- | Update replication configuration EBS encryption request.
    ebsEncryption :: Prelude.Maybe ReplicationConfigurationEbsEncryption,
    -- | Update replication configuration replicated disks request.
    replicatedDisks :: Prelude.Maybe [ReplicationConfigurationReplicatedDisk],
    -- | Update replication configuration use dedicated Replication Server
    -- request.
    useDedicatedReplicationServer :: Prelude.Maybe Prelude.Bool,
    -- | Update replication configuration Replication Server Security Groups IDs
    -- request.
    replicationServersSecurityGroupsIDs :: Prelude.Maybe [Prelude.Text],
    -- | Update replication configuration EBS encryption key ARN request.
    ebsEncryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Update replication configuration Source Server ID request.
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
-- 'bandwidthThrottling', 'updateReplicationConfiguration_bandwidthThrottling' - Update replication configuration bandwidth throttling request.
--
-- 'name', 'updateReplicationConfiguration_name' - Update replication configuration name request.
--
-- 'replicationServerInstanceType', 'updateReplicationConfiguration_replicationServerInstanceType' - Update replication configuration Replication Server instance type
-- request.
--
-- 'stagingAreaTags', 'updateReplicationConfiguration_stagingAreaTags' - Update replication configuration Staging Area Tags request.
--
-- 'associateDefaultSecurityGroup', 'updateReplicationConfiguration_associateDefaultSecurityGroup' - Update replication configuration associate default Application Migration
-- Service Security group request.
--
-- 'defaultLargeStagingDiskType', 'updateReplicationConfiguration_defaultLargeStagingDiskType' - Update replication configuration use default large Staging Disk type
-- request.
--
-- 'stagingAreaSubnetId', 'updateReplicationConfiguration_stagingAreaSubnetId' - Update replication configuration Staging Area subnet request.
--
-- 'createPublicIP', 'updateReplicationConfiguration_createPublicIP' - Update replication configuration create Public IP request.
--
-- 'dataPlaneRouting', 'updateReplicationConfiguration_dataPlaneRouting' - Update replication configuration data plane routing request.
--
-- 'ebsEncryption', 'updateReplicationConfiguration_ebsEncryption' - Update replication configuration EBS encryption request.
--
-- 'replicatedDisks', 'updateReplicationConfiguration_replicatedDisks' - Update replication configuration replicated disks request.
--
-- 'useDedicatedReplicationServer', 'updateReplicationConfiguration_useDedicatedReplicationServer' - Update replication configuration use dedicated Replication Server
-- request.
--
-- 'replicationServersSecurityGroupsIDs', 'updateReplicationConfiguration_replicationServersSecurityGroupsIDs' - Update replication configuration Replication Server Security Groups IDs
-- request.
--
-- 'ebsEncryptionKeyArn', 'updateReplicationConfiguration_ebsEncryptionKeyArn' - Update replication configuration EBS encryption key ARN request.
--
-- 'sourceServerID', 'updateReplicationConfiguration_sourceServerID' - Update replication configuration Source Server ID request.
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
      useDedicatedReplicationServer =
        Prelude.Nothing,
      replicationServersSecurityGroupsIDs =
        Prelude.Nothing,
      ebsEncryptionKeyArn = Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | Update replication configuration bandwidth throttling request.
updateReplicationConfiguration_bandwidthThrottling :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Natural)
updateReplicationConfiguration_bandwidthThrottling = Lens.lens (\UpdateReplicationConfiguration' {bandwidthThrottling} -> bandwidthThrottling) (\s@UpdateReplicationConfiguration' {} a -> s {bandwidthThrottling = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration name request.
updateReplicationConfiguration_name :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateReplicationConfiguration_name = Lens.lens (\UpdateReplicationConfiguration' {name} -> name) (\s@UpdateReplicationConfiguration' {} a -> s {name = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration Replication Server instance type
-- request.
updateReplicationConfiguration_replicationServerInstanceType :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateReplicationConfiguration_replicationServerInstanceType = Lens.lens (\UpdateReplicationConfiguration' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@UpdateReplicationConfiguration' {} a -> s {replicationServerInstanceType = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration Staging Area Tags request.
updateReplicationConfiguration_stagingAreaTags :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateReplicationConfiguration_stagingAreaTags = Lens.lens (\UpdateReplicationConfiguration' {stagingAreaTags} -> stagingAreaTags) (\s@UpdateReplicationConfiguration' {} a -> s {stagingAreaTags = a} :: UpdateReplicationConfiguration) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Update replication configuration associate default Application Migration
-- Service Security group request.
updateReplicationConfiguration_associateDefaultSecurityGroup :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Bool)
updateReplicationConfiguration_associateDefaultSecurityGroup = Lens.lens (\UpdateReplicationConfiguration' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@UpdateReplicationConfiguration' {} a -> s {associateDefaultSecurityGroup = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration use default large Staging Disk type
-- request.
updateReplicationConfiguration_defaultLargeStagingDiskType :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType)
updateReplicationConfiguration_defaultLargeStagingDiskType = Lens.lens (\UpdateReplicationConfiguration' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@UpdateReplicationConfiguration' {} a -> s {defaultLargeStagingDiskType = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration Staging Area subnet request.
updateReplicationConfiguration_stagingAreaSubnetId :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateReplicationConfiguration_stagingAreaSubnetId = Lens.lens (\UpdateReplicationConfiguration' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@UpdateReplicationConfiguration' {} a -> s {stagingAreaSubnetId = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration create Public IP request.
updateReplicationConfiguration_createPublicIP :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Bool)
updateReplicationConfiguration_createPublicIP = Lens.lens (\UpdateReplicationConfiguration' {createPublicIP} -> createPublicIP) (\s@UpdateReplicationConfiguration' {} a -> s {createPublicIP = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration data plane routing request.
updateReplicationConfiguration_dataPlaneRouting :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe ReplicationConfigurationDataPlaneRouting)
updateReplicationConfiguration_dataPlaneRouting = Lens.lens (\UpdateReplicationConfiguration' {dataPlaneRouting} -> dataPlaneRouting) (\s@UpdateReplicationConfiguration' {} a -> s {dataPlaneRouting = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration EBS encryption request.
updateReplicationConfiguration_ebsEncryption :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe ReplicationConfigurationEbsEncryption)
updateReplicationConfiguration_ebsEncryption = Lens.lens (\UpdateReplicationConfiguration' {ebsEncryption} -> ebsEncryption) (\s@UpdateReplicationConfiguration' {} a -> s {ebsEncryption = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration replicated disks request.
updateReplicationConfiguration_replicatedDisks :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe [ReplicationConfigurationReplicatedDisk])
updateReplicationConfiguration_replicatedDisks = Lens.lens (\UpdateReplicationConfiguration' {replicatedDisks} -> replicatedDisks) (\s@UpdateReplicationConfiguration' {} a -> s {replicatedDisks = a} :: UpdateReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Update replication configuration use dedicated Replication Server
-- request.
updateReplicationConfiguration_useDedicatedReplicationServer :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Bool)
updateReplicationConfiguration_useDedicatedReplicationServer = Lens.lens (\UpdateReplicationConfiguration' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@UpdateReplicationConfiguration' {} a -> s {useDedicatedReplicationServer = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration Replication Server Security Groups IDs
-- request.
updateReplicationConfiguration_replicationServersSecurityGroupsIDs :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe [Prelude.Text])
updateReplicationConfiguration_replicationServersSecurityGroupsIDs = Lens.lens (\UpdateReplicationConfiguration' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@UpdateReplicationConfiguration' {} a -> s {replicationServersSecurityGroupsIDs = a} :: UpdateReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Update replication configuration EBS encryption key ARN request.
updateReplicationConfiguration_ebsEncryptionKeyArn :: Lens.Lens' UpdateReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateReplicationConfiguration_ebsEncryptionKeyArn = Lens.lens (\UpdateReplicationConfiguration' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@UpdateReplicationConfiguration' {} a -> s {ebsEncryptionKeyArn = a} :: UpdateReplicationConfiguration)

-- | Update replication configuration Source Server ID request.
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
