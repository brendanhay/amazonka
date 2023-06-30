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
-- Module      : Amazonka.DrS.UpdateReplicationConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a ReplicationConfigurationTemplate by ID.
module Amazonka.DrS.UpdateReplicationConfigurationTemplate
  ( -- * Creating a Request
    UpdateReplicationConfigurationTemplate (..),
    newUpdateReplicationConfigurationTemplate,

    -- * Request Lenses
    updateReplicationConfigurationTemplate_arn,
    updateReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    updateReplicationConfigurationTemplate_bandwidthThrottling,
    updateReplicationConfigurationTemplate_createPublicIP,
    updateReplicationConfigurationTemplate_dataPlaneRouting,
    updateReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    updateReplicationConfigurationTemplate_ebsEncryption,
    updateReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    updateReplicationConfigurationTemplate_pitPolicy,
    updateReplicationConfigurationTemplate_replicationServerInstanceType,
    updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    updateReplicationConfigurationTemplate_stagingAreaSubnetId,
    updateReplicationConfigurationTemplate_stagingAreaTags,
    updateReplicationConfigurationTemplate_useDedicatedReplicationServer,
    updateReplicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- * Destructuring the Response
    ReplicationConfigurationTemplate (..),
    newReplicationConfigurationTemplate,

    -- * Response Lenses
    replicationConfigurationTemplate_arn,
    replicationConfigurationTemplate_associateDefaultSecurityGroup,
    replicationConfigurationTemplate_bandwidthThrottling,
    replicationConfigurationTemplate_createPublicIP,
    replicationConfigurationTemplate_dataPlaneRouting,
    replicationConfigurationTemplate_defaultLargeStagingDiskType,
    replicationConfigurationTemplate_ebsEncryption,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_pitPolicy,
    replicationConfigurationTemplate_replicationServerInstanceType,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_stagingAreaSubnetId,
    replicationConfigurationTemplate_stagingAreaTags,
    replicationConfigurationTemplate_tags,
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateReplicationConfigurationTemplate' smart constructor.
data UpdateReplicationConfigurationTemplate = UpdateReplicationConfigurationTemplate'
  { -- | The Replication Configuration Template ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Whether to associate the default Elastic Disaster Recovery Security
    -- group with the Replication Configuration Template.
    associateDefaultSecurityGroup :: Prelude.Maybe Prelude.Bool,
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
    -- | The Point in time (PIT) policy to manage snapshots taken during
    -- replication.
    pitPolicy :: Prelude.Maybe (Prelude.NonEmpty PITPolicyRule),
    -- | The instance type to be used for the replication server.
    replicationServerInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The security group IDs that will be used by the replication server.
    replicationServersSecurityGroupsIDs :: Prelude.Maybe [Prelude.Text],
    -- | The subnet to be used by the replication staging area.
    stagingAreaSubnetId :: Prelude.Maybe Prelude.Text,
    -- | A set of tags to be associated with all resources created in the
    -- replication staging area: EC2 replication server, EBS volumes, EBS
    -- snapshots, etc.
    stagingAreaTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Whether to use a dedicated Replication Server in the replication staging
    -- area.
    useDedicatedReplicationServer :: Prelude.Maybe Prelude.Bool,
    -- | The Replication Configuration Template ID.
    replicationConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReplicationConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateReplicationConfigurationTemplate_arn' - The Replication Configuration Template ARN.
--
-- 'associateDefaultSecurityGroup', 'updateReplicationConfigurationTemplate_associateDefaultSecurityGroup' - Whether to associate the default Elastic Disaster Recovery Security
-- group with the Replication Configuration Template.
--
-- 'bandwidthThrottling', 'updateReplicationConfigurationTemplate_bandwidthThrottling' - Configure bandwidth throttling for the outbound data transfer rate of
-- the Source Server in Mbps.
--
-- 'createPublicIP', 'updateReplicationConfigurationTemplate_createPublicIP' - Whether to create a Public IP for the Recovery Instance by default.
--
-- 'dataPlaneRouting', 'updateReplicationConfigurationTemplate_dataPlaneRouting' - The data plane routing mechanism that will be used for replication.
--
-- 'defaultLargeStagingDiskType', 'updateReplicationConfigurationTemplate_defaultLargeStagingDiskType' - The Staging Disk EBS volume type to be used during replication.
--
-- 'ebsEncryption', 'updateReplicationConfigurationTemplate_ebsEncryption' - The type of EBS encryption to be used during replication.
--
-- 'ebsEncryptionKeyArn', 'updateReplicationConfigurationTemplate_ebsEncryptionKeyArn' - The ARN of the EBS encryption key to be used during replication.
--
-- 'pitPolicy', 'updateReplicationConfigurationTemplate_pitPolicy' - The Point in time (PIT) policy to manage snapshots taken during
-- replication.
--
-- 'replicationServerInstanceType', 'updateReplicationConfigurationTemplate_replicationServerInstanceType' - The instance type to be used for the replication server.
--
-- 'replicationServersSecurityGroupsIDs', 'updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs' - The security group IDs that will be used by the replication server.
--
-- 'stagingAreaSubnetId', 'updateReplicationConfigurationTemplate_stagingAreaSubnetId' - The subnet to be used by the replication staging area.
--
-- 'stagingAreaTags', 'updateReplicationConfigurationTemplate_stagingAreaTags' - A set of tags to be associated with all resources created in the
-- replication staging area: EC2 replication server, EBS volumes, EBS
-- snapshots, etc.
--
-- 'useDedicatedReplicationServer', 'updateReplicationConfigurationTemplate_useDedicatedReplicationServer' - Whether to use a dedicated Replication Server in the replication staging
-- area.
--
-- 'replicationConfigurationTemplateID', 'updateReplicationConfigurationTemplate_replicationConfigurationTemplateID' - The Replication Configuration Template ID.
newUpdateReplicationConfigurationTemplate ::
  -- | 'replicationConfigurationTemplateID'
  Prelude.Text ->
  UpdateReplicationConfigurationTemplate
newUpdateReplicationConfigurationTemplate
  pReplicationConfigurationTemplateID_ =
    UpdateReplicationConfigurationTemplate'
      { arn =
          Prelude.Nothing,
        associateDefaultSecurityGroup =
          Prelude.Nothing,
        bandwidthThrottling =
          Prelude.Nothing,
        createPublicIP = Prelude.Nothing,
        dataPlaneRouting = Prelude.Nothing,
        defaultLargeStagingDiskType =
          Prelude.Nothing,
        ebsEncryption = Prelude.Nothing,
        ebsEncryptionKeyArn =
          Prelude.Nothing,
        pitPolicy = Prelude.Nothing,
        replicationServerInstanceType =
          Prelude.Nothing,
        replicationServersSecurityGroupsIDs =
          Prelude.Nothing,
        stagingAreaSubnetId =
          Prelude.Nothing,
        stagingAreaTags = Prelude.Nothing,
        useDedicatedReplicationServer =
          Prelude.Nothing,
        replicationConfigurationTemplateID =
          pReplicationConfigurationTemplateID_
      }

-- | The Replication Configuration Template ARN.
updateReplicationConfigurationTemplate_arn :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateReplicationConfigurationTemplate_arn = Lens.lens (\UpdateReplicationConfigurationTemplate' {arn} -> arn) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {arn = a} :: UpdateReplicationConfigurationTemplate)

-- | Whether to associate the default Elastic Disaster Recovery Security
-- group with the Replication Configuration Template.
updateReplicationConfigurationTemplate_associateDefaultSecurityGroup :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateReplicationConfigurationTemplate_associateDefaultSecurityGroup = Lens.lens (\UpdateReplicationConfigurationTemplate' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {associateDefaultSecurityGroup = a} :: UpdateReplicationConfigurationTemplate)

-- | Configure bandwidth throttling for the outbound data transfer rate of
-- the Source Server in Mbps.
updateReplicationConfigurationTemplate_bandwidthThrottling :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Natural)
updateReplicationConfigurationTemplate_bandwidthThrottling = Lens.lens (\UpdateReplicationConfigurationTemplate' {bandwidthThrottling} -> bandwidthThrottling) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {bandwidthThrottling = a} :: UpdateReplicationConfigurationTemplate)

-- | Whether to create a Public IP for the Recovery Instance by default.
updateReplicationConfigurationTemplate_createPublicIP :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateReplicationConfigurationTemplate_createPublicIP = Lens.lens (\UpdateReplicationConfigurationTemplate' {createPublicIP} -> createPublicIP) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {createPublicIP = a} :: UpdateReplicationConfigurationTemplate)

-- | The data plane routing mechanism that will be used for replication.
updateReplicationConfigurationTemplate_dataPlaneRouting :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationDataPlaneRouting)
updateReplicationConfigurationTemplate_dataPlaneRouting = Lens.lens (\UpdateReplicationConfigurationTemplate' {dataPlaneRouting} -> dataPlaneRouting) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {dataPlaneRouting = a} :: UpdateReplicationConfigurationTemplate)

-- | The Staging Disk EBS volume type to be used during replication.
updateReplicationConfigurationTemplate_defaultLargeStagingDiskType :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType)
updateReplicationConfigurationTemplate_defaultLargeStagingDiskType = Lens.lens (\UpdateReplicationConfigurationTemplate' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {defaultLargeStagingDiskType = a} :: UpdateReplicationConfigurationTemplate)

-- | The type of EBS encryption to be used during replication.
updateReplicationConfigurationTemplate_ebsEncryption :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationEbsEncryption)
updateReplicationConfigurationTemplate_ebsEncryption = Lens.lens (\UpdateReplicationConfigurationTemplate' {ebsEncryption} -> ebsEncryption) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {ebsEncryption = a} :: UpdateReplicationConfigurationTemplate)

-- | The ARN of the EBS encryption key to be used during replication.
updateReplicationConfigurationTemplate_ebsEncryptionKeyArn :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateReplicationConfigurationTemplate_ebsEncryptionKeyArn = Lens.lens (\UpdateReplicationConfigurationTemplate' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {ebsEncryptionKeyArn = a} :: UpdateReplicationConfigurationTemplate)

-- | The Point in time (PIT) policy to manage snapshots taken during
-- replication.
updateReplicationConfigurationTemplate_pitPolicy :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe (Prelude.NonEmpty PITPolicyRule))
updateReplicationConfigurationTemplate_pitPolicy = Lens.lens (\UpdateReplicationConfigurationTemplate' {pitPolicy} -> pitPolicy) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {pitPolicy = a} :: UpdateReplicationConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The instance type to be used for the replication server.
updateReplicationConfigurationTemplate_replicationServerInstanceType :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateReplicationConfigurationTemplate_replicationServerInstanceType = Lens.lens (\UpdateReplicationConfigurationTemplate' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {replicationServerInstanceType = a} :: UpdateReplicationConfigurationTemplate)

-- | The security group IDs that will be used by the replication server.
updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe [Prelude.Text])
updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs = Lens.lens (\UpdateReplicationConfigurationTemplate' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {replicationServersSecurityGroupsIDs = a} :: UpdateReplicationConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The subnet to be used by the replication staging area.
updateReplicationConfigurationTemplate_stagingAreaSubnetId :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateReplicationConfigurationTemplate_stagingAreaSubnetId = Lens.lens (\UpdateReplicationConfigurationTemplate' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {stagingAreaSubnetId = a} :: UpdateReplicationConfigurationTemplate)

-- | A set of tags to be associated with all resources created in the
-- replication staging area: EC2 replication server, EBS volumes, EBS
-- snapshots, etc.
updateReplicationConfigurationTemplate_stagingAreaTags :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateReplicationConfigurationTemplate_stagingAreaTags = Lens.lens (\UpdateReplicationConfigurationTemplate' {stagingAreaTags} -> stagingAreaTags) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {stagingAreaTags = a} :: UpdateReplicationConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Whether to use a dedicated Replication Server in the replication staging
-- area.
updateReplicationConfigurationTemplate_useDedicatedReplicationServer :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateReplicationConfigurationTemplate_useDedicatedReplicationServer = Lens.lens (\UpdateReplicationConfigurationTemplate' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {useDedicatedReplicationServer = a} :: UpdateReplicationConfigurationTemplate)

-- | The Replication Configuration Template ID.
updateReplicationConfigurationTemplate_replicationConfigurationTemplateID :: Lens.Lens' UpdateReplicationConfigurationTemplate Prelude.Text
updateReplicationConfigurationTemplate_replicationConfigurationTemplateID = Lens.lens (\UpdateReplicationConfigurationTemplate' {replicationConfigurationTemplateID} -> replicationConfigurationTemplateID) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {replicationConfigurationTemplateID = a} :: UpdateReplicationConfigurationTemplate)

instance
  Core.AWSRequest
    UpdateReplicationConfigurationTemplate
  where
  type
    AWSResponse
      UpdateReplicationConfigurationTemplate =
      ReplicationConfigurationTemplate
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    UpdateReplicationConfigurationTemplate
  where
  hashWithSalt
    _salt
    UpdateReplicationConfigurationTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` associateDefaultSecurityGroup
        `Prelude.hashWithSalt` bandwidthThrottling
        `Prelude.hashWithSalt` createPublicIP
        `Prelude.hashWithSalt` dataPlaneRouting
        `Prelude.hashWithSalt` defaultLargeStagingDiskType
        `Prelude.hashWithSalt` ebsEncryption
        `Prelude.hashWithSalt` ebsEncryptionKeyArn
        `Prelude.hashWithSalt` pitPolicy
        `Prelude.hashWithSalt` replicationServerInstanceType
        `Prelude.hashWithSalt` replicationServersSecurityGroupsIDs
        `Prelude.hashWithSalt` stagingAreaSubnetId
        `Prelude.hashWithSalt` stagingAreaTags
        `Prelude.hashWithSalt` useDedicatedReplicationServer
        `Prelude.hashWithSalt` replicationConfigurationTemplateID

instance
  Prelude.NFData
    UpdateReplicationConfigurationTemplate
  where
  rnf UpdateReplicationConfigurationTemplate' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf associateDefaultSecurityGroup
      `Prelude.seq` Prelude.rnf bandwidthThrottling
      `Prelude.seq` Prelude.rnf createPublicIP
      `Prelude.seq` Prelude.rnf dataPlaneRouting
      `Prelude.seq` Prelude.rnf defaultLargeStagingDiskType
      `Prelude.seq` Prelude.rnf ebsEncryption
      `Prelude.seq` Prelude.rnf ebsEncryptionKeyArn
      `Prelude.seq` Prelude.rnf pitPolicy
      `Prelude.seq` Prelude.rnf replicationServerInstanceType
      `Prelude.seq` Prelude.rnf replicationServersSecurityGroupsIDs
      `Prelude.seq` Prelude.rnf stagingAreaSubnetId
      `Prelude.seq` Prelude.rnf stagingAreaTags
      `Prelude.seq` Prelude.rnf useDedicatedReplicationServer
      `Prelude.seq` Prelude.rnf
        replicationConfigurationTemplateID

instance
  Data.ToHeaders
    UpdateReplicationConfigurationTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateReplicationConfigurationTemplate
  where
  toJSON UpdateReplicationConfigurationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arn" Data..=) Prelude.<$> arn,
            ("associateDefaultSecurityGroup" Data..=)
              Prelude.<$> associateDefaultSecurityGroup,
            ("bandwidthThrottling" Data..=)
              Prelude.<$> bandwidthThrottling,
            ("createPublicIP" Data..=)
              Prelude.<$> createPublicIP,
            ("dataPlaneRouting" Data..=)
              Prelude.<$> dataPlaneRouting,
            ("defaultLargeStagingDiskType" Data..=)
              Prelude.<$> defaultLargeStagingDiskType,
            ("ebsEncryption" Data..=) Prelude.<$> ebsEncryption,
            ("ebsEncryptionKeyArn" Data..=)
              Prelude.<$> ebsEncryptionKeyArn,
            ("pitPolicy" Data..=) Prelude.<$> pitPolicy,
            ("replicationServerInstanceType" Data..=)
              Prelude.<$> replicationServerInstanceType,
            ("replicationServersSecurityGroupsIDs" Data..=)
              Prelude.<$> replicationServersSecurityGroupsIDs,
            ("stagingAreaSubnetId" Data..=)
              Prelude.<$> stagingAreaSubnetId,
            ("stagingAreaTags" Data..=)
              Prelude.<$> stagingAreaTags,
            ("useDedicatedReplicationServer" Data..=)
              Prelude.<$> useDedicatedReplicationServer,
            Prelude.Just
              ( "replicationConfigurationTemplateID"
                  Data..= replicationConfigurationTemplateID
              )
          ]
      )

instance
  Data.ToPath
    UpdateReplicationConfigurationTemplate
  where
  toPath =
    Prelude.const
      "/UpdateReplicationConfigurationTemplate"

instance
  Data.ToQuery
    UpdateReplicationConfigurationTemplate
  where
  toQuery = Prelude.const Prelude.mempty
