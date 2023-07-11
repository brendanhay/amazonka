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
-- Module      : Amazonka.DrS.CreateReplicationConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new ReplicationConfigurationTemplate.
module Amazonka.DrS.CreateReplicationConfigurationTemplate
  ( -- * Creating a Request
    CreateReplicationConfigurationTemplate (..),
    newCreateReplicationConfigurationTemplate,

    -- * Request Lenses
    createReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    createReplicationConfigurationTemplate_tags,
    createReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    createReplicationConfigurationTemplate_bandwidthThrottling,
    createReplicationConfigurationTemplate_createPublicIP,
    createReplicationConfigurationTemplate_dataPlaneRouting,
    createReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    createReplicationConfigurationTemplate_ebsEncryption,
    createReplicationConfigurationTemplate_pitPolicy,
    createReplicationConfigurationTemplate_replicationServerInstanceType,
    createReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    createReplicationConfigurationTemplate_stagingAreaSubnetId,
    createReplicationConfigurationTemplate_stagingAreaTags,
    createReplicationConfigurationTemplate_useDedicatedReplicationServer,

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

-- | /See:/ 'newCreateReplicationConfigurationTemplate' smart constructor.
data CreateReplicationConfigurationTemplate = CreateReplicationConfigurationTemplate'
  { -- | The ARN of the EBS encryption key to be used during replication.
    ebsEncryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | A set of tags to be associated with the Replication Configuration
    -- Template resource.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Whether to associate the default Elastic Disaster Recovery Security
    -- group with the Replication Configuration Template.
    associateDefaultSecurityGroup :: Prelude.Bool,
    -- | Configure bandwidth throttling for the outbound data transfer rate of
    -- the Source Server in Mbps.
    bandwidthThrottling :: Prelude.Natural,
    -- | Whether to create a Public IP for the Recovery Instance by default.
    createPublicIP :: Prelude.Bool,
    -- | The data plane routing mechanism that will be used for replication.
    dataPlaneRouting :: ReplicationConfigurationDataPlaneRouting,
    -- | The Staging Disk EBS volume type to be used during replication.
    defaultLargeStagingDiskType :: ReplicationConfigurationDefaultLargeStagingDiskType,
    -- | The type of EBS encryption to be used during replication.
    ebsEncryption :: ReplicationConfigurationEbsEncryption,
    -- | The Point in time (PIT) policy to manage snapshots taken during
    -- replication.
    pitPolicy :: Prelude.NonEmpty PITPolicyRule,
    -- | The instance type to be used for the replication server.
    replicationServerInstanceType :: Prelude.Text,
    -- | The security group IDs that will be used by the replication server.
    replicationServersSecurityGroupsIDs :: [Prelude.Text],
    -- | The subnet to be used by the replication staging area.
    stagingAreaSubnetId :: Prelude.Text,
    -- | A set of tags to be associated with all resources created in the
    -- replication staging area: EC2 replication server, EBS volumes, EBS
    -- snapshots, etc.
    stagingAreaTags :: Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Whether to use a dedicated Replication Server in the replication staging
    -- area.
    useDedicatedReplicationServer :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsEncryptionKeyArn', 'createReplicationConfigurationTemplate_ebsEncryptionKeyArn' - The ARN of the EBS encryption key to be used during replication.
--
-- 'tags', 'createReplicationConfigurationTemplate_tags' - A set of tags to be associated with the Replication Configuration
-- Template resource.
--
-- 'associateDefaultSecurityGroup', 'createReplicationConfigurationTemplate_associateDefaultSecurityGroup' - Whether to associate the default Elastic Disaster Recovery Security
-- group with the Replication Configuration Template.
--
-- 'bandwidthThrottling', 'createReplicationConfigurationTemplate_bandwidthThrottling' - Configure bandwidth throttling for the outbound data transfer rate of
-- the Source Server in Mbps.
--
-- 'createPublicIP', 'createReplicationConfigurationTemplate_createPublicIP' - Whether to create a Public IP for the Recovery Instance by default.
--
-- 'dataPlaneRouting', 'createReplicationConfigurationTemplate_dataPlaneRouting' - The data plane routing mechanism that will be used for replication.
--
-- 'defaultLargeStagingDiskType', 'createReplicationConfigurationTemplate_defaultLargeStagingDiskType' - The Staging Disk EBS volume type to be used during replication.
--
-- 'ebsEncryption', 'createReplicationConfigurationTemplate_ebsEncryption' - The type of EBS encryption to be used during replication.
--
-- 'pitPolicy', 'createReplicationConfigurationTemplate_pitPolicy' - The Point in time (PIT) policy to manage snapshots taken during
-- replication.
--
-- 'replicationServerInstanceType', 'createReplicationConfigurationTemplate_replicationServerInstanceType' - The instance type to be used for the replication server.
--
-- 'replicationServersSecurityGroupsIDs', 'createReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs' - The security group IDs that will be used by the replication server.
--
-- 'stagingAreaSubnetId', 'createReplicationConfigurationTemplate_stagingAreaSubnetId' - The subnet to be used by the replication staging area.
--
-- 'stagingAreaTags', 'createReplicationConfigurationTemplate_stagingAreaTags' - A set of tags to be associated with all resources created in the
-- replication staging area: EC2 replication server, EBS volumes, EBS
-- snapshots, etc.
--
-- 'useDedicatedReplicationServer', 'createReplicationConfigurationTemplate_useDedicatedReplicationServer' - Whether to use a dedicated Replication Server in the replication staging
-- area.
newCreateReplicationConfigurationTemplate ::
  -- | 'associateDefaultSecurityGroup'
  Prelude.Bool ->
  -- | 'bandwidthThrottling'
  Prelude.Natural ->
  -- | 'createPublicIP'
  Prelude.Bool ->
  -- | 'dataPlaneRouting'
  ReplicationConfigurationDataPlaneRouting ->
  -- | 'defaultLargeStagingDiskType'
  ReplicationConfigurationDefaultLargeStagingDiskType ->
  -- | 'ebsEncryption'
  ReplicationConfigurationEbsEncryption ->
  -- | 'pitPolicy'
  Prelude.NonEmpty PITPolicyRule ->
  -- | 'replicationServerInstanceType'
  Prelude.Text ->
  -- | 'stagingAreaSubnetId'
  Prelude.Text ->
  -- | 'useDedicatedReplicationServer'
  Prelude.Bool ->
  CreateReplicationConfigurationTemplate
newCreateReplicationConfigurationTemplate
  pAssociateDefaultSecurityGroup_
  pBandwidthThrottling_
  pCreatePublicIP_
  pDataPlaneRouting_
  pDefaultLargeStagingDiskType_
  pEbsEncryption_
  pPitPolicy_
  pReplicationServerInstanceType_
  pStagingAreaSubnetId_
  pUseDedicatedReplicationServer_ =
    CreateReplicationConfigurationTemplate'
      { ebsEncryptionKeyArn =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        associateDefaultSecurityGroup =
          pAssociateDefaultSecurityGroup_,
        bandwidthThrottling =
          pBandwidthThrottling_,
        createPublicIP = pCreatePublicIP_,
        dataPlaneRouting =
          pDataPlaneRouting_,
        defaultLargeStagingDiskType =
          pDefaultLargeStagingDiskType_,
        ebsEncryption = pEbsEncryption_,
        pitPolicy =
          Lens.coerced Lens.# pPitPolicy_,
        replicationServerInstanceType =
          pReplicationServerInstanceType_,
        replicationServersSecurityGroupsIDs =
          Prelude.mempty,
        stagingAreaSubnetId =
          pStagingAreaSubnetId_,
        stagingAreaTags = Prelude.mempty,
        useDedicatedReplicationServer =
          pUseDedicatedReplicationServer_
      }

-- | The ARN of the EBS encryption key to be used during replication.
createReplicationConfigurationTemplate_ebsEncryptionKeyArn :: Lens.Lens' CreateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
createReplicationConfigurationTemplate_ebsEncryptionKeyArn = Lens.lens (\CreateReplicationConfigurationTemplate' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@CreateReplicationConfigurationTemplate' {} a -> s {ebsEncryptionKeyArn = a} :: CreateReplicationConfigurationTemplate)

-- | A set of tags to be associated with the Replication Configuration
-- Template resource.
createReplicationConfigurationTemplate_tags :: Lens.Lens' CreateReplicationConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createReplicationConfigurationTemplate_tags = Lens.lens (\CreateReplicationConfigurationTemplate' {tags} -> tags) (\s@CreateReplicationConfigurationTemplate' {} a -> s {tags = a} :: CreateReplicationConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Whether to associate the default Elastic Disaster Recovery Security
-- group with the Replication Configuration Template.
createReplicationConfigurationTemplate_associateDefaultSecurityGroup :: Lens.Lens' CreateReplicationConfigurationTemplate Prelude.Bool
createReplicationConfigurationTemplate_associateDefaultSecurityGroup = Lens.lens (\CreateReplicationConfigurationTemplate' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@CreateReplicationConfigurationTemplate' {} a -> s {associateDefaultSecurityGroup = a} :: CreateReplicationConfigurationTemplate)

-- | Configure bandwidth throttling for the outbound data transfer rate of
-- the Source Server in Mbps.
createReplicationConfigurationTemplate_bandwidthThrottling :: Lens.Lens' CreateReplicationConfigurationTemplate Prelude.Natural
createReplicationConfigurationTemplate_bandwidthThrottling = Lens.lens (\CreateReplicationConfigurationTemplate' {bandwidthThrottling} -> bandwidthThrottling) (\s@CreateReplicationConfigurationTemplate' {} a -> s {bandwidthThrottling = a} :: CreateReplicationConfigurationTemplate)

-- | Whether to create a Public IP for the Recovery Instance by default.
createReplicationConfigurationTemplate_createPublicIP :: Lens.Lens' CreateReplicationConfigurationTemplate Prelude.Bool
createReplicationConfigurationTemplate_createPublicIP = Lens.lens (\CreateReplicationConfigurationTemplate' {createPublicIP} -> createPublicIP) (\s@CreateReplicationConfigurationTemplate' {} a -> s {createPublicIP = a} :: CreateReplicationConfigurationTemplate)

-- | The data plane routing mechanism that will be used for replication.
createReplicationConfigurationTemplate_dataPlaneRouting :: Lens.Lens' CreateReplicationConfigurationTemplate ReplicationConfigurationDataPlaneRouting
createReplicationConfigurationTemplate_dataPlaneRouting = Lens.lens (\CreateReplicationConfigurationTemplate' {dataPlaneRouting} -> dataPlaneRouting) (\s@CreateReplicationConfigurationTemplate' {} a -> s {dataPlaneRouting = a} :: CreateReplicationConfigurationTemplate)

-- | The Staging Disk EBS volume type to be used during replication.
createReplicationConfigurationTemplate_defaultLargeStagingDiskType :: Lens.Lens' CreateReplicationConfigurationTemplate ReplicationConfigurationDefaultLargeStagingDiskType
createReplicationConfigurationTemplate_defaultLargeStagingDiskType = Lens.lens (\CreateReplicationConfigurationTemplate' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@CreateReplicationConfigurationTemplate' {} a -> s {defaultLargeStagingDiskType = a} :: CreateReplicationConfigurationTemplate)

-- | The type of EBS encryption to be used during replication.
createReplicationConfigurationTemplate_ebsEncryption :: Lens.Lens' CreateReplicationConfigurationTemplate ReplicationConfigurationEbsEncryption
createReplicationConfigurationTemplate_ebsEncryption = Lens.lens (\CreateReplicationConfigurationTemplate' {ebsEncryption} -> ebsEncryption) (\s@CreateReplicationConfigurationTemplate' {} a -> s {ebsEncryption = a} :: CreateReplicationConfigurationTemplate)

-- | The Point in time (PIT) policy to manage snapshots taken during
-- replication.
createReplicationConfigurationTemplate_pitPolicy :: Lens.Lens' CreateReplicationConfigurationTemplate (Prelude.NonEmpty PITPolicyRule)
createReplicationConfigurationTemplate_pitPolicy = Lens.lens (\CreateReplicationConfigurationTemplate' {pitPolicy} -> pitPolicy) (\s@CreateReplicationConfigurationTemplate' {} a -> s {pitPolicy = a} :: CreateReplicationConfigurationTemplate) Prelude.. Lens.coerced

-- | The instance type to be used for the replication server.
createReplicationConfigurationTemplate_replicationServerInstanceType :: Lens.Lens' CreateReplicationConfigurationTemplate Prelude.Text
createReplicationConfigurationTemplate_replicationServerInstanceType = Lens.lens (\CreateReplicationConfigurationTemplate' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@CreateReplicationConfigurationTemplate' {} a -> s {replicationServerInstanceType = a} :: CreateReplicationConfigurationTemplate)

-- | The security group IDs that will be used by the replication server.
createReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs :: Lens.Lens' CreateReplicationConfigurationTemplate [Prelude.Text]
createReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs = Lens.lens (\CreateReplicationConfigurationTemplate' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@CreateReplicationConfigurationTemplate' {} a -> s {replicationServersSecurityGroupsIDs = a} :: CreateReplicationConfigurationTemplate) Prelude.. Lens.coerced

-- | The subnet to be used by the replication staging area.
createReplicationConfigurationTemplate_stagingAreaSubnetId :: Lens.Lens' CreateReplicationConfigurationTemplate Prelude.Text
createReplicationConfigurationTemplate_stagingAreaSubnetId = Lens.lens (\CreateReplicationConfigurationTemplate' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@CreateReplicationConfigurationTemplate' {} a -> s {stagingAreaSubnetId = a} :: CreateReplicationConfigurationTemplate)

-- | A set of tags to be associated with all resources created in the
-- replication staging area: EC2 replication server, EBS volumes, EBS
-- snapshots, etc.
createReplicationConfigurationTemplate_stagingAreaTags :: Lens.Lens' CreateReplicationConfigurationTemplate (Prelude.HashMap Prelude.Text Prelude.Text)
createReplicationConfigurationTemplate_stagingAreaTags = Lens.lens (\CreateReplicationConfigurationTemplate' {stagingAreaTags} -> stagingAreaTags) (\s@CreateReplicationConfigurationTemplate' {} a -> s {stagingAreaTags = a} :: CreateReplicationConfigurationTemplate) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | Whether to use a dedicated Replication Server in the replication staging
-- area.
createReplicationConfigurationTemplate_useDedicatedReplicationServer :: Lens.Lens' CreateReplicationConfigurationTemplate Prelude.Bool
createReplicationConfigurationTemplate_useDedicatedReplicationServer = Lens.lens (\CreateReplicationConfigurationTemplate' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@CreateReplicationConfigurationTemplate' {} a -> s {useDedicatedReplicationServer = a} :: CreateReplicationConfigurationTemplate)

instance
  Core.AWSRequest
    CreateReplicationConfigurationTemplate
  where
  type
    AWSResponse
      CreateReplicationConfigurationTemplate =
      ReplicationConfigurationTemplate
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    CreateReplicationConfigurationTemplate
  where
  hashWithSalt
    _salt
    CreateReplicationConfigurationTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` ebsEncryptionKeyArn
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` associateDefaultSecurityGroup
        `Prelude.hashWithSalt` bandwidthThrottling
        `Prelude.hashWithSalt` createPublicIP
        `Prelude.hashWithSalt` dataPlaneRouting
        `Prelude.hashWithSalt` defaultLargeStagingDiskType
        `Prelude.hashWithSalt` ebsEncryption
        `Prelude.hashWithSalt` pitPolicy
        `Prelude.hashWithSalt` replicationServerInstanceType
        `Prelude.hashWithSalt` replicationServersSecurityGroupsIDs
        `Prelude.hashWithSalt` stagingAreaSubnetId
        `Prelude.hashWithSalt` stagingAreaTags
        `Prelude.hashWithSalt` useDedicatedReplicationServer

instance
  Prelude.NFData
    CreateReplicationConfigurationTemplate
  where
  rnf CreateReplicationConfigurationTemplate' {..} =
    Prelude.rnf ebsEncryptionKeyArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf associateDefaultSecurityGroup
      `Prelude.seq` Prelude.rnf bandwidthThrottling
      `Prelude.seq` Prelude.rnf createPublicIP
      `Prelude.seq` Prelude.rnf dataPlaneRouting
      `Prelude.seq` Prelude.rnf defaultLargeStagingDiskType
      `Prelude.seq` Prelude.rnf ebsEncryption
      `Prelude.seq` Prelude.rnf pitPolicy
      `Prelude.seq` Prelude.rnf replicationServerInstanceType
      `Prelude.seq` Prelude.rnf replicationServersSecurityGroupsIDs
      `Prelude.seq` Prelude.rnf stagingAreaSubnetId
      `Prelude.seq` Prelude.rnf stagingAreaTags
      `Prelude.seq` Prelude.rnf useDedicatedReplicationServer

instance
  Data.ToHeaders
    CreateReplicationConfigurationTemplate
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
    CreateReplicationConfigurationTemplate
  where
  toJSON CreateReplicationConfigurationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ebsEncryptionKeyArn" Data..=)
              Prelude.<$> ebsEncryptionKeyArn,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "associateDefaultSecurityGroup"
                  Data..= associateDefaultSecurityGroup
              ),
            Prelude.Just
              ("bandwidthThrottling" Data..= bandwidthThrottling),
            Prelude.Just
              ("createPublicIP" Data..= createPublicIP),
            Prelude.Just
              ("dataPlaneRouting" Data..= dataPlaneRouting),
            Prelude.Just
              ( "defaultLargeStagingDiskType"
                  Data..= defaultLargeStagingDiskType
              ),
            Prelude.Just ("ebsEncryption" Data..= ebsEncryption),
            Prelude.Just ("pitPolicy" Data..= pitPolicy),
            Prelude.Just
              ( "replicationServerInstanceType"
                  Data..= replicationServerInstanceType
              ),
            Prelude.Just
              ( "replicationServersSecurityGroupsIDs"
                  Data..= replicationServersSecurityGroupsIDs
              ),
            Prelude.Just
              ("stagingAreaSubnetId" Data..= stagingAreaSubnetId),
            Prelude.Just
              ("stagingAreaTags" Data..= stagingAreaTags),
            Prelude.Just
              ( "useDedicatedReplicationServer"
                  Data..= useDedicatedReplicationServer
              )
          ]
      )

instance
  Data.ToPath
    CreateReplicationConfigurationTemplate
  where
  toPath =
    Prelude.const
      "/CreateReplicationConfigurationTemplate"

instance
  Data.ToQuery
    CreateReplicationConfigurationTemplate
  where
  toQuery = Prelude.const Prelude.mempty
