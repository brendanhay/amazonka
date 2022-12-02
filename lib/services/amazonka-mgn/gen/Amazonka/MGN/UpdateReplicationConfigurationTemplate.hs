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
-- Module      : Amazonka.MGN.UpdateReplicationConfigurationTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates multiple ReplicationConfigurationTemplates by ID.
module Amazonka.MGN.UpdateReplicationConfigurationTemplate
  ( -- * Creating a Request
    UpdateReplicationConfigurationTemplate (..),
    newUpdateReplicationConfigurationTemplate,

    -- * Request Lenses
    updateReplicationConfigurationTemplate_bandwidthThrottling,
    updateReplicationConfigurationTemplate_replicationServerInstanceType,
    updateReplicationConfigurationTemplate_stagingAreaTags,
    updateReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    updateReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    updateReplicationConfigurationTemplate_arn,
    updateReplicationConfigurationTemplate_stagingAreaSubnetId,
    updateReplicationConfigurationTemplate_createPublicIP,
    updateReplicationConfigurationTemplate_dataPlaneRouting,
    updateReplicationConfigurationTemplate_ebsEncryption,
    updateReplicationConfigurationTemplate_useDedicatedReplicationServer,
    updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    updateReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    updateReplicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- * Destructuring the Response
    ReplicationConfigurationTemplate (..),
    newReplicationConfigurationTemplate,

    -- * Response Lenses
    replicationConfigurationTemplate_tags,
    replicationConfigurationTemplate_bandwidthThrottling,
    replicationConfigurationTemplate_replicationServerInstanceType,
    replicationConfigurationTemplate_stagingAreaTags,
    replicationConfigurationTemplate_associateDefaultSecurityGroup,
    replicationConfigurationTemplate_defaultLargeStagingDiskType,
    replicationConfigurationTemplate_arn,
    replicationConfigurationTemplate_stagingAreaSubnetId,
    replicationConfigurationTemplate_createPublicIP,
    replicationConfigurationTemplate_dataPlaneRouting,
    replicationConfigurationTemplate_ebsEncryption,
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateReplicationConfigurationTemplate' smart constructor.
data UpdateReplicationConfigurationTemplate = UpdateReplicationConfigurationTemplate'
  { -- | Update replication configuration template bandwidth throttling request.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | Update replication configuration template Replication Server instance
    -- type request.
    replicationServerInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Update replication configuration template Staging Area Tags request.
    stagingAreaTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Update replication configuration template associate default Application
    -- Migration Service Security group request.
    associateDefaultSecurityGroup :: Prelude.Maybe Prelude.Bool,
    -- | Update replication configuration template use default large Staging Disk
    -- type request.
    defaultLargeStagingDiskType :: Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType,
    -- | Update replication configuration template ARN request.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Update replication configuration template Staging Area subnet ID
    -- request.
    stagingAreaSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Update replication configuration template create Public IP request.
    createPublicIP :: Prelude.Maybe Prelude.Bool,
    -- | Update replication configuration template data plane routing request.
    dataPlaneRouting :: Prelude.Maybe ReplicationConfigurationDataPlaneRouting,
    -- | Update replication configuration template EBS encryption request.
    ebsEncryption :: Prelude.Maybe ReplicationConfigurationEbsEncryption,
    -- | Update replication configuration template use dedicated Replication
    -- Server request.
    useDedicatedReplicationServer :: Prelude.Maybe Prelude.Bool,
    -- | Update replication configuration template Replication Server Security
    -- groups IDs request.
    replicationServersSecurityGroupsIDs :: Prelude.Maybe [Prelude.Text],
    -- | Update replication configuration template EBS encryption key ARN
    -- request.
    ebsEncryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Update replication configuration template template ID request.
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
-- 'bandwidthThrottling', 'updateReplicationConfigurationTemplate_bandwidthThrottling' - Update replication configuration template bandwidth throttling request.
--
-- 'replicationServerInstanceType', 'updateReplicationConfigurationTemplate_replicationServerInstanceType' - Update replication configuration template Replication Server instance
-- type request.
--
-- 'stagingAreaTags', 'updateReplicationConfigurationTemplate_stagingAreaTags' - Update replication configuration template Staging Area Tags request.
--
-- 'associateDefaultSecurityGroup', 'updateReplicationConfigurationTemplate_associateDefaultSecurityGroup' - Update replication configuration template associate default Application
-- Migration Service Security group request.
--
-- 'defaultLargeStagingDiskType', 'updateReplicationConfigurationTemplate_defaultLargeStagingDiskType' - Update replication configuration template use default large Staging Disk
-- type request.
--
-- 'arn', 'updateReplicationConfigurationTemplate_arn' - Update replication configuration template ARN request.
--
-- 'stagingAreaSubnetId', 'updateReplicationConfigurationTemplate_stagingAreaSubnetId' - Update replication configuration template Staging Area subnet ID
-- request.
--
-- 'createPublicIP', 'updateReplicationConfigurationTemplate_createPublicIP' - Update replication configuration template create Public IP request.
--
-- 'dataPlaneRouting', 'updateReplicationConfigurationTemplate_dataPlaneRouting' - Update replication configuration template data plane routing request.
--
-- 'ebsEncryption', 'updateReplicationConfigurationTemplate_ebsEncryption' - Update replication configuration template EBS encryption request.
--
-- 'useDedicatedReplicationServer', 'updateReplicationConfigurationTemplate_useDedicatedReplicationServer' - Update replication configuration template use dedicated Replication
-- Server request.
--
-- 'replicationServersSecurityGroupsIDs', 'updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs' - Update replication configuration template Replication Server Security
-- groups IDs request.
--
-- 'ebsEncryptionKeyArn', 'updateReplicationConfigurationTemplate_ebsEncryptionKeyArn' - Update replication configuration template EBS encryption key ARN
-- request.
--
-- 'replicationConfigurationTemplateID', 'updateReplicationConfigurationTemplate_replicationConfigurationTemplateID' - Update replication configuration template template ID request.
newUpdateReplicationConfigurationTemplate ::
  -- | 'replicationConfigurationTemplateID'
  Prelude.Text ->
  UpdateReplicationConfigurationTemplate
newUpdateReplicationConfigurationTemplate
  pReplicationConfigurationTemplateID_ =
    UpdateReplicationConfigurationTemplate'
      { bandwidthThrottling =
          Prelude.Nothing,
        replicationServerInstanceType =
          Prelude.Nothing,
        stagingAreaTags = Prelude.Nothing,
        associateDefaultSecurityGroup =
          Prelude.Nothing,
        defaultLargeStagingDiskType =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        stagingAreaSubnetId =
          Prelude.Nothing,
        createPublicIP = Prelude.Nothing,
        dataPlaneRouting = Prelude.Nothing,
        ebsEncryption = Prelude.Nothing,
        useDedicatedReplicationServer =
          Prelude.Nothing,
        replicationServersSecurityGroupsIDs =
          Prelude.Nothing,
        ebsEncryptionKeyArn =
          Prelude.Nothing,
        replicationConfigurationTemplateID =
          pReplicationConfigurationTemplateID_
      }

-- | Update replication configuration template bandwidth throttling request.
updateReplicationConfigurationTemplate_bandwidthThrottling :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Natural)
updateReplicationConfigurationTemplate_bandwidthThrottling = Lens.lens (\UpdateReplicationConfigurationTemplate' {bandwidthThrottling} -> bandwidthThrottling) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {bandwidthThrottling = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template Replication Server instance
-- type request.
updateReplicationConfigurationTemplate_replicationServerInstanceType :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateReplicationConfigurationTemplate_replicationServerInstanceType = Lens.lens (\UpdateReplicationConfigurationTemplate' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {replicationServerInstanceType = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template Staging Area Tags request.
updateReplicationConfigurationTemplate_stagingAreaTags :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateReplicationConfigurationTemplate_stagingAreaTags = Lens.lens (\UpdateReplicationConfigurationTemplate' {stagingAreaTags} -> stagingAreaTags) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {stagingAreaTags = a} :: UpdateReplicationConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Update replication configuration template associate default Application
-- Migration Service Security group request.
updateReplicationConfigurationTemplate_associateDefaultSecurityGroup :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateReplicationConfigurationTemplate_associateDefaultSecurityGroup = Lens.lens (\UpdateReplicationConfigurationTemplate' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {associateDefaultSecurityGroup = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template use default large Staging Disk
-- type request.
updateReplicationConfigurationTemplate_defaultLargeStagingDiskType :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType)
updateReplicationConfigurationTemplate_defaultLargeStagingDiskType = Lens.lens (\UpdateReplicationConfigurationTemplate' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {defaultLargeStagingDiskType = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template ARN request.
updateReplicationConfigurationTemplate_arn :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateReplicationConfigurationTemplate_arn = Lens.lens (\UpdateReplicationConfigurationTemplate' {arn} -> arn) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {arn = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template Staging Area subnet ID
-- request.
updateReplicationConfigurationTemplate_stagingAreaSubnetId :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateReplicationConfigurationTemplate_stagingAreaSubnetId = Lens.lens (\UpdateReplicationConfigurationTemplate' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {stagingAreaSubnetId = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template create Public IP request.
updateReplicationConfigurationTemplate_createPublicIP :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateReplicationConfigurationTemplate_createPublicIP = Lens.lens (\UpdateReplicationConfigurationTemplate' {createPublicIP} -> createPublicIP) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {createPublicIP = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template data plane routing request.
updateReplicationConfigurationTemplate_dataPlaneRouting :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationDataPlaneRouting)
updateReplicationConfigurationTemplate_dataPlaneRouting = Lens.lens (\UpdateReplicationConfigurationTemplate' {dataPlaneRouting} -> dataPlaneRouting) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {dataPlaneRouting = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template EBS encryption request.
updateReplicationConfigurationTemplate_ebsEncryption :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationEbsEncryption)
updateReplicationConfigurationTemplate_ebsEncryption = Lens.lens (\UpdateReplicationConfigurationTemplate' {ebsEncryption} -> ebsEncryption) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {ebsEncryption = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template use dedicated Replication
-- Server request.
updateReplicationConfigurationTemplate_useDedicatedReplicationServer :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateReplicationConfigurationTemplate_useDedicatedReplicationServer = Lens.lens (\UpdateReplicationConfigurationTemplate' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {useDedicatedReplicationServer = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template Replication Server Security
-- groups IDs request.
updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe [Prelude.Text])
updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs = Lens.lens (\UpdateReplicationConfigurationTemplate' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {replicationServersSecurityGroupsIDs = a} :: UpdateReplicationConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Update replication configuration template EBS encryption key ARN
-- request.
updateReplicationConfigurationTemplate_ebsEncryptionKeyArn :: Lens.Lens' UpdateReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateReplicationConfigurationTemplate_ebsEncryptionKeyArn = Lens.lens (\UpdateReplicationConfigurationTemplate' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@UpdateReplicationConfigurationTemplate' {} a -> s {ebsEncryptionKeyArn = a} :: UpdateReplicationConfigurationTemplate)

-- | Update replication configuration template template ID request.
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
      _salt `Prelude.hashWithSalt` bandwidthThrottling
        `Prelude.hashWithSalt` replicationServerInstanceType
        `Prelude.hashWithSalt` stagingAreaTags
        `Prelude.hashWithSalt` associateDefaultSecurityGroup
        `Prelude.hashWithSalt` defaultLargeStagingDiskType
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` stagingAreaSubnetId
        `Prelude.hashWithSalt` createPublicIP
        `Prelude.hashWithSalt` dataPlaneRouting
        `Prelude.hashWithSalt` ebsEncryption
        `Prelude.hashWithSalt` useDedicatedReplicationServer
        `Prelude.hashWithSalt` replicationServersSecurityGroupsIDs
        `Prelude.hashWithSalt` ebsEncryptionKeyArn
        `Prelude.hashWithSalt` replicationConfigurationTemplateID

instance
  Prelude.NFData
    UpdateReplicationConfigurationTemplate
  where
  rnf UpdateReplicationConfigurationTemplate' {..} =
    Prelude.rnf bandwidthThrottling
      `Prelude.seq` Prelude.rnf replicationServerInstanceType
      `Prelude.seq` Prelude.rnf stagingAreaTags
      `Prelude.seq` Prelude.rnf associateDefaultSecurityGroup
      `Prelude.seq` Prelude.rnf defaultLargeStagingDiskType
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf stagingAreaSubnetId
      `Prelude.seq` Prelude.rnf createPublicIP
      `Prelude.seq` Prelude.rnf dataPlaneRouting
      `Prelude.seq` Prelude.rnf ebsEncryption
      `Prelude.seq` Prelude.rnf useDedicatedReplicationServer
      `Prelude.seq` Prelude.rnf
        replicationServersSecurityGroupsIDs
      `Prelude.seq` Prelude.rnf ebsEncryptionKeyArn
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
          [ ("bandwidthThrottling" Data..=)
              Prelude.<$> bandwidthThrottling,
            ("replicationServerInstanceType" Data..=)
              Prelude.<$> replicationServerInstanceType,
            ("stagingAreaTags" Data..=)
              Prelude.<$> stagingAreaTags,
            ("associateDefaultSecurityGroup" Data..=)
              Prelude.<$> associateDefaultSecurityGroup,
            ("defaultLargeStagingDiskType" Data..=)
              Prelude.<$> defaultLargeStagingDiskType,
            ("arn" Data..=) Prelude.<$> arn,
            ("stagingAreaSubnetId" Data..=)
              Prelude.<$> stagingAreaSubnetId,
            ("createPublicIP" Data..=)
              Prelude.<$> createPublicIP,
            ("dataPlaneRouting" Data..=)
              Prelude.<$> dataPlaneRouting,
            ("ebsEncryption" Data..=) Prelude.<$> ebsEncryption,
            ("useDedicatedReplicationServer" Data..=)
              Prelude.<$> useDedicatedReplicationServer,
            ("replicationServersSecurityGroupsIDs" Data..=)
              Prelude.<$> replicationServersSecurityGroupsIDs,
            ("ebsEncryptionKeyArn" Data..=)
              Prelude.<$> ebsEncryptionKeyArn,
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
