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
-- Module      : Amazonka.MGN.Types.ReplicationConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ReplicationConfigurationTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ReplicationConfigurationDataPlaneRouting
import Amazonka.MGN.Types.ReplicationConfigurationDefaultLargeStagingDiskType
import Amazonka.MGN.Types.ReplicationConfigurationEbsEncryption
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newReplicationConfigurationTemplate' smart constructor.
data ReplicationConfigurationTemplate = ReplicationConfigurationTemplate'
  { -- | Replication Configuration template ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration template associate default Application
    -- Migration Service Security group.
    associateDefaultSecurityGroup :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration template bandwidth throttling.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | Replication Configuration template create Public IP.
    createPublicIP :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration template data plane routing.
    dataPlaneRouting :: Prelude.Maybe ReplicationConfigurationDataPlaneRouting,
    -- | Replication Configuration template use default large Staging Disk type.
    defaultLargeStagingDiskType :: Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType,
    -- | Replication Configuration template EBS encryption.
    ebsEncryption :: Prelude.Maybe ReplicationConfigurationEbsEncryption,
    -- | Replication Configuration template EBS encryption key ARN.
    ebsEncryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration template server instance type.
    replicationServerInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration template server Security Groups IDs.
    replicationServersSecurityGroupsIDs :: Prelude.Maybe [Prelude.Text],
    -- | Replication Configuration template Staging Area subnet ID.
    stagingAreaSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration template Staging Area Tags.
    stagingAreaTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Replication Configuration template Tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Replication Configuration template use Dedicated Replication Server.
    useDedicatedReplicationServer :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration template ID.
    replicationConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'replicationConfigurationTemplate_arn' - Replication Configuration template ARN.
--
-- 'associateDefaultSecurityGroup', 'replicationConfigurationTemplate_associateDefaultSecurityGroup' - Replication Configuration template associate default Application
-- Migration Service Security group.
--
-- 'bandwidthThrottling', 'replicationConfigurationTemplate_bandwidthThrottling' - Replication Configuration template bandwidth throttling.
--
-- 'createPublicIP', 'replicationConfigurationTemplate_createPublicIP' - Replication Configuration template create Public IP.
--
-- 'dataPlaneRouting', 'replicationConfigurationTemplate_dataPlaneRouting' - Replication Configuration template data plane routing.
--
-- 'defaultLargeStagingDiskType', 'replicationConfigurationTemplate_defaultLargeStagingDiskType' - Replication Configuration template use default large Staging Disk type.
--
-- 'ebsEncryption', 'replicationConfigurationTemplate_ebsEncryption' - Replication Configuration template EBS encryption.
--
-- 'ebsEncryptionKeyArn', 'replicationConfigurationTemplate_ebsEncryptionKeyArn' - Replication Configuration template EBS encryption key ARN.
--
-- 'replicationServerInstanceType', 'replicationConfigurationTemplate_replicationServerInstanceType' - Replication Configuration template server instance type.
--
-- 'replicationServersSecurityGroupsIDs', 'replicationConfigurationTemplate_replicationServersSecurityGroupsIDs' - Replication Configuration template server Security Groups IDs.
--
-- 'stagingAreaSubnetId', 'replicationConfigurationTemplate_stagingAreaSubnetId' - Replication Configuration template Staging Area subnet ID.
--
-- 'stagingAreaTags', 'replicationConfigurationTemplate_stagingAreaTags' - Replication Configuration template Staging Area Tags.
--
-- 'tags', 'replicationConfigurationTemplate_tags' - Replication Configuration template Tags.
--
-- 'useDedicatedReplicationServer', 'replicationConfigurationTemplate_useDedicatedReplicationServer' - Replication Configuration template use Dedicated Replication Server.
--
-- 'replicationConfigurationTemplateID', 'replicationConfigurationTemplate_replicationConfigurationTemplateID' - Replication Configuration template ID.
newReplicationConfigurationTemplate ::
  -- | 'replicationConfigurationTemplateID'
  Prelude.Text ->
  ReplicationConfigurationTemplate
newReplicationConfigurationTemplate
  pReplicationConfigurationTemplateID_ =
    ReplicationConfigurationTemplate'
      { arn =
          Prelude.Nothing,
        associateDefaultSecurityGroup =
          Prelude.Nothing,
        bandwidthThrottling = Prelude.Nothing,
        createPublicIP = Prelude.Nothing,
        dataPlaneRouting = Prelude.Nothing,
        defaultLargeStagingDiskType =
          Prelude.Nothing,
        ebsEncryption = Prelude.Nothing,
        ebsEncryptionKeyArn = Prelude.Nothing,
        replicationServerInstanceType =
          Prelude.Nothing,
        replicationServersSecurityGroupsIDs =
          Prelude.Nothing,
        stagingAreaSubnetId = Prelude.Nothing,
        stagingAreaTags = Prelude.Nothing,
        tags = Prelude.Nothing,
        useDedicatedReplicationServer =
          Prelude.Nothing,
        replicationConfigurationTemplateID =
          pReplicationConfigurationTemplateID_
      }

-- | Replication Configuration template ARN.
replicationConfigurationTemplate_arn :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
replicationConfigurationTemplate_arn = Lens.lens (\ReplicationConfigurationTemplate' {arn} -> arn) (\s@ReplicationConfigurationTemplate' {} a -> s {arn = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template associate default Application
-- Migration Service Security group.
replicationConfigurationTemplate_associateDefaultSecurityGroup :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
replicationConfigurationTemplate_associateDefaultSecurityGroup = Lens.lens (\ReplicationConfigurationTemplate' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@ReplicationConfigurationTemplate' {} a -> s {associateDefaultSecurityGroup = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template bandwidth throttling.
replicationConfigurationTemplate_bandwidthThrottling :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Natural)
replicationConfigurationTemplate_bandwidthThrottling = Lens.lens (\ReplicationConfigurationTemplate' {bandwidthThrottling} -> bandwidthThrottling) (\s@ReplicationConfigurationTemplate' {} a -> s {bandwidthThrottling = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template create Public IP.
replicationConfigurationTemplate_createPublicIP :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
replicationConfigurationTemplate_createPublicIP = Lens.lens (\ReplicationConfigurationTemplate' {createPublicIP} -> createPublicIP) (\s@ReplicationConfigurationTemplate' {} a -> s {createPublicIP = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template data plane routing.
replicationConfigurationTemplate_dataPlaneRouting :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationDataPlaneRouting)
replicationConfigurationTemplate_dataPlaneRouting = Lens.lens (\ReplicationConfigurationTemplate' {dataPlaneRouting} -> dataPlaneRouting) (\s@ReplicationConfigurationTemplate' {} a -> s {dataPlaneRouting = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template use default large Staging Disk type.
replicationConfigurationTemplate_defaultLargeStagingDiskType :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType)
replicationConfigurationTemplate_defaultLargeStagingDiskType = Lens.lens (\ReplicationConfigurationTemplate' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@ReplicationConfigurationTemplate' {} a -> s {defaultLargeStagingDiskType = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template EBS encryption.
replicationConfigurationTemplate_ebsEncryption :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationEbsEncryption)
replicationConfigurationTemplate_ebsEncryption = Lens.lens (\ReplicationConfigurationTemplate' {ebsEncryption} -> ebsEncryption) (\s@ReplicationConfigurationTemplate' {} a -> s {ebsEncryption = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template EBS encryption key ARN.
replicationConfigurationTemplate_ebsEncryptionKeyArn :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
replicationConfigurationTemplate_ebsEncryptionKeyArn = Lens.lens (\ReplicationConfigurationTemplate' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@ReplicationConfigurationTemplate' {} a -> s {ebsEncryptionKeyArn = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template server instance type.
replicationConfigurationTemplate_replicationServerInstanceType :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
replicationConfigurationTemplate_replicationServerInstanceType = Lens.lens (\ReplicationConfigurationTemplate' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@ReplicationConfigurationTemplate' {} a -> s {replicationServerInstanceType = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template server Security Groups IDs.
replicationConfigurationTemplate_replicationServersSecurityGroupsIDs :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe [Prelude.Text])
replicationConfigurationTemplate_replicationServersSecurityGroupsIDs = Lens.lens (\ReplicationConfigurationTemplate' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@ReplicationConfigurationTemplate' {} a -> s {replicationServersSecurityGroupsIDs = a} :: ReplicationConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Replication Configuration template Staging Area subnet ID.
replicationConfigurationTemplate_stagingAreaSubnetId :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
replicationConfigurationTemplate_stagingAreaSubnetId = Lens.lens (\ReplicationConfigurationTemplate' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@ReplicationConfigurationTemplate' {} a -> s {stagingAreaSubnetId = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template Staging Area Tags.
replicationConfigurationTemplate_stagingAreaTags :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
replicationConfigurationTemplate_stagingAreaTags = Lens.lens (\ReplicationConfigurationTemplate' {stagingAreaTags} -> stagingAreaTags) (\s@ReplicationConfigurationTemplate' {} a -> s {stagingAreaTags = a} :: ReplicationConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Replication Configuration template Tags.
replicationConfigurationTemplate_tags :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
replicationConfigurationTemplate_tags = Lens.lens (\ReplicationConfigurationTemplate' {tags} -> tags) (\s@ReplicationConfigurationTemplate' {} a -> s {tags = a} :: ReplicationConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Replication Configuration template use Dedicated Replication Server.
replicationConfigurationTemplate_useDedicatedReplicationServer :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
replicationConfigurationTemplate_useDedicatedReplicationServer = Lens.lens (\ReplicationConfigurationTemplate' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@ReplicationConfigurationTemplate' {} a -> s {useDedicatedReplicationServer = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template ID.
replicationConfigurationTemplate_replicationConfigurationTemplateID :: Lens.Lens' ReplicationConfigurationTemplate Prelude.Text
replicationConfigurationTemplate_replicationConfigurationTemplateID = Lens.lens (\ReplicationConfigurationTemplate' {replicationConfigurationTemplateID} -> replicationConfigurationTemplateID) (\s@ReplicationConfigurationTemplate' {} a -> s {replicationConfigurationTemplateID = a} :: ReplicationConfigurationTemplate)

instance
  Data.FromJSON
    ReplicationConfigurationTemplate
  where
  parseJSON =
    Data.withObject
      "ReplicationConfigurationTemplate"
      ( \x ->
          ReplicationConfigurationTemplate'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "associateDefaultSecurityGroup")
            Prelude.<*> (x Data..:? "bandwidthThrottling")
            Prelude.<*> (x Data..:? "createPublicIP")
            Prelude.<*> (x Data..:? "dataPlaneRouting")
            Prelude.<*> (x Data..:? "defaultLargeStagingDiskType")
            Prelude.<*> (x Data..:? "ebsEncryption")
            Prelude.<*> (x Data..:? "ebsEncryptionKeyArn")
            Prelude.<*> (x Data..:? "replicationServerInstanceType")
            Prelude.<*> ( x
                            Data..:? "replicationServersSecurityGroupsIDs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "stagingAreaSubnetId")
            Prelude.<*> ( x
                            Data..:? "stagingAreaTags"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "useDedicatedReplicationServer")
            Prelude.<*> (x Data..: "replicationConfigurationTemplateID")
      )

instance
  Prelude.Hashable
    ReplicationConfigurationTemplate
  where
  hashWithSalt
    _salt
    ReplicationConfigurationTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` associateDefaultSecurityGroup
        `Prelude.hashWithSalt` bandwidthThrottling
        `Prelude.hashWithSalt` createPublicIP
        `Prelude.hashWithSalt` dataPlaneRouting
        `Prelude.hashWithSalt` defaultLargeStagingDiskType
        `Prelude.hashWithSalt` ebsEncryption
        `Prelude.hashWithSalt` ebsEncryptionKeyArn
        `Prelude.hashWithSalt` replicationServerInstanceType
        `Prelude.hashWithSalt` replicationServersSecurityGroupsIDs
        `Prelude.hashWithSalt` stagingAreaSubnetId
        `Prelude.hashWithSalt` stagingAreaTags
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` useDedicatedReplicationServer
        `Prelude.hashWithSalt` replicationConfigurationTemplateID

instance
  Prelude.NFData
    ReplicationConfigurationTemplate
  where
  rnf ReplicationConfigurationTemplate' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf associateDefaultSecurityGroup
      `Prelude.seq` Prelude.rnf bandwidthThrottling
      `Prelude.seq` Prelude.rnf createPublicIP
      `Prelude.seq` Prelude.rnf dataPlaneRouting
      `Prelude.seq` Prelude.rnf defaultLargeStagingDiskType
      `Prelude.seq` Prelude.rnf ebsEncryption
      `Prelude.seq` Prelude.rnf ebsEncryptionKeyArn
      `Prelude.seq` Prelude.rnf replicationServerInstanceType
      `Prelude.seq` Prelude.rnf replicationServersSecurityGroupsIDs
      `Prelude.seq` Prelude.rnf stagingAreaSubnetId
      `Prelude.seq` Prelude.rnf stagingAreaTags
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf useDedicatedReplicationServer
      `Prelude.seq` Prelude.rnf
        replicationConfigurationTemplateID
