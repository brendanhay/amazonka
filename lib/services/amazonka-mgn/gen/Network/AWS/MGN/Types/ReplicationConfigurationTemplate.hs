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
-- Module      : Network.AWS.MGN.Types.ReplicationConfigurationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types.ReplicationConfigurationTemplate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MGN.Types.ReplicationConfigurationDataPlaneRouting
import Network.AWS.MGN.Types.ReplicationConfigurationDefaultLargeStagingDiskType
import Network.AWS.MGN.Types.ReplicationConfigurationEbsEncryption
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newReplicationConfigurationTemplate' smart constructor.
data ReplicationConfigurationTemplate = ReplicationConfigurationTemplate'
  { -- | Replication Configuration template create Public IP.
    createPublicIP :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration template Staging Area Tags.
    stagingAreaTags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Replication Configuration template ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration template Staging Area subnet ID.
    stagingAreaSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration template server instance type.
    replicationServerInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration template EBS encryption.
    ebsEncryption :: Prelude.Maybe ReplicationConfigurationEbsEncryption,
    -- | Replication Configuration template associate default Application
    -- Migration Service Security group.
    associateDefaultSecurityGroup :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration template server Security Groups IDs.
    replicationServersSecurityGroupsIDs :: Prelude.Maybe [Prelude.Text],
    -- | Replication Configuration template EBS encryption key ARN.
    ebsEncryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration template use dedault large Staging Disk type.
    defaultLargeStagingDiskType :: Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType,
    -- | Replication Configuration template bandwidth throtting.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | Replication Configuration template data plane routing.
    dataPlaneRouting :: Prelude.Maybe ReplicationConfigurationDataPlaneRouting,
    -- | Replication Configuration template use Dedicated Replication Server.
    useDedicatedReplicationServer :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration template Tags.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Replication Configuration template template ID.
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
-- 'createPublicIP', 'replicationConfigurationTemplate_createPublicIP' - Replication Configuration template create Public IP.
--
-- 'stagingAreaTags', 'replicationConfigurationTemplate_stagingAreaTags' - Replication Configuration template Staging Area Tags.
--
-- 'arn', 'replicationConfigurationTemplate_arn' - Replication Configuration template ARN.
--
-- 'stagingAreaSubnetId', 'replicationConfigurationTemplate_stagingAreaSubnetId' - Replication Configuration template Staging Area subnet ID.
--
-- 'replicationServerInstanceType', 'replicationConfigurationTemplate_replicationServerInstanceType' - Replication Configuration template server instance type.
--
-- 'ebsEncryption', 'replicationConfigurationTemplate_ebsEncryption' - Replication Configuration template EBS encryption.
--
-- 'associateDefaultSecurityGroup', 'replicationConfigurationTemplate_associateDefaultSecurityGroup' - Replication Configuration template associate default Application
-- Migration Service Security group.
--
-- 'replicationServersSecurityGroupsIDs', 'replicationConfigurationTemplate_replicationServersSecurityGroupsIDs' - Replication Configuration template server Security Groups IDs.
--
-- 'ebsEncryptionKeyArn', 'replicationConfigurationTemplate_ebsEncryptionKeyArn' - Replication Configuration template EBS encryption key ARN.
--
-- 'defaultLargeStagingDiskType', 'replicationConfigurationTemplate_defaultLargeStagingDiskType' - Replication Configuration template use dedault large Staging Disk type.
--
-- 'bandwidthThrottling', 'replicationConfigurationTemplate_bandwidthThrottling' - Replication Configuration template bandwidth throtting.
--
-- 'dataPlaneRouting', 'replicationConfigurationTemplate_dataPlaneRouting' - Replication Configuration template data plane routing.
--
-- 'useDedicatedReplicationServer', 'replicationConfigurationTemplate_useDedicatedReplicationServer' - Replication Configuration template use Dedicated Replication Server.
--
-- 'tags', 'replicationConfigurationTemplate_tags' - Replication Configuration template Tags.
--
-- 'replicationConfigurationTemplateID', 'replicationConfigurationTemplate_replicationConfigurationTemplateID' - Replication Configuration template template ID.
newReplicationConfigurationTemplate ::
  -- | 'replicationConfigurationTemplateID'
  Prelude.Text ->
  ReplicationConfigurationTemplate
newReplicationConfigurationTemplate
  pReplicationConfigurationTemplateID_ =
    ReplicationConfigurationTemplate'
      { createPublicIP =
          Prelude.Nothing,
        stagingAreaTags = Prelude.Nothing,
        arn = Prelude.Nothing,
        stagingAreaSubnetId = Prelude.Nothing,
        replicationServerInstanceType =
          Prelude.Nothing,
        ebsEncryption = Prelude.Nothing,
        associateDefaultSecurityGroup =
          Prelude.Nothing,
        replicationServersSecurityGroupsIDs =
          Prelude.Nothing,
        ebsEncryptionKeyArn = Prelude.Nothing,
        defaultLargeStagingDiskType =
          Prelude.Nothing,
        bandwidthThrottling = Prelude.Nothing,
        dataPlaneRouting = Prelude.Nothing,
        useDedicatedReplicationServer =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        replicationConfigurationTemplateID =
          pReplicationConfigurationTemplateID_
      }

-- | Replication Configuration template create Public IP.
replicationConfigurationTemplate_createPublicIP :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
replicationConfigurationTemplate_createPublicIP = Lens.lens (\ReplicationConfigurationTemplate' {createPublicIP} -> createPublicIP) (\s@ReplicationConfigurationTemplate' {} a -> s {createPublicIP = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template Staging Area Tags.
replicationConfigurationTemplate_stagingAreaTags :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
replicationConfigurationTemplate_stagingAreaTags = Lens.lens (\ReplicationConfigurationTemplate' {stagingAreaTags} -> stagingAreaTags) (\s@ReplicationConfigurationTemplate' {} a -> s {stagingAreaTags = a} :: ReplicationConfigurationTemplate) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Replication Configuration template ARN.
replicationConfigurationTemplate_arn :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
replicationConfigurationTemplate_arn = Lens.lens (\ReplicationConfigurationTemplate' {arn} -> arn) (\s@ReplicationConfigurationTemplate' {} a -> s {arn = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template Staging Area subnet ID.
replicationConfigurationTemplate_stagingAreaSubnetId :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
replicationConfigurationTemplate_stagingAreaSubnetId = Lens.lens (\ReplicationConfigurationTemplate' {stagingAreaSubnetId} -> stagingAreaSubnetId) (\s@ReplicationConfigurationTemplate' {} a -> s {stagingAreaSubnetId = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template server instance type.
replicationConfigurationTemplate_replicationServerInstanceType :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
replicationConfigurationTemplate_replicationServerInstanceType = Lens.lens (\ReplicationConfigurationTemplate' {replicationServerInstanceType} -> replicationServerInstanceType) (\s@ReplicationConfigurationTemplate' {} a -> s {replicationServerInstanceType = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template EBS encryption.
replicationConfigurationTemplate_ebsEncryption :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationEbsEncryption)
replicationConfigurationTemplate_ebsEncryption = Lens.lens (\ReplicationConfigurationTemplate' {ebsEncryption} -> ebsEncryption) (\s@ReplicationConfigurationTemplate' {} a -> s {ebsEncryption = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template associate default Application
-- Migration Service Security group.
replicationConfigurationTemplate_associateDefaultSecurityGroup :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
replicationConfigurationTemplate_associateDefaultSecurityGroup = Lens.lens (\ReplicationConfigurationTemplate' {associateDefaultSecurityGroup} -> associateDefaultSecurityGroup) (\s@ReplicationConfigurationTemplate' {} a -> s {associateDefaultSecurityGroup = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template server Security Groups IDs.
replicationConfigurationTemplate_replicationServersSecurityGroupsIDs :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe [Prelude.Text])
replicationConfigurationTemplate_replicationServersSecurityGroupsIDs = Lens.lens (\ReplicationConfigurationTemplate' {replicationServersSecurityGroupsIDs} -> replicationServersSecurityGroupsIDs) (\s@ReplicationConfigurationTemplate' {} a -> s {replicationServersSecurityGroupsIDs = a} :: ReplicationConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Replication Configuration template EBS encryption key ARN.
replicationConfigurationTemplate_ebsEncryptionKeyArn :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Text)
replicationConfigurationTemplate_ebsEncryptionKeyArn = Lens.lens (\ReplicationConfigurationTemplate' {ebsEncryptionKeyArn} -> ebsEncryptionKeyArn) (\s@ReplicationConfigurationTemplate' {} a -> s {ebsEncryptionKeyArn = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template use dedault large Staging Disk type.
replicationConfigurationTemplate_defaultLargeStagingDiskType :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationDefaultLargeStagingDiskType)
replicationConfigurationTemplate_defaultLargeStagingDiskType = Lens.lens (\ReplicationConfigurationTemplate' {defaultLargeStagingDiskType} -> defaultLargeStagingDiskType) (\s@ReplicationConfigurationTemplate' {} a -> s {defaultLargeStagingDiskType = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template bandwidth throtting.
replicationConfigurationTemplate_bandwidthThrottling :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Natural)
replicationConfigurationTemplate_bandwidthThrottling = Lens.lens (\ReplicationConfigurationTemplate' {bandwidthThrottling} -> bandwidthThrottling) (\s@ReplicationConfigurationTemplate' {} a -> s {bandwidthThrottling = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template data plane routing.
replicationConfigurationTemplate_dataPlaneRouting :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe ReplicationConfigurationDataPlaneRouting)
replicationConfigurationTemplate_dataPlaneRouting = Lens.lens (\ReplicationConfigurationTemplate' {dataPlaneRouting} -> dataPlaneRouting) (\s@ReplicationConfigurationTemplate' {} a -> s {dataPlaneRouting = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template use Dedicated Replication Server.
replicationConfigurationTemplate_useDedicatedReplicationServer :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe Prelude.Bool)
replicationConfigurationTemplate_useDedicatedReplicationServer = Lens.lens (\ReplicationConfigurationTemplate' {useDedicatedReplicationServer} -> useDedicatedReplicationServer) (\s@ReplicationConfigurationTemplate' {} a -> s {useDedicatedReplicationServer = a} :: ReplicationConfigurationTemplate)

-- | Replication Configuration template Tags.
replicationConfigurationTemplate_tags :: Lens.Lens' ReplicationConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
replicationConfigurationTemplate_tags = Lens.lens (\ReplicationConfigurationTemplate' {tags} -> tags) (\s@ReplicationConfigurationTemplate' {} a -> s {tags = a} :: ReplicationConfigurationTemplate) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Replication Configuration template template ID.
replicationConfigurationTemplate_replicationConfigurationTemplateID :: Lens.Lens' ReplicationConfigurationTemplate Prelude.Text
replicationConfigurationTemplate_replicationConfigurationTemplateID = Lens.lens (\ReplicationConfigurationTemplate' {replicationConfigurationTemplateID} -> replicationConfigurationTemplateID) (\s@ReplicationConfigurationTemplate' {} a -> s {replicationConfigurationTemplateID = a} :: ReplicationConfigurationTemplate)

instance
  Core.FromJSON
    ReplicationConfigurationTemplate
  where
  parseJSON =
    Core.withObject
      "ReplicationConfigurationTemplate"
      ( \x ->
          ReplicationConfigurationTemplate'
            Prelude.<$> (x Core..:? "createPublicIP")
            Prelude.<*> ( x Core..:? "stagingAreaTags"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "stagingAreaSubnetId")
            Prelude.<*> (x Core..:? "replicationServerInstanceType")
            Prelude.<*> (x Core..:? "ebsEncryption")
            Prelude.<*> (x Core..:? "associateDefaultSecurityGroup")
            Prelude.<*> ( x Core..:? "replicationServersSecurityGroupsIDs"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ebsEncryptionKeyArn")
            Prelude.<*> (x Core..:? "defaultLargeStagingDiskType")
            Prelude.<*> (x Core..:? "bandwidthThrottling")
            Prelude.<*> (x Core..:? "dataPlaneRouting")
            Prelude.<*> (x Core..:? "useDedicatedReplicationServer")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "replicationConfigurationTemplateID")
      )

instance
  Prelude.Hashable
    ReplicationConfigurationTemplate

instance
  Prelude.NFData
    ReplicationConfigurationTemplate
