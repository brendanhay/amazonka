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
-- Module      : Amazonka.FSx.Types.CreateFileSystemOntapConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateFileSystemOntapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DiskIopsConfiguration
import Amazonka.FSx.Types.OntapDeploymentType
import qualified Amazonka.Prelude as Prelude

-- | The ONTAP configuration properties of the FSx for ONTAP file system that
-- you are creating.
--
-- /See:/ 'newCreateFileSystemOntapConfiguration' smart constructor.
data CreateFileSystemOntapConfiguration = CreateFileSystemOntapConfiguration'
  { weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | (Multi-AZ only) Specifies the virtual private cloud (VPC) route tables
    -- in which your file system\'s endpoints will be created. You should
    -- specify all VPC route tables associated with the subnets in which your
    -- clients are located. By default, Amazon FSx selects your VPC\'s default
    -- route table.
    routeTableIds :: Prelude.Maybe [Prelude.Text],
    -- | The ONTAP administrative password for the @fsxadmin@ user with which you
    -- administer your file system using the NetApp ONTAP CLI and REST API.
    fsxAdminPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | (Multi-AZ only) Specifies the IP address range in which the endpoints to
    -- access your file system will be created. By default, Amazon FSx selects
    -- an unused IP address range for you from the 198.19.* range.
    --
    -- The Endpoint IP address range you select for your file system must exist
    -- outside the VPC\'s CIDR range and must be at least \/30 or larger.
    endpointIpAddressRange :: Prelude.Maybe Prelude.Text,
    -- | The SSD IOPS configuration for the FSx for ONTAP file system.
    diskIopsConfiguration :: Prelude.Maybe DiskIopsConfiguration,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
    -- the subnet in which you want the preferred file server to be located.
    preferredSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the FSx for ONTAP file system deployment type to use in
    -- creating the file system.
    --
    -- -   @MULTI_AZ_1@ - (Default) A high availability file system configured
    --     for Multi-AZ redundancy to tolerate temporary Availability Zone (AZ)
    --     unavailability.
    --
    -- -   @SINGLE_AZ_1@ - A file system configured for Single-AZ redundancy.
    --
    -- For information about the use cases for Multi-AZ and Single-AZ
    -- deployments, refer to
    -- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-AZ.html Choosing a file system deployment type>.
    deploymentType :: OntapDeploymentType,
    -- | Sets the throughput capacity for the file system that you\'re creating.
    -- Valid values are 128, 256, 512, 1024, and 2048 MBps.
    throughputCapacity :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystemOntapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weeklyMaintenanceStartTime', 'createFileSystemOntapConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
--
-- 'automaticBackupRetentionDays', 'createFileSystemOntapConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'routeTableIds', 'createFileSystemOntapConfiguration_routeTableIds' - (Multi-AZ only) Specifies the virtual private cloud (VPC) route tables
-- in which your file system\'s endpoints will be created. You should
-- specify all VPC route tables associated with the subnets in which your
-- clients are located. By default, Amazon FSx selects your VPC\'s default
-- route table.
--
-- 'fsxAdminPassword', 'createFileSystemOntapConfiguration_fsxAdminPassword' - The ONTAP administrative password for the @fsxadmin@ user with which you
-- administer your file system using the NetApp ONTAP CLI and REST API.
--
-- 'endpointIpAddressRange', 'createFileSystemOntapConfiguration_endpointIpAddressRange' - (Multi-AZ only) Specifies the IP address range in which the endpoints to
-- access your file system will be created. By default, Amazon FSx selects
-- an unused IP address range for you from the 198.19.* range.
--
-- The Endpoint IP address range you select for your file system must exist
-- outside the VPC\'s CIDR range and must be at least \/30 or larger.
--
-- 'diskIopsConfiguration', 'createFileSystemOntapConfiguration_diskIopsConfiguration' - The SSD IOPS configuration for the FSx for ONTAP file system.
--
-- 'dailyAutomaticBackupStartTime', 'createFileSystemOntapConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'preferredSubnetId', 'createFileSystemOntapConfiguration_preferredSubnetId' - Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
-- the subnet in which you want the preferred file server to be located.
--
-- 'deploymentType', 'createFileSystemOntapConfiguration_deploymentType' - Specifies the FSx for ONTAP file system deployment type to use in
-- creating the file system.
--
-- -   @MULTI_AZ_1@ - (Default) A high availability file system configured
--     for Multi-AZ redundancy to tolerate temporary Availability Zone (AZ)
--     unavailability.
--
-- -   @SINGLE_AZ_1@ - A file system configured for Single-AZ redundancy.
--
-- For information about the use cases for Multi-AZ and Single-AZ
-- deployments, refer to
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-AZ.html Choosing a file system deployment type>.
--
-- 'throughputCapacity', 'createFileSystemOntapConfiguration_throughputCapacity' - Sets the throughput capacity for the file system that you\'re creating.
-- Valid values are 128, 256, 512, 1024, and 2048 MBps.
newCreateFileSystemOntapConfiguration ::
  -- | 'deploymentType'
  OntapDeploymentType ->
  -- | 'throughputCapacity'
  Prelude.Natural ->
  CreateFileSystemOntapConfiguration
newCreateFileSystemOntapConfiguration
  pDeploymentType_
  pThroughputCapacity_ =
    CreateFileSystemOntapConfiguration'
      { weeklyMaintenanceStartTime =
          Prelude.Nothing,
        automaticBackupRetentionDays =
          Prelude.Nothing,
        routeTableIds = Prelude.Nothing,
        fsxAdminPassword = Prelude.Nothing,
        endpointIpAddressRange =
          Prelude.Nothing,
        diskIopsConfiguration = Prelude.Nothing,
        dailyAutomaticBackupStartTime =
          Prelude.Nothing,
        preferredSubnetId = Prelude.Nothing,
        deploymentType = pDeploymentType_,
        throughputCapacity =
          pThroughputCapacity_
      }

-- | Undocumented member.
createFileSystemOntapConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_weeklyMaintenanceStartTime = Lens.lens (\CreateFileSystemOntapConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@CreateFileSystemOntapConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: CreateFileSystemOntapConfiguration)

-- | Undocumented member.
createFileSystemOntapConfiguration_automaticBackupRetentionDays :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Natural)
createFileSystemOntapConfiguration_automaticBackupRetentionDays = Lens.lens (\CreateFileSystemOntapConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@CreateFileSystemOntapConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: CreateFileSystemOntapConfiguration)

-- | (Multi-AZ only) Specifies the virtual private cloud (VPC) route tables
-- in which your file system\'s endpoints will be created. You should
-- specify all VPC route tables associated with the subnets in which your
-- clients are located. By default, Amazon FSx selects your VPC\'s default
-- route table.
createFileSystemOntapConfiguration_routeTableIds :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe [Prelude.Text])
createFileSystemOntapConfiguration_routeTableIds = Lens.lens (\CreateFileSystemOntapConfiguration' {routeTableIds} -> routeTableIds) (\s@CreateFileSystemOntapConfiguration' {} a -> s {routeTableIds = a} :: CreateFileSystemOntapConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ONTAP administrative password for the @fsxadmin@ user with which you
-- administer your file system using the NetApp ONTAP CLI and REST API.
createFileSystemOntapConfiguration_fsxAdminPassword :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_fsxAdminPassword = Lens.lens (\CreateFileSystemOntapConfiguration' {fsxAdminPassword} -> fsxAdminPassword) (\s@CreateFileSystemOntapConfiguration' {} a -> s {fsxAdminPassword = a} :: CreateFileSystemOntapConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | (Multi-AZ only) Specifies the IP address range in which the endpoints to
-- access your file system will be created. By default, Amazon FSx selects
-- an unused IP address range for you from the 198.19.* range.
--
-- The Endpoint IP address range you select for your file system must exist
-- outside the VPC\'s CIDR range and must be at least \/30 or larger.
createFileSystemOntapConfiguration_endpointIpAddressRange :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_endpointIpAddressRange = Lens.lens (\CreateFileSystemOntapConfiguration' {endpointIpAddressRange} -> endpointIpAddressRange) (\s@CreateFileSystemOntapConfiguration' {} a -> s {endpointIpAddressRange = a} :: CreateFileSystemOntapConfiguration)

-- | The SSD IOPS configuration for the FSx for ONTAP file system.
createFileSystemOntapConfiguration_diskIopsConfiguration :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe DiskIopsConfiguration)
createFileSystemOntapConfiguration_diskIopsConfiguration = Lens.lens (\CreateFileSystemOntapConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@CreateFileSystemOntapConfiguration' {} a -> s {diskIopsConfiguration = a} :: CreateFileSystemOntapConfiguration)

-- | Undocumented member.
createFileSystemOntapConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\CreateFileSystemOntapConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@CreateFileSystemOntapConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: CreateFileSystemOntapConfiguration)

-- | Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
-- the subnet in which you want the preferred file server to be located.
createFileSystemOntapConfiguration_preferredSubnetId :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_preferredSubnetId = Lens.lens (\CreateFileSystemOntapConfiguration' {preferredSubnetId} -> preferredSubnetId) (\s@CreateFileSystemOntapConfiguration' {} a -> s {preferredSubnetId = a} :: CreateFileSystemOntapConfiguration)

-- | Specifies the FSx for ONTAP file system deployment type to use in
-- creating the file system.
--
-- -   @MULTI_AZ_1@ - (Default) A high availability file system configured
--     for Multi-AZ redundancy to tolerate temporary Availability Zone (AZ)
--     unavailability.
--
-- -   @SINGLE_AZ_1@ - A file system configured for Single-AZ redundancy.
--
-- For information about the use cases for Multi-AZ and Single-AZ
-- deployments, refer to
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-AZ.html Choosing a file system deployment type>.
createFileSystemOntapConfiguration_deploymentType :: Lens.Lens' CreateFileSystemOntapConfiguration OntapDeploymentType
createFileSystemOntapConfiguration_deploymentType = Lens.lens (\CreateFileSystemOntapConfiguration' {deploymentType} -> deploymentType) (\s@CreateFileSystemOntapConfiguration' {} a -> s {deploymentType = a} :: CreateFileSystemOntapConfiguration)

-- | Sets the throughput capacity for the file system that you\'re creating.
-- Valid values are 128, 256, 512, 1024, and 2048 MBps.
createFileSystemOntapConfiguration_throughputCapacity :: Lens.Lens' CreateFileSystemOntapConfiguration Prelude.Natural
createFileSystemOntapConfiguration_throughputCapacity = Lens.lens (\CreateFileSystemOntapConfiguration' {throughputCapacity} -> throughputCapacity) (\s@CreateFileSystemOntapConfiguration' {} a -> s {throughputCapacity = a} :: CreateFileSystemOntapConfiguration)

instance
  Prelude.Hashable
    CreateFileSystemOntapConfiguration
  where
  hashWithSalt
    _salt
    CreateFileSystemOntapConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` routeTableIds
        `Prelude.hashWithSalt` fsxAdminPassword
        `Prelude.hashWithSalt` endpointIpAddressRange
        `Prelude.hashWithSalt` diskIopsConfiguration
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` preferredSubnetId
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` throughputCapacity

instance
  Prelude.NFData
    CreateFileSystemOntapConfiguration
  where
  rnf CreateFileSystemOntapConfiguration' {..} =
    Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf routeTableIds
      `Prelude.seq` Prelude.rnf fsxAdminPassword
      `Prelude.seq` Prelude.rnf endpointIpAddressRange
      `Prelude.seq` Prelude.rnf diskIopsConfiguration
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf preferredSubnetId
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf throughputCapacity

instance
  Data.ToJSON
    CreateFileSystemOntapConfiguration
  where
  toJSON CreateFileSystemOntapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime,
            ("AutomaticBackupRetentionDays" Data..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("RouteTableIds" Data..=) Prelude.<$> routeTableIds,
            ("FsxAdminPassword" Data..=)
              Prelude.<$> fsxAdminPassword,
            ("EndpointIpAddressRange" Data..=)
              Prelude.<$> endpointIpAddressRange,
            ("DiskIopsConfiguration" Data..=)
              Prelude.<$> diskIopsConfiguration,
            ("DailyAutomaticBackupStartTime" Data..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("PreferredSubnetId" Data..=)
              Prelude.<$> preferredSubnetId,
            Prelude.Just
              ("DeploymentType" Data..= deploymentType),
            Prelude.Just
              ("ThroughputCapacity" Data..= throughputCapacity)
          ]
      )
