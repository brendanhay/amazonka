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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | The SSD IOPS configuration for the FSx for ONTAP file system.
    diskIopsConfiguration :: Prelude.Maybe DiskIopsConfiguration,
    -- | (Multi-AZ only) Specifies the IP address range in which the endpoints to
    -- access your file system will be created. By default in the Amazon FSx
    -- API, Amazon FSx selects an unused IP address range for you from the
    -- 198.19.* range. By default in the Amazon FSx console, Amazon FSx chooses
    -- the last 64 IP addresses from the VPC’s primary CIDR range to use as the
    -- endpoint IP address range for the file system. You can have overlapping
    -- endpoint IP addresses for file systems deployed in the same VPC\/route
    -- tables.
    endpointIpAddressRange :: Prelude.Maybe Prelude.Text,
    -- | The ONTAP administrative password for the @fsxadmin@ user with which you
    -- administer your file system using the NetApp ONTAP CLI and REST API.
    fsxAdminPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
    -- the subnet in which you want the preferred file server to be located.
    preferredSubnetId :: Prelude.Maybe Prelude.Text,
    -- | (Multi-AZ only) Specifies the virtual private cloud (VPC) route tables
    -- in which your file system\'s endpoints will be created. You should
    -- specify all VPC route tables associated with the subnets in which your
    -- clients are located. By default, Amazon FSx selects your VPC\'s default
    -- route table.
    routeTableIds :: Prelude.Maybe [Prelude.Text],
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
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
    -- Valid values are 128, 256, 512, 1024, 2048, and 4096 MBps.
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
-- 'automaticBackupRetentionDays', 'createFileSystemOntapConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'dailyAutomaticBackupStartTime', 'createFileSystemOntapConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'diskIopsConfiguration', 'createFileSystemOntapConfiguration_diskIopsConfiguration' - The SSD IOPS configuration for the FSx for ONTAP file system.
--
-- 'endpointIpAddressRange', 'createFileSystemOntapConfiguration_endpointIpAddressRange' - (Multi-AZ only) Specifies the IP address range in which the endpoints to
-- access your file system will be created. By default in the Amazon FSx
-- API, Amazon FSx selects an unused IP address range for you from the
-- 198.19.* range. By default in the Amazon FSx console, Amazon FSx chooses
-- the last 64 IP addresses from the VPC’s primary CIDR range to use as the
-- endpoint IP address range for the file system. You can have overlapping
-- endpoint IP addresses for file systems deployed in the same VPC\/route
-- tables.
--
-- 'fsxAdminPassword', 'createFileSystemOntapConfiguration_fsxAdminPassword' - The ONTAP administrative password for the @fsxadmin@ user with which you
-- administer your file system using the NetApp ONTAP CLI and REST API.
--
-- 'preferredSubnetId', 'createFileSystemOntapConfiguration_preferredSubnetId' - Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
-- the subnet in which you want the preferred file server to be located.
--
-- 'routeTableIds', 'createFileSystemOntapConfiguration_routeTableIds' - (Multi-AZ only) Specifies the virtual private cloud (VPC) route tables
-- in which your file system\'s endpoints will be created. You should
-- specify all VPC route tables associated with the subnets in which your
-- clients are located. By default, Amazon FSx selects your VPC\'s default
-- route table.
--
-- 'weeklyMaintenanceStartTime', 'createFileSystemOntapConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
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
-- Valid values are 128, 256, 512, 1024, 2048, and 4096 MBps.
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
      { automaticBackupRetentionDays =
          Prelude.Nothing,
        dailyAutomaticBackupStartTime =
          Prelude.Nothing,
        diskIopsConfiguration = Prelude.Nothing,
        endpointIpAddressRange =
          Prelude.Nothing,
        fsxAdminPassword = Prelude.Nothing,
        preferredSubnetId = Prelude.Nothing,
        routeTableIds = Prelude.Nothing,
        weeklyMaintenanceStartTime =
          Prelude.Nothing,
        deploymentType = pDeploymentType_,
        throughputCapacity =
          pThroughputCapacity_
      }

-- | Undocumented member.
createFileSystemOntapConfiguration_automaticBackupRetentionDays :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Natural)
createFileSystemOntapConfiguration_automaticBackupRetentionDays = Lens.lens (\CreateFileSystemOntapConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@CreateFileSystemOntapConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: CreateFileSystemOntapConfiguration)

-- | Undocumented member.
createFileSystemOntapConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\CreateFileSystemOntapConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@CreateFileSystemOntapConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: CreateFileSystemOntapConfiguration)

-- | The SSD IOPS configuration for the FSx for ONTAP file system.
createFileSystemOntapConfiguration_diskIopsConfiguration :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe DiskIopsConfiguration)
createFileSystemOntapConfiguration_diskIopsConfiguration = Lens.lens (\CreateFileSystemOntapConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@CreateFileSystemOntapConfiguration' {} a -> s {diskIopsConfiguration = a} :: CreateFileSystemOntapConfiguration)

-- | (Multi-AZ only) Specifies the IP address range in which the endpoints to
-- access your file system will be created. By default in the Amazon FSx
-- API, Amazon FSx selects an unused IP address range for you from the
-- 198.19.* range. By default in the Amazon FSx console, Amazon FSx chooses
-- the last 64 IP addresses from the VPC’s primary CIDR range to use as the
-- endpoint IP address range for the file system. You can have overlapping
-- endpoint IP addresses for file systems deployed in the same VPC\/route
-- tables.
createFileSystemOntapConfiguration_endpointIpAddressRange :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_endpointIpAddressRange = Lens.lens (\CreateFileSystemOntapConfiguration' {endpointIpAddressRange} -> endpointIpAddressRange) (\s@CreateFileSystemOntapConfiguration' {} a -> s {endpointIpAddressRange = a} :: CreateFileSystemOntapConfiguration)

-- | The ONTAP administrative password for the @fsxadmin@ user with which you
-- administer your file system using the NetApp ONTAP CLI and REST API.
createFileSystemOntapConfiguration_fsxAdminPassword :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_fsxAdminPassword = Lens.lens (\CreateFileSystemOntapConfiguration' {fsxAdminPassword} -> fsxAdminPassword) (\s@CreateFileSystemOntapConfiguration' {} a -> s {fsxAdminPassword = a} :: CreateFileSystemOntapConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
-- the subnet in which you want the preferred file server to be located.
createFileSystemOntapConfiguration_preferredSubnetId :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_preferredSubnetId = Lens.lens (\CreateFileSystemOntapConfiguration' {preferredSubnetId} -> preferredSubnetId) (\s@CreateFileSystemOntapConfiguration' {} a -> s {preferredSubnetId = a} :: CreateFileSystemOntapConfiguration)

-- | (Multi-AZ only) Specifies the virtual private cloud (VPC) route tables
-- in which your file system\'s endpoints will be created. You should
-- specify all VPC route tables associated with the subnets in which your
-- clients are located. By default, Amazon FSx selects your VPC\'s default
-- route table.
createFileSystemOntapConfiguration_routeTableIds :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe [Prelude.Text])
createFileSystemOntapConfiguration_routeTableIds = Lens.lens (\CreateFileSystemOntapConfiguration' {routeTableIds} -> routeTableIds) (\s@CreateFileSystemOntapConfiguration' {} a -> s {routeTableIds = a} :: CreateFileSystemOntapConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createFileSystemOntapConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' CreateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemOntapConfiguration_weeklyMaintenanceStartTime = Lens.lens (\CreateFileSystemOntapConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@CreateFileSystemOntapConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: CreateFileSystemOntapConfiguration)

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
-- Valid values are 128, 256, 512, 1024, 2048, and 4096 MBps.
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
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` diskIopsConfiguration
        `Prelude.hashWithSalt` endpointIpAddressRange
        `Prelude.hashWithSalt` fsxAdminPassword
        `Prelude.hashWithSalt` preferredSubnetId
        `Prelude.hashWithSalt` routeTableIds
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` throughputCapacity

instance
  Prelude.NFData
    CreateFileSystemOntapConfiguration
  where
  rnf CreateFileSystemOntapConfiguration' {..} =
    Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf diskIopsConfiguration
      `Prelude.seq` Prelude.rnf endpointIpAddressRange
      `Prelude.seq` Prelude.rnf fsxAdminPassword
      `Prelude.seq` Prelude.rnf preferredSubnetId
      `Prelude.seq` Prelude.rnf routeTableIds
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf throughputCapacity

instance
  Data.ToJSON
    CreateFileSystemOntapConfiguration
  where
  toJSON CreateFileSystemOntapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutomaticBackupRetentionDays" Data..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("DailyAutomaticBackupStartTime" Data..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("DiskIopsConfiguration" Data..=)
              Prelude.<$> diskIopsConfiguration,
            ("EndpointIpAddressRange" Data..=)
              Prelude.<$> endpointIpAddressRange,
            ("FsxAdminPassword" Data..=)
              Prelude.<$> fsxAdminPassword,
            ("PreferredSubnetId" Data..=)
              Prelude.<$> preferredSubnetId,
            ("RouteTableIds" Data..=) Prelude.<$> routeTableIds,
            ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime,
            Prelude.Just
              ("DeploymentType" Data..= deploymentType),
            Prelude.Just
              ("ThroughputCapacity" Data..= throughputCapacity)
          ]
      )
