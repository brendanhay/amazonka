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
-- Module      : Amazonka.FSx.Types.OntapFileSystemConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OntapFileSystemConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DiskIopsConfiguration
import Amazonka.FSx.Types.FileSystemEndpoints
import Amazonka.FSx.Types.OntapDeploymentType
import qualified Amazonka.Prelude as Prelude

-- | Configuration for the FSx for NetApp ONTAP file system.
--
-- /See:/ 'newOntapFileSystemConfiguration' smart constructor.
data OntapFileSystemConfiguration = OntapFileSystemConfiguration'
  { automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the FSx for ONTAP file system deployment type in use in the
    -- file system.
    --
    -- -   @MULTI_AZ_1@ - (Default) A high availability file system configured
    --     for Multi-AZ redundancy to tolerate temporary Availability Zone (AZ)
    --     unavailability.
    --
    -- -   @SINGLE_AZ_1@ - A file system configured for Single-AZ redundancy.
    --
    -- For information about the use cases for Multi-AZ and Single-AZ
    -- deployments, refer to
    -- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-multiAZ.html Choosing Multi-AZ or Single-AZ file system deployment>.
    deploymentType :: Prelude.Maybe OntapDeploymentType,
    -- | The SSD IOPS configuration for the ONTAP file system, specifying the
    -- number of provisioned IOPS and the provision mode.
    diskIopsConfiguration :: Prelude.Maybe DiskIopsConfiguration,
    -- | (Multi-AZ only) The IP address range in which the endpoints to access
    -- your file system are created.
    --
    -- The Endpoint IP address range you select for your file system must exist
    -- outside the VPC\'s CIDR range and must be at least \/30 or larger. If
    -- you do not specify this optional parameter, Amazon FSx will
    -- automatically select a CIDR block for you.
    endpointIpAddressRange :: Prelude.Maybe Prelude.Text,
    -- | The @Management@ and @Intercluster@ endpoints that are used to access
    -- data or to manage the file system using the NetApp ONTAP CLI, REST API,
    -- or NetApp SnapMirror.
    endpoints :: Prelude.Maybe FileSystemEndpoints,
    preferredSubnetId :: Prelude.Maybe Prelude.Text,
    -- | (Multi-AZ only) The VPC route tables in which your file system\'s
    -- endpoints are created.
    routeTableIds :: Prelude.Maybe [Prelude.Text],
    throughputCapacity :: Prelude.Maybe Prelude.Natural,
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OntapFileSystemConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticBackupRetentionDays', 'ontapFileSystemConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'dailyAutomaticBackupStartTime', 'ontapFileSystemConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'deploymentType', 'ontapFileSystemConfiguration_deploymentType' - Specifies the FSx for ONTAP file system deployment type in use in the
-- file system.
--
-- -   @MULTI_AZ_1@ - (Default) A high availability file system configured
--     for Multi-AZ redundancy to tolerate temporary Availability Zone (AZ)
--     unavailability.
--
-- -   @SINGLE_AZ_1@ - A file system configured for Single-AZ redundancy.
--
-- For information about the use cases for Multi-AZ and Single-AZ
-- deployments, refer to
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-multiAZ.html Choosing Multi-AZ or Single-AZ file system deployment>.
--
-- 'diskIopsConfiguration', 'ontapFileSystemConfiguration_diskIopsConfiguration' - The SSD IOPS configuration for the ONTAP file system, specifying the
-- number of provisioned IOPS and the provision mode.
--
-- 'endpointIpAddressRange', 'ontapFileSystemConfiguration_endpointIpAddressRange' - (Multi-AZ only) The IP address range in which the endpoints to access
-- your file system are created.
--
-- The Endpoint IP address range you select for your file system must exist
-- outside the VPC\'s CIDR range and must be at least \/30 or larger. If
-- you do not specify this optional parameter, Amazon FSx will
-- automatically select a CIDR block for you.
--
-- 'endpoints', 'ontapFileSystemConfiguration_endpoints' - The @Management@ and @Intercluster@ endpoints that are used to access
-- data or to manage the file system using the NetApp ONTAP CLI, REST API,
-- or NetApp SnapMirror.
--
-- 'preferredSubnetId', 'ontapFileSystemConfiguration_preferredSubnetId' - Undocumented member.
--
-- 'routeTableIds', 'ontapFileSystemConfiguration_routeTableIds' - (Multi-AZ only) The VPC route tables in which your file system\'s
-- endpoints are created.
--
-- 'throughputCapacity', 'ontapFileSystemConfiguration_throughputCapacity' - Undocumented member.
--
-- 'weeklyMaintenanceStartTime', 'ontapFileSystemConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
newOntapFileSystemConfiguration ::
  OntapFileSystemConfiguration
newOntapFileSystemConfiguration =
  OntapFileSystemConfiguration'
    { automaticBackupRetentionDays =
        Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      diskIopsConfiguration = Prelude.Nothing,
      endpointIpAddressRange = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      preferredSubnetId = Prelude.Nothing,
      routeTableIds = Prelude.Nothing,
      throughputCapacity = Prelude.Nothing,
      weeklyMaintenanceStartTime = Prelude.Nothing
    }

-- | Undocumented member.
ontapFileSystemConfiguration_automaticBackupRetentionDays :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
ontapFileSystemConfiguration_automaticBackupRetentionDays = Lens.lens (\OntapFileSystemConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@OntapFileSystemConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: OntapFileSystemConfiguration)

-- | Undocumented member.
ontapFileSystemConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe Prelude.Text)
ontapFileSystemConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\OntapFileSystemConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@OntapFileSystemConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: OntapFileSystemConfiguration)

-- | Specifies the FSx for ONTAP file system deployment type in use in the
-- file system.
--
-- -   @MULTI_AZ_1@ - (Default) A high availability file system configured
--     for Multi-AZ redundancy to tolerate temporary Availability Zone (AZ)
--     unavailability.
--
-- -   @SINGLE_AZ_1@ - A file system configured for Single-AZ redundancy.
--
-- For information about the use cases for Multi-AZ and Single-AZ
-- deployments, refer to
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-multiAZ.html Choosing Multi-AZ or Single-AZ file system deployment>.
ontapFileSystemConfiguration_deploymentType :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe OntapDeploymentType)
ontapFileSystemConfiguration_deploymentType = Lens.lens (\OntapFileSystemConfiguration' {deploymentType} -> deploymentType) (\s@OntapFileSystemConfiguration' {} a -> s {deploymentType = a} :: OntapFileSystemConfiguration)

-- | The SSD IOPS configuration for the ONTAP file system, specifying the
-- number of provisioned IOPS and the provision mode.
ontapFileSystemConfiguration_diskIopsConfiguration :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe DiskIopsConfiguration)
ontapFileSystemConfiguration_diskIopsConfiguration = Lens.lens (\OntapFileSystemConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@OntapFileSystemConfiguration' {} a -> s {diskIopsConfiguration = a} :: OntapFileSystemConfiguration)

-- | (Multi-AZ only) The IP address range in which the endpoints to access
-- your file system are created.
--
-- The Endpoint IP address range you select for your file system must exist
-- outside the VPC\'s CIDR range and must be at least \/30 or larger. If
-- you do not specify this optional parameter, Amazon FSx will
-- automatically select a CIDR block for you.
ontapFileSystemConfiguration_endpointIpAddressRange :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe Prelude.Text)
ontapFileSystemConfiguration_endpointIpAddressRange = Lens.lens (\OntapFileSystemConfiguration' {endpointIpAddressRange} -> endpointIpAddressRange) (\s@OntapFileSystemConfiguration' {} a -> s {endpointIpAddressRange = a} :: OntapFileSystemConfiguration)

-- | The @Management@ and @Intercluster@ endpoints that are used to access
-- data or to manage the file system using the NetApp ONTAP CLI, REST API,
-- or NetApp SnapMirror.
ontapFileSystemConfiguration_endpoints :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe FileSystemEndpoints)
ontapFileSystemConfiguration_endpoints = Lens.lens (\OntapFileSystemConfiguration' {endpoints} -> endpoints) (\s@OntapFileSystemConfiguration' {} a -> s {endpoints = a} :: OntapFileSystemConfiguration)

-- | Undocumented member.
ontapFileSystemConfiguration_preferredSubnetId :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe Prelude.Text)
ontapFileSystemConfiguration_preferredSubnetId = Lens.lens (\OntapFileSystemConfiguration' {preferredSubnetId} -> preferredSubnetId) (\s@OntapFileSystemConfiguration' {} a -> s {preferredSubnetId = a} :: OntapFileSystemConfiguration)

-- | (Multi-AZ only) The VPC route tables in which your file system\'s
-- endpoints are created.
ontapFileSystemConfiguration_routeTableIds :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe [Prelude.Text])
ontapFileSystemConfiguration_routeTableIds = Lens.lens (\OntapFileSystemConfiguration' {routeTableIds} -> routeTableIds) (\s@OntapFileSystemConfiguration' {} a -> s {routeTableIds = a} :: OntapFileSystemConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
ontapFileSystemConfiguration_throughputCapacity :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
ontapFileSystemConfiguration_throughputCapacity = Lens.lens (\OntapFileSystemConfiguration' {throughputCapacity} -> throughputCapacity) (\s@OntapFileSystemConfiguration' {} a -> s {throughputCapacity = a} :: OntapFileSystemConfiguration)

-- | Undocumented member.
ontapFileSystemConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' OntapFileSystemConfiguration (Prelude.Maybe Prelude.Text)
ontapFileSystemConfiguration_weeklyMaintenanceStartTime = Lens.lens (\OntapFileSystemConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@OntapFileSystemConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: OntapFileSystemConfiguration)

instance Data.FromJSON OntapFileSystemConfiguration where
  parseJSON =
    Data.withObject
      "OntapFileSystemConfiguration"
      ( \x ->
          OntapFileSystemConfiguration'
            Prelude.<$> (x Data..:? "AutomaticBackupRetentionDays")
            Prelude.<*> (x Data..:? "DailyAutomaticBackupStartTime")
            Prelude.<*> (x Data..:? "DeploymentType")
            Prelude.<*> (x Data..:? "DiskIopsConfiguration")
            Prelude.<*> (x Data..:? "EndpointIpAddressRange")
            Prelude.<*> (x Data..:? "Endpoints")
            Prelude.<*> (x Data..:? "PreferredSubnetId")
            Prelude.<*> (x Data..:? "RouteTableIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ThroughputCapacity")
            Prelude.<*> (x Data..:? "WeeklyMaintenanceStartTime")
      )

instance
  Prelude.Hashable
    OntapFileSystemConfiguration
  where
  hashWithSalt _salt OntapFileSystemConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` automaticBackupRetentionDays
      `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` diskIopsConfiguration
      `Prelude.hashWithSalt` endpointIpAddressRange
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` preferredSubnetId
      `Prelude.hashWithSalt` routeTableIds
      `Prelude.hashWithSalt` throughputCapacity
      `Prelude.hashWithSalt` weeklyMaintenanceStartTime

instance Prelude.NFData OntapFileSystemConfiguration where
  rnf OntapFileSystemConfiguration' {..} =
    Prelude.rnf automaticBackupRetentionDays `Prelude.seq`
      Prelude.rnf dailyAutomaticBackupStartTime `Prelude.seq`
        Prelude.rnf deploymentType `Prelude.seq`
          Prelude.rnf diskIopsConfiguration `Prelude.seq`
            Prelude.rnf endpointIpAddressRange `Prelude.seq`
              Prelude.rnf endpoints `Prelude.seq`
                Prelude.rnf preferredSubnetId `Prelude.seq`
                  Prelude.rnf routeTableIds `Prelude.seq`
                    Prelude.rnf throughputCapacity `Prelude.seq`
                      Prelude.rnf weeklyMaintenanceStartTime
