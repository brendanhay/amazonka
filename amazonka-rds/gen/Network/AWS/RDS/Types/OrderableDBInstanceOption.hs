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
-- Module      : Network.AWS.RDS.Types.OrderableDBInstanceOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OrderableDBInstanceOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.AvailabilityZone
import Network.AWS.RDS.Types.AvailableProcessorFeature

-- | Contains a list of available options for a DB instance.
--
-- This data type is used as a response element in the
-- @DescribeOrderableDBInstanceOptions@ action.
--
-- /See:/ 'newOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { -- | Minimum total provisioned IOPS for a DB instance.
    minIopsPerDbInstance :: Core.Maybe Core.Int,
    -- | A list of Availability Zones for a DB instance.
    availabilityZones :: Core.Maybe [AvailabilityZone],
    -- | Whether a DB instance supports Kerberos Authentication.
    supportsKerberosAuthentication :: Core.Maybe Core.Bool,
    -- | Indicates whether a DB instance supports provisioned IOPS.
    supportsIops :: Core.Maybe Core.Bool,
    -- | Indicates the storage type for a DB instance.
    storageType :: Core.Maybe Core.Text,
    -- | Indicates whether a DB instance supports Enhanced Monitoring at
    -- intervals from 1 to 60 seconds.
    supportsEnhancedMonitoring :: Core.Maybe Core.Bool,
    -- | A list of the available processor features for the DB instance class of
    -- a DB instance.
    availableProcessorFeatures :: Core.Maybe [AvailableProcessorFeature],
    -- | Maximum storage size for a DB instance.
    maxStorageSize :: Core.Maybe Core.Int,
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Core.Maybe [Core.Text],
    -- | Maximum provisioned IOPS per GiB for a DB instance.
    maxIopsPerGib :: Core.Maybe Core.Double,
    -- | Indicates whether a DB instance supports encrypted storage.
    supportsStorageEncryption :: Core.Maybe Core.Bool,
    -- | Indicates whether a DB instance is Multi-AZ capable.
    multiAZCapable :: Core.Maybe Core.Bool,
    -- | Whether Amazon RDS can automatically scale storage for DB instances that
    -- use the specified DB instance class.
    supportsStorageAutoscaling :: Core.Maybe Core.Bool,
    -- | The engine version of a DB instance.
    engineVersion :: Core.Maybe Core.Text,
    -- | Minimum provisioned IOPS per GiB for a DB instance.
    minIopsPerGib :: Core.Maybe Core.Double,
    -- | Indicates whether a DB instance supports IAM database authentication.
    supportsIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | The license model for a DB instance.
    licenseModel :: Core.Maybe Core.Text,
    -- | A value that indicates whether you can use Aurora global databases with
    -- a specific combination of other DB engine attributes.
    supportsGlobalDatabases :: Core.Maybe Core.Bool,
    -- | True if a DB instance supports Performance Insights, otherwise false.
    supportsPerformanceInsights :: Core.Maybe Core.Bool,
    -- | Maximum total provisioned IOPS for a DB instance.
    maxIopsPerDbInstance :: Core.Maybe Core.Int,
    -- | The DB instance class for a DB instance.
    dbInstanceClass :: Core.Maybe Core.Text,
    -- | Whether a DB instance supports RDS on Outposts.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts>
    -- in the /Amazon RDS User Guide./
    outpostCapable :: Core.Maybe Core.Bool,
    -- | The engine type of a DB instance.
    engine :: Core.Maybe Core.Text,
    -- | Minimum storage size for a DB instance.
    minStorageSize :: Core.Maybe Core.Int,
    -- | The Availability Zone group for a DB instance.
    availabilityZoneGroup :: Core.Maybe Core.Text,
    -- | Indicates whether a DB instance is in a VPC.
    vpc :: Core.Maybe Core.Bool,
    -- | Indicates whether a DB instance can have a read replica.
    readReplicaCapable :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OrderableDBInstanceOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minIopsPerDbInstance', 'orderableDBInstanceOption_minIopsPerDbInstance' - Minimum total provisioned IOPS for a DB instance.
--
-- 'availabilityZones', 'orderableDBInstanceOption_availabilityZones' - A list of Availability Zones for a DB instance.
--
-- 'supportsKerberosAuthentication', 'orderableDBInstanceOption_supportsKerberosAuthentication' - Whether a DB instance supports Kerberos Authentication.
--
-- 'supportsIops', 'orderableDBInstanceOption_supportsIops' - Indicates whether a DB instance supports provisioned IOPS.
--
-- 'storageType', 'orderableDBInstanceOption_storageType' - Indicates the storage type for a DB instance.
--
-- 'supportsEnhancedMonitoring', 'orderableDBInstanceOption_supportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
--
-- 'availableProcessorFeatures', 'orderableDBInstanceOption_availableProcessorFeatures' - A list of the available processor features for the DB instance class of
-- a DB instance.
--
-- 'maxStorageSize', 'orderableDBInstanceOption_maxStorageSize' - Maximum storage size for a DB instance.
--
-- 'supportedEngineModes', 'orderableDBInstanceOption_supportedEngineModes' - A list of the supported DB engine modes.
--
-- 'maxIopsPerGib', 'orderableDBInstanceOption_maxIopsPerGib' - Maximum provisioned IOPS per GiB for a DB instance.
--
-- 'supportsStorageEncryption', 'orderableDBInstanceOption_supportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
--
-- 'multiAZCapable', 'orderableDBInstanceOption_multiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
--
-- 'supportsStorageAutoscaling', 'orderableDBInstanceOption_supportsStorageAutoscaling' - Whether Amazon RDS can automatically scale storage for DB instances that
-- use the specified DB instance class.
--
-- 'engineVersion', 'orderableDBInstanceOption_engineVersion' - The engine version of a DB instance.
--
-- 'minIopsPerGib', 'orderableDBInstanceOption_minIopsPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
--
-- 'supportsIAMDatabaseAuthentication', 'orderableDBInstanceOption_supportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
--
-- 'licenseModel', 'orderableDBInstanceOption_licenseModel' - The license model for a DB instance.
--
-- 'supportsGlobalDatabases', 'orderableDBInstanceOption_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- a specific combination of other DB engine attributes.
--
-- 'supportsPerformanceInsights', 'orderableDBInstanceOption_supportsPerformanceInsights' - True if a DB instance supports Performance Insights, otherwise false.
--
-- 'maxIopsPerDbInstance', 'orderableDBInstanceOption_maxIopsPerDbInstance' - Maximum total provisioned IOPS for a DB instance.
--
-- 'dbInstanceClass', 'orderableDBInstanceOption_dbInstanceClass' - The DB instance class for a DB instance.
--
-- 'outpostCapable', 'orderableDBInstanceOption_outpostCapable' - Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide./
--
-- 'engine', 'orderableDBInstanceOption_engine' - The engine type of a DB instance.
--
-- 'minStorageSize', 'orderableDBInstanceOption_minStorageSize' - Minimum storage size for a DB instance.
--
-- 'availabilityZoneGroup', 'orderableDBInstanceOption_availabilityZoneGroup' - The Availability Zone group for a DB instance.
--
-- 'vpc', 'orderableDBInstanceOption_vpc' - Indicates whether a DB instance is in a VPC.
--
-- 'readReplicaCapable', 'orderableDBInstanceOption_readReplicaCapable' - Indicates whether a DB instance can have a read replica.
newOrderableDBInstanceOption ::
  OrderableDBInstanceOption
newOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { minIopsPerDbInstance =
        Core.Nothing,
      availabilityZones = Core.Nothing,
      supportsKerberosAuthentication = Core.Nothing,
      supportsIops = Core.Nothing,
      storageType = Core.Nothing,
      supportsEnhancedMonitoring = Core.Nothing,
      availableProcessorFeatures = Core.Nothing,
      maxStorageSize = Core.Nothing,
      supportedEngineModes = Core.Nothing,
      maxIopsPerGib = Core.Nothing,
      supportsStorageEncryption = Core.Nothing,
      multiAZCapable = Core.Nothing,
      supportsStorageAutoscaling = Core.Nothing,
      engineVersion = Core.Nothing,
      minIopsPerGib = Core.Nothing,
      supportsIAMDatabaseAuthentication = Core.Nothing,
      licenseModel = Core.Nothing,
      supportsGlobalDatabases = Core.Nothing,
      supportsPerformanceInsights = Core.Nothing,
      maxIopsPerDbInstance = Core.Nothing,
      dbInstanceClass = Core.Nothing,
      outpostCapable = Core.Nothing,
      engine = Core.Nothing,
      minStorageSize = Core.Nothing,
      availabilityZoneGroup = Core.Nothing,
      vpc = Core.Nothing,
      readReplicaCapable = Core.Nothing
    }

-- | Minimum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_minIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
orderableDBInstanceOption_minIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {minIopsPerDbInstance} -> minIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | A list of Availability Zones for a DB instance.
orderableDBInstanceOption_availabilityZones :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [AvailabilityZone])
orderableDBInstanceOption_availabilityZones = Lens.lens (\OrderableDBInstanceOption' {availabilityZones} -> availabilityZones) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZones = a} :: OrderableDBInstanceOption) Core.. Lens.mapping Lens._Coerce

-- | Whether a DB instance supports Kerberos Authentication.
orderableDBInstanceOption_supportsKerberosAuthentication :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_supportsKerberosAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsKerberosAuthentication} -> supportsKerberosAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsKerberosAuthentication = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports provisioned IOPS.
orderableDBInstanceOption_supportsIops :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_supportsIops = Lens.lens (\OrderableDBInstanceOption' {supportsIops} -> supportsIops) (\s@OrderableDBInstanceOption' {} a -> s {supportsIops = a} :: OrderableDBInstanceOption)

-- | Indicates the storage type for a DB instance.
orderableDBInstanceOption_storageType :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
orderableDBInstanceOption_storageType = Lens.lens (\OrderableDBInstanceOption' {storageType} -> storageType) (\s@OrderableDBInstanceOption' {} a -> s {storageType = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
orderableDBInstanceOption_supportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_supportsEnhancedMonitoring = Lens.lens (\OrderableDBInstanceOption' {supportsEnhancedMonitoring} -> supportsEnhancedMonitoring) (\s@OrderableDBInstanceOption' {} a -> s {supportsEnhancedMonitoring = a} :: OrderableDBInstanceOption)

-- | A list of the available processor features for the DB instance class of
-- a DB instance.
orderableDBInstanceOption_availableProcessorFeatures :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [AvailableProcessorFeature])
orderableDBInstanceOption_availableProcessorFeatures = Lens.lens (\OrderableDBInstanceOption' {availableProcessorFeatures} -> availableProcessorFeatures) (\s@OrderableDBInstanceOption' {} a -> s {availableProcessorFeatures = a} :: OrderableDBInstanceOption) Core.. Lens.mapping Lens._Coerce

-- | Maximum storage size for a DB instance.
orderableDBInstanceOption_maxStorageSize :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
orderableDBInstanceOption_maxStorageSize = Lens.lens (\OrderableDBInstanceOption' {maxStorageSize} -> maxStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {maxStorageSize = a} :: OrderableDBInstanceOption)

-- | A list of the supported DB engine modes.
orderableDBInstanceOption_supportedEngineModes :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [Core.Text])
orderableDBInstanceOption_supportedEngineModes = Lens.lens (\OrderableDBInstanceOption' {supportedEngineModes} -> supportedEngineModes) (\s@OrderableDBInstanceOption' {} a -> s {supportedEngineModes = a} :: OrderableDBInstanceOption) Core.. Lens.mapping Lens._Coerce

-- | Maximum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_maxIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Double)
orderableDBInstanceOption_maxIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerGib} -> maxIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports encrypted storage.
orderableDBInstanceOption_supportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_supportsStorageEncryption = Lens.lens (\OrderableDBInstanceOption' {supportsStorageEncryption} -> supportsStorageEncryption) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageEncryption = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is Multi-AZ capable.
orderableDBInstanceOption_multiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_multiAZCapable = Lens.lens (\OrderableDBInstanceOption' {multiAZCapable} -> multiAZCapable) (\s@OrderableDBInstanceOption' {} a -> s {multiAZCapable = a} :: OrderableDBInstanceOption)

-- | Whether Amazon RDS can automatically scale storage for DB instances that
-- use the specified DB instance class.
orderableDBInstanceOption_supportsStorageAutoscaling :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_supportsStorageAutoscaling = Lens.lens (\OrderableDBInstanceOption' {supportsStorageAutoscaling} -> supportsStorageAutoscaling) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageAutoscaling = a} :: OrderableDBInstanceOption)

-- | The engine version of a DB instance.
orderableDBInstanceOption_engineVersion :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
orderableDBInstanceOption_engineVersion = Lens.lens (\OrderableDBInstanceOption' {engineVersion} -> engineVersion) (\s@OrderableDBInstanceOption' {} a -> s {engineVersion = a} :: OrderableDBInstanceOption)

-- | Minimum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_minIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Double)
orderableDBInstanceOption_minIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {minIopsPerGib} -> minIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports IAM database authentication.
orderableDBInstanceOption_supportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_supportsIAMDatabaseAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsIAMDatabaseAuthentication} -> supportsIAMDatabaseAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsIAMDatabaseAuthentication = a} :: OrderableDBInstanceOption)

-- | The license model for a DB instance.
orderableDBInstanceOption_licenseModel :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
orderableDBInstanceOption_licenseModel = Lens.lens (\OrderableDBInstanceOption' {licenseModel} -> licenseModel) (\s@OrderableDBInstanceOption' {} a -> s {licenseModel = a} :: OrderableDBInstanceOption)

-- | A value that indicates whether you can use Aurora global databases with
-- a specific combination of other DB engine attributes.
orderableDBInstanceOption_supportsGlobalDatabases :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_supportsGlobalDatabases = Lens.lens (\OrderableDBInstanceOption' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@OrderableDBInstanceOption' {} a -> s {supportsGlobalDatabases = a} :: OrderableDBInstanceOption)

-- | True if a DB instance supports Performance Insights, otherwise false.
orderableDBInstanceOption_supportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_supportsPerformanceInsights = Lens.lens (\OrderableDBInstanceOption' {supportsPerformanceInsights} -> supportsPerformanceInsights) (\s@OrderableDBInstanceOption' {} a -> s {supportsPerformanceInsights = a} :: OrderableDBInstanceOption)

-- | Maximum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_maxIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
orderableDBInstanceOption_maxIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerDbInstance} -> maxIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | The DB instance class for a DB instance.
orderableDBInstanceOption_dbInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
orderableDBInstanceOption_dbInstanceClass = Lens.lens (\OrderableDBInstanceOption' {dbInstanceClass} -> dbInstanceClass) (\s@OrderableDBInstanceOption' {} a -> s {dbInstanceClass = a} :: OrderableDBInstanceOption)

-- | Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide./
orderableDBInstanceOption_outpostCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_outpostCapable = Lens.lens (\OrderableDBInstanceOption' {outpostCapable} -> outpostCapable) (\s@OrderableDBInstanceOption' {} a -> s {outpostCapable = a} :: OrderableDBInstanceOption)

-- | The engine type of a DB instance.
orderableDBInstanceOption_engine :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
orderableDBInstanceOption_engine = Lens.lens (\OrderableDBInstanceOption' {engine} -> engine) (\s@OrderableDBInstanceOption' {} a -> s {engine = a} :: OrderableDBInstanceOption)

-- | Minimum storage size for a DB instance.
orderableDBInstanceOption_minStorageSize :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
orderableDBInstanceOption_minStorageSize = Lens.lens (\OrderableDBInstanceOption' {minStorageSize} -> minStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {minStorageSize = a} :: OrderableDBInstanceOption)

-- | The Availability Zone group for a DB instance.
orderableDBInstanceOption_availabilityZoneGroup :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
orderableDBInstanceOption_availabilityZoneGroup = Lens.lens (\OrderableDBInstanceOption' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZoneGroup = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is in a VPC.
orderableDBInstanceOption_vpc :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_vpc = Lens.lens (\OrderableDBInstanceOption' {vpc} -> vpc) (\s@OrderableDBInstanceOption' {} a -> s {vpc = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance can have a read replica.
orderableDBInstanceOption_readReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
orderableDBInstanceOption_readReplicaCapable = Lens.lens (\OrderableDBInstanceOption' {readReplicaCapable} -> readReplicaCapable) (\s@OrderableDBInstanceOption' {} a -> s {readReplicaCapable = a} :: OrderableDBInstanceOption)

instance Core.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Core.<$> (x Core..@? "MinIopsPerDbInstance")
      Core.<*> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "AvailabilityZone")
               )
      Core.<*> (x Core..@? "SupportsKerberosAuthentication")
      Core.<*> (x Core..@? "SupportsIops")
      Core.<*> (x Core..@? "StorageType")
      Core.<*> (x Core..@? "SupportsEnhancedMonitoring")
      Core.<*> ( x Core..@? "AvailableProcessorFeatures"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "AvailableProcessorFeature")
               )
      Core.<*> (x Core..@? "MaxStorageSize")
      Core.<*> ( x Core..@? "SupportedEngineModes"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "MaxIopsPerGib")
      Core.<*> (x Core..@? "SupportsStorageEncryption")
      Core.<*> (x Core..@? "MultiAZCapable")
      Core.<*> (x Core..@? "SupportsStorageAutoscaling")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "MinIopsPerGib")
      Core.<*> (x Core..@? "SupportsIAMDatabaseAuthentication")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "SupportsGlobalDatabases")
      Core.<*> (x Core..@? "SupportsPerformanceInsights")
      Core.<*> (x Core..@? "MaxIopsPerDbInstance")
      Core.<*> (x Core..@? "DBInstanceClass")
      Core.<*> (x Core..@? "OutpostCapable")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "MinStorageSize")
      Core.<*> (x Core..@? "AvailabilityZoneGroup")
      Core.<*> (x Core..@? "Vpc")
      Core.<*> (x Core..@? "ReadReplicaCapable")

instance Core.Hashable OrderableDBInstanceOption

instance Core.NFData OrderableDBInstanceOption
