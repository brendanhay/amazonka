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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.AvailabilityZone
import Network.AWS.RDS.Types.AvailableProcessorFeature

-- | Contains a list of available options for a DB instance.
--
-- This data type is used as a response element in the
-- @DescribeOrderableDBInstanceOptions@ action.
--
-- /See:/ 'newOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { -- | The engine version of a DB instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Minimum provisioned IOPS per GiB for a DB instance.
    minIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether a DB instance supports IAM database authentication.
    supportsIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | Minimum total provisioned IOPS for a DB instance.
    minIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether a DB instance is Multi-AZ capable.
    multiAZCapable :: Prelude.Maybe Prelude.Bool,
    -- | Maximum storage size for a DB instance.
    maxStorageSize :: Prelude.Maybe Prelude.Int,
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Prelude.Maybe [Prelude.Text],
    -- | The Availability Zone group for a DB instance.
    availabilityZoneGroup :: Prelude.Maybe Prelude.Text,
    -- | A list of the available processor features for the DB instance class of
    -- a DB instance.
    availableProcessorFeatures :: Prelude.Maybe [AvailableProcessorFeature],
    -- | The engine type of a DB instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Minimum storage size for a DB instance.
    minStorageSize :: Prelude.Maybe Prelude.Int,
    -- | Whether a DB instance supports RDS on Outposts.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide./
    outpostCapable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports provisioned IOPS.
    supportsIops :: Prelude.Maybe Prelude.Bool,
    -- | Whether a DB instance supports Kerberos Authentication.
    supportsKerberosAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | True if a DB instance supports Performance Insights, otherwise false.
    supportsPerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance class for a DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether you can use Aurora global databases with
    -- a specific combination of other DB engine attributes.
    supportsGlobalDatabases :: Prelude.Maybe Prelude.Bool,
    -- | The license model for a DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | A list of Availability Zones for a DB instance.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The list of supported modes for Database Activity Streams. Aurora
    -- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
    -- Oracle return @[async]@ only. If Database Activity Streams isn\'t
    -- supported, the return value is an empty list.
    supportedActivityStreamModes :: Prelude.Maybe [Prelude.Text],
    -- | Whether Amazon RDS can automatically scale storage for DB instances that
    -- use the specified DB instance class.
    supportsStorageAutoscaling :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports encrypted storage.
    supportsStorageEncryption :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance can have a read replica.
    readReplicaCapable :: Prelude.Maybe Prelude.Bool,
    -- | Maximum provisioned IOPS per GiB for a DB instance.
    maxIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether a DB instance is in a VPC.
    vpc :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports Enhanced Monitoring at
    -- intervals from 1 to 60 seconds.
    supportsEnhancedMonitoring :: Prelude.Maybe Prelude.Bool,
    -- | Maximum total provisioned IOPS for a DB instance.
    maxIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Indicates the storage type for a DB instance.
    storageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrderableDBInstanceOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'orderableDBInstanceOption_engineVersion' - The engine version of a DB instance.
--
-- 'minIopsPerGib', 'orderableDBInstanceOption_minIopsPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
--
-- 'supportsIAMDatabaseAuthentication', 'orderableDBInstanceOption_supportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
--
-- 'minIopsPerDbInstance', 'orderableDBInstanceOption_minIopsPerDbInstance' - Minimum total provisioned IOPS for a DB instance.
--
-- 'multiAZCapable', 'orderableDBInstanceOption_multiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
--
-- 'maxStorageSize', 'orderableDBInstanceOption_maxStorageSize' - Maximum storage size for a DB instance.
--
-- 'supportedEngineModes', 'orderableDBInstanceOption_supportedEngineModes' - A list of the supported DB engine modes.
--
-- 'availabilityZoneGroup', 'orderableDBInstanceOption_availabilityZoneGroup' - The Availability Zone group for a DB instance.
--
-- 'availableProcessorFeatures', 'orderableDBInstanceOption_availableProcessorFeatures' - A list of the available processor features for the DB instance class of
-- a DB instance.
--
-- 'engine', 'orderableDBInstanceOption_engine' - The engine type of a DB instance.
--
-- 'minStorageSize', 'orderableDBInstanceOption_minStorageSize' - Minimum storage size for a DB instance.
--
-- 'outpostCapable', 'orderableDBInstanceOption_outpostCapable' - Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide./
--
-- 'supportsIops', 'orderableDBInstanceOption_supportsIops' - Indicates whether a DB instance supports provisioned IOPS.
--
-- 'supportsKerberosAuthentication', 'orderableDBInstanceOption_supportsKerberosAuthentication' - Whether a DB instance supports Kerberos Authentication.
--
-- 'supportsPerformanceInsights', 'orderableDBInstanceOption_supportsPerformanceInsights' - True if a DB instance supports Performance Insights, otherwise false.
--
-- 'dbInstanceClass', 'orderableDBInstanceOption_dbInstanceClass' - The DB instance class for a DB instance.
--
-- 'supportsGlobalDatabases', 'orderableDBInstanceOption_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- a specific combination of other DB engine attributes.
--
-- 'licenseModel', 'orderableDBInstanceOption_licenseModel' - The license model for a DB instance.
--
-- 'availabilityZones', 'orderableDBInstanceOption_availabilityZones' - A list of Availability Zones for a DB instance.
--
-- 'supportedActivityStreamModes', 'orderableDBInstanceOption_supportedActivityStreamModes' - The list of supported modes for Database Activity Streams. Aurora
-- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
-- Oracle return @[async]@ only. If Database Activity Streams isn\'t
-- supported, the return value is an empty list.
--
-- 'supportsStorageAutoscaling', 'orderableDBInstanceOption_supportsStorageAutoscaling' - Whether Amazon RDS can automatically scale storage for DB instances that
-- use the specified DB instance class.
--
-- 'supportsStorageEncryption', 'orderableDBInstanceOption_supportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
--
-- 'readReplicaCapable', 'orderableDBInstanceOption_readReplicaCapable' - Indicates whether a DB instance can have a read replica.
--
-- 'maxIopsPerGib', 'orderableDBInstanceOption_maxIopsPerGib' - Maximum provisioned IOPS per GiB for a DB instance.
--
-- 'vpc', 'orderableDBInstanceOption_vpc' - Indicates whether a DB instance is in a VPC.
--
-- 'supportsEnhancedMonitoring', 'orderableDBInstanceOption_supportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
--
-- 'maxIopsPerDbInstance', 'orderableDBInstanceOption_maxIopsPerDbInstance' - Maximum total provisioned IOPS for a DB instance.
--
-- 'storageType', 'orderableDBInstanceOption_storageType' - Indicates the storage type for a DB instance.
newOrderableDBInstanceOption ::
  OrderableDBInstanceOption
newOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { engineVersion =
        Prelude.Nothing,
      minIopsPerGib = Prelude.Nothing,
      supportsIAMDatabaseAuthentication =
        Prelude.Nothing,
      minIopsPerDbInstance = Prelude.Nothing,
      multiAZCapable = Prelude.Nothing,
      maxStorageSize = Prelude.Nothing,
      supportedEngineModes = Prelude.Nothing,
      availabilityZoneGroup = Prelude.Nothing,
      availableProcessorFeatures = Prelude.Nothing,
      engine = Prelude.Nothing,
      minStorageSize = Prelude.Nothing,
      outpostCapable = Prelude.Nothing,
      supportsIops = Prelude.Nothing,
      supportsKerberosAuthentication = Prelude.Nothing,
      supportsPerformanceInsights = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      supportsGlobalDatabases = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      supportedActivityStreamModes = Prelude.Nothing,
      supportsStorageAutoscaling = Prelude.Nothing,
      supportsStorageEncryption = Prelude.Nothing,
      readReplicaCapable = Prelude.Nothing,
      maxIopsPerGib = Prelude.Nothing,
      vpc = Prelude.Nothing,
      supportsEnhancedMonitoring = Prelude.Nothing,
      maxIopsPerDbInstance = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | The engine version of a DB instance.
orderableDBInstanceOption_engineVersion :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engineVersion = Lens.lens (\OrderableDBInstanceOption' {engineVersion} -> engineVersion) (\s@OrderableDBInstanceOption' {} a -> s {engineVersion = a} :: OrderableDBInstanceOption)

-- | Minimum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_minIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_minIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {minIopsPerGib} -> minIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports IAM database authentication.
orderableDBInstanceOption_supportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIAMDatabaseAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsIAMDatabaseAuthentication} -> supportsIAMDatabaseAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsIAMDatabaseAuthentication = a} :: OrderableDBInstanceOption)

-- | Minimum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_minIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {minIopsPerDbInstance} -> minIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is Multi-AZ capable.
orderableDBInstanceOption_multiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_multiAZCapable = Lens.lens (\OrderableDBInstanceOption' {multiAZCapable} -> multiAZCapable) (\s@OrderableDBInstanceOption' {} a -> s {multiAZCapable = a} :: OrderableDBInstanceOption)

-- | Maximum storage size for a DB instance.
orderableDBInstanceOption_maxStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxStorageSize = Lens.lens (\OrderableDBInstanceOption' {maxStorageSize} -> maxStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {maxStorageSize = a} :: OrderableDBInstanceOption)

-- | A list of the supported DB engine modes.
orderableDBInstanceOption_supportedEngineModes :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [Prelude.Text])
orderableDBInstanceOption_supportedEngineModes = Lens.lens (\OrderableDBInstanceOption' {supportedEngineModes} -> supportedEngineModes) (\s@OrderableDBInstanceOption' {} a -> s {supportedEngineModes = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zone group for a DB instance.
orderableDBInstanceOption_availabilityZoneGroup :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_availabilityZoneGroup = Lens.lens (\OrderableDBInstanceOption' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZoneGroup = a} :: OrderableDBInstanceOption)

-- | A list of the available processor features for the DB instance class of
-- a DB instance.
orderableDBInstanceOption_availableProcessorFeatures :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailableProcessorFeature])
orderableDBInstanceOption_availableProcessorFeatures = Lens.lens (\OrderableDBInstanceOption' {availableProcessorFeatures} -> availableProcessorFeatures) (\s@OrderableDBInstanceOption' {} a -> s {availableProcessorFeatures = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | The engine type of a DB instance.
orderableDBInstanceOption_engine :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engine = Lens.lens (\OrderableDBInstanceOption' {engine} -> engine) (\s@OrderableDBInstanceOption' {} a -> s {engine = a} :: OrderableDBInstanceOption)

-- | Minimum storage size for a DB instance.
orderableDBInstanceOption_minStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minStorageSize = Lens.lens (\OrderableDBInstanceOption' {minStorageSize} -> minStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {minStorageSize = a} :: OrderableDBInstanceOption)

-- | Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide./
orderableDBInstanceOption_outpostCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_outpostCapable = Lens.lens (\OrderableDBInstanceOption' {outpostCapable} -> outpostCapable) (\s@OrderableDBInstanceOption' {} a -> s {outpostCapable = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports provisioned IOPS.
orderableDBInstanceOption_supportsIops :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIops = Lens.lens (\OrderableDBInstanceOption' {supportsIops} -> supportsIops) (\s@OrderableDBInstanceOption' {} a -> s {supportsIops = a} :: OrderableDBInstanceOption)

-- | Whether a DB instance supports Kerberos Authentication.
orderableDBInstanceOption_supportsKerberosAuthentication :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsKerberosAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsKerberosAuthentication} -> supportsKerberosAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsKerberosAuthentication = a} :: OrderableDBInstanceOption)

-- | True if a DB instance supports Performance Insights, otherwise false.
orderableDBInstanceOption_supportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsPerformanceInsights = Lens.lens (\OrderableDBInstanceOption' {supportsPerformanceInsights} -> supportsPerformanceInsights) (\s@OrderableDBInstanceOption' {} a -> s {supportsPerformanceInsights = a} :: OrderableDBInstanceOption)

-- | The DB instance class for a DB instance.
orderableDBInstanceOption_dbInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_dbInstanceClass = Lens.lens (\OrderableDBInstanceOption' {dbInstanceClass} -> dbInstanceClass) (\s@OrderableDBInstanceOption' {} a -> s {dbInstanceClass = a} :: OrderableDBInstanceOption)

-- | A value that indicates whether you can use Aurora global databases with
-- a specific combination of other DB engine attributes.
orderableDBInstanceOption_supportsGlobalDatabases :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsGlobalDatabases = Lens.lens (\OrderableDBInstanceOption' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@OrderableDBInstanceOption' {} a -> s {supportsGlobalDatabases = a} :: OrderableDBInstanceOption)

-- | The license model for a DB instance.
orderableDBInstanceOption_licenseModel :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_licenseModel = Lens.lens (\OrderableDBInstanceOption' {licenseModel} -> licenseModel) (\s@OrderableDBInstanceOption' {} a -> s {licenseModel = a} :: OrderableDBInstanceOption)

-- | A list of Availability Zones for a DB instance.
orderableDBInstanceOption_availabilityZones :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailabilityZone])
orderableDBInstanceOption_availabilityZones = Lens.lens (\OrderableDBInstanceOption' {availabilityZones} -> availabilityZones) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZones = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | The list of supported modes for Database Activity Streams. Aurora
-- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
-- Oracle return @[async]@ only. If Database Activity Streams isn\'t
-- supported, the return value is an empty list.
orderableDBInstanceOption_supportedActivityStreamModes :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [Prelude.Text])
orderableDBInstanceOption_supportedActivityStreamModes = Lens.lens (\OrderableDBInstanceOption' {supportedActivityStreamModes} -> supportedActivityStreamModes) (\s@OrderableDBInstanceOption' {} a -> s {supportedActivityStreamModes = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | Whether Amazon RDS can automatically scale storage for DB instances that
-- use the specified DB instance class.
orderableDBInstanceOption_supportsStorageAutoscaling :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageAutoscaling = Lens.lens (\OrderableDBInstanceOption' {supportsStorageAutoscaling} -> supportsStorageAutoscaling) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageAutoscaling = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports encrypted storage.
orderableDBInstanceOption_supportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageEncryption = Lens.lens (\OrderableDBInstanceOption' {supportsStorageEncryption} -> supportsStorageEncryption) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageEncryption = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance can have a read replica.
orderableDBInstanceOption_readReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_readReplicaCapable = Lens.lens (\OrderableDBInstanceOption' {readReplicaCapable} -> readReplicaCapable) (\s@OrderableDBInstanceOption' {} a -> s {readReplicaCapable = a} :: OrderableDBInstanceOption)

-- | Maximum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_maxIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_maxIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerGib} -> maxIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is in a VPC.
orderableDBInstanceOption_vpc :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_vpc = Lens.lens (\OrderableDBInstanceOption' {vpc} -> vpc) (\s@OrderableDBInstanceOption' {} a -> s {vpc = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
orderableDBInstanceOption_supportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsEnhancedMonitoring = Lens.lens (\OrderableDBInstanceOption' {supportsEnhancedMonitoring} -> supportsEnhancedMonitoring) (\s@OrderableDBInstanceOption' {} a -> s {supportsEnhancedMonitoring = a} :: OrderableDBInstanceOption)

-- | Maximum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_maxIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerDbInstance} -> maxIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Indicates the storage type for a DB instance.
orderableDBInstanceOption_storageType :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_storageType = Lens.lens (\OrderableDBInstanceOption' {storageType} -> storageType) (\s@OrderableDBInstanceOption' {} a -> s {storageType = a} :: OrderableDBInstanceOption)

instance Core.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "MinIopsPerGib")
      Prelude.<*> (x Core..@? "SupportsIAMDatabaseAuthentication")
      Prelude.<*> (x Core..@? "MinIopsPerDbInstance")
      Prelude.<*> (x Core..@? "MultiAZCapable")
      Prelude.<*> (x Core..@? "MaxStorageSize")
      Prelude.<*> ( x Core..@? "SupportedEngineModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "AvailabilityZoneGroup")
      Prelude.<*> ( x Core..@? "AvailableProcessorFeatures"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "AvailableProcessorFeature")
                  )
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "MinStorageSize")
      Prelude.<*> (x Core..@? "OutpostCapable")
      Prelude.<*> (x Core..@? "SupportsIops")
      Prelude.<*> (x Core..@? "SupportsKerberosAuthentication")
      Prelude.<*> (x Core..@? "SupportsPerformanceInsights")
      Prelude.<*> (x Core..@? "DBInstanceClass")
      Prelude.<*> (x Core..@? "SupportsGlobalDatabases")
      Prelude.<*> (x Core..@? "LicenseModel")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> ( x Core..@? "SupportedActivityStreamModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "SupportsStorageAutoscaling")
      Prelude.<*> (x Core..@? "SupportsStorageEncryption")
      Prelude.<*> (x Core..@? "ReadReplicaCapable")
      Prelude.<*> (x Core..@? "MaxIopsPerGib")
      Prelude.<*> (x Core..@? "Vpc")
      Prelude.<*> (x Core..@? "SupportsEnhancedMonitoring")
      Prelude.<*> (x Core..@? "MaxIopsPerDbInstance")
      Prelude.<*> (x Core..@? "StorageType")

instance Prelude.Hashable OrderableDBInstanceOption

instance Prelude.NFData OrderableDBInstanceOption
