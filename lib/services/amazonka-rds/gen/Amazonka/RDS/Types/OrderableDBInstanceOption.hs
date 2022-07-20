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
-- Module      : Amazonka.RDS.Types.OrderableDBInstanceOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.OrderableDBInstanceOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.AvailabilityZone
import Amazonka.RDS.Types.AvailableProcessorFeature

-- | Contains a list of available options for a DB instance.
--
-- This data type is used as a response element in the
-- @DescribeOrderableDBInstanceOptions@ action.
--
-- /See:/ 'newOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { -- | Indicates whether a DB instance supports encrypted storage.
    supportsStorageEncryption :: Prelude.Maybe Prelude.Bool,
    -- | Maximum storage size for a DB instance.
    maxStorageSize :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether a DB instance is Multi-AZ capable.
    multiAZCapable :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance class for a DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a DB instance is in a VPC.
    vpc :: Prelude.Maybe Prelude.Bool,
    -- | True if a DB instance supports Performance Insights, otherwise false.
    supportsPerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | A list of Availability Zones for a DB instance.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The Availability Zone group for a DB instance.
    availabilityZoneGroup :: Prelude.Maybe Prelude.Text,
    -- | Minimum total provisioned IOPS for a DB instance.
    minIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | The list of supported modes for Database Activity Streams. Aurora
    -- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
    -- Oracle return @[async]@ only. If Database Activity Streams isn\'t
    -- supported, the return value is an empty list.
    supportedActivityStreamModes :: Prelude.Maybe [Prelude.Text],
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Prelude.Maybe [Prelude.Text],
    -- | Minimum storage size for a DB instance.
    minStorageSize :: Prelude.Maybe Prelude.Int,
    -- | Indicates the storage type for a DB instance.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | Whether a DB instance supports RDS on Outposts.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide./
    outpostCapable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports provisioned IOPS.
    supportsIops :: Prelude.Maybe Prelude.Bool,
    -- | Maximum total provisioned IOPS for a DB instance.
    maxIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether a DB instance supports IAM database authentication.
    supportsIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports Enhanced Monitoring at
    -- intervals from 1 to 60 seconds.
    supportsEnhancedMonitoring :: Prelude.Maybe Prelude.Bool,
    -- | A list of the available processor features for the DB instance class of
    -- a DB instance.
    availableProcessorFeatures :: Prelude.Maybe [AvailableProcessorFeature],
    -- | The engine type of a DB instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a DB instance can have a read replica.
    readReplicaCapable :: Prelude.Maybe Prelude.Bool,
    -- | Whether Amazon RDS can automatically scale storage for DB instances that
    -- use the specified DB instance class.
    supportsStorageAutoscaling :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether you can use Aurora global databases with
    -- a specific combination of other DB engine attributes.
    supportsGlobalDatabases :: Prelude.Maybe Prelude.Bool,
    -- | Maximum provisioned IOPS per GiB for a DB instance.
    maxIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | The engine version of a DB instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Whether a DB instance supports Kerberos Authentication.
    supportsKerberosAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | Minimum provisioned IOPS per GiB for a DB instance.
    minIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | The license model for a DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text
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
-- 'supportsStorageEncryption', 'orderableDBInstanceOption_supportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
--
-- 'maxStorageSize', 'orderableDBInstanceOption_maxStorageSize' - Maximum storage size for a DB instance.
--
-- 'multiAZCapable', 'orderableDBInstanceOption_multiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
--
-- 'dbInstanceClass', 'orderableDBInstanceOption_dbInstanceClass' - The DB instance class for a DB instance.
--
-- 'vpc', 'orderableDBInstanceOption_vpc' - Indicates whether a DB instance is in a VPC.
--
-- 'supportsPerformanceInsights', 'orderableDBInstanceOption_supportsPerformanceInsights' - True if a DB instance supports Performance Insights, otherwise false.
--
-- 'availabilityZones', 'orderableDBInstanceOption_availabilityZones' - A list of Availability Zones for a DB instance.
--
-- 'availabilityZoneGroup', 'orderableDBInstanceOption_availabilityZoneGroup' - The Availability Zone group for a DB instance.
--
-- 'minIopsPerDbInstance', 'orderableDBInstanceOption_minIopsPerDbInstance' - Minimum total provisioned IOPS for a DB instance.
--
-- 'supportedActivityStreamModes', 'orderableDBInstanceOption_supportedActivityStreamModes' - The list of supported modes for Database Activity Streams. Aurora
-- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
-- Oracle return @[async]@ only. If Database Activity Streams isn\'t
-- supported, the return value is an empty list.
--
-- 'supportedEngineModes', 'orderableDBInstanceOption_supportedEngineModes' - A list of the supported DB engine modes.
--
-- 'minStorageSize', 'orderableDBInstanceOption_minStorageSize' - Minimum storage size for a DB instance.
--
-- 'storageType', 'orderableDBInstanceOption_storageType' - Indicates the storage type for a DB instance.
--
-- 'outpostCapable', 'orderableDBInstanceOption_outpostCapable' - Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide./
--
-- 'supportsIops', 'orderableDBInstanceOption_supportsIops' - Indicates whether a DB instance supports provisioned IOPS.
--
-- 'maxIopsPerDbInstance', 'orderableDBInstanceOption_maxIopsPerDbInstance' - Maximum total provisioned IOPS for a DB instance.
--
-- 'supportsIAMDatabaseAuthentication', 'orderableDBInstanceOption_supportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
--
-- 'supportsEnhancedMonitoring', 'orderableDBInstanceOption_supportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
--
-- 'availableProcessorFeatures', 'orderableDBInstanceOption_availableProcessorFeatures' - A list of the available processor features for the DB instance class of
-- a DB instance.
--
-- 'engine', 'orderableDBInstanceOption_engine' - The engine type of a DB instance.
--
-- 'readReplicaCapable', 'orderableDBInstanceOption_readReplicaCapable' - Indicates whether a DB instance can have a read replica.
--
-- 'supportsStorageAutoscaling', 'orderableDBInstanceOption_supportsStorageAutoscaling' - Whether Amazon RDS can automatically scale storage for DB instances that
-- use the specified DB instance class.
--
-- 'supportsGlobalDatabases', 'orderableDBInstanceOption_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- a specific combination of other DB engine attributes.
--
-- 'maxIopsPerGib', 'orderableDBInstanceOption_maxIopsPerGib' - Maximum provisioned IOPS per GiB for a DB instance.
--
-- 'engineVersion', 'orderableDBInstanceOption_engineVersion' - The engine version of a DB instance.
--
-- 'supportsKerberosAuthentication', 'orderableDBInstanceOption_supportsKerberosAuthentication' - Whether a DB instance supports Kerberos Authentication.
--
-- 'minIopsPerGib', 'orderableDBInstanceOption_minIopsPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
--
-- 'licenseModel', 'orderableDBInstanceOption_licenseModel' - The license model for a DB instance.
newOrderableDBInstanceOption ::
  OrderableDBInstanceOption
newOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { supportsStorageEncryption =
        Prelude.Nothing,
      maxStorageSize = Prelude.Nothing,
      multiAZCapable = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      vpc = Prelude.Nothing,
      supportsPerformanceInsights = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      availabilityZoneGroup = Prelude.Nothing,
      minIopsPerDbInstance = Prelude.Nothing,
      supportedActivityStreamModes = Prelude.Nothing,
      supportedEngineModes = Prelude.Nothing,
      minStorageSize = Prelude.Nothing,
      storageType = Prelude.Nothing,
      outpostCapable = Prelude.Nothing,
      supportsIops = Prelude.Nothing,
      maxIopsPerDbInstance = Prelude.Nothing,
      supportsIAMDatabaseAuthentication =
        Prelude.Nothing,
      supportsEnhancedMonitoring = Prelude.Nothing,
      availableProcessorFeatures = Prelude.Nothing,
      engine = Prelude.Nothing,
      readReplicaCapable = Prelude.Nothing,
      supportsStorageAutoscaling = Prelude.Nothing,
      supportsGlobalDatabases = Prelude.Nothing,
      maxIopsPerGib = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      supportsKerberosAuthentication = Prelude.Nothing,
      minIopsPerGib = Prelude.Nothing,
      licenseModel = Prelude.Nothing
    }

-- | Indicates whether a DB instance supports encrypted storage.
orderableDBInstanceOption_supportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageEncryption = Lens.lens (\OrderableDBInstanceOption' {supportsStorageEncryption} -> supportsStorageEncryption) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageEncryption = a} :: OrderableDBInstanceOption)

-- | Maximum storage size for a DB instance.
orderableDBInstanceOption_maxStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxStorageSize = Lens.lens (\OrderableDBInstanceOption' {maxStorageSize} -> maxStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {maxStorageSize = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is Multi-AZ capable.
orderableDBInstanceOption_multiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_multiAZCapable = Lens.lens (\OrderableDBInstanceOption' {multiAZCapable} -> multiAZCapable) (\s@OrderableDBInstanceOption' {} a -> s {multiAZCapable = a} :: OrderableDBInstanceOption)

-- | The DB instance class for a DB instance.
orderableDBInstanceOption_dbInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_dbInstanceClass = Lens.lens (\OrderableDBInstanceOption' {dbInstanceClass} -> dbInstanceClass) (\s@OrderableDBInstanceOption' {} a -> s {dbInstanceClass = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is in a VPC.
orderableDBInstanceOption_vpc :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_vpc = Lens.lens (\OrderableDBInstanceOption' {vpc} -> vpc) (\s@OrderableDBInstanceOption' {} a -> s {vpc = a} :: OrderableDBInstanceOption)

-- | True if a DB instance supports Performance Insights, otherwise false.
orderableDBInstanceOption_supportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsPerformanceInsights = Lens.lens (\OrderableDBInstanceOption' {supportsPerformanceInsights} -> supportsPerformanceInsights) (\s@OrderableDBInstanceOption' {} a -> s {supportsPerformanceInsights = a} :: OrderableDBInstanceOption)

-- | A list of Availability Zones for a DB instance.
orderableDBInstanceOption_availabilityZones :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailabilityZone])
orderableDBInstanceOption_availabilityZones = Lens.lens (\OrderableDBInstanceOption' {availabilityZones} -> availabilityZones) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZones = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zone group for a DB instance.
orderableDBInstanceOption_availabilityZoneGroup :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_availabilityZoneGroup = Lens.lens (\OrderableDBInstanceOption' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZoneGroup = a} :: OrderableDBInstanceOption)

-- | Minimum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_minIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {minIopsPerDbInstance} -> minIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | The list of supported modes for Database Activity Streams. Aurora
-- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
-- Oracle return @[async]@ only. If Database Activity Streams isn\'t
-- supported, the return value is an empty list.
orderableDBInstanceOption_supportedActivityStreamModes :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [Prelude.Text])
orderableDBInstanceOption_supportedActivityStreamModes = Lens.lens (\OrderableDBInstanceOption' {supportedActivityStreamModes} -> supportedActivityStreamModes) (\s@OrderableDBInstanceOption' {} a -> s {supportedActivityStreamModes = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | A list of the supported DB engine modes.
orderableDBInstanceOption_supportedEngineModes :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [Prelude.Text])
orderableDBInstanceOption_supportedEngineModes = Lens.lens (\OrderableDBInstanceOption' {supportedEngineModes} -> supportedEngineModes) (\s@OrderableDBInstanceOption' {} a -> s {supportedEngineModes = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | Minimum storage size for a DB instance.
orderableDBInstanceOption_minStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minStorageSize = Lens.lens (\OrderableDBInstanceOption' {minStorageSize} -> minStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {minStorageSize = a} :: OrderableDBInstanceOption)

-- | Indicates the storage type for a DB instance.
orderableDBInstanceOption_storageType :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_storageType = Lens.lens (\OrderableDBInstanceOption' {storageType} -> storageType) (\s@OrderableDBInstanceOption' {} a -> s {storageType = a} :: OrderableDBInstanceOption)

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

-- | Maximum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_maxIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerDbInstance} -> maxIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports IAM database authentication.
orderableDBInstanceOption_supportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIAMDatabaseAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsIAMDatabaseAuthentication} -> supportsIAMDatabaseAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsIAMDatabaseAuthentication = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
orderableDBInstanceOption_supportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsEnhancedMonitoring = Lens.lens (\OrderableDBInstanceOption' {supportsEnhancedMonitoring} -> supportsEnhancedMonitoring) (\s@OrderableDBInstanceOption' {} a -> s {supportsEnhancedMonitoring = a} :: OrderableDBInstanceOption)

-- | A list of the available processor features for the DB instance class of
-- a DB instance.
orderableDBInstanceOption_availableProcessorFeatures :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailableProcessorFeature])
orderableDBInstanceOption_availableProcessorFeatures = Lens.lens (\OrderableDBInstanceOption' {availableProcessorFeatures} -> availableProcessorFeatures) (\s@OrderableDBInstanceOption' {} a -> s {availableProcessorFeatures = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | The engine type of a DB instance.
orderableDBInstanceOption_engine :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engine = Lens.lens (\OrderableDBInstanceOption' {engine} -> engine) (\s@OrderableDBInstanceOption' {} a -> s {engine = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance can have a read replica.
orderableDBInstanceOption_readReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_readReplicaCapable = Lens.lens (\OrderableDBInstanceOption' {readReplicaCapable} -> readReplicaCapable) (\s@OrderableDBInstanceOption' {} a -> s {readReplicaCapable = a} :: OrderableDBInstanceOption)

-- | Whether Amazon RDS can automatically scale storage for DB instances that
-- use the specified DB instance class.
orderableDBInstanceOption_supportsStorageAutoscaling :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageAutoscaling = Lens.lens (\OrderableDBInstanceOption' {supportsStorageAutoscaling} -> supportsStorageAutoscaling) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageAutoscaling = a} :: OrderableDBInstanceOption)

-- | A value that indicates whether you can use Aurora global databases with
-- a specific combination of other DB engine attributes.
orderableDBInstanceOption_supportsGlobalDatabases :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsGlobalDatabases = Lens.lens (\OrderableDBInstanceOption' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@OrderableDBInstanceOption' {} a -> s {supportsGlobalDatabases = a} :: OrderableDBInstanceOption)

-- | Maximum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_maxIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_maxIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerGib} -> maxIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerGib = a} :: OrderableDBInstanceOption)

-- | The engine version of a DB instance.
orderableDBInstanceOption_engineVersion :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engineVersion = Lens.lens (\OrderableDBInstanceOption' {engineVersion} -> engineVersion) (\s@OrderableDBInstanceOption' {} a -> s {engineVersion = a} :: OrderableDBInstanceOption)

-- | Whether a DB instance supports Kerberos Authentication.
orderableDBInstanceOption_supportsKerberosAuthentication :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsKerberosAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsKerberosAuthentication} -> supportsKerberosAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsKerberosAuthentication = a} :: OrderableDBInstanceOption)

-- | Minimum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_minIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_minIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {minIopsPerGib} -> minIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerGib = a} :: OrderableDBInstanceOption)

-- | The license model for a DB instance.
orderableDBInstanceOption_licenseModel :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_licenseModel = Lens.lens (\OrderableDBInstanceOption' {licenseModel} -> licenseModel) (\s@OrderableDBInstanceOption' {} a -> s {licenseModel = a} :: OrderableDBInstanceOption)

instance Core.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Prelude.<$> (x Core..@? "SupportsStorageEncryption")
      Prelude.<*> (x Core..@? "MaxStorageSize")
      Prelude.<*> (x Core..@? "MultiAZCapable")
      Prelude.<*> (x Core..@? "DBInstanceClass")
      Prelude.<*> (x Core..@? "Vpc")
      Prelude.<*> (x Core..@? "SupportsPerformanceInsights")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Core..@? "AvailabilityZoneGroup")
      Prelude.<*> (x Core..@? "MinIopsPerDbInstance")
      Prelude.<*> ( x Core..@? "SupportedActivityStreamModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "SupportedEngineModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "MinStorageSize")
      Prelude.<*> (x Core..@? "StorageType")
      Prelude.<*> (x Core..@? "OutpostCapable")
      Prelude.<*> (x Core..@? "SupportsIops")
      Prelude.<*> (x Core..@? "MaxIopsPerDbInstance")
      Prelude.<*> (x Core..@? "SupportsIAMDatabaseAuthentication")
      Prelude.<*> (x Core..@? "SupportsEnhancedMonitoring")
      Prelude.<*> ( x Core..@? "AvailableProcessorFeatures"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "AvailableProcessorFeature")
                  )
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "ReadReplicaCapable")
      Prelude.<*> (x Core..@? "SupportsStorageAutoscaling")
      Prelude.<*> (x Core..@? "SupportsGlobalDatabases")
      Prelude.<*> (x Core..@? "MaxIopsPerGib")
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "SupportsKerberosAuthentication")
      Prelude.<*> (x Core..@? "MinIopsPerGib")
      Prelude.<*> (x Core..@? "LicenseModel")

instance Prelude.Hashable OrderableDBInstanceOption where
  hashWithSalt _salt OrderableDBInstanceOption' {..} =
    _salt
      `Prelude.hashWithSalt` supportsStorageEncryption
      `Prelude.hashWithSalt` maxStorageSize
      `Prelude.hashWithSalt` multiAZCapable
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` supportsPerformanceInsights
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` availabilityZoneGroup
      `Prelude.hashWithSalt` minIopsPerDbInstance
      `Prelude.hashWithSalt` supportedActivityStreamModes
      `Prelude.hashWithSalt` supportedEngineModes
      `Prelude.hashWithSalt` minStorageSize
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` outpostCapable
      `Prelude.hashWithSalt` supportsIops
      `Prelude.hashWithSalt` maxIopsPerDbInstance
      `Prelude.hashWithSalt` supportsIAMDatabaseAuthentication
      `Prelude.hashWithSalt` supportsEnhancedMonitoring
      `Prelude.hashWithSalt` availableProcessorFeatures
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` readReplicaCapable
      `Prelude.hashWithSalt` supportsStorageAutoscaling
      `Prelude.hashWithSalt` supportsGlobalDatabases
      `Prelude.hashWithSalt` maxIopsPerGib
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` supportsKerberosAuthentication
      `Prelude.hashWithSalt` minIopsPerGib
      `Prelude.hashWithSalt` licenseModel

instance Prelude.NFData OrderableDBInstanceOption where
  rnf OrderableDBInstanceOption' {..} =
    Prelude.rnf supportsStorageEncryption
      `Prelude.seq` Prelude.rnf maxStorageSize
      `Prelude.seq` Prelude.rnf multiAZCapable
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf supportsPerformanceInsights
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf availabilityZoneGroup
      `Prelude.seq` Prelude.rnf minIopsPerDbInstance
      `Prelude.seq` Prelude.rnf supportedActivityStreamModes
      `Prelude.seq` Prelude.rnf supportedEngineModes
      `Prelude.seq` Prelude.rnf minStorageSize
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf outpostCapable
      `Prelude.seq` Prelude.rnf supportsIops
      `Prelude.seq` Prelude.rnf maxIopsPerDbInstance
      `Prelude.seq` Prelude.rnf
        supportsIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf
        supportsEnhancedMonitoring
      `Prelude.seq` Prelude.rnf
        availableProcessorFeatures
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf
        readReplicaCapable
      `Prelude.seq` Prelude.rnf
        supportsStorageAutoscaling
      `Prelude.seq` Prelude.rnf
        supportsGlobalDatabases
      `Prelude.seq` Prelude.rnf
        maxIopsPerGib
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        supportsKerberosAuthentication
      `Prelude.seq` Prelude.rnf
        minIopsPerGib
      `Prelude.seq` Prelude.rnf
        licenseModel
