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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.OrderableDBInstanceOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The Availability Zone group for a DB instance.
    availabilityZoneGroup :: Prelude.Maybe Prelude.Text,
    -- | A list of Availability Zones for a DB instance.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | A list of the available processor features for the DB instance class of
    -- a DB instance.
    availableProcessorFeatures :: Prelude.Maybe [AvailableProcessorFeature],
    -- | The DB instance class for a DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The engine type of a DB instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The engine version of a DB instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The license model for a DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Maximum total provisioned IOPS for a DB instance.
    maxIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Maximum provisioned IOPS per GiB for a DB instance.
    maxIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | Maximum storage size for a DB instance.
    maxStorageSize :: Prelude.Maybe Prelude.Int,
    -- | Maximum storage throughput for a DB instance.
    maxStorageThroughputPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Maximum storage throughput to provisioned IOPS ratio for a DB instance.
    maxStorageThroughputPerIops :: Prelude.Maybe Prelude.Double,
    -- | Minimum total provisioned IOPS for a DB instance.
    minIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Minimum provisioned IOPS per GiB for a DB instance.
    minIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | Minimum storage size for a DB instance.
    minStorageSize :: Prelude.Maybe Prelude.Int,
    -- | Minimum storage throughput for a DB instance.
    minStorageThroughputPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Minimum storage throughput to provisioned IOPS ratio for a DB instance.
    minStorageThroughputPerIops :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether a DB instance is Multi-AZ capable.
    multiAZCapable :: Prelude.Maybe Prelude.Bool,
    -- | Whether a DB instance supports RDS on Outposts.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide./
    outpostCapable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance can have a read replica.
    readReplicaCapable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the storage type for a DB instance.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The list of supported modes for Database Activity Streams. Aurora
    -- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
    -- Oracle return @[async]@ only. If Database Activity Streams isn\'t
    -- supported, the return value is an empty list.
    supportedActivityStreamModes :: Prelude.Maybe [Prelude.Text],
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Prelude.Maybe [Prelude.Text],
    -- | The network types supported by the DB instance (@IPV4@ or @DUAL@).
    --
    -- A DB instance can support only the IPv4 protocol or the IPv4 and the
    -- IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon RDS User Guide./
    supportedNetworkTypes :: Prelude.Maybe [Prelude.Text],
    -- | Whether DB instances can be configured as a Multi-AZ DB cluster.
    --
    -- For more information on Multi-AZ DB clusters, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
    -- in the /Amazon RDS User Guide./
    supportsClusters :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports Enhanced Monitoring at
    -- intervals from 1 to 60 seconds.
    supportsEnhancedMonitoring :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether you can use Aurora global databases with
    -- a specific combination of other DB engine attributes.
    supportsGlobalDatabases :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports IAM database authentication.
    supportsIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports provisioned IOPS.
    supportsIops :: Prelude.Maybe Prelude.Bool,
    -- | Whether a DB instance supports Kerberos Authentication.
    supportsKerberosAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | True if a DB instance supports Performance Insights, otherwise false.
    supportsPerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | Whether Amazon RDS can automatically scale storage for DB instances that
    -- use the specified DB instance class.
    supportsStorageAutoscaling :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports encrypted storage.
    supportsStorageEncryption :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports storage throughput.
    supportsStorageThroughput :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance is in a VPC.
    vpc :: Prelude.Maybe Prelude.Bool
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
-- 'availabilityZoneGroup', 'orderableDBInstanceOption_availabilityZoneGroup' - The Availability Zone group for a DB instance.
--
-- 'availabilityZones', 'orderableDBInstanceOption_availabilityZones' - A list of Availability Zones for a DB instance.
--
-- 'availableProcessorFeatures', 'orderableDBInstanceOption_availableProcessorFeatures' - A list of the available processor features for the DB instance class of
-- a DB instance.
--
-- 'dbInstanceClass', 'orderableDBInstanceOption_dbInstanceClass' - The DB instance class for a DB instance.
--
-- 'engine', 'orderableDBInstanceOption_engine' - The engine type of a DB instance.
--
-- 'engineVersion', 'orderableDBInstanceOption_engineVersion' - The engine version of a DB instance.
--
-- 'licenseModel', 'orderableDBInstanceOption_licenseModel' - The license model for a DB instance.
--
-- 'maxIopsPerDbInstance', 'orderableDBInstanceOption_maxIopsPerDbInstance' - Maximum total provisioned IOPS for a DB instance.
--
-- 'maxIopsPerGib', 'orderableDBInstanceOption_maxIopsPerGib' - Maximum provisioned IOPS per GiB for a DB instance.
--
-- 'maxStorageSize', 'orderableDBInstanceOption_maxStorageSize' - Maximum storage size for a DB instance.
--
-- 'maxStorageThroughputPerDbInstance', 'orderableDBInstanceOption_maxStorageThroughputPerDbInstance' - Maximum storage throughput for a DB instance.
--
-- 'maxStorageThroughputPerIops', 'orderableDBInstanceOption_maxStorageThroughputPerIops' - Maximum storage throughput to provisioned IOPS ratio for a DB instance.
--
-- 'minIopsPerDbInstance', 'orderableDBInstanceOption_minIopsPerDbInstance' - Minimum total provisioned IOPS for a DB instance.
--
-- 'minIopsPerGib', 'orderableDBInstanceOption_minIopsPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
--
-- 'minStorageSize', 'orderableDBInstanceOption_minStorageSize' - Minimum storage size for a DB instance.
--
-- 'minStorageThroughputPerDbInstance', 'orderableDBInstanceOption_minStorageThroughputPerDbInstance' - Minimum storage throughput for a DB instance.
--
-- 'minStorageThroughputPerIops', 'orderableDBInstanceOption_minStorageThroughputPerIops' - Minimum storage throughput to provisioned IOPS ratio for a DB instance.
--
-- 'multiAZCapable', 'orderableDBInstanceOption_multiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
--
-- 'outpostCapable', 'orderableDBInstanceOption_outpostCapable' - Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide./
--
-- 'readReplicaCapable', 'orderableDBInstanceOption_readReplicaCapable' - Indicates whether a DB instance can have a read replica.
--
-- 'storageType', 'orderableDBInstanceOption_storageType' - Indicates the storage type for a DB instance.
--
-- 'supportedActivityStreamModes', 'orderableDBInstanceOption_supportedActivityStreamModes' - The list of supported modes for Database Activity Streams. Aurora
-- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
-- Oracle return @[async]@ only. If Database Activity Streams isn\'t
-- supported, the return value is an empty list.
--
-- 'supportedEngineModes', 'orderableDBInstanceOption_supportedEngineModes' - A list of the supported DB engine modes.
--
-- 'supportedNetworkTypes', 'orderableDBInstanceOption_supportedNetworkTypes' - The network types supported by the DB instance (@IPV4@ or @DUAL@).
--
-- A DB instance can support only the IPv4 protocol or the IPv4 and the
-- IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
--
-- 'supportsClusters', 'orderableDBInstanceOption_supportsClusters' - Whether DB instances can be configured as a Multi-AZ DB cluster.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide./
--
-- 'supportsEnhancedMonitoring', 'orderableDBInstanceOption_supportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
--
-- 'supportsGlobalDatabases', 'orderableDBInstanceOption_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- a specific combination of other DB engine attributes.
--
-- 'supportsIAMDatabaseAuthentication', 'orderableDBInstanceOption_supportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
--
-- 'supportsIops', 'orderableDBInstanceOption_supportsIops' - Indicates whether a DB instance supports provisioned IOPS.
--
-- 'supportsKerberosAuthentication', 'orderableDBInstanceOption_supportsKerberosAuthentication' - Whether a DB instance supports Kerberos Authentication.
--
-- 'supportsPerformanceInsights', 'orderableDBInstanceOption_supportsPerformanceInsights' - True if a DB instance supports Performance Insights, otherwise false.
--
-- 'supportsStorageAutoscaling', 'orderableDBInstanceOption_supportsStorageAutoscaling' - Whether Amazon RDS can automatically scale storage for DB instances that
-- use the specified DB instance class.
--
-- 'supportsStorageEncryption', 'orderableDBInstanceOption_supportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
--
-- 'supportsStorageThroughput', 'orderableDBInstanceOption_supportsStorageThroughput' - Indicates whether a DB instance supports storage throughput.
--
-- 'vpc', 'orderableDBInstanceOption_vpc' - Indicates whether a DB instance is in a VPC.
newOrderableDBInstanceOption ::
  OrderableDBInstanceOption
newOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { availabilityZoneGroup =
        Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      availableProcessorFeatures = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      maxIopsPerDbInstance = Prelude.Nothing,
      maxIopsPerGib = Prelude.Nothing,
      maxStorageSize = Prelude.Nothing,
      maxStorageThroughputPerDbInstance =
        Prelude.Nothing,
      maxStorageThroughputPerIops = Prelude.Nothing,
      minIopsPerDbInstance = Prelude.Nothing,
      minIopsPerGib = Prelude.Nothing,
      minStorageSize = Prelude.Nothing,
      minStorageThroughputPerDbInstance =
        Prelude.Nothing,
      minStorageThroughputPerIops = Prelude.Nothing,
      multiAZCapable = Prelude.Nothing,
      outpostCapable = Prelude.Nothing,
      readReplicaCapable = Prelude.Nothing,
      storageType = Prelude.Nothing,
      supportedActivityStreamModes = Prelude.Nothing,
      supportedEngineModes = Prelude.Nothing,
      supportedNetworkTypes = Prelude.Nothing,
      supportsClusters = Prelude.Nothing,
      supportsEnhancedMonitoring = Prelude.Nothing,
      supportsGlobalDatabases = Prelude.Nothing,
      supportsIAMDatabaseAuthentication =
        Prelude.Nothing,
      supportsIops = Prelude.Nothing,
      supportsKerberosAuthentication = Prelude.Nothing,
      supportsPerformanceInsights = Prelude.Nothing,
      supportsStorageAutoscaling = Prelude.Nothing,
      supportsStorageEncryption = Prelude.Nothing,
      supportsStorageThroughput = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | The Availability Zone group for a DB instance.
orderableDBInstanceOption_availabilityZoneGroup :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_availabilityZoneGroup = Lens.lens (\OrderableDBInstanceOption' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZoneGroup = a} :: OrderableDBInstanceOption)

-- | A list of Availability Zones for a DB instance.
orderableDBInstanceOption_availabilityZones :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailabilityZone])
orderableDBInstanceOption_availabilityZones = Lens.lens (\OrderableDBInstanceOption' {availabilityZones} -> availabilityZones) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZones = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | A list of the available processor features for the DB instance class of
-- a DB instance.
orderableDBInstanceOption_availableProcessorFeatures :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailableProcessorFeature])
orderableDBInstanceOption_availableProcessorFeatures = Lens.lens (\OrderableDBInstanceOption' {availableProcessorFeatures} -> availableProcessorFeatures) (\s@OrderableDBInstanceOption' {} a -> s {availableProcessorFeatures = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | The DB instance class for a DB instance.
orderableDBInstanceOption_dbInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_dbInstanceClass = Lens.lens (\OrderableDBInstanceOption' {dbInstanceClass} -> dbInstanceClass) (\s@OrderableDBInstanceOption' {} a -> s {dbInstanceClass = a} :: OrderableDBInstanceOption)

-- | The engine type of a DB instance.
orderableDBInstanceOption_engine :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engine = Lens.lens (\OrderableDBInstanceOption' {engine} -> engine) (\s@OrderableDBInstanceOption' {} a -> s {engine = a} :: OrderableDBInstanceOption)

-- | The engine version of a DB instance.
orderableDBInstanceOption_engineVersion :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engineVersion = Lens.lens (\OrderableDBInstanceOption' {engineVersion} -> engineVersion) (\s@OrderableDBInstanceOption' {} a -> s {engineVersion = a} :: OrderableDBInstanceOption)

-- | The license model for a DB instance.
orderableDBInstanceOption_licenseModel :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_licenseModel = Lens.lens (\OrderableDBInstanceOption' {licenseModel} -> licenseModel) (\s@OrderableDBInstanceOption' {} a -> s {licenseModel = a} :: OrderableDBInstanceOption)

-- | Maximum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_maxIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerDbInstance} -> maxIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Maximum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_maxIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_maxIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerGib} -> maxIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Maximum storage size for a DB instance.
orderableDBInstanceOption_maxStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxStorageSize = Lens.lens (\OrderableDBInstanceOption' {maxStorageSize} -> maxStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {maxStorageSize = a} :: OrderableDBInstanceOption)

-- | Maximum storage throughput for a DB instance.
orderableDBInstanceOption_maxStorageThroughputPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxStorageThroughputPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {maxStorageThroughputPerDbInstance} -> maxStorageThroughputPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {maxStorageThroughputPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Maximum storage throughput to provisioned IOPS ratio for a DB instance.
orderableDBInstanceOption_maxStorageThroughputPerIops :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_maxStorageThroughputPerIops = Lens.lens (\OrderableDBInstanceOption' {maxStorageThroughputPerIops} -> maxStorageThroughputPerIops) (\s@OrderableDBInstanceOption' {} a -> s {maxStorageThroughputPerIops = a} :: OrderableDBInstanceOption)

-- | Minimum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_minIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {minIopsPerDbInstance} -> minIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Minimum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_minIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_minIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {minIopsPerGib} -> minIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Minimum storage size for a DB instance.
orderableDBInstanceOption_minStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minStorageSize = Lens.lens (\OrderableDBInstanceOption' {minStorageSize} -> minStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {minStorageSize = a} :: OrderableDBInstanceOption)

-- | Minimum storage throughput for a DB instance.
orderableDBInstanceOption_minStorageThroughputPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minStorageThroughputPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {minStorageThroughputPerDbInstance} -> minStorageThroughputPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {minStorageThroughputPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Minimum storage throughput to provisioned IOPS ratio for a DB instance.
orderableDBInstanceOption_minStorageThroughputPerIops :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_minStorageThroughputPerIops = Lens.lens (\OrderableDBInstanceOption' {minStorageThroughputPerIops} -> minStorageThroughputPerIops) (\s@OrderableDBInstanceOption' {} a -> s {minStorageThroughputPerIops = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is Multi-AZ capable.
orderableDBInstanceOption_multiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_multiAZCapable = Lens.lens (\OrderableDBInstanceOption' {multiAZCapable} -> multiAZCapable) (\s@OrderableDBInstanceOption' {} a -> s {multiAZCapable = a} :: OrderableDBInstanceOption)

-- | Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide./
orderableDBInstanceOption_outpostCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_outpostCapable = Lens.lens (\OrderableDBInstanceOption' {outpostCapable} -> outpostCapable) (\s@OrderableDBInstanceOption' {} a -> s {outpostCapable = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance can have a read replica.
orderableDBInstanceOption_readReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_readReplicaCapable = Lens.lens (\OrderableDBInstanceOption' {readReplicaCapable} -> readReplicaCapable) (\s@OrderableDBInstanceOption' {} a -> s {readReplicaCapable = a} :: OrderableDBInstanceOption)

-- | Indicates the storage type for a DB instance.
orderableDBInstanceOption_storageType :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_storageType = Lens.lens (\OrderableDBInstanceOption' {storageType} -> storageType) (\s@OrderableDBInstanceOption' {} a -> s {storageType = a} :: OrderableDBInstanceOption)

-- | The list of supported modes for Database Activity Streams. Aurora
-- PostgreSQL returns the value @[sync, async]@. Aurora MySQL and RDS for
-- Oracle return @[async]@ only. If Database Activity Streams isn\'t
-- supported, the return value is an empty list.
orderableDBInstanceOption_supportedActivityStreamModes :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [Prelude.Text])
orderableDBInstanceOption_supportedActivityStreamModes = Lens.lens (\OrderableDBInstanceOption' {supportedActivityStreamModes} -> supportedActivityStreamModes) (\s@OrderableDBInstanceOption' {} a -> s {supportedActivityStreamModes = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | A list of the supported DB engine modes.
orderableDBInstanceOption_supportedEngineModes :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [Prelude.Text])
orderableDBInstanceOption_supportedEngineModes = Lens.lens (\OrderableDBInstanceOption' {supportedEngineModes} -> supportedEngineModes) (\s@OrderableDBInstanceOption' {} a -> s {supportedEngineModes = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | The network types supported by the DB instance (@IPV4@ or @DUAL@).
--
-- A DB instance can support only the IPv4 protocol or the IPv4 and the
-- IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
orderableDBInstanceOption_supportedNetworkTypes :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [Prelude.Text])
orderableDBInstanceOption_supportedNetworkTypes = Lens.lens (\OrderableDBInstanceOption' {supportedNetworkTypes} -> supportedNetworkTypes) (\s@OrderableDBInstanceOption' {} a -> s {supportedNetworkTypes = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | Whether DB instances can be configured as a Multi-AZ DB cluster.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide./
orderableDBInstanceOption_supportsClusters :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsClusters = Lens.lens (\OrderableDBInstanceOption' {supportsClusters} -> supportsClusters) (\s@OrderableDBInstanceOption' {} a -> s {supportsClusters = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
orderableDBInstanceOption_supportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsEnhancedMonitoring = Lens.lens (\OrderableDBInstanceOption' {supportsEnhancedMonitoring} -> supportsEnhancedMonitoring) (\s@OrderableDBInstanceOption' {} a -> s {supportsEnhancedMonitoring = a} :: OrderableDBInstanceOption)

-- | A value that indicates whether you can use Aurora global databases with
-- a specific combination of other DB engine attributes.
orderableDBInstanceOption_supportsGlobalDatabases :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsGlobalDatabases = Lens.lens (\OrderableDBInstanceOption' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@OrderableDBInstanceOption' {} a -> s {supportsGlobalDatabases = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports IAM database authentication.
orderableDBInstanceOption_supportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIAMDatabaseAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsIAMDatabaseAuthentication} -> supportsIAMDatabaseAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsIAMDatabaseAuthentication = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports provisioned IOPS.
orderableDBInstanceOption_supportsIops :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIops = Lens.lens (\OrderableDBInstanceOption' {supportsIops} -> supportsIops) (\s@OrderableDBInstanceOption' {} a -> s {supportsIops = a} :: OrderableDBInstanceOption)

-- | Whether a DB instance supports Kerberos Authentication.
orderableDBInstanceOption_supportsKerberosAuthentication :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsKerberosAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsKerberosAuthentication} -> supportsKerberosAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsKerberosAuthentication = a} :: OrderableDBInstanceOption)

-- | True if a DB instance supports Performance Insights, otherwise false.
orderableDBInstanceOption_supportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsPerformanceInsights = Lens.lens (\OrderableDBInstanceOption' {supportsPerformanceInsights} -> supportsPerformanceInsights) (\s@OrderableDBInstanceOption' {} a -> s {supportsPerformanceInsights = a} :: OrderableDBInstanceOption)

-- | Whether Amazon RDS can automatically scale storage for DB instances that
-- use the specified DB instance class.
orderableDBInstanceOption_supportsStorageAutoscaling :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageAutoscaling = Lens.lens (\OrderableDBInstanceOption' {supportsStorageAutoscaling} -> supportsStorageAutoscaling) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageAutoscaling = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports encrypted storage.
orderableDBInstanceOption_supportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageEncryption = Lens.lens (\OrderableDBInstanceOption' {supportsStorageEncryption} -> supportsStorageEncryption) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageEncryption = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports storage throughput.
orderableDBInstanceOption_supportsStorageThroughput :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageThroughput = Lens.lens (\OrderableDBInstanceOption' {supportsStorageThroughput} -> supportsStorageThroughput) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageThroughput = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is in a VPC.
orderableDBInstanceOption_vpc :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_vpc = Lens.lens (\OrderableDBInstanceOption' {vpc} -> vpc) (\s@OrderableDBInstanceOption' {} a -> s {vpc = a} :: OrderableDBInstanceOption)

instance Data.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Prelude.<$> (x Data..@? "AvailabilityZoneGroup")
      Prelude.<*> ( x
                      Data..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> ( x
                      Data..@? "AvailableProcessorFeatures"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "AvailableProcessorFeature")
                  )
      Prelude.<*> (x Data..@? "DBInstanceClass")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "LicenseModel")
      Prelude.<*> (x Data..@? "MaxIopsPerDbInstance")
      Prelude.<*> (x Data..@? "MaxIopsPerGib")
      Prelude.<*> (x Data..@? "MaxStorageSize")
      Prelude.<*> (x Data..@? "MaxStorageThroughputPerDbInstance")
      Prelude.<*> (x Data..@? "MaxStorageThroughputPerIops")
      Prelude.<*> (x Data..@? "MinIopsPerDbInstance")
      Prelude.<*> (x Data..@? "MinIopsPerGib")
      Prelude.<*> (x Data..@? "MinStorageSize")
      Prelude.<*> (x Data..@? "MinStorageThroughputPerDbInstance")
      Prelude.<*> (x Data..@? "MinStorageThroughputPerIops")
      Prelude.<*> (x Data..@? "MultiAZCapable")
      Prelude.<*> (x Data..@? "OutpostCapable")
      Prelude.<*> (x Data..@? "ReadReplicaCapable")
      Prelude.<*> (x Data..@? "StorageType")
      Prelude.<*> ( x
                      Data..@? "SupportedActivityStreamModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "SupportedEngineModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "SupportedNetworkTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "SupportsClusters")
      Prelude.<*> (x Data..@? "SupportsEnhancedMonitoring")
      Prelude.<*> (x Data..@? "SupportsGlobalDatabases")
      Prelude.<*> (x Data..@? "SupportsIAMDatabaseAuthentication")
      Prelude.<*> (x Data..@? "SupportsIops")
      Prelude.<*> (x Data..@? "SupportsKerberosAuthentication")
      Prelude.<*> (x Data..@? "SupportsPerformanceInsights")
      Prelude.<*> (x Data..@? "SupportsStorageAutoscaling")
      Prelude.<*> (x Data..@? "SupportsStorageEncryption")
      Prelude.<*> (x Data..@? "SupportsStorageThroughput")
      Prelude.<*> (x Data..@? "Vpc")

instance Prelude.Hashable OrderableDBInstanceOption where
  hashWithSalt _salt OrderableDBInstanceOption' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZoneGroup
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` availableProcessorFeatures
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` maxIopsPerDbInstance
      `Prelude.hashWithSalt` maxIopsPerGib
      `Prelude.hashWithSalt` maxStorageSize
      `Prelude.hashWithSalt` maxStorageThroughputPerDbInstance
      `Prelude.hashWithSalt` maxStorageThroughputPerIops
      `Prelude.hashWithSalt` minIopsPerDbInstance
      `Prelude.hashWithSalt` minIopsPerGib
      `Prelude.hashWithSalt` minStorageSize
      `Prelude.hashWithSalt` minStorageThroughputPerDbInstance
      `Prelude.hashWithSalt` minStorageThroughputPerIops
      `Prelude.hashWithSalt` multiAZCapable
      `Prelude.hashWithSalt` outpostCapable
      `Prelude.hashWithSalt` readReplicaCapable
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` supportedActivityStreamModes
      `Prelude.hashWithSalt` supportedEngineModes
      `Prelude.hashWithSalt` supportedNetworkTypes
      `Prelude.hashWithSalt` supportsClusters
      `Prelude.hashWithSalt` supportsEnhancedMonitoring
      `Prelude.hashWithSalt` supportsGlobalDatabases
      `Prelude.hashWithSalt` supportsIAMDatabaseAuthentication
      `Prelude.hashWithSalt` supportsIops
      `Prelude.hashWithSalt` supportsKerberosAuthentication
      `Prelude.hashWithSalt` supportsPerformanceInsights
      `Prelude.hashWithSalt` supportsStorageAutoscaling
      `Prelude.hashWithSalt` supportsStorageEncryption
      `Prelude.hashWithSalt` supportsStorageThroughput
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData OrderableDBInstanceOption where
  rnf OrderableDBInstanceOption' {..} =
    Prelude.rnf availabilityZoneGroup
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf availableProcessorFeatures
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf maxIopsPerDbInstance
      `Prelude.seq` Prelude.rnf maxIopsPerGib
      `Prelude.seq` Prelude.rnf maxStorageSize
      `Prelude.seq` Prelude.rnf maxStorageThroughputPerDbInstance
      `Prelude.seq` Prelude.rnf maxStorageThroughputPerIops
      `Prelude.seq` Prelude.rnf minIopsPerDbInstance
      `Prelude.seq` Prelude.rnf minIopsPerGib
      `Prelude.seq` Prelude.rnf minStorageSize
      `Prelude.seq` Prelude.rnf
        minStorageThroughputPerDbInstance
      `Prelude.seq` Prelude.rnf
        minStorageThroughputPerIops
      `Prelude.seq` Prelude.rnf multiAZCapable
      `Prelude.seq` Prelude.rnf outpostCapable
      `Prelude.seq` Prelude.rnf readReplicaCapable
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf
        supportedActivityStreamModes
      `Prelude.seq` Prelude.rnf
        supportedEngineModes
      `Prelude.seq` Prelude.rnf
        supportedNetworkTypes
      `Prelude.seq` Prelude.rnf
        supportsClusters
      `Prelude.seq` Prelude.rnf
        supportsEnhancedMonitoring
      `Prelude.seq` Prelude.rnf
        supportsGlobalDatabases
      `Prelude.seq` Prelude.rnf
        supportsIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf
        supportsIops
      `Prelude.seq` Prelude.rnf
        supportsKerberosAuthentication
      `Prelude.seq` Prelude.rnf
        supportsPerformanceInsights
      `Prelude.seq` Prelude.rnf
        supportsStorageAutoscaling
      `Prelude.seq` Prelude.rnf
        supportsStorageEncryption
      `Prelude.seq` Prelude.rnf
        supportsStorageThroughput
      `Prelude.seq` Prelude.rnf
        vpc
