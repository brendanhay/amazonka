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
-- Module      : Amazonka.Neptune.Types.OrderableDBInstanceOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.OrderableDBInstanceOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types.AvailabilityZone
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of available options for a DB instance.
--
-- This data type is used as a response element in the
-- DescribeOrderableDBInstanceOptions action.
--
-- /See:/ 'newOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { -- | A list of Availability Zones for a DB instance.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
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
    -- | Minimum total provisioned IOPS for a DB instance.
    minIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Minimum provisioned IOPS per GiB for a DB instance.
    minIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | Minimum storage size for a DB instance.
    minStorageSize :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether a DB instance is Multi-AZ capable.
    multiAZCapable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance can have a Read Replica.
    readReplicaCapable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the storage type for a DB instance.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a DB instance supports Enhanced Monitoring at
    -- intervals from 1 to 60 seconds.
    supportsEnhancedMonitoring :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether you can use Neptune global databases with
    -- a specific combination of other DB engine attributes.
    supportsGlobalDatabases :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports IAM database authentication.
    supportsIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports provisioned IOPS.
    supportsIops :: Prelude.Maybe Prelude.Bool,
    -- | /(Not supported by Neptune)/
    supportsPerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports encrypted storage.
    supportsStorageEncryption :: Prelude.Maybe Prelude.Bool,
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
-- 'availabilityZones', 'orderableDBInstanceOption_availabilityZones' - A list of Availability Zones for a DB instance.
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
-- 'minIopsPerDbInstance', 'orderableDBInstanceOption_minIopsPerDbInstance' - Minimum total provisioned IOPS for a DB instance.
--
-- 'minIopsPerGib', 'orderableDBInstanceOption_minIopsPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
--
-- 'minStorageSize', 'orderableDBInstanceOption_minStorageSize' - Minimum storage size for a DB instance.
--
-- 'multiAZCapable', 'orderableDBInstanceOption_multiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
--
-- 'readReplicaCapable', 'orderableDBInstanceOption_readReplicaCapable' - Indicates whether a DB instance can have a Read Replica.
--
-- 'storageType', 'orderableDBInstanceOption_storageType' - Indicates the storage type for a DB instance.
--
-- 'supportsEnhancedMonitoring', 'orderableDBInstanceOption_supportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
--
-- 'supportsGlobalDatabases', 'orderableDBInstanceOption_supportsGlobalDatabases' - A value that indicates whether you can use Neptune global databases with
-- a specific combination of other DB engine attributes.
--
-- 'supportsIAMDatabaseAuthentication', 'orderableDBInstanceOption_supportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
--
-- 'supportsIops', 'orderableDBInstanceOption_supportsIops' - Indicates whether a DB instance supports provisioned IOPS.
--
-- 'supportsPerformanceInsights', 'orderableDBInstanceOption_supportsPerformanceInsights' - /(Not supported by Neptune)/
--
-- 'supportsStorageEncryption', 'orderableDBInstanceOption_supportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
--
-- 'vpc', 'orderableDBInstanceOption_vpc' - Indicates whether a DB instance is in a VPC.
newOrderableDBInstanceOption ::
  OrderableDBInstanceOption
newOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { availabilityZones =
        Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      maxIopsPerDbInstance = Prelude.Nothing,
      maxIopsPerGib = Prelude.Nothing,
      maxStorageSize = Prelude.Nothing,
      minIopsPerDbInstance = Prelude.Nothing,
      minIopsPerGib = Prelude.Nothing,
      minStorageSize = Prelude.Nothing,
      multiAZCapable = Prelude.Nothing,
      readReplicaCapable = Prelude.Nothing,
      storageType = Prelude.Nothing,
      supportsEnhancedMonitoring = Prelude.Nothing,
      supportsGlobalDatabases = Prelude.Nothing,
      supportsIAMDatabaseAuthentication =
        Prelude.Nothing,
      supportsIops = Prelude.Nothing,
      supportsPerformanceInsights = Prelude.Nothing,
      supportsStorageEncryption = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | A list of Availability Zones for a DB instance.
orderableDBInstanceOption_availabilityZones :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailabilityZone])
orderableDBInstanceOption_availabilityZones = Lens.lens (\OrderableDBInstanceOption' {availabilityZones} -> availabilityZones) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZones = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

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

-- | Minimum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_minIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {minIopsPerDbInstance} -> minIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Minimum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_minIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_minIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {minIopsPerGib} -> minIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Minimum storage size for a DB instance.
orderableDBInstanceOption_minStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minStorageSize = Lens.lens (\OrderableDBInstanceOption' {minStorageSize} -> minStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {minStorageSize = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is Multi-AZ capable.
orderableDBInstanceOption_multiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_multiAZCapable = Lens.lens (\OrderableDBInstanceOption' {multiAZCapable} -> multiAZCapable) (\s@OrderableDBInstanceOption' {} a -> s {multiAZCapable = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance can have a Read Replica.
orderableDBInstanceOption_readReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_readReplicaCapable = Lens.lens (\OrderableDBInstanceOption' {readReplicaCapable} -> readReplicaCapable) (\s@OrderableDBInstanceOption' {} a -> s {readReplicaCapable = a} :: OrderableDBInstanceOption)

-- | Indicates the storage type for a DB instance.
orderableDBInstanceOption_storageType :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_storageType = Lens.lens (\OrderableDBInstanceOption' {storageType} -> storageType) (\s@OrderableDBInstanceOption' {} a -> s {storageType = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
orderableDBInstanceOption_supportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsEnhancedMonitoring = Lens.lens (\OrderableDBInstanceOption' {supportsEnhancedMonitoring} -> supportsEnhancedMonitoring) (\s@OrderableDBInstanceOption' {} a -> s {supportsEnhancedMonitoring = a} :: OrderableDBInstanceOption)

-- | A value that indicates whether you can use Neptune global databases with
-- a specific combination of other DB engine attributes.
orderableDBInstanceOption_supportsGlobalDatabases :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsGlobalDatabases = Lens.lens (\OrderableDBInstanceOption' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@OrderableDBInstanceOption' {} a -> s {supportsGlobalDatabases = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports IAM database authentication.
orderableDBInstanceOption_supportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIAMDatabaseAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsIAMDatabaseAuthentication} -> supportsIAMDatabaseAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsIAMDatabaseAuthentication = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports provisioned IOPS.
orderableDBInstanceOption_supportsIops :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIops = Lens.lens (\OrderableDBInstanceOption' {supportsIops} -> supportsIops) (\s@OrderableDBInstanceOption' {} a -> s {supportsIops = a} :: OrderableDBInstanceOption)

-- | /(Not supported by Neptune)/
orderableDBInstanceOption_supportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsPerformanceInsights = Lens.lens (\OrderableDBInstanceOption' {supportsPerformanceInsights} -> supportsPerformanceInsights) (\s@OrderableDBInstanceOption' {} a -> s {supportsPerformanceInsights = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports encrypted storage.
orderableDBInstanceOption_supportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageEncryption = Lens.lens (\OrderableDBInstanceOption' {supportsStorageEncryption} -> supportsStorageEncryption) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageEncryption = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is in a VPC.
orderableDBInstanceOption_vpc :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_vpc = Lens.lens (\OrderableDBInstanceOption' {vpc} -> vpc) (\s@OrderableDBInstanceOption' {} a -> s {vpc = a} :: OrderableDBInstanceOption)

instance Data.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Prelude.<$> ( x
                      Data..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Data..@? "DBInstanceClass")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "LicenseModel")
      Prelude.<*> (x Data..@? "MaxIopsPerDbInstance")
      Prelude.<*> (x Data..@? "MaxIopsPerGib")
      Prelude.<*> (x Data..@? "MaxStorageSize")
      Prelude.<*> (x Data..@? "MinIopsPerDbInstance")
      Prelude.<*> (x Data..@? "MinIopsPerGib")
      Prelude.<*> (x Data..@? "MinStorageSize")
      Prelude.<*> (x Data..@? "MultiAZCapable")
      Prelude.<*> (x Data..@? "ReadReplicaCapable")
      Prelude.<*> (x Data..@? "StorageType")
      Prelude.<*> (x Data..@? "SupportsEnhancedMonitoring")
      Prelude.<*> (x Data..@? "SupportsGlobalDatabases")
      Prelude.<*> (x Data..@? "SupportsIAMDatabaseAuthentication")
      Prelude.<*> (x Data..@? "SupportsIops")
      Prelude.<*> (x Data..@? "SupportsPerformanceInsights")
      Prelude.<*> (x Data..@? "SupportsStorageEncryption")
      Prelude.<*> (x Data..@? "Vpc")

instance Prelude.Hashable OrderableDBInstanceOption where
  hashWithSalt _salt OrderableDBInstanceOption' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` maxIopsPerDbInstance
      `Prelude.hashWithSalt` maxIopsPerGib
      `Prelude.hashWithSalt` maxStorageSize
      `Prelude.hashWithSalt` minIopsPerDbInstance
      `Prelude.hashWithSalt` minIopsPerGib
      `Prelude.hashWithSalt` minStorageSize
      `Prelude.hashWithSalt` multiAZCapable
      `Prelude.hashWithSalt` readReplicaCapable
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` supportsEnhancedMonitoring
      `Prelude.hashWithSalt` supportsGlobalDatabases
      `Prelude.hashWithSalt` supportsIAMDatabaseAuthentication
      `Prelude.hashWithSalt` supportsIops
      `Prelude.hashWithSalt` supportsPerformanceInsights
      `Prelude.hashWithSalt` supportsStorageEncryption
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData OrderableDBInstanceOption where
  rnf OrderableDBInstanceOption' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf maxIopsPerDbInstance
      `Prelude.seq` Prelude.rnf maxIopsPerGib
      `Prelude.seq` Prelude.rnf maxStorageSize
      `Prelude.seq` Prelude.rnf minIopsPerDbInstance
      `Prelude.seq` Prelude.rnf minIopsPerGib
      `Prelude.seq` Prelude.rnf minStorageSize
      `Prelude.seq` Prelude.rnf multiAZCapable
      `Prelude.seq` Prelude.rnf readReplicaCapable
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf supportsEnhancedMonitoring
      `Prelude.seq` Prelude.rnf supportsGlobalDatabases
      `Prelude.seq` Prelude.rnf
        supportsIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf supportsIops
      `Prelude.seq` Prelude.rnf
        supportsPerformanceInsights
      `Prelude.seq` Prelude.rnf
        supportsStorageEncryption
      `Prelude.seq` Prelude.rnf vpc
