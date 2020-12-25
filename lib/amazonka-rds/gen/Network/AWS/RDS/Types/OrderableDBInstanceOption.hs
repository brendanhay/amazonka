{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OrderableDBInstanceOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OrderableDBInstanceOption
  ( OrderableDBInstanceOption (..),

    -- * Smart constructor
    mkOrderableDBInstanceOption,

    -- * Lenses
    odbioAvailabilityZoneGroup,
    odbioAvailabilityZones,
    odbioAvailableProcessorFeatures,
    odbioDBInstanceClass,
    odbioEngine,
    odbioEngineVersion,
    odbioLicenseModel,
    odbioMaxIopsPerDbInstance,
    odbioMaxIopsPerGib,
    odbioMaxStorageSize,
    odbioMinIopsPerDbInstance,
    odbioMinIopsPerGib,
    odbioMinStorageSize,
    odbioMultiAZCapable,
    odbioOutpostCapable,
    odbioReadReplicaCapable,
    odbioStorageType,
    odbioSupportedEngineModes,
    odbioSupportsEnhancedMonitoring,
    odbioSupportsGlobalDatabases,
    odbioSupportsIAMDatabaseAuthentication,
    odbioSupportsIops,
    odbioSupportsKerberosAuthentication,
    odbioSupportsPerformanceInsights,
    odbioSupportsStorageAutoscaling,
    odbioSupportsStorageEncryption,
    odbioVpc,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.AvailabilityZone as Types
import qualified Network.AWS.RDS.Types.AvailableProcessorFeature as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains a list of available options for a DB instance.
--
-- This data type is used as a response element in the @DescribeOrderableDBInstanceOptions@ action.
--
-- /See:/ 'mkOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { -- | The Availability Zone group for a DB instance.
    availabilityZoneGroup :: Core.Maybe Types.String,
    -- | A list of Availability Zones for a DB instance.
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | A list of the available processor features for the DB instance class of a DB instance.
    availableProcessorFeatures :: Core.Maybe [Types.AvailableProcessorFeature],
    -- | The DB instance class for a DB instance.
    dBInstanceClass :: Core.Maybe Types.String,
    -- | The engine type of a DB instance.
    engine :: Core.Maybe Types.String,
    -- | The engine version of a DB instance.
    engineVersion :: Core.Maybe Types.String,
    -- | The license model for a DB instance.
    licenseModel :: Core.Maybe Types.String,
    -- | Maximum total provisioned IOPS for a DB instance.
    maxIopsPerDbInstance :: Core.Maybe Core.Int,
    -- | Maximum provisioned IOPS per GiB for a DB instance.
    maxIopsPerGib :: Core.Maybe Core.Double,
    -- | Maximum storage size for a DB instance.
    maxStorageSize :: Core.Maybe Core.Int,
    -- | Minimum total provisioned IOPS for a DB instance.
    minIopsPerDbInstance :: Core.Maybe Core.Int,
    -- | Minimum provisioned IOPS per GiB for a DB instance.
    minIopsPerGib :: Core.Maybe Core.Double,
    -- | Minimum storage size for a DB instance.
    minStorageSize :: Core.Maybe Core.Int,
    -- | Indicates whether a DB instance is Multi-AZ capable.
    multiAZCapable :: Core.Maybe Core.Bool,
    -- | Whether a DB instance supports RDS on Outposts.
    --
    -- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
    outpostCapable :: Core.Maybe Core.Bool,
    -- | Indicates whether a DB instance can have a read replica.
    readReplicaCapable :: Core.Maybe Core.Bool,
    -- | Indicates the storage type for a DB instance.
    storageType :: Core.Maybe Types.String,
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Core.Maybe [Types.String],
    -- | Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
    supportsEnhancedMonitoring :: Core.Maybe Core.Bool,
    -- | A value that indicates whether you can use Aurora global databases with a specific combination of other DB engine attributes.
    supportsGlobalDatabases :: Core.Maybe Core.Bool,
    -- | Indicates whether a DB instance supports IAM database authentication.
    supportsIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | Indicates whether a DB instance supports provisioned IOPS.
    supportsIops :: Core.Maybe Core.Bool,
    -- | Whether a DB instance supports Kerberos Authentication.
    supportsKerberosAuthentication :: Core.Maybe Core.Bool,
    -- | True if a DB instance supports Performance Insights, otherwise false.
    supportsPerformanceInsights :: Core.Maybe Core.Bool,
    -- | Whether Amazon RDS can automatically scale storage for DB instances that use the specified DB instance class.
    supportsStorageAutoscaling :: Core.Maybe Core.Bool,
    -- | Indicates whether a DB instance supports encrypted storage.
    supportsStorageEncryption :: Core.Maybe Core.Bool,
    -- | Indicates whether a DB instance is in a VPC.
    vpc :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrderableDBInstanceOption' value with any optional fields omitted.
mkOrderableDBInstanceOption ::
  OrderableDBInstanceOption
mkOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { availabilityZoneGroup = Core.Nothing,
      availabilityZones = Core.Nothing,
      availableProcessorFeatures = Core.Nothing,
      dBInstanceClass = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      licenseModel = Core.Nothing,
      maxIopsPerDbInstance = Core.Nothing,
      maxIopsPerGib = Core.Nothing,
      maxStorageSize = Core.Nothing,
      minIopsPerDbInstance = Core.Nothing,
      minIopsPerGib = Core.Nothing,
      minStorageSize = Core.Nothing,
      multiAZCapable = Core.Nothing,
      outpostCapable = Core.Nothing,
      readReplicaCapable = Core.Nothing,
      storageType = Core.Nothing,
      supportedEngineModes = Core.Nothing,
      supportsEnhancedMonitoring = Core.Nothing,
      supportsGlobalDatabases = Core.Nothing,
      supportsIAMDatabaseAuthentication = Core.Nothing,
      supportsIops = Core.Nothing,
      supportsKerberosAuthentication = Core.Nothing,
      supportsPerformanceInsights = Core.Nothing,
      supportsStorageAutoscaling = Core.Nothing,
      supportsStorageEncryption = Core.Nothing,
      vpc = Core.Nothing
    }

-- | The Availability Zone group for a DB instance.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioAvailabilityZoneGroup :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Types.String)
odbioAvailabilityZoneGroup = Lens.field @"availabilityZoneGroup"
{-# DEPRECATED odbioAvailabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead." #-}

-- | A list of Availability Zones for a DB instance.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioAvailabilityZones :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [Types.AvailabilityZone])
odbioAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED odbioAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of the available processor features for the DB instance class of a DB instance.
--
-- /Note:/ Consider using 'availableProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioAvailableProcessorFeatures :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [Types.AvailableProcessorFeature])
odbioAvailableProcessorFeatures = Lens.field @"availableProcessorFeatures"
{-# DEPRECATED odbioAvailableProcessorFeatures "Use generic-lens or generic-optics with 'availableProcessorFeatures' instead." #-}

-- | The DB instance class for a DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioDBInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Types.String)
odbioDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED odbioDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | The engine type of a DB instance.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioEngine :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Types.String)
odbioEngine = Lens.field @"engine"
{-# DEPRECATED odbioEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The engine version of a DB instance.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioEngineVersion :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Types.String)
odbioEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED odbioEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The license model for a DB instance.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioLicenseModel :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Types.String)
odbioLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED odbioLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | Maximum total provisioned IOPS for a DB instance.
--
-- /Note:/ Consider using 'maxIopsPerDbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMaxIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
odbioMaxIopsPerDbInstance = Lens.field @"maxIopsPerDbInstance"
{-# DEPRECATED odbioMaxIopsPerDbInstance "Use generic-lens or generic-optics with 'maxIopsPerDbInstance' instead." #-}

-- | Maximum provisioned IOPS per GiB for a DB instance.
--
-- /Note:/ Consider using 'maxIopsPerGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMaxIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Double)
odbioMaxIopsPerGib = Lens.field @"maxIopsPerGib"
{-# DEPRECATED odbioMaxIopsPerGib "Use generic-lens or generic-optics with 'maxIopsPerGib' instead." #-}

-- | Maximum storage size for a DB instance.
--
-- /Note:/ Consider using 'maxStorageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMaxStorageSize :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
odbioMaxStorageSize = Lens.field @"maxStorageSize"
{-# DEPRECATED odbioMaxStorageSize "Use generic-lens or generic-optics with 'maxStorageSize' instead." #-}

-- | Minimum total provisioned IOPS for a DB instance.
--
-- /Note:/ Consider using 'minIopsPerDbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMinIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
odbioMinIopsPerDbInstance = Lens.field @"minIopsPerDbInstance"
{-# DEPRECATED odbioMinIopsPerDbInstance "Use generic-lens or generic-optics with 'minIopsPerDbInstance' instead." #-}

-- | Minimum provisioned IOPS per GiB for a DB instance.
--
-- /Note:/ Consider using 'minIopsPerGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMinIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Double)
odbioMinIopsPerGib = Lens.field @"minIopsPerGib"
{-# DEPRECATED odbioMinIopsPerGib "Use generic-lens or generic-optics with 'minIopsPerGib' instead." #-}

-- | Minimum storage size for a DB instance.
--
-- /Note:/ Consider using 'minStorageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMinStorageSize :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
odbioMinStorageSize = Lens.field @"minStorageSize"
{-# DEPRECATED odbioMinStorageSize "Use generic-lens or generic-optics with 'minStorageSize' instead." #-}

-- | Indicates whether a DB instance is Multi-AZ capable.
--
-- /Note:/ Consider using 'multiAZCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMultiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioMultiAZCapable = Lens.field @"multiAZCapable"
{-# DEPRECATED odbioMultiAZCapable "Use generic-lens or generic-optics with 'multiAZCapable' instead." #-}

-- | Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'outpostCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioOutpostCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioOutpostCapable = Lens.field @"outpostCapable"
{-# DEPRECATED odbioOutpostCapable "Use generic-lens or generic-optics with 'outpostCapable' instead." #-}

-- | Indicates whether a DB instance can have a read replica.
--
-- /Note:/ Consider using 'readReplicaCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioReadReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioReadReplicaCapable = Lens.field @"readReplicaCapable"
{-# DEPRECATED odbioReadReplicaCapable "Use generic-lens or generic-optics with 'readReplicaCapable' instead." #-}

-- | Indicates the storage type for a DB instance.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioStorageType :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Types.String)
odbioStorageType = Lens.field @"storageType"
{-# DEPRECATED odbioStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | A list of the supported DB engine modes.
--
-- /Note:/ Consider using 'supportedEngineModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportedEngineModes :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [Types.String])
odbioSupportedEngineModes = Lens.field @"supportedEngineModes"
{-# DEPRECATED odbioSupportedEngineModes "Use generic-lens or generic-optics with 'supportedEngineModes' instead." #-}

-- | Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
--
-- /Note:/ Consider using 'supportsEnhancedMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsEnhancedMonitoring = Lens.field @"supportsEnhancedMonitoring"
{-# DEPRECATED odbioSupportsEnhancedMonitoring "Use generic-lens or generic-optics with 'supportsEnhancedMonitoring' instead." #-}

-- | A value that indicates whether you can use Aurora global databases with a specific combination of other DB engine attributes.
--
-- /Note:/ Consider using 'supportsGlobalDatabases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsGlobalDatabases :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsGlobalDatabases = Lens.field @"supportsGlobalDatabases"
{-# DEPRECATED odbioSupportsGlobalDatabases "Use generic-lens or generic-optics with 'supportsGlobalDatabases' instead." #-}

-- | Indicates whether a DB instance supports IAM database authentication.
--
-- /Note:/ Consider using 'supportsIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsIAMDatabaseAuthentication = Lens.field @"supportsIAMDatabaseAuthentication"
{-# DEPRECATED odbioSupportsIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'supportsIAMDatabaseAuthentication' instead." #-}

-- | Indicates whether a DB instance supports provisioned IOPS.
--
-- /Note:/ Consider using 'supportsIops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsIops :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsIops = Lens.field @"supportsIops"
{-# DEPRECATED odbioSupportsIops "Use generic-lens or generic-optics with 'supportsIops' instead." #-}

-- | Whether a DB instance supports Kerberos Authentication.
--
-- /Note:/ Consider using 'supportsKerberosAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsKerberosAuthentication :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsKerberosAuthentication = Lens.field @"supportsKerberosAuthentication"
{-# DEPRECATED odbioSupportsKerberosAuthentication "Use generic-lens or generic-optics with 'supportsKerberosAuthentication' instead." #-}

-- | True if a DB instance supports Performance Insights, otherwise false.
--
-- /Note:/ Consider using 'supportsPerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsPerformanceInsights = Lens.field @"supportsPerformanceInsights"
{-# DEPRECATED odbioSupportsPerformanceInsights "Use generic-lens or generic-optics with 'supportsPerformanceInsights' instead." #-}

-- | Whether Amazon RDS can automatically scale storage for DB instances that use the specified DB instance class.
--
-- /Note:/ Consider using 'supportsStorageAutoscaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsStorageAutoscaling :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsStorageAutoscaling = Lens.field @"supportsStorageAutoscaling"
{-# DEPRECATED odbioSupportsStorageAutoscaling "Use generic-lens or generic-optics with 'supportsStorageAutoscaling' instead." #-}

-- | Indicates whether a DB instance supports encrypted storage.
--
-- /Note:/ Consider using 'supportsStorageEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsStorageEncryption = Lens.field @"supportsStorageEncryption"
{-# DEPRECATED odbioSupportsStorageEncryption "Use generic-lens or generic-optics with 'supportsStorageEncryption' instead." #-}

-- | Indicates whether a DB instance is in a VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioVpc :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioVpc = Lens.field @"vpc"
{-# DEPRECATED odbioVpc "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Core.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Core.<$> (x Core..@? "AvailabilityZoneGroup")
      Core.<*> ( x Core..@? "AvailabilityZones"
                   Core..<@> Core.parseXMLList "AvailabilityZone"
               )
      Core.<*> ( x Core..@? "AvailableProcessorFeatures"
                   Core..<@> Core.parseXMLList "AvailableProcessorFeature"
               )
      Core.<*> (x Core..@? "DBInstanceClass")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "MaxIopsPerDbInstance")
      Core.<*> (x Core..@? "MaxIopsPerGib")
      Core.<*> (x Core..@? "MaxStorageSize")
      Core.<*> (x Core..@? "MinIopsPerDbInstance")
      Core.<*> (x Core..@? "MinIopsPerGib")
      Core.<*> (x Core..@? "MinStorageSize")
      Core.<*> (x Core..@? "MultiAZCapable")
      Core.<*> (x Core..@? "OutpostCapable")
      Core.<*> (x Core..@? "ReadReplicaCapable")
      Core.<*> (x Core..@? "StorageType")
      Core.<*> ( x Core..@? "SupportedEngineModes"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "SupportsEnhancedMonitoring")
      Core.<*> (x Core..@? "SupportsGlobalDatabases")
      Core.<*> (x Core..@? "SupportsIAMDatabaseAuthentication")
      Core.<*> (x Core..@? "SupportsIops")
      Core.<*> (x Core..@? "SupportsKerberosAuthentication")
      Core.<*> (x Core..@? "SupportsPerformanceInsights")
      Core.<*> (x Core..@? "SupportsStorageAutoscaling")
      Core.<*> (x Core..@? "SupportsStorageEncryption")
      Core.<*> (x Core..@? "Vpc")
