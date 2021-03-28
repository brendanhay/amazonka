{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OrderableDBInstanceOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.OrderableDBInstanceOption
  ( OrderableDBInstanceOption (..)
  -- * Smart constructor
  , mkOrderableDBInstanceOption
  -- * Lenses
  , odbioAvailabilityZoneGroup
  , odbioAvailabilityZones
  , odbioAvailableProcessorFeatures
  , odbioDBInstanceClass
  , odbioEngine
  , odbioEngineVersion
  , odbioLicenseModel
  , odbioMaxIopsPerDbInstance
  , odbioMaxIopsPerGib
  , odbioMaxStorageSize
  , odbioMinIopsPerDbInstance
  , odbioMinIopsPerGib
  , odbioMinStorageSize
  , odbioMultiAZCapable
  , odbioOutpostCapable
  , odbioReadReplicaCapable
  , odbioStorageType
  , odbioSupportedEngineModes
  , odbioSupportsEnhancedMonitoring
  , odbioSupportsGlobalDatabases
  , odbioSupportsIAMDatabaseAuthentication
  , odbioSupportsIops
  , odbioSupportsKerberosAuthentication
  , odbioSupportsPerformanceInsights
  , odbioSupportsStorageAutoscaling
  , odbioSupportsStorageEncryption
  , odbioVpc
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.AvailabilityZone as Types
import qualified Network.AWS.RDS.Types.AvailableProcessorFeature as Types

-- | Contains a list of available options for a DB instance.
--
-- This data type is used as a response element in the @DescribeOrderableDBInstanceOptions@ action. 
--
-- /See:/ 'mkOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { availabilityZoneGroup :: Core.Maybe Core.Text
    -- ^ The Availability Zone group for a DB instance.
  , availabilityZones :: Core.Maybe [Types.AvailabilityZone]
    -- ^ A list of Availability Zones for a DB instance.
  , availableProcessorFeatures :: Core.Maybe [Types.AvailableProcessorFeature]
    -- ^ A list of the available processor features for the DB instance class of a DB instance.
  , dBInstanceClass :: Core.Maybe Core.Text
    -- ^ The DB instance class for a DB instance.
  , engine :: Core.Maybe Core.Text
    -- ^ The engine type of a DB instance.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The engine version of a DB instance.
  , licenseModel :: Core.Maybe Core.Text
    -- ^ The license model for a DB instance.
  , maxIopsPerDbInstance :: Core.Maybe Core.Int
    -- ^ Maximum total provisioned IOPS for a DB instance.
  , maxIopsPerGib :: Core.Maybe Core.Double
    -- ^ Maximum provisioned IOPS per GiB for a DB instance.
  , maxStorageSize :: Core.Maybe Core.Int
    -- ^ Maximum storage size for a DB instance.
  , minIopsPerDbInstance :: Core.Maybe Core.Int
    -- ^ Minimum total provisioned IOPS for a DB instance.
  , minIopsPerGib :: Core.Maybe Core.Double
    -- ^ Minimum provisioned IOPS per GiB for a DB instance.
  , minStorageSize :: Core.Maybe Core.Int
    -- ^ Minimum storage size for a DB instance.
  , multiAZCapable :: Core.Maybe Core.Bool
    -- ^ Indicates whether a DB instance is Multi-AZ capable.
  , outpostCapable :: Core.Maybe Core.Bool
    -- ^ Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./ 
  , readReplicaCapable :: Core.Maybe Core.Bool
    -- ^ Indicates whether a DB instance can have a read replica.
  , storageType :: Core.Maybe Core.Text
    -- ^ Indicates the storage type for a DB instance.
  , supportedEngineModes :: Core.Maybe [Core.Text]
    -- ^ A list of the supported DB engine modes.
  , supportsEnhancedMonitoring :: Core.Maybe Core.Bool
    -- ^ Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
  , supportsGlobalDatabases :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether you can use Aurora global databases with a specific combination of other DB engine attributes.
  , supportsIAMDatabaseAuthentication :: Core.Maybe Core.Bool
    -- ^ Indicates whether a DB instance supports IAM database authentication.
  , supportsIops :: Core.Maybe Core.Bool
    -- ^ Indicates whether a DB instance supports provisioned IOPS.
  , supportsKerberosAuthentication :: Core.Maybe Core.Bool
    -- ^ Whether a DB instance supports Kerberos Authentication.
  , supportsPerformanceInsights :: Core.Maybe Core.Bool
    -- ^ True if a DB instance supports Performance Insights, otherwise false.
  , supportsStorageAutoscaling :: Core.Maybe Core.Bool
    -- ^ Whether Amazon RDS can automatically scale storage for DB instances that use the specified DB instance class.
  , supportsStorageEncryption :: Core.Maybe Core.Bool
    -- ^ Indicates whether a DB instance supports encrypted storage.
  , vpc :: Core.Maybe Core.Bool
    -- ^ Indicates whether a DB instance is in a VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrderableDBInstanceOption' value with any optional fields omitted.
mkOrderableDBInstanceOption
    :: OrderableDBInstanceOption
mkOrderableDBInstanceOption
  = OrderableDBInstanceOption'{availabilityZoneGroup = Core.Nothing,
                               availabilityZones = Core.Nothing,
                               availableProcessorFeatures = Core.Nothing,
                               dBInstanceClass = Core.Nothing, engine = Core.Nothing,
                               engineVersion = Core.Nothing, licenseModel = Core.Nothing,
                               maxIopsPerDbInstance = Core.Nothing, maxIopsPerGib = Core.Nothing,
                               maxStorageSize = Core.Nothing, minIopsPerDbInstance = Core.Nothing,
                               minIopsPerGib = Core.Nothing, minStorageSize = Core.Nothing,
                               multiAZCapable = Core.Nothing, outpostCapable = Core.Nothing,
                               readReplicaCapable = Core.Nothing, storageType = Core.Nothing,
                               supportedEngineModes = Core.Nothing,
                               supportsEnhancedMonitoring = Core.Nothing,
                               supportsGlobalDatabases = Core.Nothing,
                               supportsIAMDatabaseAuthentication = Core.Nothing,
                               supportsIops = Core.Nothing,
                               supportsKerberosAuthentication = Core.Nothing,
                               supportsPerformanceInsights = Core.Nothing,
                               supportsStorageAutoscaling = Core.Nothing,
                               supportsStorageEncryption = Core.Nothing, vpc = Core.Nothing}

-- | The Availability Zone group for a DB instance.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioAvailabilityZoneGroup :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
odbioAvailabilityZoneGroup = Lens.field @"availabilityZoneGroup"
{-# INLINEABLE odbioAvailabilityZoneGroup #-}
{-# DEPRECATED availabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead"  #-}

-- | A list of Availability Zones for a DB instance.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioAvailabilityZones :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [Types.AvailabilityZone])
odbioAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE odbioAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | A list of the available processor features for the DB instance class of a DB instance.
--
-- /Note:/ Consider using 'availableProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioAvailableProcessorFeatures :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [Types.AvailableProcessorFeature])
odbioAvailableProcessorFeatures = Lens.field @"availableProcessorFeatures"
{-# INLINEABLE odbioAvailableProcessorFeatures #-}
{-# DEPRECATED availableProcessorFeatures "Use generic-lens or generic-optics with 'availableProcessorFeatures' instead"  #-}

-- | The DB instance class for a DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioDBInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
odbioDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE odbioDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | The engine type of a DB instance.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioEngine :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
odbioEngine = Lens.field @"engine"
{-# INLINEABLE odbioEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The engine version of a DB instance.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioEngineVersion :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
odbioEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE odbioEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The license model for a DB instance.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioLicenseModel :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
odbioLicenseModel = Lens.field @"licenseModel"
{-# INLINEABLE odbioLicenseModel #-}
{-# DEPRECATED licenseModel "Use generic-lens or generic-optics with 'licenseModel' instead"  #-}

-- | Maximum total provisioned IOPS for a DB instance.
--
-- /Note:/ Consider using 'maxIopsPerDbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMaxIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
odbioMaxIopsPerDbInstance = Lens.field @"maxIopsPerDbInstance"
{-# INLINEABLE odbioMaxIopsPerDbInstance #-}
{-# DEPRECATED maxIopsPerDbInstance "Use generic-lens or generic-optics with 'maxIopsPerDbInstance' instead"  #-}

-- | Maximum provisioned IOPS per GiB for a DB instance.
--
-- /Note:/ Consider using 'maxIopsPerGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMaxIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Double)
odbioMaxIopsPerGib = Lens.field @"maxIopsPerGib"
{-# INLINEABLE odbioMaxIopsPerGib #-}
{-# DEPRECATED maxIopsPerGib "Use generic-lens or generic-optics with 'maxIopsPerGib' instead"  #-}

-- | Maximum storage size for a DB instance.
--
-- /Note:/ Consider using 'maxStorageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMaxStorageSize :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
odbioMaxStorageSize = Lens.field @"maxStorageSize"
{-# INLINEABLE odbioMaxStorageSize #-}
{-# DEPRECATED maxStorageSize "Use generic-lens or generic-optics with 'maxStorageSize' instead"  #-}

-- | Minimum total provisioned IOPS for a DB instance.
--
-- /Note:/ Consider using 'minIopsPerDbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMinIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
odbioMinIopsPerDbInstance = Lens.field @"minIopsPerDbInstance"
{-# INLINEABLE odbioMinIopsPerDbInstance #-}
{-# DEPRECATED minIopsPerDbInstance "Use generic-lens or generic-optics with 'minIopsPerDbInstance' instead"  #-}

-- | Minimum provisioned IOPS per GiB for a DB instance.
--
-- /Note:/ Consider using 'minIopsPerGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMinIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Double)
odbioMinIopsPerGib = Lens.field @"minIopsPerGib"
{-# INLINEABLE odbioMinIopsPerGib #-}
{-# DEPRECATED minIopsPerGib "Use generic-lens or generic-optics with 'minIopsPerGib' instead"  #-}

-- | Minimum storage size for a DB instance.
--
-- /Note:/ Consider using 'minStorageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMinStorageSize :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Int)
odbioMinStorageSize = Lens.field @"minStorageSize"
{-# INLINEABLE odbioMinStorageSize #-}
{-# DEPRECATED minStorageSize "Use generic-lens or generic-optics with 'minStorageSize' instead"  #-}

-- | Indicates whether a DB instance is Multi-AZ capable.
--
-- /Note:/ Consider using 'multiAZCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioMultiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioMultiAZCapable = Lens.field @"multiAZCapable"
{-# INLINEABLE odbioMultiAZCapable #-}
{-# DEPRECATED multiAZCapable "Use generic-lens or generic-optics with 'multiAZCapable' instead"  #-}

-- | Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'outpostCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioOutpostCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioOutpostCapable = Lens.field @"outpostCapable"
{-# INLINEABLE odbioOutpostCapable #-}
{-# DEPRECATED outpostCapable "Use generic-lens or generic-optics with 'outpostCapable' instead"  #-}

-- | Indicates whether a DB instance can have a read replica.
--
-- /Note:/ Consider using 'readReplicaCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioReadReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioReadReplicaCapable = Lens.field @"readReplicaCapable"
{-# INLINEABLE odbioReadReplicaCapable #-}
{-# DEPRECATED readReplicaCapable "Use generic-lens or generic-optics with 'readReplicaCapable' instead"  #-}

-- | Indicates the storage type for a DB instance.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioStorageType :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Text)
odbioStorageType = Lens.field @"storageType"
{-# INLINEABLE odbioStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | A list of the supported DB engine modes.
--
-- /Note:/ Consider using 'supportedEngineModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportedEngineModes :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe [Core.Text])
odbioSupportedEngineModes = Lens.field @"supportedEngineModes"
{-# INLINEABLE odbioSupportedEngineModes #-}
{-# DEPRECATED supportedEngineModes "Use generic-lens or generic-optics with 'supportedEngineModes' instead"  #-}

-- | Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
--
-- /Note:/ Consider using 'supportsEnhancedMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsEnhancedMonitoring = Lens.field @"supportsEnhancedMonitoring"
{-# INLINEABLE odbioSupportsEnhancedMonitoring #-}
{-# DEPRECATED supportsEnhancedMonitoring "Use generic-lens or generic-optics with 'supportsEnhancedMonitoring' instead"  #-}

-- | A value that indicates whether you can use Aurora global databases with a specific combination of other DB engine attributes.
--
-- /Note:/ Consider using 'supportsGlobalDatabases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsGlobalDatabases :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsGlobalDatabases = Lens.field @"supportsGlobalDatabases"
{-# INLINEABLE odbioSupportsGlobalDatabases #-}
{-# DEPRECATED supportsGlobalDatabases "Use generic-lens or generic-optics with 'supportsGlobalDatabases' instead"  #-}

-- | Indicates whether a DB instance supports IAM database authentication.
--
-- /Note:/ Consider using 'supportsIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsIAMDatabaseAuthentication = Lens.field @"supportsIAMDatabaseAuthentication"
{-# INLINEABLE odbioSupportsIAMDatabaseAuthentication #-}
{-# DEPRECATED supportsIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'supportsIAMDatabaseAuthentication' instead"  #-}

-- | Indicates whether a DB instance supports provisioned IOPS.
--
-- /Note:/ Consider using 'supportsIops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsIops :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsIops = Lens.field @"supportsIops"
{-# INLINEABLE odbioSupportsIops #-}
{-# DEPRECATED supportsIops "Use generic-lens or generic-optics with 'supportsIops' instead"  #-}

-- | Whether a DB instance supports Kerberos Authentication.
--
-- /Note:/ Consider using 'supportsKerberosAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsKerberosAuthentication :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsKerberosAuthentication = Lens.field @"supportsKerberosAuthentication"
{-# INLINEABLE odbioSupportsKerberosAuthentication #-}
{-# DEPRECATED supportsKerberosAuthentication "Use generic-lens or generic-optics with 'supportsKerberosAuthentication' instead"  #-}

-- | True if a DB instance supports Performance Insights, otherwise false.
--
-- /Note:/ Consider using 'supportsPerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsPerformanceInsights = Lens.field @"supportsPerformanceInsights"
{-# INLINEABLE odbioSupportsPerformanceInsights #-}
{-# DEPRECATED supportsPerformanceInsights "Use generic-lens or generic-optics with 'supportsPerformanceInsights' instead"  #-}

-- | Whether Amazon RDS can automatically scale storage for DB instances that use the specified DB instance class.
--
-- /Note:/ Consider using 'supportsStorageAutoscaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsStorageAutoscaling :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsStorageAutoscaling = Lens.field @"supportsStorageAutoscaling"
{-# INLINEABLE odbioSupportsStorageAutoscaling #-}
{-# DEPRECATED supportsStorageAutoscaling "Use generic-lens or generic-optics with 'supportsStorageAutoscaling' instead"  #-}

-- | Indicates whether a DB instance supports encrypted storage.
--
-- /Note:/ Consider using 'supportsStorageEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioSupportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioSupportsStorageEncryption = Lens.field @"supportsStorageEncryption"
{-# INLINEABLE odbioSupportsStorageEncryption #-}
{-# DEPRECATED supportsStorageEncryption "Use generic-lens or generic-optics with 'supportsStorageEncryption' instead"  #-}

-- | Indicates whether a DB instance is in a VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odbioVpc :: Lens.Lens' OrderableDBInstanceOption (Core.Maybe Core.Bool)
odbioVpc = Lens.field @"vpc"
{-# INLINEABLE odbioVpc #-}
{-# DEPRECATED vpc "Use generic-lens or generic-optics with 'vpc' instead"  #-}

instance Core.FromXML OrderableDBInstanceOption where
        parseXML x
          = OrderableDBInstanceOption' Core.<$>
              (x Core..@? "AvailabilityZoneGroup") Core.<*>
                x Core..@? "AvailabilityZones" Core..<@>
                  Core.parseXMLList "AvailabilityZone"
                Core.<*>
                x Core..@? "AvailableProcessorFeatures" Core..<@>
                  Core.parseXMLList "AvailableProcessorFeature"
                Core.<*> x Core..@? "DBInstanceClass"
                Core.<*> x Core..@? "Engine"
                Core.<*> x Core..@? "EngineVersion"
                Core.<*> x Core..@? "LicenseModel"
                Core.<*> x Core..@? "MaxIopsPerDbInstance"
                Core.<*> x Core..@? "MaxIopsPerGib"
                Core.<*> x Core..@? "MaxStorageSize"
                Core.<*> x Core..@? "MinIopsPerDbInstance"
                Core.<*> x Core..@? "MinIopsPerGib"
                Core.<*> x Core..@? "MinStorageSize"
                Core.<*> x Core..@? "MultiAZCapable"
                Core.<*> x Core..@? "OutpostCapable"
                Core.<*> x Core..@? "ReadReplicaCapable"
                Core.<*> x Core..@? "StorageType"
                Core.<*>
                x Core..@? "SupportedEngineModes" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "SupportsEnhancedMonitoring"
                Core.<*> x Core..@? "SupportsGlobalDatabases"
                Core.<*> x Core..@? "SupportsIAMDatabaseAuthentication"
                Core.<*> x Core..@? "SupportsIops"
                Core.<*> x Core..@? "SupportsKerberosAuthentication"
                Core.<*> x Core..@? "SupportsPerformanceInsights"
                Core.<*> x Core..@? "SupportsStorageAutoscaling"
                Core.<*> x Core..@? "SupportsStorageEncryption"
                Core.<*> x Core..@? "Vpc"
