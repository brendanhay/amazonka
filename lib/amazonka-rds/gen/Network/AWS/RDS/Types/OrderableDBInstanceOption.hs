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
    odioEngineVersion,
    odioMinIOPSPerGib,
    odioSupportsIAMDatabaseAuthentication,
    odioMinIOPSPerDBInstance,
    odioMultiAZCapable,
    odioMaxStorageSize,
    odioSupportedEngineModes,
    odioAvailabilityZoneGroup,
    odioAvailableProcessorFeatures,
    odioEngine,
    odioMinStorageSize,
    odioOutpostCapable,
    odioSupportsIOPS,
    odioSupportsKerberosAuthentication,
    odioSupportsPerformanceInsights,
    odioDBInstanceClass,
    odioSupportsGlobalDatabases,
    odioLicenseModel,
    odioAvailabilityZones,
    odioSupportsStorageAutoscaling,
    odioSupportsStorageEncryption,
    odioReadReplicaCapable,
    odioMaxIOPSPerGib,
    odioVPC,
    odioSupportsEnhancedMonitoring,
    odioMaxIOPSPerDBInstance,
    odioStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.AvailabilityZone
import Network.AWS.RDS.Types.AvailableProcessorFeature

-- | Contains a list of available options for a DB instance.
--
-- This data type is used as a response element in the @DescribeOrderableDBInstanceOptions@ action.
--
-- /See:/ 'mkOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { -- | The engine version of a DB instance.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | Minimum provisioned IOPS per GiB for a DB instance.
    minIOPSPerGib :: Lude.Maybe Lude.Double,
    -- | Indicates whether a DB instance supports IAM database authentication.
    supportsIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    -- | Minimum total provisioned IOPS for a DB instance.
    minIOPSPerDBInstance :: Lude.Maybe Lude.Int,
    -- | Indicates whether a DB instance is Multi-AZ capable.
    multiAZCapable :: Lude.Maybe Lude.Bool,
    -- | Maximum storage size for a DB instance.
    maxStorageSize :: Lude.Maybe Lude.Int,
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Lude.Maybe [Lude.Text],
    -- | The Availability Zone group for a DB instance.
    availabilityZoneGroup :: Lude.Maybe Lude.Text,
    -- | A list of the available processor features for the DB instance class of a DB instance.
    availableProcessorFeatures :: Lude.Maybe [AvailableProcessorFeature],
    -- | The engine type of a DB instance.
    engine :: Lude.Maybe Lude.Text,
    -- | Minimum storage size for a DB instance.
    minStorageSize :: Lude.Maybe Lude.Int,
    -- | Whether a DB instance supports RDS on Outposts.
    --
    -- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
    outpostCapable :: Lude.Maybe Lude.Bool,
    -- | Indicates whether a DB instance supports provisioned IOPS.
    supportsIOPS :: Lude.Maybe Lude.Bool,
    -- | Whether a DB instance supports Kerberos Authentication.
    supportsKerberosAuthentication :: Lude.Maybe Lude.Bool,
    -- | True if a DB instance supports Performance Insights, otherwise false.
    supportsPerformanceInsights :: Lude.Maybe Lude.Bool,
    -- | The DB instance class for a DB instance.
    dbInstanceClass :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether you can use Aurora global databases with a specific combination of other DB engine attributes.
    supportsGlobalDatabases :: Lude.Maybe Lude.Bool,
    -- | The license model for a DB instance.
    licenseModel :: Lude.Maybe Lude.Text,
    -- | A list of Availability Zones for a DB instance.
    availabilityZones :: Lude.Maybe [AvailabilityZone],
    -- | Whether Amazon RDS can automatically scale storage for DB instances that use the specified DB instance class.
    supportsStorageAutoscaling :: Lude.Maybe Lude.Bool,
    -- | Indicates whether a DB instance supports encrypted storage.
    supportsStorageEncryption :: Lude.Maybe Lude.Bool,
    -- | Indicates whether a DB instance can have a read replica.
    readReplicaCapable :: Lude.Maybe Lude.Bool,
    -- | Maximum provisioned IOPS per GiB for a DB instance.
    maxIOPSPerGib :: Lude.Maybe Lude.Double,
    -- | Indicates whether a DB instance is in a VPC.
    vpc :: Lude.Maybe Lude.Bool,
    -- | Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
    supportsEnhancedMonitoring :: Lude.Maybe Lude.Bool,
    -- | Maximum total provisioned IOPS for a DB instance.
    maxIOPSPerDBInstance :: Lude.Maybe Lude.Int,
    -- | Indicates the storage type for a DB instance.
    storageType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrderableDBInstanceOption' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The engine version of a DB instance.
-- * 'minIOPSPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
-- * 'supportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
-- * 'minIOPSPerDBInstance' - Minimum total provisioned IOPS for a DB instance.
-- * 'multiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
-- * 'maxStorageSize' - Maximum storage size for a DB instance.
-- * 'supportedEngineModes' - A list of the supported DB engine modes.
-- * 'availabilityZoneGroup' - The Availability Zone group for a DB instance.
-- * 'availableProcessorFeatures' - A list of the available processor features for the DB instance class of a DB instance.
-- * 'engine' - The engine type of a DB instance.
-- * 'minStorageSize' - Minimum storage size for a DB instance.
-- * 'outpostCapable' - Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
-- * 'supportsIOPS' - Indicates whether a DB instance supports provisioned IOPS.
-- * 'supportsKerberosAuthentication' - Whether a DB instance supports Kerberos Authentication.
-- * 'supportsPerformanceInsights' - True if a DB instance supports Performance Insights, otherwise false.
-- * 'dbInstanceClass' - The DB instance class for a DB instance.
-- * 'supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with a specific combination of other DB engine attributes.
-- * 'licenseModel' - The license model for a DB instance.
-- * 'availabilityZones' - A list of Availability Zones for a DB instance.
-- * 'supportsStorageAutoscaling' - Whether Amazon RDS can automatically scale storage for DB instances that use the specified DB instance class.
-- * 'supportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
-- * 'readReplicaCapable' - Indicates whether a DB instance can have a read replica.
-- * 'maxIOPSPerGib' - Maximum provisioned IOPS per GiB for a DB instance.
-- * 'vpc' - Indicates whether a DB instance is in a VPC.
-- * 'supportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
-- * 'maxIOPSPerDBInstance' - Maximum total provisioned IOPS for a DB instance.
-- * 'storageType' - Indicates the storage type for a DB instance.
mkOrderableDBInstanceOption ::
  OrderableDBInstanceOption
mkOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { engineVersion = Lude.Nothing,
      minIOPSPerGib = Lude.Nothing,
      supportsIAMDatabaseAuthentication = Lude.Nothing,
      minIOPSPerDBInstance = Lude.Nothing,
      multiAZCapable = Lude.Nothing,
      maxStorageSize = Lude.Nothing,
      supportedEngineModes = Lude.Nothing,
      availabilityZoneGroup = Lude.Nothing,
      availableProcessorFeatures = Lude.Nothing,
      engine = Lude.Nothing,
      minStorageSize = Lude.Nothing,
      outpostCapable = Lude.Nothing,
      supportsIOPS = Lude.Nothing,
      supportsKerberosAuthentication = Lude.Nothing,
      supportsPerformanceInsights = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      supportsGlobalDatabases = Lude.Nothing,
      licenseModel = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      supportsStorageAutoscaling = Lude.Nothing,
      supportsStorageEncryption = Lude.Nothing,
      readReplicaCapable = Lude.Nothing,
      maxIOPSPerGib = Lude.Nothing,
      vpc = Lude.Nothing,
      supportsEnhancedMonitoring = Lude.Nothing,
      maxIOPSPerDBInstance = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | The engine version of a DB instance.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioEngineVersion :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Text)
odioEngineVersion = Lens.lens (engineVersion :: OrderableDBInstanceOption -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Minimum provisioned IOPS per GiB for a DB instance.
--
-- /Note:/ Consider using 'minIOPSPerGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioMinIOPSPerGib :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Double)
odioMinIOPSPerGib = Lens.lens (minIOPSPerGib :: OrderableDBInstanceOption -> Lude.Maybe Lude.Double) (\s a -> s {minIOPSPerGib = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioMinIOPSPerGib "Use generic-lens or generic-optics with 'minIOPSPerGib' instead." #-}

-- | Indicates whether a DB instance supports IAM database authentication.
--
-- /Note:/ Consider using 'supportsIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioSupportsIAMDatabaseAuthentication = Lens.lens (supportsIAMDatabaseAuthentication :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsIAMDatabaseAuthentication = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportsIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'supportsIAMDatabaseAuthentication' instead." #-}

-- | Minimum total provisioned IOPS for a DB instance.
--
-- /Note:/ Consider using 'minIOPSPerDBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioMinIOPSPerDBInstance :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Int)
odioMinIOPSPerDBInstance = Lens.lens (minIOPSPerDBInstance :: OrderableDBInstanceOption -> Lude.Maybe Lude.Int) (\s a -> s {minIOPSPerDBInstance = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioMinIOPSPerDBInstance "Use generic-lens or generic-optics with 'minIOPSPerDBInstance' instead." #-}

-- | Indicates whether a DB instance is Multi-AZ capable.
--
-- /Note:/ Consider using 'multiAZCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioMultiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioMultiAZCapable = Lens.lens (multiAZCapable :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZCapable = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioMultiAZCapable "Use generic-lens or generic-optics with 'multiAZCapable' instead." #-}

-- | Maximum storage size for a DB instance.
--
-- /Note:/ Consider using 'maxStorageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioMaxStorageSize :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Int)
odioMaxStorageSize = Lens.lens (maxStorageSize :: OrderableDBInstanceOption -> Lude.Maybe Lude.Int) (\s a -> s {maxStorageSize = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioMaxStorageSize "Use generic-lens or generic-optics with 'maxStorageSize' instead." #-}

-- | A list of the supported DB engine modes.
--
-- /Note:/ Consider using 'supportedEngineModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportedEngineModes :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe [Lude.Text])
odioSupportedEngineModes = Lens.lens (supportedEngineModes :: OrderableDBInstanceOption -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedEngineModes = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportedEngineModes "Use generic-lens or generic-optics with 'supportedEngineModes' instead." #-}

-- | The Availability Zone group for a DB instance.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioAvailabilityZoneGroup :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Text)
odioAvailabilityZoneGroup = Lens.lens (availabilityZoneGroup :: OrderableDBInstanceOption -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneGroup = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioAvailabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead." #-}

-- | A list of the available processor features for the DB instance class of a DB instance.
--
-- /Note:/ Consider using 'availableProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioAvailableProcessorFeatures :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe [AvailableProcessorFeature])
odioAvailableProcessorFeatures = Lens.lens (availableProcessorFeatures :: OrderableDBInstanceOption -> Lude.Maybe [AvailableProcessorFeature]) (\s a -> s {availableProcessorFeatures = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioAvailableProcessorFeatures "Use generic-lens or generic-optics with 'availableProcessorFeatures' instead." #-}

-- | The engine type of a DB instance.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioEngine :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Text)
odioEngine = Lens.lens (engine :: OrderableDBInstanceOption -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Minimum storage size for a DB instance.
--
-- /Note:/ Consider using 'minStorageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioMinStorageSize :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Int)
odioMinStorageSize = Lens.lens (minStorageSize :: OrderableDBInstanceOption -> Lude.Maybe Lude.Int) (\s a -> s {minStorageSize = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioMinStorageSize "Use generic-lens or generic-optics with 'minStorageSize' instead." #-}

-- | Whether a DB instance supports RDS on Outposts.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'outpostCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioOutpostCapable :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioOutpostCapable = Lens.lens (outpostCapable :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {outpostCapable = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioOutpostCapable "Use generic-lens or generic-optics with 'outpostCapable' instead." #-}

-- | Indicates whether a DB instance supports provisioned IOPS.
--
-- /Note:/ Consider using 'supportsIOPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportsIOPS :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioSupportsIOPS = Lens.lens (supportsIOPS :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsIOPS = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportsIOPS "Use generic-lens or generic-optics with 'supportsIOPS' instead." #-}

-- | Whether a DB instance supports Kerberos Authentication.
--
-- /Note:/ Consider using 'supportsKerberosAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportsKerberosAuthentication :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioSupportsKerberosAuthentication = Lens.lens (supportsKerberosAuthentication :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsKerberosAuthentication = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportsKerberosAuthentication "Use generic-lens or generic-optics with 'supportsKerberosAuthentication' instead." #-}

-- | True if a DB instance supports Performance Insights, otherwise false.
--
-- /Note:/ Consider using 'supportsPerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioSupportsPerformanceInsights = Lens.lens (supportsPerformanceInsights :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsPerformanceInsights = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportsPerformanceInsights "Use generic-lens or generic-optics with 'supportsPerformanceInsights' instead." #-}

-- | The DB instance class for a DB instance.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioDBInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Text)
odioDBInstanceClass = Lens.lens (dbInstanceClass :: OrderableDBInstanceOption -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | A value that indicates whether you can use Aurora global databases with a specific combination of other DB engine attributes.
--
-- /Note:/ Consider using 'supportsGlobalDatabases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportsGlobalDatabases :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioSupportsGlobalDatabases = Lens.lens (supportsGlobalDatabases :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsGlobalDatabases = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportsGlobalDatabases "Use generic-lens or generic-optics with 'supportsGlobalDatabases' instead." #-}

-- | The license model for a DB instance.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioLicenseModel :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Text)
odioLicenseModel = Lens.lens (licenseModel :: OrderableDBInstanceOption -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | A list of Availability Zones for a DB instance.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioAvailabilityZones :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe [AvailabilityZone])
odioAvailabilityZones = Lens.lens (availabilityZones :: OrderableDBInstanceOption -> Lude.Maybe [AvailabilityZone]) (\s a -> s {availabilityZones = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Whether Amazon RDS can automatically scale storage for DB instances that use the specified DB instance class.
--
-- /Note:/ Consider using 'supportsStorageAutoscaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportsStorageAutoscaling :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioSupportsStorageAutoscaling = Lens.lens (supportsStorageAutoscaling :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsStorageAutoscaling = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportsStorageAutoscaling "Use generic-lens or generic-optics with 'supportsStorageAutoscaling' instead." #-}

-- | Indicates whether a DB instance supports encrypted storage.
--
-- /Note:/ Consider using 'supportsStorageEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioSupportsStorageEncryption = Lens.lens (supportsStorageEncryption :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsStorageEncryption = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportsStorageEncryption "Use generic-lens or generic-optics with 'supportsStorageEncryption' instead." #-}

-- | Indicates whether a DB instance can have a read replica.
--
-- /Note:/ Consider using 'readReplicaCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioReadReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioReadReplicaCapable = Lens.lens (readReplicaCapable :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {readReplicaCapable = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioReadReplicaCapable "Use generic-lens or generic-optics with 'readReplicaCapable' instead." #-}

-- | Maximum provisioned IOPS per GiB for a DB instance.
--
-- /Note:/ Consider using 'maxIOPSPerGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioMaxIOPSPerGib :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Double)
odioMaxIOPSPerGib = Lens.lens (maxIOPSPerGib :: OrderableDBInstanceOption -> Lude.Maybe Lude.Double) (\s a -> s {maxIOPSPerGib = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioMaxIOPSPerGib "Use generic-lens or generic-optics with 'maxIOPSPerGib' instead." #-}

-- | Indicates whether a DB instance is in a VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioVPC :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioVPC = Lens.lens (vpc :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {vpc = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
--
-- /Note:/ Consider using 'supportsEnhancedMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioSupportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Bool)
odioSupportsEnhancedMonitoring = Lens.lens (supportsEnhancedMonitoring :: OrderableDBInstanceOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsEnhancedMonitoring = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioSupportsEnhancedMonitoring "Use generic-lens or generic-optics with 'supportsEnhancedMonitoring' instead." #-}

-- | Maximum total provisioned IOPS for a DB instance.
--
-- /Note:/ Consider using 'maxIOPSPerDBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioMaxIOPSPerDBInstance :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Int)
odioMaxIOPSPerDBInstance = Lens.lens (maxIOPSPerDBInstance :: OrderableDBInstanceOption -> Lude.Maybe Lude.Int) (\s a -> s {maxIOPSPerDBInstance = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioMaxIOPSPerDBInstance "Use generic-lens or generic-optics with 'maxIOPSPerDBInstance' instead." #-}

-- | Indicates the storage type for a DB instance.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odioStorageType :: Lens.Lens' OrderableDBInstanceOption (Lude.Maybe Lude.Text)
odioStorageType = Lens.lens (storageType :: OrderableDBInstanceOption -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: OrderableDBInstanceOption)
{-# DEPRECATED odioStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "MinIopsPerGib")
      Lude.<*> (x Lude..@? "SupportsIAMDatabaseAuthentication")
      Lude.<*> (x Lude..@? "MinIopsPerDbInstance")
      Lude.<*> (x Lude..@? "MultiAZCapable")
      Lude.<*> (x Lude..@? "MaxStorageSize")
      Lude.<*> ( x Lude..@? "SupportedEngineModes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "AvailabilityZoneGroup")
      Lude.<*> ( x Lude..@? "AvailableProcessorFeatures" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AvailableProcessorFeature")
               )
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "MinStorageSize")
      Lude.<*> (x Lude..@? "OutpostCapable")
      Lude.<*> (x Lude..@? "SupportsIops")
      Lude.<*> (x Lude..@? "SupportsKerberosAuthentication")
      Lude.<*> (x Lude..@? "SupportsPerformanceInsights")
      Lude.<*> (x Lude..@? "DBInstanceClass")
      Lude.<*> (x Lude..@? "SupportsGlobalDatabases")
      Lude.<*> (x Lude..@? "LicenseModel")
      Lude.<*> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AvailabilityZone")
               )
      Lude.<*> (x Lude..@? "SupportsStorageAutoscaling")
      Lude.<*> (x Lude..@? "SupportsStorageEncryption")
      Lude.<*> (x Lude..@? "ReadReplicaCapable")
      Lude.<*> (x Lude..@? "MaxIopsPerGib")
      Lude.<*> (x Lude..@? "Vpc")
      Lude.<*> (x Lude..@? "SupportsEnhancedMonitoring")
      Lude.<*> (x Lude..@? "MaxIopsPerDbInstance")
      Lude.<*> (x Lude..@? "StorageType")
