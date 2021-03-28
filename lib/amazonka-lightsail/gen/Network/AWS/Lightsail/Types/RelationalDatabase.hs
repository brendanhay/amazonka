{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.RelationalDatabase
  ( RelationalDatabase (..)
  -- * Smart constructor
  , mkRelationalDatabase
  -- * Lenses
  , rdArn
  , rdBackupRetentionEnabled
  , rdCaCertificateIdentifier
  , rdCreatedAt
  , rdEngine
  , rdEngineVersion
  , rdHardware
  , rdLatestRestorableTime
  , rdLocation
  , rdMasterDatabaseName
  , rdMasterEndpoint
  , rdMasterUsername
  , rdName
  , rdParameterApplyStatus
  , rdPendingMaintenanceActions
  , rdPendingModifiedValues
  , rdPreferredBackupWindow
  , rdPreferredMaintenanceWindow
  , rdPubliclyAccessible
  , rdRelationalDatabaseBlueprintId
  , rdRelationalDatabaseBundleId
  , rdResourceType
  , rdSecondaryAvailabilityZone
  , rdState
  , rdSupportCode
  , rdTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.PendingMaintenanceAction as Types
import qualified Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues as Types
import qualified Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint as Types
import qualified Network.AWS.Lightsail.Types.RelationalDatabaseHardware as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a database.
--
-- /See:/ 'mkRelationalDatabase' smart constructor.
data RelationalDatabase = RelationalDatabase'
  { arn :: Core.Maybe Types.NonEmptyString
    -- ^ The Amazon Resource Name (ARN) of the database.
  , backupRetentionEnabled :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether automated backup retention is enabled for the database.
  , caCertificateIdentifier :: Core.Maybe Core.Text
    -- ^ The certificate associated with the database.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the database was created. Formatted in Unix time.
  , engine :: Core.Maybe Types.NonEmptyString
    -- ^ The database software (for example, @MySQL@ ).
  , engineVersion :: Core.Maybe Types.NonEmptyString
    -- ^ The database engine version (for example, @5.7.23@ ).
  , hardware :: Core.Maybe Types.RelationalDatabaseHardware
    -- ^ Describes the hardware of the database.
  , latestRestorableTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The latest point in time to which the database can be restored. Formatted in Unix time.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The Region name and Availability Zone where the database is located.
  , masterDatabaseName :: Core.Maybe Core.Text
    -- ^ The name of the master database created when the Lightsail database resource is created.
  , masterEndpoint :: Core.Maybe Types.RelationalDatabaseEndpoint
    -- ^ The master endpoint for the database.
  , masterUsername :: Core.Maybe Types.NonEmptyString
    -- ^ The master user name of the database.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The unique name of the database resource in Lightsail.
  , parameterApplyStatus :: Core.Maybe Types.NonEmptyString
    -- ^ The status of parameter updates for the database.
  , pendingMaintenanceActions :: Core.Maybe [Types.PendingMaintenanceAction]
    -- ^ Describes the pending maintenance actions for the database.
  , pendingModifiedValues :: Core.Maybe Types.PendingModifiedRelationalDatabaseValues
    -- ^ Describes pending database value modifications.
  , preferredBackupWindow :: Core.Maybe Types.NonEmptyString
    -- ^ The daily time range during which automated backups are created for the database (for example, @16:00-16:30@ ).
  , preferredMaintenanceWindow :: Core.Maybe Types.NonEmptyString
    -- ^ The weekly time range during which system maintenance can occur on the database.
--
-- In the format @ddd:hh24:mi-ddd:hh24:mi@ . For example, @Tue:17:00-Tue:17:30@ .
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the database is publicly accessible.
  , relationalDatabaseBlueprintId :: Core.Maybe Types.NonEmptyString
    -- ^ The blueprint ID for the database. A blueprint describes the major engine version of a database.
  , relationalDatabaseBundleId :: Core.Maybe Types.NonEmptyString
    -- ^ The bundle ID for the database. A bundle describes the performance specifications for your database.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The Lightsail resource type for the database (for example, @RelationalDatabase@ ).
  , secondaryAvailabilityZone :: Core.Maybe Core.Text
    -- ^ Describes the secondary Availability Zone of a high availability database.
--
-- The secondary database is used for failover support of a high availability database.
  , state :: Core.Maybe Types.NonEmptyString
    -- ^ Describes the current state of the database.
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code for the database. Include this code in your email to support when you have questions about a database in Lightsail. This code enables our support team to look up your Lightsail information more easily.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RelationalDatabase' value with any optional fields omitted.
mkRelationalDatabase
    :: RelationalDatabase
mkRelationalDatabase
  = RelationalDatabase'{arn = Core.Nothing,
                        backupRetentionEnabled = Core.Nothing,
                        caCertificateIdentifier = Core.Nothing, createdAt = Core.Nothing,
                        engine = Core.Nothing, engineVersion = Core.Nothing,
                        hardware = Core.Nothing, latestRestorableTime = Core.Nothing,
                        location = Core.Nothing, masterDatabaseName = Core.Nothing,
                        masterEndpoint = Core.Nothing, masterUsername = Core.Nothing,
                        name = Core.Nothing, parameterApplyStatus = Core.Nothing,
                        pendingMaintenanceActions = Core.Nothing,
                        pendingModifiedValues = Core.Nothing,
                        preferredBackupWindow = Core.Nothing,
                        preferredMaintenanceWindow = Core.Nothing,
                        publiclyAccessible = Core.Nothing,
                        relationalDatabaseBlueprintId = Core.Nothing,
                        relationalDatabaseBundleId = Core.Nothing,
                        resourceType = Core.Nothing,
                        secondaryAvailabilityZone = Core.Nothing, state = Core.Nothing,
                        supportCode = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the database.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdArn :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdArn = Lens.field @"arn"
{-# INLINEABLE rdArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A Boolean value indicating whether automated backup retention is enabled for the database.
--
-- /Note:/ Consider using 'backupRetentionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdBackupRetentionEnabled :: Lens.Lens' RelationalDatabase (Core.Maybe Core.Bool)
rdBackupRetentionEnabled = Lens.field @"backupRetentionEnabled"
{-# INLINEABLE rdBackupRetentionEnabled #-}
{-# DEPRECATED backupRetentionEnabled "Use generic-lens or generic-optics with 'backupRetentionEnabled' instead"  #-}

-- | The certificate associated with the database.
--
-- /Note:/ Consider using 'caCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCaCertificateIdentifier :: Lens.Lens' RelationalDatabase (Core.Maybe Core.Text)
rdCaCertificateIdentifier = Lens.field @"caCertificateIdentifier"
{-# INLINEABLE rdCaCertificateIdentifier #-}
{-# DEPRECATED caCertificateIdentifier "Use generic-lens or generic-optics with 'caCertificateIdentifier' instead"  #-}

-- | The timestamp when the database was created. Formatted in Unix time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCreatedAt :: Lens.Lens' RelationalDatabase (Core.Maybe Core.NominalDiffTime)
rdCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE rdCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The database software (for example, @MySQL@ ).
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdEngine :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdEngine = Lens.field @"engine"
{-# INLINEABLE rdEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The database engine version (for example, @5.7.23@ ).
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdEngineVersion :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE rdEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | Describes the hardware of the database.
--
-- /Note:/ Consider using 'hardware' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdHardware :: Lens.Lens' RelationalDatabase (Core.Maybe Types.RelationalDatabaseHardware)
rdHardware = Lens.field @"hardware"
{-# INLINEABLE rdHardware #-}
{-# DEPRECATED hardware "Use generic-lens or generic-optics with 'hardware' instead"  #-}

-- | The latest point in time to which the database can be restored. Formatted in Unix time.
--
-- /Note:/ Consider using 'latestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLatestRestorableTime :: Lens.Lens' RelationalDatabase (Core.Maybe Core.NominalDiffTime)
rdLatestRestorableTime = Lens.field @"latestRestorableTime"
{-# INLINEABLE rdLatestRestorableTime #-}
{-# DEPRECATED latestRestorableTime "Use generic-lens or generic-optics with 'latestRestorableTime' instead"  #-}

-- | The Region name and Availability Zone where the database is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLocation :: Lens.Lens' RelationalDatabase (Core.Maybe Types.ResourceLocation)
rdLocation = Lens.field @"location"
{-# INLINEABLE rdLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the master database created when the Lightsail database resource is created.
--
-- /Note:/ Consider using 'masterDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdMasterDatabaseName :: Lens.Lens' RelationalDatabase (Core.Maybe Core.Text)
rdMasterDatabaseName = Lens.field @"masterDatabaseName"
{-# INLINEABLE rdMasterDatabaseName #-}
{-# DEPRECATED masterDatabaseName "Use generic-lens or generic-optics with 'masterDatabaseName' instead"  #-}

-- | The master endpoint for the database.
--
-- /Note:/ Consider using 'masterEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdMasterEndpoint :: Lens.Lens' RelationalDatabase (Core.Maybe Types.RelationalDatabaseEndpoint)
rdMasterEndpoint = Lens.field @"masterEndpoint"
{-# INLINEABLE rdMasterEndpoint #-}
{-# DEPRECATED masterEndpoint "Use generic-lens or generic-optics with 'masterEndpoint' instead"  #-}

-- | The master user name of the database.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdMasterUsername :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdMasterUsername = Lens.field @"masterUsername"
{-# INLINEABLE rdMasterUsername #-}
{-# DEPRECATED masterUsername "Use generic-lens or generic-optics with 'masterUsername' instead"  #-}

-- | The unique name of the database resource in Lightsail.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdName :: Lens.Lens' RelationalDatabase (Core.Maybe Types.ResourceName)
rdName = Lens.field @"name"
{-# INLINEABLE rdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The status of parameter updates for the database.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdParameterApplyStatus :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdParameterApplyStatus = Lens.field @"parameterApplyStatus"
{-# INLINEABLE rdParameterApplyStatus #-}
{-# DEPRECATED parameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead"  #-}

-- | Describes the pending maintenance actions for the database.
--
-- /Note:/ Consider using 'pendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPendingMaintenanceActions :: Lens.Lens' RelationalDatabase (Core.Maybe [Types.PendingMaintenanceAction])
rdPendingMaintenanceActions = Lens.field @"pendingMaintenanceActions"
{-# INLINEABLE rdPendingMaintenanceActions #-}
{-# DEPRECATED pendingMaintenanceActions "Use generic-lens or generic-optics with 'pendingMaintenanceActions' instead"  #-}

-- | Describes pending database value modifications.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPendingModifiedValues :: Lens.Lens' RelationalDatabase (Core.Maybe Types.PendingModifiedRelationalDatabaseValues)
rdPendingModifiedValues = Lens.field @"pendingModifiedValues"
{-# INLINEABLE rdPendingModifiedValues #-}
{-# DEPRECATED pendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead"  #-}

-- | The daily time range during which automated backups are created for the database (for example, @16:00-16:30@ ).
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPreferredBackupWindow :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# INLINEABLE rdPreferredBackupWindow #-}
{-# DEPRECATED preferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead"  #-}

-- | The weekly time range during which system maintenance can occur on the database.
--
-- In the format @ddd:hh24:mi-ddd:hh24:mi@ . For example, @Tue:17:00-Tue:17:30@ .
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPreferredMaintenanceWindow :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE rdPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | A Boolean value indicating whether the database is publicly accessible.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPubliclyAccessible :: Lens.Lens' RelationalDatabase (Core.Maybe Core.Bool)
rdPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE rdPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | The blueprint ID for the database. A blueprint describes the major engine version of a database.
--
-- /Note:/ Consider using 'relationalDatabaseBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRelationalDatabaseBlueprintId :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdRelationalDatabaseBlueprintId = Lens.field @"relationalDatabaseBlueprintId"
{-# INLINEABLE rdRelationalDatabaseBlueprintId #-}
{-# DEPRECATED relationalDatabaseBlueprintId "Use generic-lens or generic-optics with 'relationalDatabaseBlueprintId' instead"  #-}

-- | The bundle ID for the database. A bundle describes the performance specifications for your database.
--
-- /Note:/ Consider using 'relationalDatabaseBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRelationalDatabaseBundleId :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdRelationalDatabaseBundleId = Lens.field @"relationalDatabaseBundleId"
{-# INLINEABLE rdRelationalDatabaseBundleId #-}
{-# DEPRECATED relationalDatabaseBundleId "Use generic-lens or generic-optics with 'relationalDatabaseBundleId' instead"  #-}

-- | The Lightsail resource type for the database (for example, @RelationalDatabase@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdResourceType :: Lens.Lens' RelationalDatabase (Core.Maybe Types.ResourceType)
rdResourceType = Lens.field @"resourceType"
{-# INLINEABLE rdResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Describes the secondary Availability Zone of a high availability database.
--
-- The secondary database is used for failover support of a high availability database.
--
-- /Note:/ Consider using 'secondaryAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdSecondaryAvailabilityZone :: Lens.Lens' RelationalDatabase (Core.Maybe Core.Text)
rdSecondaryAvailabilityZone = Lens.field @"secondaryAvailabilityZone"
{-# INLINEABLE rdSecondaryAvailabilityZone #-}
{-# DEPRECATED secondaryAvailabilityZone "Use generic-lens or generic-optics with 'secondaryAvailabilityZone' instead"  #-}

-- | Describes the current state of the database.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdState :: Lens.Lens' RelationalDatabase (Core.Maybe Types.NonEmptyString)
rdState = Lens.field @"state"
{-# INLINEABLE rdState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The support code for the database. Include this code in your email to support when you have questions about a database in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdSupportCode :: Lens.Lens' RelationalDatabase (Core.Maybe Core.Text)
rdSupportCode = Lens.field @"supportCode"
{-# INLINEABLE rdSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdTags :: Lens.Lens' RelationalDatabase (Core.Maybe [Types.Tag])
rdTags = Lens.field @"tags"
{-# INLINEABLE rdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON RelationalDatabase where
        parseJSON
          = Core.withObject "RelationalDatabase" Core.$
              \ x ->
                RelationalDatabase' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "backupRetentionEnabled"
                    Core.<*> x Core..:? "caCertificateIdentifier"
                    Core.<*> x Core..:? "createdAt"
                    Core.<*> x Core..:? "engine"
                    Core.<*> x Core..:? "engineVersion"
                    Core.<*> x Core..:? "hardware"
                    Core.<*> x Core..:? "latestRestorableTime"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "masterDatabaseName"
                    Core.<*> x Core..:? "masterEndpoint"
                    Core.<*> x Core..:? "masterUsername"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "parameterApplyStatus"
                    Core.<*> x Core..:? "pendingMaintenanceActions"
                    Core.<*> x Core..:? "pendingModifiedValues"
                    Core.<*> x Core..:? "preferredBackupWindow"
                    Core.<*> x Core..:? "preferredMaintenanceWindow"
                    Core.<*> x Core..:? "publiclyAccessible"
                    Core.<*> x Core..:? "relationalDatabaseBlueprintId"
                    Core.<*> x Core..:? "relationalDatabaseBundleId"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "secondaryAvailabilityZone"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "supportCode"
                    Core.<*> x Core..:? "tags"
