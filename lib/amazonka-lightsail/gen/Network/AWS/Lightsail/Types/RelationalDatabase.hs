-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabase
  ( RelationalDatabase (..),

    -- * Smart constructor
    mkRelationalDatabase,

    -- * Lenses
    rdEngineVersion,
    rdRelationalDatabaseBundleId,
    rdMasterEndpoint,
    rdState,
    rdResourceType,
    rdPubliclyAccessible,
    rdMasterUsername,
    rdArn,
    rdCreatedAt,
    rdLocation,
    rdEngine,
    rdLatestRestorableTime,
    rdPreferredMaintenanceWindow,
    rdRelationalDatabaseBlueprintId,
    rdCaCertificateIdentifier,
    rdName,
    rdBackupRetentionEnabled,
    rdPreferredBackupWindow,
    rdPendingMaintenanceActions,
    rdSupportCode,
    rdSecondaryAvailabilityZone,
    rdPendingModifiedValues,
    rdMasterDatabaseName,
    rdHardware,
    rdParameterApplyStatus,
    rdTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes a database.
--
-- /See:/ 'mkRelationalDatabase' smart constructor.
data RelationalDatabase = RelationalDatabase'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    relationalDatabaseBundleId :: Lude.Maybe Lude.Text,
    masterEndpoint ::
      Lude.Maybe RelationalDatabaseEndpoint,
    state :: Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    masterUsername :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    engine :: Lude.Maybe Lude.Text,
    latestRestorableTime :: Lude.Maybe Lude.Timestamp,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    relationalDatabaseBlueprintId :: Lude.Maybe Lude.Text,
    caCertificateIdentifier :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    backupRetentionEnabled :: Lude.Maybe Lude.Bool,
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    pendingMaintenanceActions ::
      Lude.Maybe [PendingMaintenanceAction],
    supportCode :: Lude.Maybe Lude.Text,
    secondaryAvailabilityZone :: Lude.Maybe Lude.Text,
    pendingModifiedValues ::
      Lude.Maybe PendingModifiedRelationalDatabaseValues,
    masterDatabaseName :: Lude.Maybe Lude.Text,
    hardware :: Lude.Maybe RelationalDatabaseHardware,
    parameterApplyStatus :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelationalDatabase' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the database.
-- * 'backupRetentionEnabled' - A Boolean value indicating whether automated backup retention is enabled for the database.
-- * 'caCertificateIdentifier' - The certificate associated with the database.
-- * 'createdAt' - The timestamp when the database was created. Formatted in Unix time.
-- * 'engine' - The database software (for example, @MySQL@ ).
-- * 'engineVersion' - The database engine version (for example, @5.7.23@ ).
-- * 'hardware' - Describes the hardware of the database.
-- * 'latestRestorableTime' - The latest point in time to which the database can be restored. Formatted in Unix time.
-- * 'location' - The Region name and Availability Zone where the database is located.
-- * 'masterDatabaseName' - The name of the master database created when the Lightsail database resource is created.
-- * 'masterEndpoint' - The master endpoint for the database.
-- * 'masterUsername' - The master user name of the database.
-- * 'name' - The unique name of the database resource in Lightsail.
-- * 'parameterApplyStatus' - The status of parameter updates for the database.
-- * 'pendingMaintenanceActions' - Describes the pending maintenance actions for the database.
-- * 'pendingModifiedValues' - Describes pending database value modifications.
-- * 'preferredBackupWindow' - The daily time range during which automated backups are created for the database (for example, @16:00-16:30@ ).
-- * 'preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur on the database.
--
-- In the format @ddd:hh24:mi-ddd:hh24:mi@ . For example, @Tue:17:00-Tue:17:30@ .
-- * 'publiclyAccessible' - A Boolean value indicating whether the database is publicly accessible.
-- * 'relationalDatabaseBlueprintId' - The blueprint ID for the database. A blueprint describes the major engine version of a database.
-- * 'relationalDatabaseBundleId' - The bundle ID for the database. A bundle describes the performance specifications for your database.
-- * 'resourceType' - The Lightsail resource type for the database (for example, @RelationalDatabase@ ).
-- * 'secondaryAvailabilityZone' - Describes the secondary Availability Zone of a high availability database.
--
-- The secondary database is used for failover support of a high availability database.
-- * 'state' - Describes the current state of the database.
-- * 'supportCode' - The support code for the database. Include this code in your email to support when you have questions about a database in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkRelationalDatabase ::
  RelationalDatabase
mkRelationalDatabase =
  RelationalDatabase'
    { engineVersion = Lude.Nothing,
      relationalDatabaseBundleId = Lude.Nothing,
      masterEndpoint = Lude.Nothing,
      state = Lude.Nothing,
      resourceType = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      masterUsername = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      engine = Lude.Nothing,
      latestRestorableTime = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      relationalDatabaseBlueprintId = Lude.Nothing,
      caCertificateIdentifier = Lude.Nothing,
      name = Lude.Nothing,
      backupRetentionEnabled = Lude.Nothing,
      preferredBackupWindow = Lude.Nothing,
      pendingMaintenanceActions = Lude.Nothing,
      supportCode = Lude.Nothing,
      secondaryAvailabilityZone = Lude.Nothing,
      pendingModifiedValues = Lude.Nothing,
      masterDatabaseName = Lude.Nothing,
      hardware = Lude.Nothing,
      parameterApplyStatus = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The database engine version (for example, @5.7.23@ ).
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdEngineVersion :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdEngineVersion = Lens.lens (engineVersion :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: RelationalDatabase)
{-# DEPRECATED rdEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The bundle ID for the database. A bundle describes the performance specifications for your database.
--
-- /Note:/ Consider using 'relationalDatabaseBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRelationalDatabaseBundleId :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdRelationalDatabaseBundleId = Lens.lens (relationalDatabaseBundleId :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {relationalDatabaseBundleId = a} :: RelationalDatabase)
{-# DEPRECATED rdRelationalDatabaseBundleId "Use generic-lens or generic-optics with 'relationalDatabaseBundleId' instead." #-}

-- | The master endpoint for the database.
--
-- /Note:/ Consider using 'masterEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdMasterEndpoint :: Lens.Lens' RelationalDatabase (Lude.Maybe RelationalDatabaseEndpoint)
rdMasterEndpoint = Lens.lens (masterEndpoint :: RelationalDatabase -> Lude.Maybe RelationalDatabaseEndpoint) (\s a -> s {masterEndpoint = a} :: RelationalDatabase)
{-# DEPRECATED rdMasterEndpoint "Use generic-lens or generic-optics with 'masterEndpoint' instead." #-}

-- | Describes the current state of the database.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdState :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdState = Lens.lens (state :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: RelationalDatabase)
{-# DEPRECATED rdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Lightsail resource type for the database (for example, @RelationalDatabase@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdResourceType :: Lens.Lens' RelationalDatabase (Lude.Maybe ResourceType)
rdResourceType = Lens.lens (resourceType :: RelationalDatabase -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: RelationalDatabase)
{-# DEPRECATED rdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A Boolean value indicating whether the database is publicly accessible.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPubliclyAccessible :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Bool)
rdPubliclyAccessible = Lens.lens (publiclyAccessible :: RelationalDatabase -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: RelationalDatabase)
{-# DEPRECATED rdPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The master user name of the database.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdMasterUsername :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdMasterUsername = Lens.lens (masterUsername :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: RelationalDatabase)
{-# DEPRECATED rdMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The Amazon Resource Name (ARN) of the database.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdArn :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdArn = Lens.lens (arn :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RelationalDatabase)
{-# DEPRECATED rdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the database was created. Formatted in Unix time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCreatedAt :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Timestamp)
rdCreatedAt = Lens.lens (createdAt :: RelationalDatabase -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: RelationalDatabase)
{-# DEPRECATED rdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The Region name and Availability Zone where the database is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLocation :: Lens.Lens' RelationalDatabase (Lude.Maybe ResourceLocation)
rdLocation = Lens.lens (location :: RelationalDatabase -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: RelationalDatabase)
{-# DEPRECATED rdLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The database software (for example, @MySQL@ ).
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdEngine :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdEngine = Lens.lens (engine :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: RelationalDatabase)
{-# DEPRECATED rdEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The latest point in time to which the database can be restored. Formatted in Unix time.
--
-- /Note:/ Consider using 'latestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLatestRestorableTime :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Timestamp)
rdLatestRestorableTime = Lens.lens (latestRestorableTime :: RelationalDatabase -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestRestorableTime = a} :: RelationalDatabase)
{-# DEPRECATED rdLatestRestorableTime "Use generic-lens or generic-optics with 'latestRestorableTime' instead." #-}

-- | The weekly time range during which system maintenance can occur on the database.
--
-- In the format @ddd:hh24:mi-ddd:hh24:mi@ . For example, @Tue:17:00-Tue:17:30@ .
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPreferredMaintenanceWindow :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: RelationalDatabase)
{-# DEPRECATED rdPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The blueprint ID for the database. A blueprint describes the major engine version of a database.
--
-- /Note:/ Consider using 'relationalDatabaseBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRelationalDatabaseBlueprintId :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdRelationalDatabaseBlueprintId = Lens.lens (relationalDatabaseBlueprintId :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {relationalDatabaseBlueprintId = a} :: RelationalDatabase)
{-# DEPRECATED rdRelationalDatabaseBlueprintId "Use generic-lens or generic-optics with 'relationalDatabaseBlueprintId' instead." #-}

-- | The certificate associated with the database.
--
-- /Note:/ Consider using 'caCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCaCertificateIdentifier :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdCaCertificateIdentifier = Lens.lens (caCertificateIdentifier :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {caCertificateIdentifier = a} :: RelationalDatabase)
{-# DEPRECATED rdCaCertificateIdentifier "Use generic-lens or generic-optics with 'caCertificateIdentifier' instead." #-}

-- | The unique name of the database resource in Lightsail.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdName :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdName = Lens.lens (name :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RelationalDatabase)
{-# DEPRECATED rdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A Boolean value indicating whether automated backup retention is enabled for the database.
--
-- /Note:/ Consider using 'backupRetentionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdBackupRetentionEnabled :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Bool)
rdBackupRetentionEnabled = Lens.lens (backupRetentionEnabled :: RelationalDatabase -> Lude.Maybe Lude.Bool) (\s a -> s {backupRetentionEnabled = a} :: RelationalDatabase)
{-# DEPRECATED rdBackupRetentionEnabled "Use generic-lens or generic-optics with 'backupRetentionEnabled' instead." #-}

-- | The daily time range during which automated backups are created for the database (for example, @16:00-16:30@ ).
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPreferredBackupWindow :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdPreferredBackupWindow = Lens.lens (preferredBackupWindow :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: RelationalDatabase)
{-# DEPRECATED rdPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | Describes the pending maintenance actions for the database.
--
-- /Note:/ Consider using 'pendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPendingMaintenanceActions :: Lens.Lens' RelationalDatabase (Lude.Maybe [PendingMaintenanceAction])
rdPendingMaintenanceActions = Lens.lens (pendingMaintenanceActions :: RelationalDatabase -> Lude.Maybe [PendingMaintenanceAction]) (\s a -> s {pendingMaintenanceActions = a} :: RelationalDatabase)
{-# DEPRECATED rdPendingMaintenanceActions "Use generic-lens or generic-optics with 'pendingMaintenanceActions' instead." #-}

-- | The support code for the database. Include this code in your email to support when you have questions about a database in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdSupportCode :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdSupportCode = Lens.lens (supportCode :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: RelationalDatabase)
{-# DEPRECATED rdSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | Describes the secondary Availability Zone of a high availability database.
--
-- The secondary database is used for failover support of a high availability database.
--
-- /Note:/ Consider using 'secondaryAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdSecondaryAvailabilityZone :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdSecondaryAvailabilityZone = Lens.lens (secondaryAvailabilityZone :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {secondaryAvailabilityZone = a} :: RelationalDatabase)
{-# DEPRECATED rdSecondaryAvailabilityZone "Use generic-lens or generic-optics with 'secondaryAvailabilityZone' instead." #-}

-- | Describes pending database value modifications.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPendingModifiedValues :: Lens.Lens' RelationalDatabase (Lude.Maybe PendingModifiedRelationalDatabaseValues)
rdPendingModifiedValues = Lens.lens (pendingModifiedValues :: RelationalDatabase -> Lude.Maybe PendingModifiedRelationalDatabaseValues) (\s a -> s {pendingModifiedValues = a} :: RelationalDatabase)
{-# DEPRECATED rdPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | The name of the master database created when the Lightsail database resource is created.
--
-- /Note:/ Consider using 'masterDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdMasterDatabaseName :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdMasterDatabaseName = Lens.lens (masterDatabaseName :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {masterDatabaseName = a} :: RelationalDatabase)
{-# DEPRECATED rdMasterDatabaseName "Use generic-lens or generic-optics with 'masterDatabaseName' instead." #-}

-- | Describes the hardware of the database.
--
-- /Note:/ Consider using 'hardware' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdHardware :: Lens.Lens' RelationalDatabase (Lude.Maybe RelationalDatabaseHardware)
rdHardware = Lens.lens (hardware :: RelationalDatabase -> Lude.Maybe RelationalDatabaseHardware) (\s a -> s {hardware = a} :: RelationalDatabase)
{-# DEPRECATED rdHardware "Use generic-lens or generic-optics with 'hardware' instead." #-}

-- | The status of parameter updates for the database.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdParameterApplyStatus :: Lens.Lens' RelationalDatabase (Lude.Maybe Lude.Text)
rdParameterApplyStatus = Lens.lens (parameterApplyStatus :: RelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {parameterApplyStatus = a} :: RelationalDatabase)
{-# DEPRECATED rdParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdTags :: Lens.Lens' RelationalDatabase (Lude.Maybe [Tag])
rdTags = Lens.lens (tags :: RelationalDatabase -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RelationalDatabase)
{-# DEPRECATED rdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON RelationalDatabase where
  parseJSON =
    Lude.withObject
      "RelationalDatabase"
      ( \x ->
          RelationalDatabase'
            Lude.<$> (x Lude..:? "engineVersion")
            Lude.<*> (x Lude..:? "relationalDatabaseBundleId")
            Lude.<*> (x Lude..:? "masterEndpoint")
            Lude.<*> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "publiclyAccessible")
            Lude.<*> (x Lude..:? "masterUsername")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "engine")
            Lude.<*> (x Lude..:? "latestRestorableTime")
            Lude.<*> (x Lude..:? "preferredMaintenanceWindow")
            Lude.<*> (x Lude..:? "relationalDatabaseBlueprintId")
            Lude.<*> (x Lude..:? "caCertificateIdentifier")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "backupRetentionEnabled")
            Lude.<*> (x Lude..:? "preferredBackupWindow")
            Lude.<*> (x Lude..:? "pendingMaintenanceActions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "secondaryAvailabilityZone")
            Lude.<*> (x Lude..:? "pendingModifiedValues")
            Lude.<*> (x Lude..:? "masterDatabaseName")
            Lude.<*> (x Lude..:? "hardware")
            Lude.<*> (x Lude..:? "parameterApplyStatus")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
