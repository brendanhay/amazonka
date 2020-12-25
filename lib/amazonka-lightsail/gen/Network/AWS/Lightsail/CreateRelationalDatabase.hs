{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database in Amazon Lightsail.
--
-- The @create relational database@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateRelationalDatabase
  ( -- * Creating a request
    CreateRelationalDatabase (..),
    mkCreateRelationalDatabase,

    -- ** Request lenses
    crdRelationalDatabaseName,
    crdRelationalDatabaseBlueprintId,
    crdRelationalDatabaseBundleId,
    crdMasterDatabaseName,
    crdMasterUsername,
    crdAvailabilityZone,
    crdMasterUserPassword,
    crdPreferredBackupWindow,
    crdPreferredMaintenanceWindow,
    crdPubliclyAccessible,
    crdTags,

    -- * Destructuring the response
    CreateRelationalDatabaseResponse (..),
    mkCreateRelationalDatabaseResponse,

    -- ** Response lenses
    crdrrsOperations,
    crdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRelationalDatabase' smart constructor.
data CreateRelationalDatabase = CreateRelationalDatabase'
  { -- | The name to use for your new database.
    --
    -- Constraints:
    --
    --     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    --
    --     * The first and last character must be a letter or number.
    relationalDatabaseName :: Types.ResourceName,
    -- | The blueprint ID for your new database. A blueprint describes the major engine version of a database.
    --
    -- You can get a list of database blueprints IDs by using the @get relational database blueprints@ operation.
    relationalDatabaseBlueprintId :: Types.String,
    -- | The bundle ID for your new database. A bundle describes the performance specifications for your database.
    --
    -- You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
    relationalDatabaseBundleId :: Types.String,
    -- | The name of the master database created when the Lightsail database resource is created.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 64 alphanumeric characters.
    --
    --
    --     * Cannot be a word reserved by the specified database engine
    masterDatabaseName :: Types.String,
    -- | The master user name for your new database.
    --
    -- Constraints:
    --
    --     * Master user name is required.
    --
    --
    --     * Must contain from 1 to 16 alphanumeric characters.
    --
    --
    --     * The first character must be a letter.
    --
    --
    --     * Cannot be a reserved word for the database engine you choose.
    -- For more information about reserved words in MySQL 5.6 or 5.7, see the Keywords and Reserved Words articles for <https://dev.mysql.com/doc/refman/5.6/en/keywords.html MySQL 5.6> or <https://dev.mysql.com/doc/refman/5.7/en/keywords.html MySQL 5.7> respectively.
    masterUsername :: Types.String,
    -- | The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format.
    --
    -- You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
    availabilityZone :: Core.Maybe Types.String,
    -- | The password for the master user of your new database. The password can include any printable ASCII character except "/", """, or "@".
    --
    -- Constraints: Must contain 8 to 41 characters.
    masterUserPassword :: Core.Maybe Types.MasterUserPassword,
    -- | The daily time range during which automated backups are created for your new database if automated backups are enabled.
    --
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. For more information about the preferred backup window time blocks for each region, see the <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Working With Backups> guide in the Amazon Relational Database Service (Amazon RDS) documentation.
    -- Constraints:
    --
    --     * Must be in the @hh24:mi-hh24:mi@ format.
    -- Example: @16:00-16:30@
    --
    --
    --     * Specified in Coordinated Universal Time (UTC).
    --
    --
    --     * Must not conflict with the preferred maintenance window.
    --
    --
    --     * Must be at least 30 minutes.
    preferredBackupWindow :: Core.Maybe Types.String,
    -- | The weekly time range during which system maintenance can occur on your new database.
    --
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.
    -- Constraints:
    --
    --     * Must be in the @ddd:hh24:mi-ddd:hh24:mi@ format.
    --
    --
    --     * Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    --
    --     * Must be at least 30 minutes.
    --
    --
    --     * Specified in Coordinated Universal Time (UTC).
    --
    --
    --     * Example: @Tue:17:00-Tue:17:30@
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRelationalDatabase' value with any optional fields omitted.
mkCreateRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Types.ResourceName ->
  -- | 'relationalDatabaseBlueprintId'
  Types.String ->
  -- | 'relationalDatabaseBundleId'
  Types.String ->
  -- | 'masterDatabaseName'
  Types.String ->
  -- | 'masterUsername'
  Types.String ->
  CreateRelationalDatabase
mkCreateRelationalDatabase
  relationalDatabaseName
  relationalDatabaseBlueprintId
  relationalDatabaseBundleId
  masterDatabaseName
  masterUsername =
    CreateRelationalDatabase'
      { relationalDatabaseName,
        relationalDatabaseBlueprintId,
        relationalDatabaseBundleId,
        masterDatabaseName,
        masterUsername,
        availabilityZone = Core.Nothing,
        masterUserPassword = Core.Nothing,
        preferredBackupWindow = Core.Nothing,
        preferredMaintenanceWindow = Core.Nothing,
        publiclyAccessible = Core.Nothing,
        tags = Core.Nothing
      }

-- | The name to use for your new database.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdRelationalDatabaseName :: Lens.Lens' CreateRelationalDatabase Types.ResourceName
crdRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# DEPRECATED crdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The blueprint ID for your new database. A blueprint describes the major engine version of a database.
--
-- You can get a list of database blueprints IDs by using the @get relational database blueprints@ operation.
--
-- /Note:/ Consider using 'relationalDatabaseBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdRelationalDatabaseBlueprintId :: Lens.Lens' CreateRelationalDatabase Types.String
crdRelationalDatabaseBlueprintId = Lens.field @"relationalDatabaseBlueprintId"
{-# DEPRECATED crdRelationalDatabaseBlueprintId "Use generic-lens or generic-optics with 'relationalDatabaseBlueprintId' instead." #-}

-- | The bundle ID for your new database. A bundle describes the performance specifications for your database.
--
-- You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
--
-- /Note:/ Consider using 'relationalDatabaseBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdRelationalDatabaseBundleId :: Lens.Lens' CreateRelationalDatabase Types.String
crdRelationalDatabaseBundleId = Lens.field @"relationalDatabaseBundleId"
{-# DEPRECATED crdRelationalDatabaseBundleId "Use generic-lens or generic-optics with 'relationalDatabaseBundleId' instead." #-}

-- | The name of the master database created when the Lightsail database resource is created.
--
-- Constraints:
--
--     * Must contain from 1 to 64 alphanumeric characters.
--
--
--     * Cannot be a word reserved by the specified database engine
--
--
--
-- /Note:/ Consider using 'masterDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdMasterDatabaseName :: Lens.Lens' CreateRelationalDatabase Types.String
crdMasterDatabaseName = Lens.field @"masterDatabaseName"
{-# DEPRECATED crdMasterDatabaseName "Use generic-lens or generic-optics with 'masterDatabaseName' instead." #-}

-- | The master user name for your new database.
--
-- Constraints:
--
--     * Master user name is required.
--
--
--     * Must contain from 1 to 16 alphanumeric characters.
--
--
--     * The first character must be a letter.
--
--
--     * Cannot be a reserved word for the database engine you choose.
-- For more information about reserved words in MySQL 5.6 or 5.7, see the Keywords and Reserved Words articles for <https://dev.mysql.com/doc/refman/5.6/en/keywords.html MySQL 5.6> or <https://dev.mysql.com/doc/refman/5.7/en/keywords.html MySQL 5.7> respectively.
--
--
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdMasterUsername :: Lens.Lens' CreateRelationalDatabase Types.String
crdMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED crdMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdAvailabilityZone :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Types.String)
crdAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED crdAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The password for the master user of your new database. The password can include any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdMasterUserPassword :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Types.MasterUserPassword)
crdMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED crdMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The daily time range during which automated backups are created for your new database if automated backups are enabled.
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. For more information about the preferred backup window time blocks for each region, see the <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Working With Backups> guide in the Amazon Relational Database Service (Amazon RDS) documentation.
-- Constraints:
--
--     * Must be in the @hh24:mi-hh24:mi@ format.
-- Example: @16:00-16:30@
--
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Must not conflict with the preferred maintenance window.
--
--
--     * Must be at least 30 minutes.
--
--
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdPreferredBackupWindow :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Types.String)
crdPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED crdPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The weekly time range during which system maintenance can occur on your new database.
--
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.
-- Constraints:
--
--     * Must be in the @ddd:hh24:mi-ddd:hh24:mi@ format.
--
--
--     * Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
--
--     * Must be at least 30 minutes.
--
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Example: @Tue:17:00-Tue:17:30@
--
--
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdPreferredMaintenanceWindow :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Types.String)
crdPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED crdPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdPubliclyAccessible :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Core.Bool)
crdPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED crdPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdTags :: Lens.Lens' CreateRelationalDatabase (Core.Maybe [Types.Tag])
crdTags = Lens.field @"tags"
{-# DEPRECATED crdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateRelationalDatabase where
  toJSON CreateRelationalDatabase {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("relationalDatabaseName" Core..= relationalDatabaseName),
            Core.Just
              ( "relationalDatabaseBlueprintId"
                  Core..= relationalDatabaseBlueprintId
              ),
            Core.Just
              ("relationalDatabaseBundleId" Core..= relationalDatabaseBundleId),
            Core.Just ("masterDatabaseName" Core..= masterDatabaseName),
            Core.Just ("masterUsername" Core..= masterUsername),
            ("availabilityZone" Core..=) Core.<$> availabilityZone,
            ("masterUserPassword" Core..=) Core.<$> masterUserPassword,
            ("preferredBackupWindow" Core..=) Core.<$> preferredBackupWindow,
            ("preferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow,
            ("publiclyAccessible" Core..=) Core.<$> publiclyAccessible,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateRelationalDatabase where
  type Rs CreateRelationalDatabase = CreateRelationalDatabaseResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.CreateRelationalDatabase")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRelationalDatabaseResponse' smart constructor.
data CreateRelationalDatabaseResponse = CreateRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateRelationalDatabaseResponse' value with any optional fields omitted.
mkCreateRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRelationalDatabaseResponse
mkCreateRelationalDatabaseResponse responseStatus =
  CreateRelationalDatabaseResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsOperations :: Lens.Lens' CreateRelationalDatabaseResponse (Core.Maybe [Types.Operation])
crdrrsOperations = Lens.field @"operations"
{-# DEPRECATED crdrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsResponseStatus :: Lens.Lens' CreateRelationalDatabaseResponse Core.Int
crdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
