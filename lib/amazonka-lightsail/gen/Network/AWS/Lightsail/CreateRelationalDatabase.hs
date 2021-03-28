{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateRelationalDatabase (..)
    , mkCreateRelationalDatabase
    -- ** Request lenses
    , crdRelationalDatabaseName
    , crdRelationalDatabaseBlueprintId
    , crdRelationalDatabaseBundleId
    , crdMasterDatabaseName
    , crdMasterUsername
    , crdAvailabilityZone
    , crdMasterUserPassword
    , crdPreferredBackupWindow
    , crdPreferredMaintenanceWindow
    , crdPubliclyAccessible
    , crdTags

    -- * Destructuring the response
    , CreateRelationalDatabaseResponse (..)
    , mkCreateRelationalDatabaseResponse
    -- ** Response lenses
    , crdrrsOperations
    , crdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRelationalDatabase' smart constructor.
data CreateRelationalDatabase = CreateRelationalDatabase'
  { relationalDatabaseName :: Types.ResourceName
    -- ^ The name to use for your new database.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
  , relationalDatabaseBlueprintId :: Core.Text
    -- ^ The blueprint ID for your new database. A blueprint describes the major engine version of a database.
--
-- You can get a list of database blueprints IDs by using the @get relational database blueprints@ operation.
  , relationalDatabaseBundleId :: Core.Text
    -- ^ The bundle ID for your new database. A bundle describes the performance specifications for your database.
--
-- You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
  , masterDatabaseName :: Core.Text
    -- ^ The name of the master database created when the Lightsail database resource is created.
--
-- Constraints:
--
--     * Must contain from 1 to 64 alphanumeric characters.
--
--
--     * Cannot be a word reserved by the specified database engine
--
--
  , masterUsername :: Core.Text
    -- ^ The master user name for your new database.
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
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
  , masterUserPassword :: Core.Maybe Types.MasterUserPassword
    -- ^ The password for the master user of your new database. The password can include any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain 8 to 41 characters.
  , preferredBackupWindow :: Core.Maybe Core.Text
    -- ^ The daily time range during which automated backups are created for your new database if automated backups are enabled.
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
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ The weekly time range during which system maintenance can occur on your new database.
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
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRelationalDatabase' value with any optional fields omitted.
mkCreateRelationalDatabase
    :: Types.ResourceName -- ^ 'relationalDatabaseName'
    -> Core.Text -- ^ 'relationalDatabaseBlueprintId'
    -> Core.Text -- ^ 'relationalDatabaseBundleId'
    -> Core.Text -- ^ 'masterDatabaseName'
    -> Core.Text -- ^ 'masterUsername'
    -> CreateRelationalDatabase
mkCreateRelationalDatabase relationalDatabaseName
  relationalDatabaseBlueprintId relationalDatabaseBundleId
  masterDatabaseName masterUsername
  = CreateRelationalDatabase'{relationalDatabaseName,
                              relationalDatabaseBlueprintId, relationalDatabaseBundleId,
                              masterDatabaseName, masterUsername,
                              availabilityZone = Core.Nothing, masterUserPassword = Core.Nothing,
                              preferredBackupWindow = Core.Nothing,
                              preferredMaintenanceWindow = Core.Nothing,
                              publiclyAccessible = Core.Nothing, tags = Core.Nothing}

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
{-# INLINEABLE crdRelationalDatabaseName #-}
{-# DEPRECATED relationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead"  #-}

-- | The blueprint ID for your new database. A blueprint describes the major engine version of a database.
--
-- You can get a list of database blueprints IDs by using the @get relational database blueprints@ operation.
--
-- /Note:/ Consider using 'relationalDatabaseBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdRelationalDatabaseBlueprintId :: Lens.Lens' CreateRelationalDatabase Core.Text
crdRelationalDatabaseBlueprintId = Lens.field @"relationalDatabaseBlueprintId"
{-# INLINEABLE crdRelationalDatabaseBlueprintId #-}
{-# DEPRECATED relationalDatabaseBlueprintId "Use generic-lens or generic-optics with 'relationalDatabaseBlueprintId' instead"  #-}

-- | The bundle ID for your new database. A bundle describes the performance specifications for your database.
--
-- You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
--
-- /Note:/ Consider using 'relationalDatabaseBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdRelationalDatabaseBundleId :: Lens.Lens' CreateRelationalDatabase Core.Text
crdRelationalDatabaseBundleId = Lens.field @"relationalDatabaseBundleId"
{-# INLINEABLE crdRelationalDatabaseBundleId #-}
{-# DEPRECATED relationalDatabaseBundleId "Use generic-lens or generic-optics with 'relationalDatabaseBundleId' instead"  #-}

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
crdMasterDatabaseName :: Lens.Lens' CreateRelationalDatabase Core.Text
crdMasterDatabaseName = Lens.field @"masterDatabaseName"
{-# INLINEABLE crdMasterDatabaseName #-}
{-# DEPRECATED masterDatabaseName "Use generic-lens or generic-optics with 'masterDatabaseName' instead"  #-}

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
crdMasterUsername :: Lens.Lens' CreateRelationalDatabase Core.Text
crdMasterUsername = Lens.field @"masterUsername"
{-# INLINEABLE crdMasterUsername #-}
{-# DEPRECATED masterUsername "Use generic-lens or generic-optics with 'masterUsername' instead"  #-}

-- | The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdAvailabilityZone :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Core.Text)
crdAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE crdAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The password for the master user of your new database. The password can include any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdMasterUserPassword :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Types.MasterUserPassword)
crdMasterUserPassword = Lens.field @"masterUserPassword"
{-# INLINEABLE crdMasterUserPassword #-}
{-# DEPRECATED masterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead"  #-}

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
crdPreferredBackupWindow :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Core.Text)
crdPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# INLINEABLE crdPreferredBackupWindow #-}
{-# DEPRECATED preferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead"  #-}

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
crdPreferredMaintenanceWindow :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Core.Text)
crdPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE crdPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdPubliclyAccessible :: Lens.Lens' CreateRelationalDatabase (Core.Maybe Core.Bool)
crdPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE crdPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdTags :: Lens.Lens' CreateRelationalDatabase (Core.Maybe [Types.Tag])
crdTags = Lens.field @"tags"
{-# INLINEABLE crdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRelationalDatabase where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRelationalDatabase where
        toHeaders CreateRelationalDatabase{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateRelationalDatabase")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRelationalDatabase where
        toJSON CreateRelationalDatabase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("relationalDatabaseName" Core..= relationalDatabaseName),
                  Core.Just
                    ("relationalDatabaseBlueprintId" Core..=
                       relationalDatabaseBlueprintId),
                  Core.Just
                    ("relationalDatabaseBundleId" Core..= relationalDatabaseBundleId),
                  Core.Just ("masterDatabaseName" Core..= masterDatabaseName),
                  Core.Just ("masterUsername" Core..= masterUsername),
                  ("availabilityZone" Core..=) Core.<$> availabilityZone,
                  ("masterUserPassword" Core..=) Core.<$> masterUserPassword,
                  ("preferredBackupWindow" Core..=) Core.<$> preferredBackupWindow,
                  ("preferredMaintenanceWindow" Core..=) Core.<$>
                    preferredMaintenanceWindow,
                  ("publiclyAccessible" Core..=) Core.<$> publiclyAccessible,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateRelationalDatabase where
        type Rs CreateRelationalDatabase = CreateRelationalDatabaseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRelationalDatabaseResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRelationalDatabaseResponse' smart constructor.
data CreateRelationalDatabaseResponse = CreateRelationalDatabaseResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateRelationalDatabaseResponse' value with any optional fields omitted.
mkCreateRelationalDatabaseResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRelationalDatabaseResponse
mkCreateRelationalDatabaseResponse responseStatus
  = CreateRelationalDatabaseResponse'{operations = Core.Nothing,
                                      responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsOperations :: Lens.Lens' CreateRelationalDatabaseResponse (Core.Maybe [Types.Operation])
crdrrsOperations = Lens.field @"operations"
{-# INLINEABLE crdrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsResponseStatus :: Lens.Lens' CreateRelationalDatabaseResponse Core.Int
crdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
