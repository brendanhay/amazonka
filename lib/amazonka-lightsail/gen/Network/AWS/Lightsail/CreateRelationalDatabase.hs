{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    crdMasterUserPassword,
    crdPubliclyAccessible,
    crdPreferredMaintenanceWindow,
    crdPreferredBackupWindow,
    crdAvailabilityZone,
    crdTags,
    crdRelationalDatabaseName,
    crdRelationalDatabaseBlueprintId,
    crdRelationalDatabaseBundleId,
    crdMasterDatabaseName,
    crdMasterUsername,

    -- * Destructuring the response
    CreateRelationalDatabaseResponse (..),
    mkCreateRelationalDatabaseResponse,

    -- ** Response lenses
    crdrsOperations,
    crdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRelationalDatabase' smart constructor.
data CreateRelationalDatabase = CreateRelationalDatabase'
  { masterUserPassword ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    publiclyAccessible ::
      Lude.Maybe Lude.Bool,
    preferredMaintenanceWindow ::
      Lude.Maybe Lude.Text,
    preferredBackupWindow ::
      Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    relationalDatabaseName :: Lude.Text,
    relationalDatabaseBlueprintId ::
      Lude.Text,
    relationalDatabaseBundleId :: Lude.Text,
    masterDatabaseName :: Lude.Text,
    masterUsername :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRelationalDatabase' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
-- * 'masterDatabaseName' - The name of the master database created when the Lightsail database resource is created.
--
-- Constraints:
--
--     * Must contain from 1 to 64 alphanumeric characters.
--
--
--     * Cannot be a word reserved by the specified database engine
--
--
-- * 'masterUserPassword' - The password for the master user of your new database. The password can include any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain 8 to 41 characters.
-- * 'masterUsername' - The master user name for your new database.
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
-- * 'preferredBackupWindow' - The daily time range during which automated backups are created for your new database if automated backups are enabled.
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
-- * 'preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur on your new database.
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
-- * 'publiclyAccessible' - Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
-- * 'relationalDatabaseBlueprintId' - The blueprint ID for your new database. A blueprint describes the major engine version of a database.
--
-- You can get a list of database blueprints IDs by using the @get relational database blueprints@ operation.
-- * 'relationalDatabaseBundleId' - The bundle ID for your new database. A bundle describes the performance specifications for your database.
--
-- You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
-- * 'relationalDatabaseName' - The name to use for your new database.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  -- | 'relationalDatabaseBlueprintId'
  Lude.Text ->
  -- | 'relationalDatabaseBundleId'
  Lude.Text ->
  -- | 'masterDatabaseName'
  Lude.Text ->
  -- | 'masterUsername'
  Lude.Text ->
  CreateRelationalDatabase
mkCreateRelationalDatabase
  pRelationalDatabaseName_
  pRelationalDatabaseBlueprintId_
  pRelationalDatabaseBundleId_
  pMasterDatabaseName_
  pMasterUsername_ =
    CreateRelationalDatabase'
      { masterUserPassword = Lude.Nothing,
        publiclyAccessible = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        preferredBackupWindow = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        tags = Lude.Nothing,
        relationalDatabaseName = pRelationalDatabaseName_,
        relationalDatabaseBlueprintId = pRelationalDatabaseBlueprintId_,
        relationalDatabaseBundleId = pRelationalDatabaseBundleId_,
        masterDatabaseName = pMasterDatabaseName_,
        masterUsername = pMasterUsername_
      }

-- | The password for the master user of your new database. The password can include any printable ASCII character except "/", """, or "@".
--
-- Constraints: Must contain 8 to 41 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdMasterUserPassword :: Lens.Lens' CreateRelationalDatabase (Lude.Maybe (Lude.Sensitive Lude.Text))
crdMasterUserPassword = Lens.lens (masterUserPassword :: CreateRelationalDatabase -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {masterUserPassword = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdPubliclyAccessible :: Lens.Lens' CreateRelationalDatabase (Lude.Maybe Lude.Bool)
crdPubliclyAccessible = Lens.lens (publiclyAccessible :: CreateRelationalDatabase -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

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
crdPreferredMaintenanceWindow :: Lens.Lens' CreateRelationalDatabase (Lude.Maybe Lude.Text)
crdPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateRelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

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
crdPreferredBackupWindow :: Lens.Lens' CreateRelationalDatabase (Lude.Maybe Lude.Text)
crdPreferredBackupWindow = Lens.lens (preferredBackupWindow :: CreateRelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdAvailabilityZone :: Lens.Lens' CreateRelationalDatabase (Lude.Maybe Lude.Text)
crdAvailabilityZone = Lens.lens (availabilityZone :: CreateRelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdTags :: Lens.Lens' CreateRelationalDatabase (Lude.Maybe [Tag])
crdTags = Lens.lens (tags :: CreateRelationalDatabase -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
crdRelationalDatabaseName :: Lens.Lens' CreateRelationalDatabase Lude.Text
crdRelationalDatabaseName = Lens.lens (relationalDatabaseName :: CreateRelationalDatabase -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The blueprint ID for your new database. A blueprint describes the major engine version of a database.
--
-- You can get a list of database blueprints IDs by using the @get relational database blueprints@ operation.
--
-- /Note:/ Consider using 'relationalDatabaseBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdRelationalDatabaseBlueprintId :: Lens.Lens' CreateRelationalDatabase Lude.Text
crdRelationalDatabaseBlueprintId = Lens.lens (relationalDatabaseBlueprintId :: CreateRelationalDatabase -> Lude.Text) (\s a -> s {relationalDatabaseBlueprintId = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdRelationalDatabaseBlueprintId "Use generic-lens or generic-optics with 'relationalDatabaseBlueprintId' instead." #-}

-- | The bundle ID for your new database. A bundle describes the performance specifications for your database.
--
-- You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
--
-- /Note:/ Consider using 'relationalDatabaseBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdRelationalDatabaseBundleId :: Lens.Lens' CreateRelationalDatabase Lude.Text
crdRelationalDatabaseBundleId = Lens.lens (relationalDatabaseBundleId :: CreateRelationalDatabase -> Lude.Text) (\s a -> s {relationalDatabaseBundleId = a} :: CreateRelationalDatabase)
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
crdMasterDatabaseName :: Lens.Lens' CreateRelationalDatabase Lude.Text
crdMasterDatabaseName = Lens.lens (masterDatabaseName :: CreateRelationalDatabase -> Lude.Text) (\s a -> s {masterDatabaseName = a} :: CreateRelationalDatabase)
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
crdMasterUsername :: Lens.Lens' CreateRelationalDatabase Lude.Text
crdMasterUsername = Lens.lens (masterUsername :: CreateRelationalDatabase -> Lude.Text) (\s a -> s {masterUsername = a} :: CreateRelationalDatabase)
{-# DEPRECATED crdMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

instance Lude.AWSRequest CreateRelationalDatabase where
  type Rs CreateRelationalDatabase = CreateRelationalDatabaseResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRelationalDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateRelationalDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRelationalDatabase where
  toJSON CreateRelationalDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("masterUserPassword" Lude..=) Lude.<$> masterUserPassword,
            ("publiclyAccessible" Lude..=) Lude.<$> publiclyAccessible,
            ("preferredMaintenanceWindow" Lude..=)
              Lude.<$> preferredMaintenanceWindow,
            ("preferredBackupWindow" Lude..=) Lude.<$> preferredBackupWindow,
            ("availabilityZone" Lude..=) Lude.<$> availabilityZone,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName),
            Lude.Just
              ( "relationalDatabaseBlueprintId"
                  Lude..= relationalDatabaseBlueprintId
              ),
            Lude.Just
              ("relationalDatabaseBundleId" Lude..= relationalDatabaseBundleId),
            Lude.Just ("masterDatabaseName" Lude..= masterDatabaseName),
            Lude.Just ("masterUsername" Lude..= masterUsername)
          ]
      )

instance Lude.ToPath CreateRelationalDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRelationalDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRelationalDatabaseResponse' smart constructor.
data CreateRelationalDatabaseResponse = CreateRelationalDatabaseResponse'
  { operations ::
      Lude.Maybe [Operation],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRelationalDatabaseResponse
mkCreateRelationalDatabaseResponse pResponseStatus_ =
  CreateRelationalDatabaseResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsOperations :: Lens.Lens' CreateRelationalDatabaseResponse (Lude.Maybe [Operation])
crdrsOperations = Lens.lens (operations :: CreateRelationalDatabaseResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateRelationalDatabaseResponse)
{-# DEPRECATED crdrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsResponseStatus :: Lens.Lens' CreateRelationalDatabaseResponse Lude.Int
crdrsResponseStatus = Lens.lens (responseStatus :: CreateRelationalDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRelationalDatabaseResponse)
{-# DEPRECATED crdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
