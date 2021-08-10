{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database in Amazon Lightsail.
--
-- The @create relational database@ operation supports tag-based access
-- control via request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateRelationalDatabase
  ( -- * Creating a Request
    CreateRelationalDatabase (..),
    newCreateRelationalDatabase,

    -- * Request Lenses
    createRelationalDatabase_preferredBackupWindow,
    createRelationalDatabase_masterUserPassword,
    createRelationalDatabase_publiclyAccessible,
    createRelationalDatabase_availabilityZone,
    createRelationalDatabase_preferredMaintenanceWindow,
    createRelationalDatabase_tags,
    createRelationalDatabase_relationalDatabaseName,
    createRelationalDatabase_relationalDatabaseBlueprintId,
    createRelationalDatabase_relationalDatabaseBundleId,
    createRelationalDatabase_masterDatabaseName,
    createRelationalDatabase_masterUsername,

    -- * Destructuring the Response
    CreateRelationalDatabaseResponse (..),
    newCreateRelationalDatabaseResponse,

    -- * Response Lenses
    createRelationalDatabaseResponse_operations,
    createRelationalDatabaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRelationalDatabase' smart constructor.
data CreateRelationalDatabase = CreateRelationalDatabase'
  { -- | The daily time range during which automated backups are created for your
    -- new database if automated backups are enabled.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each AWS Region. For more information about the
    -- preferred backup window time blocks for each region, see the
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Working With Backups>
    -- guide in the Amazon Relational Database Service (Amazon RDS)
    -- documentation.
    --
    -- Constraints:
    --
    -- -   Must be in the @hh24:mi-hh24:mi@ format.
    --
    --     Example: @16:00-16:30@
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Must not conflict with the preferred maintenance window.
    --
    -- -   Must be at least 30 minutes.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The password for the master user of your new database. The password can
    -- include any printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain 8 to 41 characters.
    masterUserPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Specifies the accessibility options for your new database. A value of
    -- @true@ specifies a database that is available to resources outside of
    -- your Lightsail account. A value of @false@ specifies a database that is
    -- available only to your Lightsail resources in the same region as your
    -- database.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone in which to create your new database. Use the
    -- @us-east-2a@ case-sensitive format.
    --
    -- You can get a list of Availability Zones by using the @get regions@
    -- operation. Be sure to add the
    -- @include relational database Availability Zones@ parameter to your
    -- request.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur on your
    -- new database.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each AWS Region, occurring on a random day of the
    -- week.
    --
    -- Constraints:
    --
    -- -   Must be in the @ddd:hh24:mi-ddd:hh24:mi@ format.
    --
    -- -   Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- -   Must be at least 30 minutes.
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Example: @Tue:17:00-Tue:17:30@
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The name to use for your new Lightsail database resource.
    --
    -- Constraints:
    --
    -- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    -- -   The first and last character must be a letter or number.
    relationalDatabaseName :: Prelude.Text,
    -- | The blueprint ID for your new database. A blueprint describes the major
    -- engine version of a database.
    --
    -- You can get a list of database blueprints IDs by using the
    -- @get relational database blueprints@ operation.
    relationalDatabaseBlueprintId :: Prelude.Text,
    -- | The bundle ID for your new database. A bundle describes the performance
    -- specifications for your database.
    --
    -- You can get a list of database bundle IDs by using the
    -- @get relational database bundles@ operation.
    relationalDatabaseBundleId :: Prelude.Text,
    -- | The name of the master database created when the Lightsail database
    -- resource is created.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 64 alphanumeric characters.
    --
    -- -   Cannot be a word reserved by the specified database engine
    masterDatabaseName :: Prelude.Text,
    -- | The master user name for your new database.
    --
    -- Constraints:
    --
    -- -   Master user name is required.
    --
    -- -   Must contain from 1 to 16 alphanumeric characters.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot be a reserved word for the database engine you choose.
    --
    --     For more information about reserved words in MySQL 5.6 or 5.7, see
    --     the Keywords and Reserved Words articles for
    --     <https://dev.mysql.com/doc/refman/5.6/en/keywords.html MySQL 5.6> or
    --     <https://dev.mysql.com/doc/refman/5.7/en/keywords.html MySQL 5.7>
    --     respectively.
    masterUsername :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredBackupWindow', 'createRelationalDatabase_preferredBackupWindow' - The daily time range during which automated backups are created for your
-- new database if automated backups are enabled.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region. For more information about the
-- preferred backup window time blocks for each region, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Working With Backups>
-- guide in the Amazon Relational Database Service (Amazon RDS)
-- documentation.
--
-- Constraints:
--
-- -   Must be in the @hh24:mi-hh24:mi@ format.
--
--     Example: @16:00-16:30@
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- 'masterUserPassword', 'createRelationalDatabase_masterUserPassword' - The password for the master user of your new database. The password can
-- include any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain 8 to 41 characters.
--
-- 'publiclyAccessible', 'createRelationalDatabase_publiclyAccessible' - Specifies the accessibility options for your new database. A value of
-- @true@ specifies a database that is available to resources outside of
-- your Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
--
-- 'availabilityZone', 'createRelationalDatabase_availabilityZone' - The Availability Zone in which to create your new database. Use the
-- @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@
-- operation. Be sure to add the
-- @include relational database Availability Zones@ parameter to your
-- request.
--
-- 'preferredMaintenanceWindow', 'createRelationalDatabase_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur on your
-- new database.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region, occurring on a random day of the
-- week.
--
-- Constraints:
--
-- -   Must be in the @ddd:hh24:mi-ddd:hh24:mi@ format.
--
-- -   Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- -   Must be at least 30 minutes.
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Example: @Tue:17:00-Tue:17:30@
--
-- 'tags', 'createRelationalDatabase_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'relationalDatabaseName', 'createRelationalDatabase_relationalDatabaseName' - The name to use for your new Lightsail database resource.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
--
-- 'relationalDatabaseBlueprintId', 'createRelationalDatabase_relationalDatabaseBlueprintId' - The blueprint ID for your new database. A blueprint describes the major
-- engine version of a database.
--
-- You can get a list of database blueprints IDs by using the
-- @get relational database blueprints@ operation.
--
-- 'relationalDatabaseBundleId', 'createRelationalDatabase_relationalDatabaseBundleId' - The bundle ID for your new database. A bundle describes the performance
-- specifications for your database.
--
-- You can get a list of database bundle IDs by using the
-- @get relational database bundles@ operation.
--
-- 'masterDatabaseName', 'createRelationalDatabase_masterDatabaseName' - The name of the master database created when the Lightsail database
-- resource is created.
--
-- Constraints:
--
-- -   Must contain from 1 to 64 alphanumeric characters.
--
-- -   Cannot be a word reserved by the specified database engine
--
-- 'masterUsername', 'createRelationalDatabase_masterUsername' - The master user name for your new database.
--
-- Constraints:
--
-- -   Master user name is required.
--
-- -   Must contain from 1 to 16 alphanumeric characters.
--
-- -   The first character must be a letter.
--
-- -   Cannot be a reserved word for the database engine you choose.
--
--     For more information about reserved words in MySQL 5.6 or 5.7, see
--     the Keywords and Reserved Words articles for
--     <https://dev.mysql.com/doc/refman/5.6/en/keywords.html MySQL 5.6> or
--     <https://dev.mysql.com/doc/refman/5.7/en/keywords.html MySQL 5.7>
--     respectively.
newCreateRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  -- | 'relationalDatabaseBlueprintId'
  Prelude.Text ->
  -- | 'relationalDatabaseBundleId'
  Prelude.Text ->
  -- | 'masterDatabaseName'
  Prelude.Text ->
  -- | 'masterUsername'
  Prelude.Text ->
  CreateRelationalDatabase
newCreateRelationalDatabase
  pRelationalDatabaseName_
  pRelationalDatabaseBlueprintId_
  pRelationalDatabaseBundleId_
  pMasterDatabaseName_
  pMasterUsername_ =
    CreateRelationalDatabase'
      { preferredBackupWindow =
          Prelude.Nothing,
        masterUserPassword = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        tags = Prelude.Nothing,
        relationalDatabaseName = pRelationalDatabaseName_,
        relationalDatabaseBlueprintId =
          pRelationalDatabaseBlueprintId_,
        relationalDatabaseBundleId =
          pRelationalDatabaseBundleId_,
        masterDatabaseName = pMasterDatabaseName_,
        masterUsername = pMasterUsername_
      }

-- | The daily time range during which automated backups are created for your
-- new database if automated backups are enabled.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region. For more information about the
-- preferred backup window time blocks for each region, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Working With Backups>
-- guide in the Amazon Relational Database Service (Amazon RDS)
-- documentation.
--
-- Constraints:
--
-- -   Must be in the @hh24:mi-hh24:mi@ format.
--
--     Example: @16:00-16:30@
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
createRelationalDatabase_preferredBackupWindow :: Lens.Lens' CreateRelationalDatabase (Prelude.Maybe Prelude.Text)
createRelationalDatabase_preferredBackupWindow = Lens.lens (\CreateRelationalDatabase' {preferredBackupWindow} -> preferredBackupWindow) (\s@CreateRelationalDatabase' {} a -> s {preferredBackupWindow = a} :: CreateRelationalDatabase)

-- | The password for the master user of your new database. The password can
-- include any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain 8 to 41 characters.
createRelationalDatabase_masterUserPassword :: Lens.Lens' CreateRelationalDatabase (Prelude.Maybe Prelude.Text)
createRelationalDatabase_masterUserPassword = Lens.lens (\CreateRelationalDatabase' {masterUserPassword} -> masterUserPassword) (\s@CreateRelationalDatabase' {} a -> s {masterUserPassword = a} :: CreateRelationalDatabase) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies the accessibility options for your new database. A value of
-- @true@ specifies a database that is available to resources outside of
-- your Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
createRelationalDatabase_publiclyAccessible :: Lens.Lens' CreateRelationalDatabase (Prelude.Maybe Prelude.Bool)
createRelationalDatabase_publiclyAccessible = Lens.lens (\CreateRelationalDatabase' {publiclyAccessible} -> publiclyAccessible) (\s@CreateRelationalDatabase' {} a -> s {publiclyAccessible = a} :: CreateRelationalDatabase)

-- | The Availability Zone in which to create your new database. Use the
-- @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@
-- operation. Be sure to add the
-- @include relational database Availability Zones@ parameter to your
-- request.
createRelationalDatabase_availabilityZone :: Lens.Lens' CreateRelationalDatabase (Prelude.Maybe Prelude.Text)
createRelationalDatabase_availabilityZone = Lens.lens (\CreateRelationalDatabase' {availabilityZone} -> availabilityZone) (\s@CreateRelationalDatabase' {} a -> s {availabilityZone = a} :: CreateRelationalDatabase)

-- | The weekly time range during which system maintenance can occur on your
-- new database.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region, occurring on a random day of the
-- week.
--
-- Constraints:
--
-- -   Must be in the @ddd:hh24:mi-ddd:hh24:mi@ format.
--
-- -   Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- -   Must be at least 30 minutes.
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Example: @Tue:17:00-Tue:17:30@
createRelationalDatabase_preferredMaintenanceWindow :: Lens.Lens' CreateRelationalDatabase (Prelude.Maybe Prelude.Text)
createRelationalDatabase_preferredMaintenanceWindow = Lens.lens (\CreateRelationalDatabase' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateRelationalDatabase' {} a -> s {preferredMaintenanceWindow = a} :: CreateRelationalDatabase)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createRelationalDatabase_tags :: Lens.Lens' CreateRelationalDatabase (Prelude.Maybe [Tag])
createRelationalDatabase_tags = Lens.lens (\CreateRelationalDatabase' {tags} -> tags) (\s@CreateRelationalDatabase' {} a -> s {tags = a} :: CreateRelationalDatabase) Prelude.. Lens.mapping Lens._Coerce

-- | The name to use for your new Lightsail database resource.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
createRelationalDatabase_relationalDatabaseName :: Lens.Lens' CreateRelationalDatabase Prelude.Text
createRelationalDatabase_relationalDatabaseName = Lens.lens (\CreateRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@CreateRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: CreateRelationalDatabase)

-- | The blueprint ID for your new database. A blueprint describes the major
-- engine version of a database.
--
-- You can get a list of database blueprints IDs by using the
-- @get relational database blueprints@ operation.
createRelationalDatabase_relationalDatabaseBlueprintId :: Lens.Lens' CreateRelationalDatabase Prelude.Text
createRelationalDatabase_relationalDatabaseBlueprintId = Lens.lens (\CreateRelationalDatabase' {relationalDatabaseBlueprintId} -> relationalDatabaseBlueprintId) (\s@CreateRelationalDatabase' {} a -> s {relationalDatabaseBlueprintId = a} :: CreateRelationalDatabase)

-- | The bundle ID for your new database. A bundle describes the performance
-- specifications for your database.
--
-- You can get a list of database bundle IDs by using the
-- @get relational database bundles@ operation.
createRelationalDatabase_relationalDatabaseBundleId :: Lens.Lens' CreateRelationalDatabase Prelude.Text
createRelationalDatabase_relationalDatabaseBundleId = Lens.lens (\CreateRelationalDatabase' {relationalDatabaseBundleId} -> relationalDatabaseBundleId) (\s@CreateRelationalDatabase' {} a -> s {relationalDatabaseBundleId = a} :: CreateRelationalDatabase)

-- | The name of the master database created when the Lightsail database
-- resource is created.
--
-- Constraints:
--
-- -   Must contain from 1 to 64 alphanumeric characters.
--
-- -   Cannot be a word reserved by the specified database engine
createRelationalDatabase_masterDatabaseName :: Lens.Lens' CreateRelationalDatabase Prelude.Text
createRelationalDatabase_masterDatabaseName = Lens.lens (\CreateRelationalDatabase' {masterDatabaseName} -> masterDatabaseName) (\s@CreateRelationalDatabase' {} a -> s {masterDatabaseName = a} :: CreateRelationalDatabase)

-- | The master user name for your new database.
--
-- Constraints:
--
-- -   Master user name is required.
--
-- -   Must contain from 1 to 16 alphanumeric characters.
--
-- -   The first character must be a letter.
--
-- -   Cannot be a reserved word for the database engine you choose.
--
--     For more information about reserved words in MySQL 5.6 or 5.7, see
--     the Keywords and Reserved Words articles for
--     <https://dev.mysql.com/doc/refman/5.6/en/keywords.html MySQL 5.6> or
--     <https://dev.mysql.com/doc/refman/5.7/en/keywords.html MySQL 5.7>
--     respectively.
createRelationalDatabase_masterUsername :: Lens.Lens' CreateRelationalDatabase Prelude.Text
createRelationalDatabase_masterUsername = Lens.lens (\CreateRelationalDatabase' {masterUsername} -> masterUsername) (\s@CreateRelationalDatabase' {} a -> s {masterUsername = a} :: CreateRelationalDatabase)

instance Core.AWSRequest CreateRelationalDatabase where
  type
    AWSResponse CreateRelationalDatabase =
      CreateRelationalDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRelationalDatabase

instance Prelude.NFData CreateRelationalDatabase

instance Core.ToHeaders CreateRelationalDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateRelationalDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRelationalDatabase where
  toJSON CreateRelationalDatabase' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("preferredBackupWindow" Core..=)
              Prelude.<$> preferredBackupWindow,
            ("masterUserPassword" Core..=)
              Prelude.<$> masterUserPassword,
            ("publiclyAccessible" Core..=)
              Prelude.<$> publiclyAccessible,
            ("availabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            ("preferredMaintenanceWindow" Core..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              ),
            Prelude.Just
              ( "relationalDatabaseBlueprintId"
                  Core..= relationalDatabaseBlueprintId
              ),
            Prelude.Just
              ( "relationalDatabaseBundleId"
                  Core..= relationalDatabaseBundleId
              ),
            Prelude.Just
              ("masterDatabaseName" Core..= masterDatabaseName),
            Prelude.Just
              ("masterUsername" Core..= masterUsername)
          ]
      )

instance Core.ToPath CreateRelationalDatabase where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateRelationalDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRelationalDatabaseResponse' smart constructor.
data CreateRelationalDatabaseResponse = CreateRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRelationalDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createRelationalDatabaseResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createRelationalDatabaseResponse_httpStatus' - The response's http status code.
newCreateRelationalDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRelationalDatabaseResponse
newCreateRelationalDatabaseResponse pHttpStatus_ =
  CreateRelationalDatabaseResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createRelationalDatabaseResponse_operations :: Lens.Lens' CreateRelationalDatabaseResponse (Prelude.Maybe [Operation])
createRelationalDatabaseResponse_operations = Lens.lens (\CreateRelationalDatabaseResponse' {operations} -> operations) (\s@CreateRelationalDatabaseResponse' {} a -> s {operations = a} :: CreateRelationalDatabaseResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createRelationalDatabaseResponse_httpStatus :: Lens.Lens' CreateRelationalDatabaseResponse Prelude.Int
createRelationalDatabaseResponse_httpStatus = Lens.lens (\CreateRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@CreateRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: CreateRelationalDatabaseResponse)

instance
  Prelude.NFData
    CreateRelationalDatabaseResponse
