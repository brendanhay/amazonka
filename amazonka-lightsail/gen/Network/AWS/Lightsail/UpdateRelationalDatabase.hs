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
-- Module      : Network.AWS.Lightsail.UpdateRelationalDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the update of one or more attributes of a database in Amazon
-- Lightsail.
--
-- Updates are applied immediately, or in cases where the updates could
-- result in an outage, are applied during the database\'s predefined
-- maintenance window.
--
-- The @update relational database@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.UpdateRelationalDatabase
  ( -- * Creating a Request
    UpdateRelationalDatabase (..),
    newUpdateRelationalDatabase,

    -- * Request Lenses
    updateRelationalDatabase_preferredBackupWindow,
    updateRelationalDatabase_caCertificateIdentifier,
    updateRelationalDatabase_disableBackupRetention,
    updateRelationalDatabase_masterUserPassword,
    updateRelationalDatabase_publiclyAccessible,
    updateRelationalDatabase_preferredMaintenanceWindow,
    updateRelationalDatabase_enableBackupRetention,
    updateRelationalDatabase_rotateMasterUserPassword,
    updateRelationalDatabase_applyImmediately,
    updateRelationalDatabase_relationalDatabaseName,

    -- * Destructuring the Response
    UpdateRelationalDatabaseResponse (..),
    newUpdateRelationalDatabaseResponse,

    -- * Response Lenses
    updateRelationalDatabaseResponse_operations,
    updateRelationalDatabaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRelationalDatabase' smart constructor.
data UpdateRelationalDatabase = UpdateRelationalDatabase'
  { -- | The daily time range during which automated backups are created for your
    -- database if automated backups are enabled.
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
    preferredBackupWindow :: Core.Maybe Core.Text,
    -- | Indicates the certificate that needs to be associated with the database.
    caCertificateIdentifier :: Core.Maybe Core.Text,
    -- | When @true@, disables automated backup retention for your database.
    --
    -- Disabling backup retention deletes all automated database backups.
    -- Before disabling this, you may want to create a snapshot of your
    -- database using the @create relational database snapshot@ operation.
    --
    -- Updates are applied during the next maintenance window because this can
    -- result in an outage.
    disableBackupRetention :: Core.Maybe Core.Bool,
    -- | The password for the master user of your database. The password can
    -- include any printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain 8 to 41 characters.
    masterUserPassword :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Specifies the accessibility options for your database. A value of @true@
    -- specifies a database that is available to resources outside of your
    -- Lightsail account. A value of @false@ specifies a database that is
    -- available only to your Lightsail resources in the same region as your
    -- database.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The weekly time range during which system maintenance can occur on your
    -- database.
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
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | When @true@, enables automated backup retention for your database.
    --
    -- Updates are applied during the next maintenance window because this can
    -- result in an outage.
    enableBackupRetention :: Core.Maybe Core.Bool,
    -- | When @true@, the master user password is changed to a new strong
    -- password generated by Lightsail.
    --
    -- Use the @get relational database master user password@ operation to get
    -- the new password.
    rotateMasterUserPassword :: Core.Maybe Core.Bool,
    -- | When @true@, applies changes immediately. When @false@, applies changes
    -- during the preferred maintenance window. Some changes may cause an
    -- outage.
    --
    -- Default: @false@
    applyImmediately :: Core.Maybe Core.Bool,
    -- | The name of your database to update.
    relationalDatabaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredBackupWindow', 'updateRelationalDatabase_preferredBackupWindow' - The daily time range during which automated backups are created for your
-- database if automated backups are enabled.
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
-- 'caCertificateIdentifier', 'updateRelationalDatabase_caCertificateIdentifier' - Indicates the certificate that needs to be associated with the database.
--
-- 'disableBackupRetention', 'updateRelationalDatabase_disableBackupRetention' - When @true@, disables automated backup retention for your database.
--
-- Disabling backup retention deletes all automated database backups.
-- Before disabling this, you may want to create a snapshot of your
-- database using the @create relational database snapshot@ operation.
--
-- Updates are applied during the next maintenance window because this can
-- result in an outage.
--
-- 'masterUserPassword', 'updateRelationalDatabase_masterUserPassword' - The password for the master user of your database. The password can
-- include any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain 8 to 41 characters.
--
-- 'publiclyAccessible', 'updateRelationalDatabase_publiclyAccessible' - Specifies the accessibility options for your database. A value of @true@
-- specifies a database that is available to resources outside of your
-- Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
--
-- 'preferredMaintenanceWindow', 'updateRelationalDatabase_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur on your
-- database.
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
-- 'enableBackupRetention', 'updateRelationalDatabase_enableBackupRetention' - When @true@, enables automated backup retention for your database.
--
-- Updates are applied during the next maintenance window because this can
-- result in an outage.
--
-- 'rotateMasterUserPassword', 'updateRelationalDatabase_rotateMasterUserPassword' - When @true@, the master user password is changed to a new strong
-- password generated by Lightsail.
--
-- Use the @get relational database master user password@ operation to get
-- the new password.
--
-- 'applyImmediately', 'updateRelationalDatabase_applyImmediately' - When @true@, applies changes immediately. When @false@, applies changes
-- during the preferred maintenance window. Some changes may cause an
-- outage.
--
-- Default: @false@
--
-- 'relationalDatabaseName', 'updateRelationalDatabase_relationalDatabaseName' - The name of your database to update.
newUpdateRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Core.Text ->
  UpdateRelationalDatabase
newUpdateRelationalDatabase pRelationalDatabaseName_ =
  UpdateRelationalDatabase'
    { preferredBackupWindow =
        Core.Nothing,
      caCertificateIdentifier = Core.Nothing,
      disableBackupRetention = Core.Nothing,
      masterUserPassword = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      enableBackupRetention = Core.Nothing,
      rotateMasterUserPassword = Core.Nothing,
      applyImmediately = Core.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The daily time range during which automated backups are created for your
-- database if automated backups are enabled.
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
updateRelationalDatabase_preferredBackupWindow :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Text)
updateRelationalDatabase_preferredBackupWindow = Lens.lens (\UpdateRelationalDatabase' {preferredBackupWindow} -> preferredBackupWindow) (\s@UpdateRelationalDatabase' {} a -> s {preferredBackupWindow = a} :: UpdateRelationalDatabase)

-- | Indicates the certificate that needs to be associated with the database.
updateRelationalDatabase_caCertificateIdentifier :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Text)
updateRelationalDatabase_caCertificateIdentifier = Lens.lens (\UpdateRelationalDatabase' {caCertificateIdentifier} -> caCertificateIdentifier) (\s@UpdateRelationalDatabase' {} a -> s {caCertificateIdentifier = a} :: UpdateRelationalDatabase)

-- | When @true@, disables automated backup retention for your database.
--
-- Disabling backup retention deletes all automated database backups.
-- Before disabling this, you may want to create a snapshot of your
-- database using the @create relational database snapshot@ operation.
--
-- Updates are applied during the next maintenance window because this can
-- result in an outage.
updateRelationalDatabase_disableBackupRetention :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Bool)
updateRelationalDatabase_disableBackupRetention = Lens.lens (\UpdateRelationalDatabase' {disableBackupRetention} -> disableBackupRetention) (\s@UpdateRelationalDatabase' {} a -> s {disableBackupRetention = a} :: UpdateRelationalDatabase)

-- | The password for the master user of your database. The password can
-- include any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain 8 to 41 characters.
updateRelationalDatabase_masterUserPassword :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Text)
updateRelationalDatabase_masterUserPassword = Lens.lens (\UpdateRelationalDatabase' {masterUserPassword} -> masterUserPassword) (\s@UpdateRelationalDatabase' {} a -> s {masterUserPassword = a} :: UpdateRelationalDatabase) Core.. Lens.mapping Core._Sensitive

-- | Specifies the accessibility options for your database. A value of @true@
-- specifies a database that is available to resources outside of your
-- Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
updateRelationalDatabase_publiclyAccessible :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Bool)
updateRelationalDatabase_publiclyAccessible = Lens.lens (\UpdateRelationalDatabase' {publiclyAccessible} -> publiclyAccessible) (\s@UpdateRelationalDatabase' {} a -> s {publiclyAccessible = a} :: UpdateRelationalDatabase)

-- | The weekly time range during which system maintenance can occur on your
-- database.
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
updateRelationalDatabase_preferredMaintenanceWindow :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Text)
updateRelationalDatabase_preferredMaintenanceWindow = Lens.lens (\UpdateRelationalDatabase' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@UpdateRelationalDatabase' {} a -> s {preferredMaintenanceWindow = a} :: UpdateRelationalDatabase)

-- | When @true@, enables automated backup retention for your database.
--
-- Updates are applied during the next maintenance window because this can
-- result in an outage.
updateRelationalDatabase_enableBackupRetention :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Bool)
updateRelationalDatabase_enableBackupRetention = Lens.lens (\UpdateRelationalDatabase' {enableBackupRetention} -> enableBackupRetention) (\s@UpdateRelationalDatabase' {} a -> s {enableBackupRetention = a} :: UpdateRelationalDatabase)

-- | When @true@, the master user password is changed to a new strong
-- password generated by Lightsail.
--
-- Use the @get relational database master user password@ operation to get
-- the new password.
updateRelationalDatabase_rotateMasterUserPassword :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Bool)
updateRelationalDatabase_rotateMasterUserPassword = Lens.lens (\UpdateRelationalDatabase' {rotateMasterUserPassword} -> rotateMasterUserPassword) (\s@UpdateRelationalDatabase' {} a -> s {rotateMasterUserPassword = a} :: UpdateRelationalDatabase)

-- | When @true@, applies changes immediately. When @false@, applies changes
-- during the preferred maintenance window. Some changes may cause an
-- outage.
--
-- Default: @false@
updateRelationalDatabase_applyImmediately :: Lens.Lens' UpdateRelationalDatabase (Core.Maybe Core.Bool)
updateRelationalDatabase_applyImmediately = Lens.lens (\UpdateRelationalDatabase' {applyImmediately} -> applyImmediately) (\s@UpdateRelationalDatabase' {} a -> s {applyImmediately = a} :: UpdateRelationalDatabase)

-- | The name of your database to update.
updateRelationalDatabase_relationalDatabaseName :: Lens.Lens' UpdateRelationalDatabase Core.Text
updateRelationalDatabase_relationalDatabaseName = Lens.lens (\UpdateRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@UpdateRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: UpdateRelationalDatabase)

instance Core.AWSRequest UpdateRelationalDatabase where
  type
    AWSResponse UpdateRelationalDatabase =
      UpdateRelationalDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRelationalDatabaseResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateRelationalDatabase

instance Core.NFData UpdateRelationalDatabase

instance Core.ToHeaders UpdateRelationalDatabase where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateRelationalDatabase" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateRelationalDatabase where
  toJSON UpdateRelationalDatabase' {..} =
    Core.object
      ( Core.catMaybes
          [ ("preferredBackupWindow" Core..=)
              Core.<$> preferredBackupWindow,
            ("caCertificateIdentifier" Core..=)
              Core.<$> caCertificateIdentifier,
            ("disableBackupRetention" Core..=)
              Core.<$> disableBackupRetention,
            ("masterUserPassword" Core..=)
              Core.<$> masterUserPassword,
            ("publiclyAccessible" Core..=)
              Core.<$> publiclyAccessible,
            ("preferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow,
            ("enableBackupRetention" Core..=)
              Core.<$> enableBackupRetention,
            ("rotateMasterUserPassword" Core..=)
              Core.<$> rotateMasterUserPassword,
            ("applyImmediately" Core..=)
              Core.<$> applyImmediately,
            Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance Core.ToPath UpdateRelationalDatabase where
  toPath = Core.const "/"

instance Core.ToQuery UpdateRelationalDatabase where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateRelationalDatabaseResponse' smart constructor.
data UpdateRelationalDatabaseResponse = UpdateRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRelationalDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'updateRelationalDatabaseResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'updateRelationalDatabaseResponse_httpStatus' - The response's http status code.
newUpdateRelationalDatabaseResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateRelationalDatabaseResponse
newUpdateRelationalDatabaseResponse pHttpStatus_ =
  UpdateRelationalDatabaseResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateRelationalDatabaseResponse_operations :: Lens.Lens' UpdateRelationalDatabaseResponse (Core.Maybe [Operation])
updateRelationalDatabaseResponse_operations = Lens.lens (\UpdateRelationalDatabaseResponse' {operations} -> operations) (\s@UpdateRelationalDatabaseResponse' {} a -> s {operations = a} :: UpdateRelationalDatabaseResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateRelationalDatabaseResponse_httpStatus :: Lens.Lens' UpdateRelationalDatabaseResponse Core.Int
updateRelationalDatabaseResponse_httpStatus = Lens.lens (\UpdateRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@UpdateRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: UpdateRelationalDatabaseResponse)

instance Core.NFData UpdateRelationalDatabaseResponse
