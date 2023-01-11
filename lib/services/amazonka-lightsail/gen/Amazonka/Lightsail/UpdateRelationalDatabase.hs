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
-- Module      : Amazonka.Lightsail.UpdateRelationalDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.UpdateRelationalDatabase
  ( -- * Creating a Request
    UpdateRelationalDatabase (..),
    newUpdateRelationalDatabase,

    -- * Request Lenses
    updateRelationalDatabase_applyImmediately,
    updateRelationalDatabase_caCertificateIdentifier,
    updateRelationalDatabase_disableBackupRetention,
    updateRelationalDatabase_enableBackupRetention,
    updateRelationalDatabase_masterUserPassword,
    updateRelationalDatabase_preferredBackupWindow,
    updateRelationalDatabase_preferredMaintenanceWindow,
    updateRelationalDatabase_publiclyAccessible,
    updateRelationalDatabase_rotateMasterUserPassword,
    updateRelationalDatabase_relationalDatabaseName,

    -- * Destructuring the Response
    UpdateRelationalDatabaseResponse (..),
    newUpdateRelationalDatabaseResponse,

    -- * Response Lenses
    updateRelationalDatabaseResponse_operations,
    updateRelationalDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRelationalDatabase' smart constructor.
data UpdateRelationalDatabase = UpdateRelationalDatabase'
  { -- | When @true@, applies changes immediately. When @false@, applies changes
    -- during the preferred maintenance window. Some changes may cause an
    -- outage.
    --
    -- Default: @false@
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the certificate that needs to be associated with the database.
    caCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | When @true@, disables automated backup retention for your database.
    --
    -- Disabling backup retention deletes all automated database backups.
    -- Before disabling this, you may want to create a snapshot of your
    -- database using the @create relational database snapshot@ operation.
    --
    -- Updates are applied during the next maintenance window because this can
    -- result in an outage.
    disableBackupRetention :: Prelude.Maybe Prelude.Bool,
    -- | When @true@, enables automated backup retention for your database.
    --
    -- Updates are applied during the next maintenance window because this can
    -- result in an outage.
    enableBackupRetention :: Prelude.Maybe Prelude.Bool,
    -- | The password for the master user. The password can include any printable
    -- ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- My__SQL__
    --
    -- Constraints: Must contain from 8 to 41 characters.
    --
    -- __PostgreSQL__
    --
    -- Constraints: Must contain from 8 to 128 characters.
    masterUserPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur on your
    -- database.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week.
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
    -- | Specifies the accessibility options for your database. A value of @true@
    -- specifies a database that is available to resources outside of your
    -- Lightsail account. A value of @false@ specifies a database that is
    -- available only to your Lightsail resources in the same region as your
    -- database.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | When @true@, the master user password is changed to a new strong
    -- password generated by Lightsail.
    --
    -- Use the @get relational database master user password@ operation to get
    -- the new password.
    rotateMasterUserPassword :: Prelude.Maybe Prelude.Bool,
    -- | The name of your Lightsail database resource to update.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyImmediately', 'updateRelationalDatabase_applyImmediately' - When @true@, applies changes immediately. When @false@, applies changes
-- during the preferred maintenance window. Some changes may cause an
-- outage.
--
-- Default: @false@
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
-- 'enableBackupRetention', 'updateRelationalDatabase_enableBackupRetention' - When @true@, enables automated backup retention for your database.
--
-- Updates are applied during the next maintenance window because this can
-- result in an outage.
--
-- 'masterUserPassword', 'updateRelationalDatabase_masterUserPassword' - The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- My__SQL__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __PostgreSQL__
--
-- Constraints: Must contain from 8 to 128 characters.
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
-- 'preferredMaintenanceWindow', 'updateRelationalDatabase_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur on your
-- database.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week.
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
-- 'publiclyAccessible', 'updateRelationalDatabase_publiclyAccessible' - Specifies the accessibility options for your database. A value of @true@
-- specifies a database that is available to resources outside of your
-- Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
--
-- 'rotateMasterUserPassword', 'updateRelationalDatabase_rotateMasterUserPassword' - When @true@, the master user password is changed to a new strong
-- password generated by Lightsail.
--
-- Use the @get relational database master user password@ operation to get
-- the new password.
--
-- 'relationalDatabaseName', 'updateRelationalDatabase_relationalDatabaseName' - The name of your Lightsail database resource to update.
newUpdateRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  UpdateRelationalDatabase
newUpdateRelationalDatabase pRelationalDatabaseName_ =
  UpdateRelationalDatabase'
    { applyImmediately =
        Prelude.Nothing,
      caCertificateIdentifier = Prelude.Nothing,
      disableBackupRetention = Prelude.Nothing,
      enableBackupRetention = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      rotateMasterUserPassword = Prelude.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | When @true@, applies changes immediately. When @false@, applies changes
-- during the preferred maintenance window. Some changes may cause an
-- outage.
--
-- Default: @false@
updateRelationalDatabase_applyImmediately :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Bool)
updateRelationalDatabase_applyImmediately = Lens.lens (\UpdateRelationalDatabase' {applyImmediately} -> applyImmediately) (\s@UpdateRelationalDatabase' {} a -> s {applyImmediately = a} :: UpdateRelationalDatabase)

-- | Indicates the certificate that needs to be associated with the database.
updateRelationalDatabase_caCertificateIdentifier :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Text)
updateRelationalDatabase_caCertificateIdentifier = Lens.lens (\UpdateRelationalDatabase' {caCertificateIdentifier} -> caCertificateIdentifier) (\s@UpdateRelationalDatabase' {} a -> s {caCertificateIdentifier = a} :: UpdateRelationalDatabase)

-- | When @true@, disables automated backup retention for your database.
--
-- Disabling backup retention deletes all automated database backups.
-- Before disabling this, you may want to create a snapshot of your
-- database using the @create relational database snapshot@ operation.
--
-- Updates are applied during the next maintenance window because this can
-- result in an outage.
updateRelationalDatabase_disableBackupRetention :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Bool)
updateRelationalDatabase_disableBackupRetention = Lens.lens (\UpdateRelationalDatabase' {disableBackupRetention} -> disableBackupRetention) (\s@UpdateRelationalDatabase' {} a -> s {disableBackupRetention = a} :: UpdateRelationalDatabase)

-- | When @true@, enables automated backup retention for your database.
--
-- Updates are applied during the next maintenance window because this can
-- result in an outage.
updateRelationalDatabase_enableBackupRetention :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Bool)
updateRelationalDatabase_enableBackupRetention = Lens.lens (\UpdateRelationalDatabase' {enableBackupRetention} -> enableBackupRetention) (\s@UpdateRelationalDatabase' {} a -> s {enableBackupRetention = a} :: UpdateRelationalDatabase)

-- | The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- My__SQL__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __PostgreSQL__
--
-- Constraints: Must contain from 8 to 128 characters.
updateRelationalDatabase_masterUserPassword :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Text)
updateRelationalDatabase_masterUserPassword = Lens.lens (\UpdateRelationalDatabase' {masterUserPassword} -> masterUserPassword) (\s@UpdateRelationalDatabase' {} a -> s {masterUserPassword = a} :: UpdateRelationalDatabase) Prelude.. Lens.mapping Data._Sensitive

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
updateRelationalDatabase_preferredBackupWindow :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Text)
updateRelationalDatabase_preferredBackupWindow = Lens.lens (\UpdateRelationalDatabase' {preferredBackupWindow} -> preferredBackupWindow) (\s@UpdateRelationalDatabase' {} a -> s {preferredBackupWindow = a} :: UpdateRelationalDatabase)

-- | The weekly time range during which system maintenance can occur on your
-- database.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week.
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
updateRelationalDatabase_preferredMaintenanceWindow :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Text)
updateRelationalDatabase_preferredMaintenanceWindow = Lens.lens (\UpdateRelationalDatabase' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@UpdateRelationalDatabase' {} a -> s {preferredMaintenanceWindow = a} :: UpdateRelationalDatabase)

-- | Specifies the accessibility options for your database. A value of @true@
-- specifies a database that is available to resources outside of your
-- Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
updateRelationalDatabase_publiclyAccessible :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Bool)
updateRelationalDatabase_publiclyAccessible = Lens.lens (\UpdateRelationalDatabase' {publiclyAccessible} -> publiclyAccessible) (\s@UpdateRelationalDatabase' {} a -> s {publiclyAccessible = a} :: UpdateRelationalDatabase)

-- | When @true@, the master user password is changed to a new strong
-- password generated by Lightsail.
--
-- Use the @get relational database master user password@ operation to get
-- the new password.
updateRelationalDatabase_rotateMasterUserPassword :: Lens.Lens' UpdateRelationalDatabase (Prelude.Maybe Prelude.Bool)
updateRelationalDatabase_rotateMasterUserPassword = Lens.lens (\UpdateRelationalDatabase' {rotateMasterUserPassword} -> rotateMasterUserPassword) (\s@UpdateRelationalDatabase' {} a -> s {rotateMasterUserPassword = a} :: UpdateRelationalDatabase)

-- | The name of your Lightsail database resource to update.
updateRelationalDatabase_relationalDatabaseName :: Lens.Lens' UpdateRelationalDatabase Prelude.Text
updateRelationalDatabase_relationalDatabaseName = Lens.lens (\UpdateRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@UpdateRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: UpdateRelationalDatabase)

instance Core.AWSRequest UpdateRelationalDatabase where
  type
    AWSResponse UpdateRelationalDatabase =
      UpdateRelationalDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRelationalDatabaseResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRelationalDatabase where
  hashWithSalt _salt UpdateRelationalDatabase' {..} =
    _salt `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` caCertificateIdentifier
      `Prelude.hashWithSalt` disableBackupRetention
      `Prelude.hashWithSalt` enableBackupRetention
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` rotateMasterUserPassword
      `Prelude.hashWithSalt` relationalDatabaseName

instance Prelude.NFData UpdateRelationalDatabase where
  rnf UpdateRelationalDatabase' {..} =
    Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf caCertificateIdentifier
      `Prelude.seq` Prelude.rnf disableBackupRetention
      `Prelude.seq` Prelude.rnf enableBackupRetention
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf rotateMasterUserPassword
      `Prelude.seq` Prelude.rnf relationalDatabaseName

instance Data.ToHeaders UpdateRelationalDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.UpdateRelationalDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRelationalDatabase where
  toJSON UpdateRelationalDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applyImmediately" Data..=)
              Prelude.<$> applyImmediately,
            ("caCertificateIdentifier" Data..=)
              Prelude.<$> caCertificateIdentifier,
            ("disableBackupRetention" Data..=)
              Prelude.<$> disableBackupRetention,
            ("enableBackupRetention" Data..=)
              Prelude.<$> enableBackupRetention,
            ("masterUserPassword" Data..=)
              Prelude.<$> masterUserPassword,
            ("preferredBackupWindow" Data..=)
              Prelude.<$> preferredBackupWindow,
            ("preferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("publiclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible,
            ("rotateMasterUserPassword" Data..=)
              Prelude.<$> rotateMasterUserPassword,
            Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              )
          ]
      )

instance Data.ToPath UpdateRelationalDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRelationalDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRelationalDatabaseResponse' smart constructor.
data UpdateRelationalDatabaseResponse = UpdateRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateRelationalDatabaseResponse
newUpdateRelationalDatabaseResponse pHttpStatus_ =
  UpdateRelationalDatabaseResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateRelationalDatabaseResponse_operations :: Lens.Lens' UpdateRelationalDatabaseResponse (Prelude.Maybe [Operation])
updateRelationalDatabaseResponse_operations = Lens.lens (\UpdateRelationalDatabaseResponse' {operations} -> operations) (\s@UpdateRelationalDatabaseResponse' {} a -> s {operations = a} :: UpdateRelationalDatabaseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateRelationalDatabaseResponse_httpStatus :: Lens.Lens' UpdateRelationalDatabaseResponse Prelude.Int
updateRelationalDatabaseResponse_httpStatus = Lens.lens (\UpdateRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@UpdateRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: UpdateRelationalDatabaseResponse)

instance
  Prelude.NFData
    UpdateRelationalDatabaseResponse
  where
  rnf UpdateRelationalDatabaseResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
