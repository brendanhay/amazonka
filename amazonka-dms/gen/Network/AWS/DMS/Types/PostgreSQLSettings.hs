{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.PostgreSQLSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.PostgreSQLSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information that defines a PostgreSQL endpoint.
--
-- /See:/ 'newPostgreSQLSettings' smart constructor.
data PostgreSQLSettings = PostgreSQLSettings'
  { -- | When set to @true@, this value causes a task to fail if the actual size
    -- of a LOB column is greater than the specified @LobMaxSize@.
    --
    -- If task is set to Limited LOB mode and this option is set to true, the
    -- task fails instead of truncating the LOB data.
    failTasksOnLobTruncation :: Core.Maybe Core.Bool,
    -- | Sets the client statement timeout for the PostgreSQL instance, in
    -- seconds. The default value is 60 seconds.
    --
    -- Example: @executeTimeout=100;@
    executeTimeout :: Core.Maybe Core.Int,
    -- | Sets the name of a previously created logical replication slot for a CDC
    -- load of the PostgreSQL source instance.
    --
    -- When used with the AWS DMS API @CdcStartPosition@ request parameter,
    -- this attribute also enables using native CDC start points.
    slotName :: Core.Maybe Core.Text,
    -- | To capture DDL events, AWS DMS creates various artifacts in the
    -- PostgreSQL database when the task starts. You can later remove these
    -- artifacts.
    --
    -- If this value is set to @N@, you don\'t have to create tables or
    -- triggers on the source database.
    captureDdls :: Core.Maybe Core.Bool,
    -- | The schema in which the operational DDL database artifacts are created.
    --
    -- Example: @ddlArtifactsSchema=xyzddlschema;@
    ddlArtifactsSchema :: Core.Maybe Core.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
    -- details.
    secretsManagerSecretId :: Core.Maybe Core.Text,
    -- | For use with change data capture (CDC) only, this attribute has AWS DMS
    -- bypass foreign keys and user triggers to reduce the time it takes to
    -- bulk load data.
    --
    -- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
    afterConnectScript :: Core.Maybe Core.Text,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Core.Maybe Core.Text,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer
    -- data to PostgreSQL.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Core.Maybe Core.Int,
    -- | Endpoint connection password.
    password :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Endpoint TCP port.
    port :: Core.Maybe Core.Int,
    -- | Endpoint connection user name.
    username :: Core.Maybe Core.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
    -- DMS as the trusted entity and grants the required permissions to access
    -- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
    -- value of the AWS Secrets Manager secret that allows access to the
    -- PostgreSQL endpoint.
    --
    -- You can specify one of two sets of values for these permissions. You can
    -- specify the values for this setting and @SecretsManagerSecretId@. Or you
    -- can specify clear-text values for @UserName@, @Password@, @ServerName@,
    -- and @Port@. You can\'t specify both. For more information on creating
    -- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
    -- @SecretsManagerSecretId@ required to access it, see
    -- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
    -- in the /AWS Database Migration Service User Guide/.
    secretsManagerAccessRoleArn :: Core.Maybe Core.Text,
    -- | Database name for the endpoint.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PostgreSQLSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failTasksOnLobTruncation', 'postgreSQLSettings_failTasksOnLobTruncation' - When set to @true@, this value causes a task to fail if the actual size
-- of a LOB column is greater than the specified @LobMaxSize@.
--
-- If task is set to Limited LOB mode and this option is set to true, the
-- task fails instead of truncating the LOB data.
--
-- 'executeTimeout', 'postgreSQLSettings_executeTimeout' - Sets the client statement timeout for the PostgreSQL instance, in
-- seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@
--
-- 'slotName', 'postgreSQLSettings_slotName' - Sets the name of a previously created logical replication slot for a CDC
-- load of the PostgreSQL source instance.
--
-- When used with the AWS DMS API @CdcStartPosition@ request parameter,
-- this attribute also enables using native CDC start points.
--
-- 'captureDdls', 'postgreSQLSettings_captureDdls' - To capture DDL events, AWS DMS creates various artifacts in the
-- PostgreSQL database when the task starts. You can later remove these
-- artifacts.
--
-- If this value is set to @N@, you don\'t have to create tables or
-- triggers on the source database.
--
-- 'ddlArtifactsSchema', 'postgreSQLSettings_ddlArtifactsSchema' - The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
--
-- 'secretsManagerSecretId', 'postgreSQLSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
-- details.
--
-- 'afterConnectScript', 'postgreSQLSettings_afterConnectScript' - For use with change data capture (CDC) only, this attribute has AWS DMS
-- bypass foreign keys and user triggers to reduce the time it takes to
-- bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
--
-- 'serverName', 'postgreSQLSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'maxFileSize', 'postgreSQLSettings_maxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to PostgreSQL.
--
-- Example: @maxFileSize=512@
--
-- 'password', 'postgreSQLSettings_password' - Endpoint connection password.
--
-- 'port', 'postgreSQLSettings_port' - Endpoint TCP port.
--
-- 'username', 'postgreSQLSettings_username' - Endpoint connection user name.
--
-- 'secretsManagerAccessRoleArn', 'postgreSQLSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the
-- PostgreSQL endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
--
-- 'databaseName', 'postgreSQLSettings_databaseName' - Database name for the endpoint.
newPostgreSQLSettings ::
  PostgreSQLSettings
newPostgreSQLSettings =
  PostgreSQLSettings'
    { failTasksOnLobTruncation =
        Core.Nothing,
      executeTimeout = Core.Nothing,
      slotName = Core.Nothing,
      captureDdls = Core.Nothing,
      ddlArtifactsSchema = Core.Nothing,
      secretsManagerSecretId = Core.Nothing,
      afterConnectScript = Core.Nothing,
      serverName = Core.Nothing,
      maxFileSize = Core.Nothing,
      password = Core.Nothing,
      port = Core.Nothing,
      username = Core.Nothing,
      secretsManagerAccessRoleArn = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | When set to @true@, this value causes a task to fail if the actual size
-- of a LOB column is greater than the specified @LobMaxSize@.
--
-- If task is set to Limited LOB mode and this option is set to true, the
-- task fails instead of truncating the LOB data.
postgreSQLSettings_failTasksOnLobTruncation :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Bool)
postgreSQLSettings_failTasksOnLobTruncation = Lens.lens (\PostgreSQLSettings' {failTasksOnLobTruncation} -> failTasksOnLobTruncation) (\s@PostgreSQLSettings' {} a -> s {failTasksOnLobTruncation = a} :: PostgreSQLSettings)

-- | Sets the client statement timeout for the PostgreSQL instance, in
-- seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@
postgreSQLSettings_executeTimeout :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
postgreSQLSettings_executeTimeout = Lens.lens (\PostgreSQLSettings' {executeTimeout} -> executeTimeout) (\s@PostgreSQLSettings' {} a -> s {executeTimeout = a} :: PostgreSQLSettings)

-- | Sets the name of a previously created logical replication slot for a CDC
-- load of the PostgreSQL source instance.
--
-- When used with the AWS DMS API @CdcStartPosition@ request parameter,
-- this attribute also enables using native CDC start points.
postgreSQLSettings_slotName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_slotName = Lens.lens (\PostgreSQLSettings' {slotName} -> slotName) (\s@PostgreSQLSettings' {} a -> s {slotName = a} :: PostgreSQLSettings)

-- | To capture DDL events, AWS DMS creates various artifacts in the
-- PostgreSQL database when the task starts. You can later remove these
-- artifacts.
--
-- If this value is set to @N@, you don\'t have to create tables or
-- triggers on the source database.
postgreSQLSettings_captureDdls :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Bool)
postgreSQLSettings_captureDdls = Lens.lens (\PostgreSQLSettings' {captureDdls} -> captureDdls) (\s@PostgreSQLSettings' {} a -> s {captureDdls = a} :: PostgreSQLSettings)

-- | The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
postgreSQLSettings_ddlArtifactsSchema :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_ddlArtifactsSchema = Lens.lens (\PostgreSQLSettings' {ddlArtifactsSchema} -> ddlArtifactsSchema) (\s@PostgreSQLSettings' {} a -> s {ddlArtifactsSchema = a} :: PostgreSQLSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
-- details.
postgreSQLSettings_secretsManagerSecretId :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_secretsManagerSecretId = Lens.lens (\PostgreSQLSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@PostgreSQLSettings' {} a -> s {secretsManagerSecretId = a} :: PostgreSQLSettings)

-- | For use with change data capture (CDC) only, this attribute has AWS DMS
-- bypass foreign keys and user triggers to reduce the time it takes to
-- bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
postgreSQLSettings_afterConnectScript :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_afterConnectScript = Lens.lens (\PostgreSQLSettings' {afterConnectScript} -> afterConnectScript) (\s@PostgreSQLSettings' {} a -> s {afterConnectScript = a} :: PostgreSQLSettings)

-- | Fully qualified domain name of the endpoint.
postgreSQLSettings_serverName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_serverName = Lens.lens (\PostgreSQLSettings' {serverName} -> serverName) (\s@PostgreSQLSettings' {} a -> s {serverName = a} :: PostgreSQLSettings)

-- | Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to PostgreSQL.
--
-- Example: @maxFileSize=512@
postgreSQLSettings_maxFileSize :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
postgreSQLSettings_maxFileSize = Lens.lens (\PostgreSQLSettings' {maxFileSize} -> maxFileSize) (\s@PostgreSQLSettings' {} a -> s {maxFileSize = a} :: PostgreSQLSettings)

-- | Endpoint connection password.
postgreSQLSettings_password :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_password = Lens.lens (\PostgreSQLSettings' {password} -> password) (\s@PostgreSQLSettings' {} a -> s {password = a} :: PostgreSQLSettings) Core.. Lens.mapping Core._Sensitive

-- | Endpoint TCP port.
postgreSQLSettings_port :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
postgreSQLSettings_port = Lens.lens (\PostgreSQLSettings' {port} -> port) (\s@PostgreSQLSettings' {} a -> s {port = a} :: PostgreSQLSettings)

-- | Endpoint connection user name.
postgreSQLSettings_username :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_username = Lens.lens (\PostgreSQLSettings' {username} -> username) (\s@PostgreSQLSettings' {} a -> s {username = a} :: PostgreSQLSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the
-- PostgreSQL endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
postgreSQLSettings_secretsManagerAccessRoleArn :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_secretsManagerAccessRoleArn = Lens.lens (\PostgreSQLSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@PostgreSQLSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: PostgreSQLSettings)

-- | Database name for the endpoint.
postgreSQLSettings_databaseName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
postgreSQLSettings_databaseName = Lens.lens (\PostgreSQLSettings' {databaseName} -> databaseName) (\s@PostgreSQLSettings' {} a -> s {databaseName = a} :: PostgreSQLSettings)

instance Core.FromJSON PostgreSQLSettings where
  parseJSON =
    Core.withObject
      "PostgreSQLSettings"
      ( \x ->
          PostgreSQLSettings'
            Core.<$> (x Core..:? "FailTasksOnLobTruncation")
            Core.<*> (x Core..:? "ExecuteTimeout")
            Core.<*> (x Core..:? "SlotName")
            Core.<*> (x Core..:? "CaptureDdls")
            Core.<*> (x Core..:? "DdlArtifactsSchema")
            Core.<*> (x Core..:? "SecretsManagerSecretId")
            Core.<*> (x Core..:? "AfterConnectScript")
            Core.<*> (x Core..:? "ServerName")
            Core.<*> (x Core..:? "MaxFileSize")
            Core.<*> (x Core..:? "Password")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable PostgreSQLSettings

instance Core.NFData PostgreSQLSettings

instance Core.ToJSON PostgreSQLSettings where
  toJSON PostgreSQLSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FailTasksOnLobTruncation" Core..=)
              Core.<$> failTasksOnLobTruncation,
            ("ExecuteTimeout" Core..=) Core.<$> executeTimeout,
            ("SlotName" Core..=) Core.<$> slotName,
            ("CaptureDdls" Core..=) Core.<$> captureDdls,
            ("DdlArtifactsSchema" Core..=)
              Core.<$> ddlArtifactsSchema,
            ("SecretsManagerSecretId" Core..=)
              Core.<$> secretsManagerSecretId,
            ("AfterConnectScript" Core..=)
              Core.<$> afterConnectScript,
            ("ServerName" Core..=) Core.<$> serverName,
            ("MaxFileSize" Core..=) Core.<$> maxFileSize,
            ("Password" Core..=) Core.<$> password,
            ("Port" Core..=) Core.<$> port,
            ("Username" Core..=) Core.<$> username,
            ("SecretsManagerAccessRoleArn" Core..=)
              Core.<$> secretsManagerAccessRoleArn,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )
