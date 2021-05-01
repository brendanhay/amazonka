{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines a PostgreSQL endpoint.
--
-- /See:/ 'newPostgreSQLSettings' smart constructor.
data PostgreSQLSettings = PostgreSQLSettings'
  { -- | When set to @true@, this value causes a task to fail if the actual size
    -- of a LOB column is greater than the specified @LobMaxSize@.
    --
    -- If task is set to Limited LOB mode and this option is set to true, the
    -- task fails instead of truncating the LOB data.
    failTasksOnLobTruncation :: Prelude.Maybe Prelude.Bool,
    -- | Sets the client statement timeout for the PostgreSQL instance, in
    -- seconds. The default value is 60 seconds.
    --
    -- Example: @executeTimeout=100;@
    executeTimeout :: Prelude.Maybe Prelude.Int,
    -- | Sets the name of a previously created logical replication slot for a CDC
    -- load of the PostgreSQL source instance.
    --
    -- When used with the AWS DMS API @CdcStartPosition@ request parameter,
    -- this attribute also enables using native CDC start points.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | To capture DDL events, AWS DMS creates various artifacts in the
    -- PostgreSQL database when the task starts. You can later remove these
    -- artifacts.
    --
    -- If this value is set to @N@, you don\'t have to create tables or
    -- triggers on the source database.
    captureDdls :: Prelude.Maybe Prelude.Bool,
    -- | The schema in which the operational DDL database artifacts are created.
    --
    -- Example: @ddlArtifactsSchema=xyzddlschema;@
    ddlArtifactsSchema :: Prelude.Maybe Prelude.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | For use with change data capture (CDC) only, this attribute has AWS DMS
    -- bypass foreign keys and user triggers to reduce the time it takes to
    -- bulk load data.
    --
    -- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
    afterConnectScript :: Prelude.Maybe Prelude.Text,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer
    -- data to PostgreSQL.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Prelude.Maybe Prelude.Int,
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
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
    secretsManagerAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      executeTimeout = Prelude.Nothing,
      slotName = Prelude.Nothing,
      captureDdls = Prelude.Nothing,
      ddlArtifactsSchema = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      afterConnectScript = Prelude.Nothing,
      serverName = Prelude.Nothing,
      maxFileSize = Prelude.Nothing,
      password = Prelude.Nothing,
      port = Prelude.Nothing,
      username = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | When set to @true@, this value causes a task to fail if the actual size
-- of a LOB column is greater than the specified @LobMaxSize@.
--
-- If task is set to Limited LOB mode and this option is set to true, the
-- task fails instead of truncating the LOB data.
postgreSQLSettings_failTasksOnLobTruncation :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Bool)
postgreSQLSettings_failTasksOnLobTruncation = Lens.lens (\PostgreSQLSettings' {failTasksOnLobTruncation} -> failTasksOnLobTruncation) (\s@PostgreSQLSettings' {} a -> s {failTasksOnLobTruncation = a} :: PostgreSQLSettings)

-- | Sets the client statement timeout for the PostgreSQL instance, in
-- seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@
postgreSQLSettings_executeTimeout :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_executeTimeout = Lens.lens (\PostgreSQLSettings' {executeTimeout} -> executeTimeout) (\s@PostgreSQLSettings' {} a -> s {executeTimeout = a} :: PostgreSQLSettings)

-- | Sets the name of a previously created logical replication slot for a CDC
-- load of the PostgreSQL source instance.
--
-- When used with the AWS DMS API @CdcStartPosition@ request parameter,
-- this attribute also enables using native CDC start points.
postgreSQLSettings_slotName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_slotName = Lens.lens (\PostgreSQLSettings' {slotName} -> slotName) (\s@PostgreSQLSettings' {} a -> s {slotName = a} :: PostgreSQLSettings)

-- | To capture DDL events, AWS DMS creates various artifacts in the
-- PostgreSQL database when the task starts. You can later remove these
-- artifacts.
--
-- If this value is set to @N@, you don\'t have to create tables or
-- triggers on the source database.
postgreSQLSettings_captureDdls :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Bool)
postgreSQLSettings_captureDdls = Lens.lens (\PostgreSQLSettings' {captureDdls} -> captureDdls) (\s@PostgreSQLSettings' {} a -> s {captureDdls = a} :: PostgreSQLSettings)

-- | The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
postgreSQLSettings_ddlArtifactsSchema :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_ddlArtifactsSchema = Lens.lens (\PostgreSQLSettings' {ddlArtifactsSchema} -> ddlArtifactsSchema) (\s@PostgreSQLSettings' {} a -> s {ddlArtifactsSchema = a} :: PostgreSQLSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
-- details.
postgreSQLSettings_secretsManagerSecretId :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_secretsManagerSecretId = Lens.lens (\PostgreSQLSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@PostgreSQLSettings' {} a -> s {secretsManagerSecretId = a} :: PostgreSQLSettings)

-- | For use with change data capture (CDC) only, this attribute has AWS DMS
-- bypass foreign keys and user triggers to reduce the time it takes to
-- bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
postgreSQLSettings_afterConnectScript :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_afterConnectScript = Lens.lens (\PostgreSQLSettings' {afterConnectScript} -> afterConnectScript) (\s@PostgreSQLSettings' {} a -> s {afterConnectScript = a} :: PostgreSQLSettings)

-- | Fully qualified domain name of the endpoint.
postgreSQLSettings_serverName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_serverName = Lens.lens (\PostgreSQLSettings' {serverName} -> serverName) (\s@PostgreSQLSettings' {} a -> s {serverName = a} :: PostgreSQLSettings)

-- | Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to PostgreSQL.
--
-- Example: @maxFileSize=512@
postgreSQLSettings_maxFileSize :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_maxFileSize = Lens.lens (\PostgreSQLSettings' {maxFileSize} -> maxFileSize) (\s@PostgreSQLSettings' {} a -> s {maxFileSize = a} :: PostgreSQLSettings)

-- | Endpoint connection password.
postgreSQLSettings_password :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_password = Lens.lens (\PostgreSQLSettings' {password} -> password) (\s@PostgreSQLSettings' {} a -> s {password = a} :: PostgreSQLSettings) Prelude.. Lens.mapping Prelude._Sensitive

-- | Endpoint TCP port.
postgreSQLSettings_port :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_port = Lens.lens (\PostgreSQLSettings' {port} -> port) (\s@PostgreSQLSettings' {} a -> s {port = a} :: PostgreSQLSettings)

-- | Endpoint connection user name.
postgreSQLSettings_username :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
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
postgreSQLSettings_secretsManagerAccessRoleArn :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_secretsManagerAccessRoleArn = Lens.lens (\PostgreSQLSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@PostgreSQLSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: PostgreSQLSettings)

-- | Database name for the endpoint.
postgreSQLSettings_databaseName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_databaseName = Lens.lens (\PostgreSQLSettings' {databaseName} -> databaseName) (\s@PostgreSQLSettings' {} a -> s {databaseName = a} :: PostgreSQLSettings)

instance Prelude.FromJSON PostgreSQLSettings where
  parseJSON =
    Prelude.withObject
      "PostgreSQLSettings"
      ( \x ->
          PostgreSQLSettings'
            Prelude.<$> (x Prelude..:? "FailTasksOnLobTruncation")
            Prelude.<*> (x Prelude..:? "ExecuteTimeout")
            Prelude.<*> (x Prelude..:? "SlotName")
            Prelude.<*> (x Prelude..:? "CaptureDdls")
            Prelude.<*> (x Prelude..:? "DdlArtifactsSchema")
            Prelude.<*> (x Prelude..:? "SecretsManagerSecretId")
            Prelude.<*> (x Prelude..:? "AfterConnectScript")
            Prelude.<*> (x Prelude..:? "ServerName")
            Prelude.<*> (x Prelude..:? "MaxFileSize")
            Prelude.<*> (x Prelude..:? "Password")
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "Username")
            Prelude.<*> (x Prelude..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable PostgreSQLSettings

instance Prelude.NFData PostgreSQLSettings

instance Prelude.ToJSON PostgreSQLSettings where
  toJSON PostgreSQLSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FailTasksOnLobTruncation" Prelude..=)
              Prelude.<$> failTasksOnLobTruncation,
            ("ExecuteTimeout" Prelude..=)
              Prelude.<$> executeTimeout,
            ("SlotName" Prelude..=) Prelude.<$> slotName,
            ("CaptureDdls" Prelude..=) Prelude.<$> captureDdls,
            ("DdlArtifactsSchema" Prelude..=)
              Prelude.<$> ddlArtifactsSchema,
            ("SecretsManagerSecretId" Prelude..=)
              Prelude.<$> secretsManagerSecretId,
            ("AfterConnectScript" Prelude..=)
              Prelude.<$> afterConnectScript,
            ("ServerName" Prelude..=) Prelude.<$> serverName,
            ("MaxFileSize" Prelude..=) Prelude.<$> maxFileSize,
            ("Password" Prelude..=) Prelude.<$> password,
            ("Port" Prelude..=) Prelude.<$> port,
            ("Username" Prelude..=) Prelude.<$> username,
            ("SecretsManagerAccessRoleArn" Prelude..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )
