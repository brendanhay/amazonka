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
import Network.AWS.DMS.Types.PluginNameValue
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
    -- | The write-ahead log (WAL) heartbeat feature mimics a dummy transaction.
    -- By doing this, it prevents idle logical replication slots from holding
    -- onto old WAL logs, which can result in storage full situations on the
    -- source. This heartbeat keeps @restart_lsn@ moving and prevents storage
    -- full scenarios.
    heartbeatEnable :: Prelude.Maybe Prelude.Bool,
    -- | Sets the schema in which the heartbeat artifacts are created.
    heartbeatSchema :: Prelude.Maybe Prelude.Text,
    -- | Specifies the plugin to use to create a replication slot.
    pluginName :: Prelude.Maybe PluginNameValue,
    -- | To capture DDL events, DMS creates various artifacts in the PostgreSQL
    -- database when the task starts. You can later remove these artifacts.
    --
    -- If this value is set to @N@, you don\'t have to create tables or
    -- triggers on the source database.
    captureDdls :: Prelude.Maybe Prelude.Bool,
    -- | Sets the name of a previously created logical replication slot for a
    -- change data capture (CDC) load of the PostgreSQL source instance.
    --
    -- When used with the @CdcStartPosition@ request parameter for the DMS API
    -- , this attribute also makes it possible to use native CDC start points.
    -- DMS verifies that the specified logical replication slot exists before
    -- starting the CDC load task. It also verifies that the task was created
    -- with a valid setting of @CdcStartPosition@. If the specified slot
    -- doesn\'t exist or the task doesn\'t have a valid @CdcStartPosition@
    -- setting, DMS raises an error.
    --
    -- For more information about setting the @CdcStartPosition@ request
    -- parameter, see
    -- <dms/latest/userguide/CHAP_Task.CDC.html#CHAP_Task.CDC.StartPoint.Native Determining a CDC native start point>
    -- in the /Database Migration Service User Guide/. For more information
    -- about using @CdcStartPosition@, see
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>,
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>,
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | The schema in which the operational DDL database artifacts are created.
    --
    -- Example: @ddlArtifactsSchema=xyzddlschema;@
    ddlArtifactsSchema :: Prelude.Maybe Prelude.Text,
    -- | For use with change data capture (CDC) only, this attribute has DMS
    -- bypass foreign keys and user triggers to reduce the time it takes to
    -- bulk load data.
    --
    -- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
    afterConnectScript :: Prelude.Maybe Prelude.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer
    -- data to PostgreSQL.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Prelude.Maybe Prelude.Int,
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Sets the WAL heartbeat frequency (in minutes).
    heartbeatFrequency :: Prelude.Maybe Prelude.Int,
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the PostgreSQL endpoint.
    --
    -- You can specify one of two sets of values for these permissions. You can
    -- specify the values for this setting and @SecretsManagerSecretId@. Or you
    -- can specify clear-text values for @UserName@, @Password@, @ServerName@,
    -- and @Port@. You can\'t specify both. For more information on creating
    -- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
    -- @SecretsManagerSecretId@ required to access it, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
    -- in the /Database Migration Service User Guide/.
    secretsManagerAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'heartbeatEnable', 'postgreSQLSettings_heartbeatEnable' - The write-ahead log (WAL) heartbeat feature mimics a dummy transaction.
-- By doing this, it prevents idle logical replication slots from holding
-- onto old WAL logs, which can result in storage full situations on the
-- source. This heartbeat keeps @restart_lsn@ moving and prevents storage
-- full scenarios.
--
-- 'heartbeatSchema', 'postgreSQLSettings_heartbeatSchema' - Sets the schema in which the heartbeat artifacts are created.
--
-- 'pluginName', 'postgreSQLSettings_pluginName' - Specifies the plugin to use to create a replication slot.
--
-- 'captureDdls', 'postgreSQLSettings_captureDdls' - To capture DDL events, DMS creates various artifacts in the PostgreSQL
-- database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@, you don\'t have to create tables or
-- triggers on the source database.
--
-- 'slotName', 'postgreSQLSettings_slotName' - Sets the name of a previously created logical replication slot for a
-- change data capture (CDC) load of the PostgreSQL source instance.
--
-- When used with the @CdcStartPosition@ request parameter for the DMS API
-- , this attribute also makes it possible to use native CDC start points.
-- DMS verifies that the specified logical replication slot exists before
-- starting the CDC load task. It also verifies that the task was created
-- with a valid setting of @CdcStartPosition@. If the specified slot
-- doesn\'t exist or the task doesn\'t have a valid @CdcStartPosition@
-- setting, DMS raises an error.
--
-- For more information about setting the @CdcStartPosition@ request
-- parameter, see
-- <dms/latest/userguide/CHAP_Task.CDC.html#CHAP_Task.CDC.StartPoint.Native Determining a CDC native start point>
-- in the /Database Migration Service User Guide/. For more information
-- about using @CdcStartPosition@, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>,
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>,
-- and
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>.
--
-- 'ddlArtifactsSchema', 'postgreSQLSettings_ddlArtifactsSchema' - The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
--
-- 'afterConnectScript', 'postgreSQLSettings_afterConnectScript' - For use with change data capture (CDC) only, this attribute has DMS
-- bypass foreign keys and user triggers to reduce the time it takes to
-- bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
--
-- 'secretsManagerSecretId', 'postgreSQLSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
-- details.
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
-- 'heartbeatFrequency', 'postgreSQLSettings_heartbeatFrequency' - Sets the WAL heartbeat frequency (in minutes).
--
-- 'port', 'postgreSQLSettings_port' - Endpoint TCP port.
--
-- 'username', 'postgreSQLSettings_username' - Endpoint connection user name.
--
-- 'secretsManagerAccessRoleArn', 'postgreSQLSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the PostgreSQL endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
--
-- 'databaseName', 'postgreSQLSettings_databaseName' - Database name for the endpoint.
newPostgreSQLSettings ::
  PostgreSQLSettings
newPostgreSQLSettings =
  PostgreSQLSettings'
    { failTasksOnLobTruncation =
        Prelude.Nothing,
      executeTimeout = Prelude.Nothing,
      heartbeatEnable = Prelude.Nothing,
      heartbeatSchema = Prelude.Nothing,
      pluginName = Prelude.Nothing,
      captureDdls = Prelude.Nothing,
      slotName = Prelude.Nothing,
      ddlArtifactsSchema = Prelude.Nothing,
      afterConnectScript = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      serverName = Prelude.Nothing,
      maxFileSize = Prelude.Nothing,
      password = Prelude.Nothing,
      heartbeatFrequency = Prelude.Nothing,
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

-- | The write-ahead log (WAL) heartbeat feature mimics a dummy transaction.
-- By doing this, it prevents idle logical replication slots from holding
-- onto old WAL logs, which can result in storage full situations on the
-- source. This heartbeat keeps @restart_lsn@ moving and prevents storage
-- full scenarios.
postgreSQLSettings_heartbeatEnable :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Bool)
postgreSQLSettings_heartbeatEnable = Lens.lens (\PostgreSQLSettings' {heartbeatEnable} -> heartbeatEnable) (\s@PostgreSQLSettings' {} a -> s {heartbeatEnable = a} :: PostgreSQLSettings)

-- | Sets the schema in which the heartbeat artifacts are created.
postgreSQLSettings_heartbeatSchema :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_heartbeatSchema = Lens.lens (\PostgreSQLSettings' {heartbeatSchema} -> heartbeatSchema) (\s@PostgreSQLSettings' {} a -> s {heartbeatSchema = a} :: PostgreSQLSettings)

-- | Specifies the plugin to use to create a replication slot.
postgreSQLSettings_pluginName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe PluginNameValue)
postgreSQLSettings_pluginName = Lens.lens (\PostgreSQLSettings' {pluginName} -> pluginName) (\s@PostgreSQLSettings' {} a -> s {pluginName = a} :: PostgreSQLSettings)

-- | To capture DDL events, DMS creates various artifacts in the PostgreSQL
-- database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@, you don\'t have to create tables or
-- triggers on the source database.
postgreSQLSettings_captureDdls :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Bool)
postgreSQLSettings_captureDdls = Lens.lens (\PostgreSQLSettings' {captureDdls} -> captureDdls) (\s@PostgreSQLSettings' {} a -> s {captureDdls = a} :: PostgreSQLSettings)

-- | Sets the name of a previously created logical replication slot for a
-- change data capture (CDC) load of the PostgreSQL source instance.
--
-- When used with the @CdcStartPosition@ request parameter for the DMS API
-- , this attribute also makes it possible to use native CDC start points.
-- DMS verifies that the specified logical replication slot exists before
-- starting the CDC load task. It also verifies that the task was created
-- with a valid setting of @CdcStartPosition@. If the specified slot
-- doesn\'t exist or the task doesn\'t have a valid @CdcStartPosition@
-- setting, DMS raises an error.
--
-- For more information about setting the @CdcStartPosition@ request
-- parameter, see
-- <dms/latest/userguide/CHAP_Task.CDC.html#CHAP_Task.CDC.StartPoint.Native Determining a CDC native start point>
-- in the /Database Migration Service User Guide/. For more information
-- about using @CdcStartPosition@, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>,
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>,
-- and
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>.
postgreSQLSettings_slotName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_slotName = Lens.lens (\PostgreSQLSettings' {slotName} -> slotName) (\s@PostgreSQLSettings' {} a -> s {slotName = a} :: PostgreSQLSettings)

-- | The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
postgreSQLSettings_ddlArtifactsSchema :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_ddlArtifactsSchema = Lens.lens (\PostgreSQLSettings' {ddlArtifactsSchema} -> ddlArtifactsSchema) (\s@PostgreSQLSettings' {} a -> s {ddlArtifactsSchema = a} :: PostgreSQLSettings)

-- | For use with change data capture (CDC) only, this attribute has DMS
-- bypass foreign keys and user triggers to reduce the time it takes to
-- bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
postgreSQLSettings_afterConnectScript :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_afterConnectScript = Lens.lens (\PostgreSQLSettings' {afterConnectScript} -> afterConnectScript) (\s@PostgreSQLSettings' {} a -> s {afterConnectScript = a} :: PostgreSQLSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
-- details.
postgreSQLSettings_secretsManagerSecretId :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_secretsManagerSecretId = Lens.lens (\PostgreSQLSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@PostgreSQLSettings' {} a -> s {secretsManagerSecretId = a} :: PostgreSQLSettings)

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
postgreSQLSettings_password = Lens.lens (\PostgreSQLSettings' {password} -> password) (\s@PostgreSQLSettings' {} a -> s {password = a} :: PostgreSQLSettings) Prelude.. Lens.mapping Core._Sensitive

-- | Sets the WAL heartbeat frequency (in minutes).
postgreSQLSettings_heartbeatFrequency :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_heartbeatFrequency = Lens.lens (\PostgreSQLSettings' {heartbeatFrequency} -> heartbeatFrequency) (\s@PostgreSQLSettings' {} a -> s {heartbeatFrequency = a} :: PostgreSQLSettings)

-- | Endpoint TCP port.
postgreSQLSettings_port :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_port = Lens.lens (\PostgreSQLSettings' {port} -> port) (\s@PostgreSQLSettings' {} a -> s {port = a} :: PostgreSQLSettings)

-- | Endpoint connection user name.
postgreSQLSettings_username :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_username = Lens.lens (\PostgreSQLSettings' {username} -> username) (\s@PostgreSQLSettings' {} a -> s {username = a} :: PostgreSQLSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the PostgreSQL endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
postgreSQLSettings_secretsManagerAccessRoleArn :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_secretsManagerAccessRoleArn = Lens.lens (\PostgreSQLSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@PostgreSQLSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: PostgreSQLSettings)

-- | Database name for the endpoint.
postgreSQLSettings_databaseName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_databaseName = Lens.lens (\PostgreSQLSettings' {databaseName} -> databaseName) (\s@PostgreSQLSettings' {} a -> s {databaseName = a} :: PostgreSQLSettings)

instance Core.FromJSON PostgreSQLSettings where
  parseJSON =
    Core.withObject
      "PostgreSQLSettings"
      ( \x ->
          PostgreSQLSettings'
            Prelude.<$> (x Core..:? "FailTasksOnLobTruncation")
            Prelude.<*> (x Core..:? "ExecuteTimeout")
            Prelude.<*> (x Core..:? "HeartbeatEnable")
            Prelude.<*> (x Core..:? "HeartbeatSchema")
            Prelude.<*> (x Core..:? "PluginName")
            Prelude.<*> (x Core..:? "CaptureDdls")
            Prelude.<*> (x Core..:? "SlotName")
            Prelude.<*> (x Core..:? "DdlArtifactsSchema")
            Prelude.<*> (x Core..:? "AfterConnectScript")
            Prelude.<*> (x Core..:? "SecretsManagerSecretId")
            Prelude.<*> (x Core..:? "ServerName")
            Prelude.<*> (x Core..:? "MaxFileSize")
            Prelude.<*> (x Core..:? "Password")
            Prelude.<*> (x Core..:? "HeartbeatFrequency")
            Prelude.<*> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Core..:? "DatabaseName")
      )

instance Prelude.Hashable PostgreSQLSettings

instance Prelude.NFData PostgreSQLSettings

instance Core.ToJSON PostgreSQLSettings where
  toJSON PostgreSQLSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FailTasksOnLobTruncation" Core..=)
              Prelude.<$> failTasksOnLobTruncation,
            ("ExecuteTimeout" Core..=)
              Prelude.<$> executeTimeout,
            ("HeartbeatEnable" Core..=)
              Prelude.<$> heartbeatEnable,
            ("HeartbeatSchema" Core..=)
              Prelude.<$> heartbeatSchema,
            ("PluginName" Core..=) Prelude.<$> pluginName,
            ("CaptureDdls" Core..=) Prelude.<$> captureDdls,
            ("SlotName" Core..=) Prelude.<$> slotName,
            ("DdlArtifactsSchema" Core..=)
              Prelude.<$> ddlArtifactsSchema,
            ("AfterConnectScript" Core..=)
              Prelude.<$> afterConnectScript,
            ("SecretsManagerSecretId" Core..=)
              Prelude.<$> secretsManagerSecretId,
            ("ServerName" Core..=) Prelude.<$> serverName,
            ("MaxFileSize" Core..=) Prelude.<$> maxFileSize,
            ("Password" Core..=) Prelude.<$> password,
            ("HeartbeatFrequency" Core..=)
              Prelude.<$> heartbeatFrequency,
            ("Port" Core..=) Prelude.<$> port,
            ("Username" Core..=) Prelude.<$> username,
            ("SecretsManagerAccessRoleArn" Core..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("DatabaseName" Core..=) Prelude.<$> databaseName
          ]
      )
