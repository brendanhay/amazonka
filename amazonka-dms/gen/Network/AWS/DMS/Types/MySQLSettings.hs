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
-- Module      : Network.AWS.DMS.Types.MySQLSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MySQLSettings where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.TargetDbType
import qualified Network.AWS.Lens as Lens

-- | Provides information that defines a MySQL endpoint.
--
-- /See:/ 'newMySQLSettings' smart constructor.
data MySQLSettings = MySQLSettings'
  { -- | Specifies where to migrate source tables on the target, either to a
    -- single database or multiple databases.
    --
    -- Example: @targetDbType=MULTIPLE_DATABASES@
    targetDbType :: Core.Maybe TargetDbType,
    -- | Specifies the time zone for the source MySQL database.
    --
    -- Example: @serverTimezone=US\/Pacific;@
    --
    -- Note: Do not enclose time zones in single quotes.
    serverTimezone :: Core.Maybe Core.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the MySQL endpoint connection
    -- details.
    secretsManagerSecretId :: Core.Maybe Core.Text,
    -- | Specifies a script to run immediately after AWS DMS connects to the
    -- endpoint. The migration task continues running regardless if the SQL
    -- statement succeeds or fails.
    afterConnectScript :: Core.Maybe Core.Text,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Core.Maybe Core.Text,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer
    -- data to a MySQL-compatible database.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Core.Maybe Core.Int,
    -- | Endpoint connection password.
    password :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Specifies how often to check the binary log for new changes\/events when
    -- the database is idle.
    --
    -- Example: @eventsPollInterval=5;@
    --
    -- In the example, AWS DMS checks for changes in the binary logs every five
    -- seconds.
    eventsPollInterval :: Core.Maybe Core.Int,
    -- | Endpoint TCP port.
    port :: Core.Maybe Core.Int,
    -- | Endpoint connection user name.
    username :: Core.Maybe Core.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
    -- DMS as the trusted entity and grants the required permissions to access
    -- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
    -- value of the AWS Secrets Manager secret that allows access to the MySQL
    -- endpoint.
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
    -- | Improves performance when loading data into the MySQL-compatible target
    -- database. Specifies how many threads to use to load the data into the
    -- MySQL-compatible target database. Setting a large number of threads can
    -- have an adverse effect on database performance, because a separate
    -- connection is required for each thread.
    --
    -- Example: @parallelLoadThreads=1@
    parallelLoadThreads :: Core.Maybe Core.Int,
    -- | Database name for the endpoint.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'MySQLSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDbType', 'mySQLSettings_targetDbType' - Specifies where to migrate source tables on the target, either to a
-- single database or multiple databases.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
--
-- 'serverTimezone', 'mySQLSettings_serverTimezone' - Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US\/Pacific;@
--
-- Note: Do not enclose time zones in single quotes.
--
-- 'secretsManagerSecretId', 'mySQLSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MySQL endpoint connection
-- details.
--
-- 'afterConnectScript', 'mySQLSettings_afterConnectScript' - Specifies a script to run immediately after AWS DMS connects to the
-- endpoint. The migration task continues running regardless if the SQL
-- statement succeeds or fails.
--
-- 'serverName', 'mySQLSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'maxFileSize', 'mySQLSettings_maxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
--
-- 'password', 'mySQLSettings_password' - Endpoint connection password.
--
-- 'eventsPollInterval', 'mySQLSettings_eventsPollInterval' - Specifies how often to check the binary log for new changes\/events when
-- the database is idle.
--
-- Example: @eventsPollInterval=5;@
--
-- In the example, AWS DMS checks for changes in the binary logs every five
-- seconds.
--
-- 'port', 'mySQLSettings_port' - Endpoint TCP port.
--
-- 'username', 'mySQLSettings_username' - Endpoint connection user name.
--
-- 'secretsManagerAccessRoleArn', 'mySQLSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the MySQL
-- endpoint.
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
-- 'parallelLoadThreads', 'mySQLSettings_parallelLoadThreads' - Improves performance when loading data into the MySQL-compatible target
-- database. Specifies how many threads to use to load the data into the
-- MySQL-compatible target database. Setting a large number of threads can
-- have an adverse effect on database performance, because a separate
-- connection is required for each thread.
--
-- Example: @parallelLoadThreads=1@
--
-- 'databaseName', 'mySQLSettings_databaseName' - Database name for the endpoint.
newMySQLSettings ::
  MySQLSettings
newMySQLSettings =
  MySQLSettings'
    { targetDbType = Core.Nothing,
      serverTimezone = Core.Nothing,
      secretsManagerSecretId = Core.Nothing,
      afterConnectScript = Core.Nothing,
      serverName = Core.Nothing,
      maxFileSize = Core.Nothing,
      password = Core.Nothing,
      eventsPollInterval = Core.Nothing,
      port = Core.Nothing,
      username = Core.Nothing,
      secretsManagerAccessRoleArn = Core.Nothing,
      parallelLoadThreads = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | Specifies where to migrate source tables on the target, either to a
-- single database or multiple databases.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
mySQLSettings_targetDbType :: Lens.Lens' MySQLSettings (Core.Maybe TargetDbType)
mySQLSettings_targetDbType = Lens.lens (\MySQLSettings' {targetDbType} -> targetDbType) (\s@MySQLSettings' {} a -> s {targetDbType = a} :: MySQLSettings)

-- | Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US\/Pacific;@
--
-- Note: Do not enclose time zones in single quotes.
mySQLSettings_serverTimezone :: Lens.Lens' MySQLSettings (Core.Maybe Core.Text)
mySQLSettings_serverTimezone = Lens.lens (\MySQLSettings' {serverTimezone} -> serverTimezone) (\s@MySQLSettings' {} a -> s {serverTimezone = a} :: MySQLSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MySQL endpoint connection
-- details.
mySQLSettings_secretsManagerSecretId :: Lens.Lens' MySQLSettings (Core.Maybe Core.Text)
mySQLSettings_secretsManagerSecretId = Lens.lens (\MySQLSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@MySQLSettings' {} a -> s {secretsManagerSecretId = a} :: MySQLSettings)

-- | Specifies a script to run immediately after AWS DMS connects to the
-- endpoint. The migration task continues running regardless if the SQL
-- statement succeeds or fails.
mySQLSettings_afterConnectScript :: Lens.Lens' MySQLSettings (Core.Maybe Core.Text)
mySQLSettings_afterConnectScript = Lens.lens (\MySQLSettings' {afterConnectScript} -> afterConnectScript) (\s@MySQLSettings' {} a -> s {afterConnectScript = a} :: MySQLSettings)

-- | Fully qualified domain name of the endpoint.
mySQLSettings_serverName :: Lens.Lens' MySQLSettings (Core.Maybe Core.Text)
mySQLSettings_serverName = Lens.lens (\MySQLSettings' {serverName} -> serverName) (\s@MySQLSettings' {} a -> s {serverName = a} :: MySQLSettings)

-- | Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
mySQLSettings_maxFileSize :: Lens.Lens' MySQLSettings (Core.Maybe Core.Int)
mySQLSettings_maxFileSize = Lens.lens (\MySQLSettings' {maxFileSize} -> maxFileSize) (\s@MySQLSettings' {} a -> s {maxFileSize = a} :: MySQLSettings)

-- | Endpoint connection password.
mySQLSettings_password :: Lens.Lens' MySQLSettings (Core.Maybe Core.Text)
mySQLSettings_password = Lens.lens (\MySQLSettings' {password} -> password) (\s@MySQLSettings' {} a -> s {password = a} :: MySQLSettings) Core.. Lens.mapping Core._Sensitive

-- | Specifies how often to check the binary log for new changes\/events when
-- the database is idle.
--
-- Example: @eventsPollInterval=5;@
--
-- In the example, AWS DMS checks for changes in the binary logs every five
-- seconds.
mySQLSettings_eventsPollInterval :: Lens.Lens' MySQLSettings (Core.Maybe Core.Int)
mySQLSettings_eventsPollInterval = Lens.lens (\MySQLSettings' {eventsPollInterval} -> eventsPollInterval) (\s@MySQLSettings' {} a -> s {eventsPollInterval = a} :: MySQLSettings)

-- | Endpoint TCP port.
mySQLSettings_port :: Lens.Lens' MySQLSettings (Core.Maybe Core.Int)
mySQLSettings_port = Lens.lens (\MySQLSettings' {port} -> port) (\s@MySQLSettings' {} a -> s {port = a} :: MySQLSettings)

-- | Endpoint connection user name.
mySQLSettings_username :: Lens.Lens' MySQLSettings (Core.Maybe Core.Text)
mySQLSettings_username = Lens.lens (\MySQLSettings' {username} -> username) (\s@MySQLSettings' {} a -> s {username = a} :: MySQLSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the MySQL
-- endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
mySQLSettings_secretsManagerAccessRoleArn :: Lens.Lens' MySQLSettings (Core.Maybe Core.Text)
mySQLSettings_secretsManagerAccessRoleArn = Lens.lens (\MySQLSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@MySQLSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: MySQLSettings)

-- | Improves performance when loading data into the MySQL-compatible target
-- database. Specifies how many threads to use to load the data into the
-- MySQL-compatible target database. Setting a large number of threads can
-- have an adverse effect on database performance, because a separate
-- connection is required for each thread.
--
-- Example: @parallelLoadThreads=1@
mySQLSettings_parallelLoadThreads :: Lens.Lens' MySQLSettings (Core.Maybe Core.Int)
mySQLSettings_parallelLoadThreads = Lens.lens (\MySQLSettings' {parallelLoadThreads} -> parallelLoadThreads) (\s@MySQLSettings' {} a -> s {parallelLoadThreads = a} :: MySQLSettings)

-- | Database name for the endpoint.
mySQLSettings_databaseName :: Lens.Lens' MySQLSettings (Core.Maybe Core.Text)
mySQLSettings_databaseName = Lens.lens (\MySQLSettings' {databaseName} -> databaseName) (\s@MySQLSettings' {} a -> s {databaseName = a} :: MySQLSettings)

instance Core.FromJSON MySQLSettings where
  parseJSON =
    Core.withObject
      "MySQLSettings"
      ( \x ->
          MySQLSettings'
            Core.<$> (x Core..:? "TargetDbType")
            Core.<*> (x Core..:? "ServerTimezone")
            Core.<*> (x Core..:? "SecretsManagerSecretId")
            Core.<*> (x Core..:? "AfterConnectScript")
            Core.<*> (x Core..:? "ServerName")
            Core.<*> (x Core..:? "MaxFileSize")
            Core.<*> (x Core..:? "Password")
            Core.<*> (x Core..:? "EventsPollInterval")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Core.<*> (x Core..:? "ParallelLoadThreads")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable MySQLSettings

instance Core.NFData MySQLSettings

instance Core.ToJSON MySQLSettings where
  toJSON MySQLSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TargetDbType" Core..=) Core.<$> targetDbType,
            ("ServerTimezone" Core..=) Core.<$> serverTimezone,
            ("SecretsManagerSecretId" Core..=)
              Core.<$> secretsManagerSecretId,
            ("AfterConnectScript" Core..=)
              Core.<$> afterConnectScript,
            ("ServerName" Core..=) Core.<$> serverName,
            ("MaxFileSize" Core..=) Core.<$> maxFileSize,
            ("Password" Core..=) Core.<$> password,
            ("EventsPollInterval" Core..=)
              Core.<$> eventsPollInterval,
            ("Port" Core..=) Core.<$> port,
            ("Username" Core..=) Core.<$> username,
            ("SecretsManagerAccessRoleArn" Core..=)
              Core.<$> secretsManagerAccessRoleArn,
            ("ParallelLoadThreads" Core..=)
              Core.<$> parallelLoadThreads,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )
