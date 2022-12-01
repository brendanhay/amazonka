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
-- Module      : Amazonka.DMS.Types.MySQLSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.MySQLSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.TargetDbType
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines a MySQL endpoint.
--
-- /See:/ 'newMySQLSettings' smart constructor.
data MySQLSettings = MySQLSettings'
  { -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer
    -- data to a MySQL-compatible database.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies where to migrate source tables on the target, either to a
    -- single database or multiple databases. If you specify
    -- @SPECIFIC_DATABASE@, specify the database name using the @DatabaseName@
    -- parameter of the @Endpoint@ object.
    --
    -- Example: @targetDbType=MULTIPLE_DATABASES@
    targetDbType :: Prelude.Maybe TargetDbType,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the MySQL endpoint.
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
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Database name for the endpoint. For a MySQL source or target endpoint,
    -- don\'t explicitly specify the database using the @DatabaseName@ request
    -- parameter on either the @CreateEndpoint@ or @ModifyEndpoint@ API call.
    -- Specifying @DatabaseName@ when you create or modify a MySQL endpoint
    -- replicates all the task tables to this single database. For MySQL
    -- endpoints, you specify the database only when you specify the schema in
    -- the table-mapping rules of the DMS task.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | Improves performance when loading data into the MySQL-compatible target
    -- database. Specifies how many threads to use to load the data into the
    -- MySQL-compatible target database. Setting a large number of threads can
    -- have an adverse effect on database performance, because a separate
    -- connection is required for each thread. The default is one.
    --
    -- Example: @parallelLoadThreads=1@
    parallelLoadThreads :: Prelude.Maybe Prelude.Int,
    -- | Specifies a script to run immediately after DMS connects to the
    -- endpoint. The migration task continues running regardless if the SQL
    -- statement succeeds or fails.
    --
    -- For this parameter, provide the code of the script itself, not the name
    -- of a file containing the script.
    afterConnectScript :: Prelude.Maybe Prelude.Text,
    -- | Specifies how often to check the binary log for new changes\/events when
    -- the database is idle. The default is five seconds.
    --
    -- Example: @eventsPollInterval=5;@
    --
    -- In the example, DMS checks for changes in the binary logs every five
    -- seconds.
    eventsPollInterval :: Prelude.Maybe Prelude.Int,
    -- | Adjusts the behavior of DMS when migrating from an SQL Server source
    -- database that is hosted as part of an Always On availability group
    -- cluster. If you need DMS to poll all the nodes in the Always On cluster
    -- for transaction backups, set this attribute to @false@.
    cleanSourceMetadataOnMismatch :: Prelude.Maybe Prelude.Bool,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the MySQL endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time zone for the source MySQL database.
    --
    -- Example: @serverTimezone=US\/Pacific;@
    --
    -- Note: Do not enclose time zones in single quotes.
    serverTimezone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MySQLSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'mySQLSettings_port' - Endpoint TCP port.
--
-- 'maxFileSize', 'mySQLSettings_maxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
--
-- 'targetDbType', 'mySQLSettings_targetDbType' - Specifies where to migrate source tables on the target, either to a
-- single database or multiple databases. If you specify
-- @SPECIFIC_DATABASE@, specify the database name using the @DatabaseName@
-- parameter of the @Endpoint@ object.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
--
-- 'secretsManagerAccessRoleArn', 'mySQLSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the MySQL endpoint.
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
-- 'password', 'mySQLSettings_password' - Endpoint connection password.
--
-- 'serverName', 'mySQLSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'databaseName', 'mySQLSettings_databaseName' - Database name for the endpoint. For a MySQL source or target endpoint,
-- don\'t explicitly specify the database using the @DatabaseName@ request
-- parameter on either the @CreateEndpoint@ or @ModifyEndpoint@ API call.
-- Specifying @DatabaseName@ when you create or modify a MySQL endpoint
-- replicates all the task tables to this single database. For MySQL
-- endpoints, you specify the database only when you specify the schema in
-- the table-mapping rules of the DMS task.
--
-- 'username', 'mySQLSettings_username' - Endpoint connection user name.
--
-- 'parallelLoadThreads', 'mySQLSettings_parallelLoadThreads' - Improves performance when loading data into the MySQL-compatible target
-- database. Specifies how many threads to use to load the data into the
-- MySQL-compatible target database. Setting a large number of threads can
-- have an adverse effect on database performance, because a separate
-- connection is required for each thread. The default is one.
--
-- Example: @parallelLoadThreads=1@
--
-- 'afterConnectScript', 'mySQLSettings_afterConnectScript' - Specifies a script to run immediately after DMS connects to the
-- endpoint. The migration task continues running regardless if the SQL
-- statement succeeds or fails.
--
-- For this parameter, provide the code of the script itself, not the name
-- of a file containing the script.
--
-- 'eventsPollInterval', 'mySQLSettings_eventsPollInterval' - Specifies how often to check the binary log for new changes\/events when
-- the database is idle. The default is five seconds.
--
-- Example: @eventsPollInterval=5;@
--
-- In the example, DMS checks for changes in the binary logs every five
-- seconds.
--
-- 'cleanSourceMetadataOnMismatch', 'mySQLSettings_cleanSourceMetadataOnMismatch' - Adjusts the behavior of DMS when migrating from an SQL Server source
-- database that is hosted as part of an Always On availability group
-- cluster. If you need DMS to poll all the nodes in the Always On cluster
-- for transaction backups, set this attribute to @false@.
--
-- 'secretsManagerSecretId', 'mySQLSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MySQL endpoint connection
-- details.
--
-- 'serverTimezone', 'mySQLSettings_serverTimezone' - Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US\/Pacific;@
--
-- Note: Do not enclose time zones in single quotes.
newMySQLSettings ::
  MySQLSettings
newMySQLSettings =
  MySQLSettings'
    { port = Prelude.Nothing,
      maxFileSize = Prelude.Nothing,
      targetDbType = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      password = Prelude.Nothing,
      serverName = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      username = Prelude.Nothing,
      parallelLoadThreads = Prelude.Nothing,
      afterConnectScript = Prelude.Nothing,
      eventsPollInterval = Prelude.Nothing,
      cleanSourceMetadataOnMismatch = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      serverTimezone = Prelude.Nothing
    }

-- | Endpoint TCP port.
mySQLSettings_port :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Int)
mySQLSettings_port = Lens.lens (\MySQLSettings' {port} -> port) (\s@MySQLSettings' {} a -> s {port = a} :: MySQLSettings)

-- | Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
mySQLSettings_maxFileSize :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Int)
mySQLSettings_maxFileSize = Lens.lens (\MySQLSettings' {maxFileSize} -> maxFileSize) (\s@MySQLSettings' {} a -> s {maxFileSize = a} :: MySQLSettings)

-- | Specifies where to migrate source tables on the target, either to a
-- single database or multiple databases. If you specify
-- @SPECIFIC_DATABASE@, specify the database name using the @DatabaseName@
-- parameter of the @Endpoint@ object.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
mySQLSettings_targetDbType :: Lens.Lens' MySQLSettings (Prelude.Maybe TargetDbType)
mySQLSettings_targetDbType = Lens.lens (\MySQLSettings' {targetDbType} -> targetDbType) (\s@MySQLSettings' {} a -> s {targetDbType = a} :: MySQLSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the MySQL endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
mySQLSettings_secretsManagerAccessRoleArn :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Text)
mySQLSettings_secretsManagerAccessRoleArn = Lens.lens (\MySQLSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@MySQLSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: MySQLSettings)

-- | Endpoint connection password.
mySQLSettings_password :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Text)
mySQLSettings_password = Lens.lens (\MySQLSettings' {password} -> password) (\s@MySQLSettings' {} a -> s {password = a} :: MySQLSettings) Prelude.. Lens.mapping Core._Sensitive

-- | Fully qualified domain name of the endpoint.
mySQLSettings_serverName :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Text)
mySQLSettings_serverName = Lens.lens (\MySQLSettings' {serverName} -> serverName) (\s@MySQLSettings' {} a -> s {serverName = a} :: MySQLSettings)

-- | Database name for the endpoint. For a MySQL source or target endpoint,
-- don\'t explicitly specify the database using the @DatabaseName@ request
-- parameter on either the @CreateEndpoint@ or @ModifyEndpoint@ API call.
-- Specifying @DatabaseName@ when you create or modify a MySQL endpoint
-- replicates all the task tables to this single database. For MySQL
-- endpoints, you specify the database only when you specify the schema in
-- the table-mapping rules of the DMS task.
mySQLSettings_databaseName :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Text)
mySQLSettings_databaseName = Lens.lens (\MySQLSettings' {databaseName} -> databaseName) (\s@MySQLSettings' {} a -> s {databaseName = a} :: MySQLSettings)

-- | Endpoint connection user name.
mySQLSettings_username :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Text)
mySQLSettings_username = Lens.lens (\MySQLSettings' {username} -> username) (\s@MySQLSettings' {} a -> s {username = a} :: MySQLSettings)

-- | Improves performance when loading data into the MySQL-compatible target
-- database. Specifies how many threads to use to load the data into the
-- MySQL-compatible target database. Setting a large number of threads can
-- have an adverse effect on database performance, because a separate
-- connection is required for each thread. The default is one.
--
-- Example: @parallelLoadThreads=1@
mySQLSettings_parallelLoadThreads :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Int)
mySQLSettings_parallelLoadThreads = Lens.lens (\MySQLSettings' {parallelLoadThreads} -> parallelLoadThreads) (\s@MySQLSettings' {} a -> s {parallelLoadThreads = a} :: MySQLSettings)

-- | Specifies a script to run immediately after DMS connects to the
-- endpoint. The migration task continues running regardless if the SQL
-- statement succeeds or fails.
--
-- For this parameter, provide the code of the script itself, not the name
-- of a file containing the script.
mySQLSettings_afterConnectScript :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Text)
mySQLSettings_afterConnectScript = Lens.lens (\MySQLSettings' {afterConnectScript} -> afterConnectScript) (\s@MySQLSettings' {} a -> s {afterConnectScript = a} :: MySQLSettings)

-- | Specifies how often to check the binary log for new changes\/events when
-- the database is idle. The default is five seconds.
--
-- Example: @eventsPollInterval=5;@
--
-- In the example, DMS checks for changes in the binary logs every five
-- seconds.
mySQLSettings_eventsPollInterval :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Int)
mySQLSettings_eventsPollInterval = Lens.lens (\MySQLSettings' {eventsPollInterval} -> eventsPollInterval) (\s@MySQLSettings' {} a -> s {eventsPollInterval = a} :: MySQLSettings)

-- | Adjusts the behavior of DMS when migrating from an SQL Server source
-- database that is hosted as part of an Always On availability group
-- cluster. If you need DMS to poll all the nodes in the Always On cluster
-- for transaction backups, set this attribute to @false@.
mySQLSettings_cleanSourceMetadataOnMismatch :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Bool)
mySQLSettings_cleanSourceMetadataOnMismatch = Lens.lens (\MySQLSettings' {cleanSourceMetadataOnMismatch} -> cleanSourceMetadataOnMismatch) (\s@MySQLSettings' {} a -> s {cleanSourceMetadataOnMismatch = a} :: MySQLSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MySQL endpoint connection
-- details.
mySQLSettings_secretsManagerSecretId :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Text)
mySQLSettings_secretsManagerSecretId = Lens.lens (\MySQLSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@MySQLSettings' {} a -> s {secretsManagerSecretId = a} :: MySQLSettings)

-- | Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US\/Pacific;@
--
-- Note: Do not enclose time zones in single quotes.
mySQLSettings_serverTimezone :: Lens.Lens' MySQLSettings (Prelude.Maybe Prelude.Text)
mySQLSettings_serverTimezone = Lens.lens (\MySQLSettings' {serverTimezone} -> serverTimezone) (\s@MySQLSettings' {} a -> s {serverTimezone = a} :: MySQLSettings)

instance Core.FromJSON MySQLSettings where
  parseJSON =
    Core.withObject
      "MySQLSettings"
      ( \x ->
          MySQLSettings'
            Prelude.<$> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "MaxFileSize")
            Prelude.<*> (x Core..:? "TargetDbType")
            Prelude.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Core..:? "Password")
            Prelude.<*> (x Core..:? "ServerName")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "ParallelLoadThreads")
            Prelude.<*> (x Core..:? "AfterConnectScript")
            Prelude.<*> (x Core..:? "EventsPollInterval")
            Prelude.<*> (x Core..:? "CleanSourceMetadataOnMismatch")
            Prelude.<*> (x Core..:? "SecretsManagerSecretId")
            Prelude.<*> (x Core..:? "ServerTimezone")
      )

instance Prelude.Hashable MySQLSettings where
  hashWithSalt _salt MySQLSettings' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` maxFileSize
      `Prelude.hashWithSalt` targetDbType
      `Prelude.hashWithSalt` secretsManagerAccessRoleArn
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` parallelLoadThreads
      `Prelude.hashWithSalt` afterConnectScript
      `Prelude.hashWithSalt` eventsPollInterval
      `Prelude.hashWithSalt` cleanSourceMetadataOnMismatch
      `Prelude.hashWithSalt` secretsManagerSecretId
      `Prelude.hashWithSalt` serverTimezone

instance Prelude.NFData MySQLSettings where
  rnf MySQLSettings' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf maxFileSize
      `Prelude.seq` Prelude.rnf targetDbType
      `Prelude.seq` Prelude.rnf secretsManagerAccessRoleArn
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf parallelLoadThreads
      `Prelude.seq` Prelude.rnf afterConnectScript
      `Prelude.seq` Prelude.rnf eventsPollInterval
      `Prelude.seq` Prelude.rnf cleanSourceMetadataOnMismatch
      `Prelude.seq` Prelude.rnf secretsManagerSecretId
      `Prelude.seq` Prelude.rnf serverTimezone

instance Core.ToJSON MySQLSettings where
  toJSON MySQLSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Port" Core..=) Prelude.<$> port,
            ("MaxFileSize" Core..=) Prelude.<$> maxFileSize,
            ("TargetDbType" Core..=) Prelude.<$> targetDbType,
            ("SecretsManagerAccessRoleArn" Core..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("Password" Core..=) Prelude.<$> password,
            ("ServerName" Core..=) Prelude.<$> serverName,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("Username" Core..=) Prelude.<$> username,
            ("ParallelLoadThreads" Core..=)
              Prelude.<$> parallelLoadThreads,
            ("AfterConnectScript" Core..=)
              Prelude.<$> afterConnectScript,
            ("EventsPollInterval" Core..=)
              Prelude.<$> eventsPollInterval,
            ("CleanSourceMetadataOnMismatch" Core..=)
              Prelude.<$> cleanSourceMetadataOnMismatch,
            ("SecretsManagerSecretId" Core..=)
              Prelude.<$> secretsManagerSecretId,
            ("ServerTimezone" Core..=)
              Prelude.<$> serverTimezone
          ]
      )
