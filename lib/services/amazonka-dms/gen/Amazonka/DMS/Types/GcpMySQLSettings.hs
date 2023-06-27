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
-- Module      : Amazonka.DMS.Types.GcpMySQLSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.GcpMySQLSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.TargetDbType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings in JSON format for the source GCP MySQL endpoint.
--
-- /See:/ 'newGcpMySQLSettings' smart constructor.
data GcpMySQLSettings = GcpMySQLSettings'
  { -- | Specifies a script to run immediately after DMS connects to the
    -- endpoint. The migration task continues running regardless if the SQL
    -- statement succeeds or fails.
    --
    -- For this parameter, provide the code of the script itself, not the name
    -- of a file containing the script.
    afterConnectScript :: Prelude.Maybe Prelude.Text,
    -- | Cleans and recreates table metadata information on the replication
    -- instance when a mismatch occurs. For example, in a situation where
    -- running an alter DDL on the table could result in different information
    -- about the table cached in the replication instance.
    cleanSourceMetadataOnMismatch :: Prelude.Maybe Prelude.Bool,
    -- | Database name for the endpoint. For a MySQL source or target endpoint,
    -- don\'t explicitly specify the database using the @DatabaseName@ request
    -- parameter on either the @CreateEndpoint@ or @ModifyEndpoint@ API call.
    -- Specifying @DatabaseName@ when you create or modify a MySQL endpoint
    -- replicates all the task tables to this single database. For MySQL
    -- endpoints, you specify the database only when you specify the schema in
    -- the table-mapping rules of the DMS task.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Specifies how often to check the binary log for new changes\/events when
    -- the database is idle. The default is five seconds.
    --
    -- Example: @eventsPollInterval=5;@
    --
    -- In the example, DMS checks for changes in the binary logs every five
    -- seconds.
    eventsPollInterval :: Prelude.Maybe Prelude.Int,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer
    -- data to a MySQL-compatible database.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Prelude.Maybe Prelude.Int,
    -- | Improves performance when loading data into the MySQL-compatible target
    -- database. Specifies how many threads to use to load the data into the
    -- MySQL-compatible target database. Setting a large number of threads can
    -- have an adverse effect on database performance, because a separate
    -- connection is required for each thread. The default is one.
    --
    -- Example: @parallelLoadThreads=1@
    parallelLoadThreads :: Prelude.Maybe Prelude.Int,
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret.@ The role must allow the @iam:PassRole@
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
    -- in the Database Migration Service User Guide.
    secretsManagerAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the MySQL endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | The MySQL host name.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time zone for the source MySQL database.
    --
    -- Example: @serverTimezone=US\/Pacific;@
    --
    -- Note: Do not enclose time zones in single quotes.
    serverTimezone :: Prelude.Maybe Prelude.Text,
    -- | Specifies where to migrate source tables on the target, either to a
    -- single database or multiple databases.
    --
    -- Example: @targetDbType=MULTIPLE_DATABASES@
    targetDbType :: Prelude.Maybe TargetDbType,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GcpMySQLSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'afterConnectScript', 'gcpMySQLSettings_afterConnectScript' - Specifies a script to run immediately after DMS connects to the
-- endpoint. The migration task continues running regardless if the SQL
-- statement succeeds or fails.
--
-- For this parameter, provide the code of the script itself, not the name
-- of a file containing the script.
--
-- 'cleanSourceMetadataOnMismatch', 'gcpMySQLSettings_cleanSourceMetadataOnMismatch' - Cleans and recreates table metadata information on the replication
-- instance when a mismatch occurs. For example, in a situation where
-- running an alter DDL on the table could result in different information
-- about the table cached in the replication instance.
--
-- 'databaseName', 'gcpMySQLSettings_databaseName' - Database name for the endpoint. For a MySQL source or target endpoint,
-- don\'t explicitly specify the database using the @DatabaseName@ request
-- parameter on either the @CreateEndpoint@ or @ModifyEndpoint@ API call.
-- Specifying @DatabaseName@ when you create or modify a MySQL endpoint
-- replicates all the task tables to this single database. For MySQL
-- endpoints, you specify the database only when you specify the schema in
-- the table-mapping rules of the DMS task.
--
-- 'eventsPollInterval', 'gcpMySQLSettings_eventsPollInterval' - Specifies how often to check the binary log for new changes\/events when
-- the database is idle. The default is five seconds.
--
-- Example: @eventsPollInterval=5;@
--
-- In the example, DMS checks for changes in the binary logs every five
-- seconds.
--
-- 'maxFileSize', 'gcpMySQLSettings_maxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
--
-- 'parallelLoadThreads', 'gcpMySQLSettings_parallelLoadThreads' - Improves performance when loading data into the MySQL-compatible target
-- database. Specifies how many threads to use to load the data into the
-- MySQL-compatible target database. Setting a large number of threads can
-- have an adverse effect on database performance, because a separate
-- connection is required for each thread. The default is one.
--
-- Example: @parallelLoadThreads=1@
--
-- 'password', 'gcpMySQLSettings_password' - Endpoint connection password.
--
-- 'port', 'gcpMySQLSettings_port' - Endpoint TCP port.
--
-- 'secretsManagerAccessRoleArn', 'gcpMySQLSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret.@ The role must allow the @iam:PassRole@
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
-- in the Database Migration Service User Guide.
--
-- 'secretsManagerSecretId', 'gcpMySQLSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MySQL endpoint connection
-- details.
--
-- 'serverName', 'gcpMySQLSettings_serverName' - The MySQL host name.
--
-- 'serverTimezone', 'gcpMySQLSettings_serverTimezone' - Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US\/Pacific;@
--
-- Note: Do not enclose time zones in single quotes.
--
-- 'targetDbType', 'gcpMySQLSettings_targetDbType' - Specifies where to migrate source tables on the target, either to a
-- single database or multiple databases.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
--
-- 'username', 'gcpMySQLSettings_username' - Endpoint connection user name.
newGcpMySQLSettings ::
  GcpMySQLSettings
newGcpMySQLSettings =
  GcpMySQLSettings'
    { afterConnectScript =
        Prelude.Nothing,
      cleanSourceMetadataOnMismatch = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      eventsPollInterval = Prelude.Nothing,
      maxFileSize = Prelude.Nothing,
      parallelLoadThreads = Prelude.Nothing,
      password = Prelude.Nothing,
      port = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      serverName = Prelude.Nothing,
      serverTimezone = Prelude.Nothing,
      targetDbType = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | Specifies a script to run immediately after DMS connects to the
-- endpoint. The migration task continues running regardless if the SQL
-- statement succeeds or fails.
--
-- For this parameter, provide the code of the script itself, not the name
-- of a file containing the script.
gcpMySQLSettings_afterConnectScript :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Text)
gcpMySQLSettings_afterConnectScript = Lens.lens (\GcpMySQLSettings' {afterConnectScript} -> afterConnectScript) (\s@GcpMySQLSettings' {} a -> s {afterConnectScript = a} :: GcpMySQLSettings)

-- | Cleans and recreates table metadata information on the replication
-- instance when a mismatch occurs. For example, in a situation where
-- running an alter DDL on the table could result in different information
-- about the table cached in the replication instance.
gcpMySQLSettings_cleanSourceMetadataOnMismatch :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Bool)
gcpMySQLSettings_cleanSourceMetadataOnMismatch = Lens.lens (\GcpMySQLSettings' {cleanSourceMetadataOnMismatch} -> cleanSourceMetadataOnMismatch) (\s@GcpMySQLSettings' {} a -> s {cleanSourceMetadataOnMismatch = a} :: GcpMySQLSettings)

-- | Database name for the endpoint. For a MySQL source or target endpoint,
-- don\'t explicitly specify the database using the @DatabaseName@ request
-- parameter on either the @CreateEndpoint@ or @ModifyEndpoint@ API call.
-- Specifying @DatabaseName@ when you create or modify a MySQL endpoint
-- replicates all the task tables to this single database. For MySQL
-- endpoints, you specify the database only when you specify the schema in
-- the table-mapping rules of the DMS task.
gcpMySQLSettings_databaseName :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Text)
gcpMySQLSettings_databaseName = Lens.lens (\GcpMySQLSettings' {databaseName} -> databaseName) (\s@GcpMySQLSettings' {} a -> s {databaseName = a} :: GcpMySQLSettings)

-- | Specifies how often to check the binary log for new changes\/events when
-- the database is idle. The default is five seconds.
--
-- Example: @eventsPollInterval=5;@
--
-- In the example, DMS checks for changes in the binary logs every five
-- seconds.
gcpMySQLSettings_eventsPollInterval :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Int)
gcpMySQLSettings_eventsPollInterval = Lens.lens (\GcpMySQLSettings' {eventsPollInterval} -> eventsPollInterval) (\s@GcpMySQLSettings' {} a -> s {eventsPollInterval = a} :: GcpMySQLSettings)

-- | Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
gcpMySQLSettings_maxFileSize :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Int)
gcpMySQLSettings_maxFileSize = Lens.lens (\GcpMySQLSettings' {maxFileSize} -> maxFileSize) (\s@GcpMySQLSettings' {} a -> s {maxFileSize = a} :: GcpMySQLSettings)

-- | Improves performance when loading data into the MySQL-compatible target
-- database. Specifies how many threads to use to load the data into the
-- MySQL-compatible target database. Setting a large number of threads can
-- have an adverse effect on database performance, because a separate
-- connection is required for each thread. The default is one.
--
-- Example: @parallelLoadThreads=1@
gcpMySQLSettings_parallelLoadThreads :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Int)
gcpMySQLSettings_parallelLoadThreads = Lens.lens (\GcpMySQLSettings' {parallelLoadThreads} -> parallelLoadThreads) (\s@GcpMySQLSettings' {} a -> s {parallelLoadThreads = a} :: GcpMySQLSettings)

-- | Endpoint connection password.
gcpMySQLSettings_password :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Text)
gcpMySQLSettings_password = Lens.lens (\GcpMySQLSettings' {password} -> password) (\s@GcpMySQLSettings' {} a -> s {password = a} :: GcpMySQLSettings) Prelude.. Lens.mapping Data._Sensitive

-- | Endpoint TCP port.
gcpMySQLSettings_port :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Int)
gcpMySQLSettings_port = Lens.lens (\GcpMySQLSettings' {port} -> port) (\s@GcpMySQLSettings' {} a -> s {port = a} :: GcpMySQLSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret.@ The role must allow the @iam:PassRole@
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
-- in the Database Migration Service User Guide.
gcpMySQLSettings_secretsManagerAccessRoleArn :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Text)
gcpMySQLSettings_secretsManagerAccessRoleArn = Lens.lens (\GcpMySQLSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@GcpMySQLSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: GcpMySQLSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MySQL endpoint connection
-- details.
gcpMySQLSettings_secretsManagerSecretId :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Text)
gcpMySQLSettings_secretsManagerSecretId = Lens.lens (\GcpMySQLSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@GcpMySQLSettings' {} a -> s {secretsManagerSecretId = a} :: GcpMySQLSettings)

-- | The MySQL host name.
gcpMySQLSettings_serverName :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Text)
gcpMySQLSettings_serverName = Lens.lens (\GcpMySQLSettings' {serverName} -> serverName) (\s@GcpMySQLSettings' {} a -> s {serverName = a} :: GcpMySQLSettings)

-- | Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US\/Pacific;@
--
-- Note: Do not enclose time zones in single quotes.
gcpMySQLSettings_serverTimezone :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Text)
gcpMySQLSettings_serverTimezone = Lens.lens (\GcpMySQLSettings' {serverTimezone} -> serverTimezone) (\s@GcpMySQLSettings' {} a -> s {serverTimezone = a} :: GcpMySQLSettings)

-- | Specifies where to migrate source tables on the target, either to a
-- single database or multiple databases.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
gcpMySQLSettings_targetDbType :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe TargetDbType)
gcpMySQLSettings_targetDbType = Lens.lens (\GcpMySQLSettings' {targetDbType} -> targetDbType) (\s@GcpMySQLSettings' {} a -> s {targetDbType = a} :: GcpMySQLSettings)

-- | Endpoint connection user name.
gcpMySQLSettings_username :: Lens.Lens' GcpMySQLSettings (Prelude.Maybe Prelude.Text)
gcpMySQLSettings_username = Lens.lens (\GcpMySQLSettings' {username} -> username) (\s@GcpMySQLSettings' {} a -> s {username = a} :: GcpMySQLSettings)

instance Data.FromJSON GcpMySQLSettings where
  parseJSON =
    Data.withObject
      "GcpMySQLSettings"
      ( \x ->
          GcpMySQLSettings'
            Prelude.<$> (x Data..:? "AfterConnectScript")
            Prelude.<*> (x Data..:? "CleanSourceMetadataOnMismatch")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "EventsPollInterval")
            Prelude.<*> (x Data..:? "MaxFileSize")
            Prelude.<*> (x Data..:? "ParallelLoadThreads")
            Prelude.<*> (x Data..:? "Password")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Data..:? "SecretsManagerSecretId")
            Prelude.<*> (x Data..:? "ServerName")
            Prelude.<*> (x Data..:? "ServerTimezone")
            Prelude.<*> (x Data..:? "TargetDbType")
            Prelude.<*> (x Data..:? "Username")
      )

instance Prelude.Hashable GcpMySQLSettings where
  hashWithSalt _salt GcpMySQLSettings' {..} =
    _salt
      `Prelude.hashWithSalt` afterConnectScript
      `Prelude.hashWithSalt` cleanSourceMetadataOnMismatch
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` eventsPollInterval
      `Prelude.hashWithSalt` maxFileSize
      `Prelude.hashWithSalt` parallelLoadThreads
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` secretsManagerAccessRoleArn
      `Prelude.hashWithSalt` secretsManagerSecretId
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` serverTimezone
      `Prelude.hashWithSalt` targetDbType
      `Prelude.hashWithSalt` username

instance Prelude.NFData GcpMySQLSettings where
  rnf GcpMySQLSettings' {..} =
    Prelude.rnf afterConnectScript
      `Prelude.seq` Prelude.rnf cleanSourceMetadataOnMismatch
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf eventsPollInterval
      `Prelude.seq` Prelude.rnf maxFileSize
      `Prelude.seq` Prelude.rnf parallelLoadThreads
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf secretsManagerAccessRoleArn
      `Prelude.seq` Prelude.rnf secretsManagerSecretId
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf serverTimezone
      `Prelude.seq` Prelude.rnf targetDbType
      `Prelude.seq` Prelude.rnf username

instance Data.ToJSON GcpMySQLSettings where
  toJSON GcpMySQLSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AfterConnectScript" Data..=)
              Prelude.<$> afterConnectScript,
            ("CleanSourceMetadataOnMismatch" Data..=)
              Prelude.<$> cleanSourceMetadataOnMismatch,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("EventsPollInterval" Data..=)
              Prelude.<$> eventsPollInterval,
            ("MaxFileSize" Data..=) Prelude.<$> maxFileSize,
            ("ParallelLoadThreads" Data..=)
              Prelude.<$> parallelLoadThreads,
            ("Password" Data..=) Prelude.<$> password,
            ("Port" Data..=) Prelude.<$> port,
            ("SecretsManagerAccessRoleArn" Data..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("SecretsManagerSecretId" Data..=)
              Prelude.<$> secretsManagerSecretId,
            ("ServerName" Data..=) Prelude.<$> serverName,
            ("ServerTimezone" Data..=)
              Prelude.<$> serverTimezone,
            ("TargetDbType" Data..=) Prelude.<$> targetDbType,
            ("Username" Data..=) Prelude.<$> username
          ]
      )
