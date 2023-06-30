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
-- Module      : Amazonka.DMS.Types.MicrosoftSQLServerSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.MicrosoftSQLServerSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.SafeguardPolicy
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines a Microsoft SQL Server endpoint.
--
-- /See:/ 'newMicrosoftSQLServerSettings' smart constructor.
data MicrosoftSQLServerSettings = MicrosoftSQLServerSettings'
  { -- | The maximum size of the packets (in bytes) used to transfer data using
    -- BCP.
    bcpPacketSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies a file group for the DMS internal tables. When the replication
    -- task starts, all the internal DMS control tables (awsdms_
    -- apply_exception, awsdms_apply, awsdms_changes) are created for the
    -- specified file group.
    controlTablesFileGroup :: Prelude.Maybe Prelude.Text,
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | Cleans and recreates table metadata information on the replication
    -- instance when a mismatch occurs. An example is a situation where running
    -- an alter DDL statement on a table might result in different information
    -- about the table cached in the replication instance.
    querySingleAlwaysOnNode :: Prelude.Maybe Prelude.Bool,
    -- | When this attribute is set to @Y@, DMS only reads changes from
    -- transaction log backups and doesn\'t read from the active transaction
    -- log file during ongoing replication. Setting this parameter to @Y@
    -- enables you to control active transaction log file growth during full
    -- load and ongoing replication tasks. However, it can add some source
    -- latency to ongoing replication.
    readBackupOnly :: Prelude.Maybe Prelude.Bool,
    -- | Use this attribute to minimize the need to access the backup log and
    -- enable DMS to prevent truncation using one of the following two methods.
    --
    -- /Start transactions in the database:/ This is the default method. When
    -- this method is used, DMS prevents TLOG truncation by mimicking a
    -- transaction in the database. As long as such a transaction is open,
    -- changes that appear after the transaction started aren\'t truncated. If
    -- you need Microsoft Replication to be enabled in your database, then you
    -- must choose this method.
    --
    -- /Exclusively use sp_repldone within a single task/: When this method is
    -- used, DMS reads the changes and then uses sp_repldone to mark the TLOG
    -- transactions as ready for truncation. Although this method doesn\'t
    -- involve any transactional activities, it can only be used when Microsoft
    -- Replication isn\'t running. Also, when using this method, only one DMS
    -- task can access the database at any given time. Therefore, if you need
    -- to run parallel DMS tasks against the same database, use the default
    -- method.
    safeguardPolicy :: Prelude.Maybe SafeguardPolicy,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the SQL Server endpoint.
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
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Use the @TrimSpaceInChar@ source endpoint setting to trim data on CHAR
    -- and NCHAR data types during migration. The default value is @true@.
    trimSpaceInChar :: Prelude.Maybe Prelude.Bool,
    -- | Use this to attribute to transfer data for full-load operations using
    -- BCP. When the target table contains an identity column that does not
    -- exist in the source table, you must disable the use BCP for loading
    -- table option.
    useBcpFullLoad :: Prelude.Maybe Prelude.Bool,
    -- | When this attribute is set to @Y@, DMS processes third-party transaction
    -- log backups if they are created in native format.
    useThirdPartyBackupDevice :: Prelude.Maybe Prelude.Bool,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MicrosoftSQLServerSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bcpPacketSize', 'microsoftSQLServerSettings_bcpPacketSize' - The maximum size of the packets (in bytes) used to transfer data using
-- BCP.
--
-- 'controlTablesFileGroup', 'microsoftSQLServerSettings_controlTablesFileGroup' - Specifies a file group for the DMS internal tables. When the replication
-- task starts, all the internal DMS control tables (awsdms_
-- apply_exception, awsdms_apply, awsdms_changes) are created for the
-- specified file group.
--
-- 'databaseName', 'microsoftSQLServerSettings_databaseName' - Database name for the endpoint.
--
-- 'password', 'microsoftSQLServerSettings_password' - Endpoint connection password.
--
-- 'port', 'microsoftSQLServerSettings_port' - Endpoint TCP port.
--
-- 'querySingleAlwaysOnNode', 'microsoftSQLServerSettings_querySingleAlwaysOnNode' - Cleans and recreates table metadata information on the replication
-- instance when a mismatch occurs. An example is a situation where running
-- an alter DDL statement on a table might result in different information
-- about the table cached in the replication instance.
--
-- 'readBackupOnly', 'microsoftSQLServerSettings_readBackupOnly' - When this attribute is set to @Y@, DMS only reads changes from
-- transaction log backups and doesn\'t read from the active transaction
-- log file during ongoing replication. Setting this parameter to @Y@
-- enables you to control active transaction log file growth during full
-- load and ongoing replication tasks. However, it can add some source
-- latency to ongoing replication.
--
-- 'safeguardPolicy', 'microsoftSQLServerSettings_safeguardPolicy' - Use this attribute to minimize the need to access the backup log and
-- enable DMS to prevent truncation using one of the following two methods.
--
-- /Start transactions in the database:/ This is the default method. When
-- this method is used, DMS prevents TLOG truncation by mimicking a
-- transaction in the database. As long as such a transaction is open,
-- changes that appear after the transaction started aren\'t truncated. If
-- you need Microsoft Replication to be enabled in your database, then you
-- must choose this method.
--
-- /Exclusively use sp_repldone within a single task/: When this method is
-- used, DMS reads the changes and then uses sp_repldone to mark the TLOG
-- transactions as ready for truncation. Although this method doesn\'t
-- involve any transactional activities, it can only be used when Microsoft
-- Replication isn\'t running. Also, when using this method, only one DMS
-- task can access the database at any given time. Therefore, if you need
-- to run parallel DMS tasks against the same database, use the default
-- method.
--
-- 'secretsManagerAccessRoleArn', 'microsoftSQLServerSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the SQL Server endpoint.
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
-- 'secretsManagerSecretId', 'microsoftSQLServerSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
-- details.
--
-- 'serverName', 'microsoftSQLServerSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'trimSpaceInChar', 'microsoftSQLServerSettings_trimSpaceInChar' - Use the @TrimSpaceInChar@ source endpoint setting to trim data on CHAR
-- and NCHAR data types during migration. The default value is @true@.
--
-- 'useBcpFullLoad', 'microsoftSQLServerSettings_useBcpFullLoad' - Use this to attribute to transfer data for full-load operations using
-- BCP. When the target table contains an identity column that does not
-- exist in the source table, you must disable the use BCP for loading
-- table option.
--
-- 'useThirdPartyBackupDevice', 'microsoftSQLServerSettings_useThirdPartyBackupDevice' - When this attribute is set to @Y@, DMS processes third-party transaction
-- log backups if they are created in native format.
--
-- 'username', 'microsoftSQLServerSettings_username' - Endpoint connection user name.
newMicrosoftSQLServerSettings ::
  MicrosoftSQLServerSettings
newMicrosoftSQLServerSettings =
  MicrosoftSQLServerSettings'
    { bcpPacketSize =
        Prelude.Nothing,
      controlTablesFileGroup = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      password = Prelude.Nothing,
      port = Prelude.Nothing,
      querySingleAlwaysOnNode = Prelude.Nothing,
      readBackupOnly = Prelude.Nothing,
      safeguardPolicy = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      serverName = Prelude.Nothing,
      trimSpaceInChar = Prelude.Nothing,
      useBcpFullLoad = Prelude.Nothing,
      useThirdPartyBackupDevice = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The maximum size of the packets (in bytes) used to transfer data using
-- BCP.
microsoftSQLServerSettings_bcpPacketSize :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Int)
microsoftSQLServerSettings_bcpPacketSize = Lens.lens (\MicrosoftSQLServerSettings' {bcpPacketSize} -> bcpPacketSize) (\s@MicrosoftSQLServerSettings' {} a -> s {bcpPacketSize = a} :: MicrosoftSQLServerSettings)

-- | Specifies a file group for the DMS internal tables. When the replication
-- task starts, all the internal DMS control tables (awsdms_
-- apply_exception, awsdms_apply, awsdms_changes) are created for the
-- specified file group.
microsoftSQLServerSettings_controlTablesFileGroup :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_controlTablesFileGroup = Lens.lens (\MicrosoftSQLServerSettings' {controlTablesFileGroup} -> controlTablesFileGroup) (\s@MicrosoftSQLServerSettings' {} a -> s {controlTablesFileGroup = a} :: MicrosoftSQLServerSettings)

-- | Database name for the endpoint.
microsoftSQLServerSettings_databaseName :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_databaseName = Lens.lens (\MicrosoftSQLServerSettings' {databaseName} -> databaseName) (\s@MicrosoftSQLServerSettings' {} a -> s {databaseName = a} :: MicrosoftSQLServerSettings)

-- | Endpoint connection password.
microsoftSQLServerSettings_password :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_password = Lens.lens (\MicrosoftSQLServerSettings' {password} -> password) (\s@MicrosoftSQLServerSettings' {} a -> s {password = a} :: MicrosoftSQLServerSettings) Prelude.. Lens.mapping Data._Sensitive

-- | Endpoint TCP port.
microsoftSQLServerSettings_port :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Int)
microsoftSQLServerSettings_port = Lens.lens (\MicrosoftSQLServerSettings' {port} -> port) (\s@MicrosoftSQLServerSettings' {} a -> s {port = a} :: MicrosoftSQLServerSettings)

-- | Cleans and recreates table metadata information on the replication
-- instance when a mismatch occurs. An example is a situation where running
-- an alter DDL statement on a table might result in different information
-- about the table cached in the replication instance.
microsoftSQLServerSettings_querySingleAlwaysOnNode :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_querySingleAlwaysOnNode = Lens.lens (\MicrosoftSQLServerSettings' {querySingleAlwaysOnNode} -> querySingleAlwaysOnNode) (\s@MicrosoftSQLServerSettings' {} a -> s {querySingleAlwaysOnNode = a} :: MicrosoftSQLServerSettings)

-- | When this attribute is set to @Y@, DMS only reads changes from
-- transaction log backups and doesn\'t read from the active transaction
-- log file during ongoing replication. Setting this parameter to @Y@
-- enables you to control active transaction log file growth during full
-- load and ongoing replication tasks. However, it can add some source
-- latency to ongoing replication.
microsoftSQLServerSettings_readBackupOnly :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_readBackupOnly = Lens.lens (\MicrosoftSQLServerSettings' {readBackupOnly} -> readBackupOnly) (\s@MicrosoftSQLServerSettings' {} a -> s {readBackupOnly = a} :: MicrosoftSQLServerSettings)

-- | Use this attribute to minimize the need to access the backup log and
-- enable DMS to prevent truncation using one of the following two methods.
--
-- /Start transactions in the database:/ This is the default method. When
-- this method is used, DMS prevents TLOG truncation by mimicking a
-- transaction in the database. As long as such a transaction is open,
-- changes that appear after the transaction started aren\'t truncated. If
-- you need Microsoft Replication to be enabled in your database, then you
-- must choose this method.
--
-- /Exclusively use sp_repldone within a single task/: When this method is
-- used, DMS reads the changes and then uses sp_repldone to mark the TLOG
-- transactions as ready for truncation. Although this method doesn\'t
-- involve any transactional activities, it can only be used when Microsoft
-- Replication isn\'t running. Also, when using this method, only one DMS
-- task can access the database at any given time. Therefore, if you need
-- to run parallel DMS tasks against the same database, use the default
-- method.
microsoftSQLServerSettings_safeguardPolicy :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe SafeguardPolicy)
microsoftSQLServerSettings_safeguardPolicy = Lens.lens (\MicrosoftSQLServerSettings' {safeguardPolicy} -> safeguardPolicy) (\s@MicrosoftSQLServerSettings' {} a -> s {safeguardPolicy = a} :: MicrosoftSQLServerSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the SQL Server endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
microsoftSQLServerSettings_secretsManagerAccessRoleArn :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_secretsManagerAccessRoleArn = Lens.lens (\MicrosoftSQLServerSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@MicrosoftSQLServerSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: MicrosoftSQLServerSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
-- details.
microsoftSQLServerSettings_secretsManagerSecretId :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_secretsManagerSecretId = Lens.lens (\MicrosoftSQLServerSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@MicrosoftSQLServerSettings' {} a -> s {secretsManagerSecretId = a} :: MicrosoftSQLServerSettings)

-- | Fully qualified domain name of the endpoint.
microsoftSQLServerSettings_serverName :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_serverName = Lens.lens (\MicrosoftSQLServerSettings' {serverName} -> serverName) (\s@MicrosoftSQLServerSettings' {} a -> s {serverName = a} :: MicrosoftSQLServerSettings)

-- | Use the @TrimSpaceInChar@ source endpoint setting to trim data on CHAR
-- and NCHAR data types during migration. The default value is @true@.
microsoftSQLServerSettings_trimSpaceInChar :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_trimSpaceInChar = Lens.lens (\MicrosoftSQLServerSettings' {trimSpaceInChar} -> trimSpaceInChar) (\s@MicrosoftSQLServerSettings' {} a -> s {trimSpaceInChar = a} :: MicrosoftSQLServerSettings)

-- | Use this to attribute to transfer data for full-load operations using
-- BCP. When the target table contains an identity column that does not
-- exist in the source table, you must disable the use BCP for loading
-- table option.
microsoftSQLServerSettings_useBcpFullLoad :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_useBcpFullLoad = Lens.lens (\MicrosoftSQLServerSettings' {useBcpFullLoad} -> useBcpFullLoad) (\s@MicrosoftSQLServerSettings' {} a -> s {useBcpFullLoad = a} :: MicrosoftSQLServerSettings)

-- | When this attribute is set to @Y@, DMS processes third-party transaction
-- log backups if they are created in native format.
microsoftSQLServerSettings_useThirdPartyBackupDevice :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_useThirdPartyBackupDevice = Lens.lens (\MicrosoftSQLServerSettings' {useThirdPartyBackupDevice} -> useThirdPartyBackupDevice) (\s@MicrosoftSQLServerSettings' {} a -> s {useThirdPartyBackupDevice = a} :: MicrosoftSQLServerSettings)

-- | Endpoint connection user name.
microsoftSQLServerSettings_username :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_username = Lens.lens (\MicrosoftSQLServerSettings' {username} -> username) (\s@MicrosoftSQLServerSettings' {} a -> s {username = a} :: MicrosoftSQLServerSettings)

instance Data.FromJSON MicrosoftSQLServerSettings where
  parseJSON =
    Data.withObject
      "MicrosoftSQLServerSettings"
      ( \x ->
          MicrosoftSQLServerSettings'
            Prelude.<$> (x Data..:? "BcpPacketSize")
            Prelude.<*> (x Data..:? "ControlTablesFileGroup")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Password")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "QuerySingleAlwaysOnNode")
            Prelude.<*> (x Data..:? "ReadBackupOnly")
            Prelude.<*> (x Data..:? "SafeguardPolicy")
            Prelude.<*> (x Data..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Data..:? "SecretsManagerSecretId")
            Prelude.<*> (x Data..:? "ServerName")
            Prelude.<*> (x Data..:? "TrimSpaceInChar")
            Prelude.<*> (x Data..:? "UseBcpFullLoad")
            Prelude.<*> (x Data..:? "UseThirdPartyBackupDevice")
            Prelude.<*> (x Data..:? "Username")
      )

instance Prelude.Hashable MicrosoftSQLServerSettings where
  hashWithSalt _salt MicrosoftSQLServerSettings' {..} =
    _salt
      `Prelude.hashWithSalt` bcpPacketSize
      `Prelude.hashWithSalt` controlTablesFileGroup
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` querySingleAlwaysOnNode
      `Prelude.hashWithSalt` readBackupOnly
      `Prelude.hashWithSalt` safeguardPolicy
      `Prelude.hashWithSalt` secretsManagerAccessRoleArn
      `Prelude.hashWithSalt` secretsManagerSecretId
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` trimSpaceInChar
      `Prelude.hashWithSalt` useBcpFullLoad
      `Prelude.hashWithSalt` useThirdPartyBackupDevice
      `Prelude.hashWithSalt` username

instance Prelude.NFData MicrosoftSQLServerSettings where
  rnf MicrosoftSQLServerSettings' {..} =
    Prelude.rnf bcpPacketSize
      `Prelude.seq` Prelude.rnf controlTablesFileGroup
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf querySingleAlwaysOnNode
      `Prelude.seq` Prelude.rnf readBackupOnly
      `Prelude.seq` Prelude.rnf safeguardPolicy
      `Prelude.seq` Prelude.rnf secretsManagerAccessRoleArn
      `Prelude.seq` Prelude.rnf secretsManagerSecretId
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf trimSpaceInChar
      `Prelude.seq` Prelude.rnf useBcpFullLoad
      `Prelude.seq` Prelude.rnf useThirdPartyBackupDevice
      `Prelude.seq` Prelude.rnf username

instance Data.ToJSON MicrosoftSQLServerSettings where
  toJSON MicrosoftSQLServerSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BcpPacketSize" Data..=) Prelude.<$> bcpPacketSize,
            ("ControlTablesFileGroup" Data..=)
              Prelude.<$> controlTablesFileGroup,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("Password" Data..=) Prelude.<$> password,
            ("Port" Data..=) Prelude.<$> port,
            ("QuerySingleAlwaysOnNode" Data..=)
              Prelude.<$> querySingleAlwaysOnNode,
            ("ReadBackupOnly" Data..=)
              Prelude.<$> readBackupOnly,
            ("SafeguardPolicy" Data..=)
              Prelude.<$> safeguardPolicy,
            ("SecretsManagerAccessRoleArn" Data..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("SecretsManagerSecretId" Data..=)
              Prelude.<$> secretsManagerSecretId,
            ("ServerName" Data..=) Prelude.<$> serverName,
            ("TrimSpaceInChar" Data..=)
              Prelude.<$> trimSpaceInChar,
            ("UseBcpFullLoad" Data..=)
              Prelude.<$> useBcpFullLoad,
            ("UseThirdPartyBackupDevice" Data..=)
              Prelude.<$> useThirdPartyBackupDevice,
            ("Username" Data..=) Prelude.<$> username
          ]
      )
