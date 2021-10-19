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
-- Module      : Network.AWS.DMS.Types.MicrosoftSQLServerSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MicrosoftSQLServerSettings where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.SafeguardPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines a Microsoft SQL Server endpoint.
--
-- /See:/ 'newMicrosoftSQLServerSettings' smart constructor.
data MicrosoftSQLServerSettings = MicrosoftSQLServerSettings'
  { -- | The maximum size of the packets (in bytes) used to transfer data using
    -- BCP.
    bcpPacketSize :: Prelude.Maybe Prelude.Int,
    -- | Use this to attribute to transfer data for full-load operations using
    -- BCP. When the target table contains an identity column that does not
    -- exist in the source table, you must disable the use BCP for loading
    -- table option.
    useBcpFullLoad :: Prelude.Maybe Prelude.Bool,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Cleans and recreates table metadata information on the replication
    -- instance when a mismatch occurs. An example is a situation where running
    -- an alter DDL statement on a table might result in different information
    -- about the table cached in the replication instance.
    querySingleAlwaysOnNode :: Prelude.Maybe Prelude.Bool,
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
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
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
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | When this attribute is set to @Y@, DMS only reads changes from
    -- transaction log backups and doesn\'t read from the active transaction
    -- log file during ongoing replication. Setting this parameter to @Y@
    -- enables you to control active transaction log file growth during full
    -- load and ongoing replication tasks. However, it can add some source
    -- latency to ongoing replication.
    readBackupOnly :: Prelude.Maybe Prelude.Bool,
    -- | When this attribute is set to @Y@, DMS processes third-party transaction
    -- log backups if they are created in native format.
    useThirdPartyBackupDevice :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a file group for the DMS internal tables. When the replication
    -- task starts, all the internal DMS control tables (awsdms_
    -- apply_exception, awsdms_apply, awsdms_changes) are created for the
    -- specified file group.
    controlTablesFileGroup :: Prelude.Maybe Prelude.Text,
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int
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
-- 'useBcpFullLoad', 'microsoftSQLServerSettings_useBcpFullLoad' - Use this to attribute to transfer data for full-load operations using
-- BCP. When the target table contains an identity column that does not
-- exist in the source table, you must disable the use BCP for loading
-- table option.
--
-- 'serverName', 'microsoftSQLServerSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'querySingleAlwaysOnNode', 'microsoftSQLServerSettings_querySingleAlwaysOnNode' - Cleans and recreates table metadata information on the replication
-- instance when a mismatch occurs. An example is a situation where running
-- an alter DDL statement on a table might result in different information
-- about the table cached in the replication instance.
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
-- 'username', 'microsoftSQLServerSettings_username' - Endpoint connection user name.
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
-- 'password', 'microsoftSQLServerSettings_password' - Endpoint connection password.
--
-- 'databaseName', 'microsoftSQLServerSettings_databaseName' - Database name for the endpoint.
--
-- 'secretsManagerSecretId', 'microsoftSQLServerSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
-- details.
--
-- 'readBackupOnly', 'microsoftSQLServerSettings_readBackupOnly' - When this attribute is set to @Y@, DMS only reads changes from
-- transaction log backups and doesn\'t read from the active transaction
-- log file during ongoing replication. Setting this parameter to @Y@
-- enables you to control active transaction log file growth during full
-- load and ongoing replication tasks. However, it can add some source
-- latency to ongoing replication.
--
-- 'useThirdPartyBackupDevice', 'microsoftSQLServerSettings_useThirdPartyBackupDevice' - When this attribute is set to @Y@, DMS processes third-party transaction
-- log backups if they are created in native format.
--
-- 'controlTablesFileGroup', 'microsoftSQLServerSettings_controlTablesFileGroup' - Specifies a file group for the DMS internal tables. When the replication
-- task starts, all the internal DMS control tables (awsdms_
-- apply_exception, awsdms_apply, awsdms_changes) are created for the
-- specified file group.
--
-- 'port', 'microsoftSQLServerSettings_port' - Endpoint TCP port.
newMicrosoftSQLServerSettings ::
  MicrosoftSQLServerSettings
newMicrosoftSQLServerSettings =
  MicrosoftSQLServerSettings'
    { bcpPacketSize =
        Prelude.Nothing,
      useBcpFullLoad = Prelude.Nothing,
      serverName = Prelude.Nothing,
      querySingleAlwaysOnNode = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      username = Prelude.Nothing,
      safeguardPolicy = Prelude.Nothing,
      password = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      readBackupOnly = Prelude.Nothing,
      useThirdPartyBackupDevice = Prelude.Nothing,
      controlTablesFileGroup = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The maximum size of the packets (in bytes) used to transfer data using
-- BCP.
microsoftSQLServerSettings_bcpPacketSize :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Int)
microsoftSQLServerSettings_bcpPacketSize = Lens.lens (\MicrosoftSQLServerSettings' {bcpPacketSize} -> bcpPacketSize) (\s@MicrosoftSQLServerSettings' {} a -> s {bcpPacketSize = a} :: MicrosoftSQLServerSettings)

-- | Use this to attribute to transfer data for full-load operations using
-- BCP. When the target table contains an identity column that does not
-- exist in the source table, you must disable the use BCP for loading
-- table option.
microsoftSQLServerSettings_useBcpFullLoad :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_useBcpFullLoad = Lens.lens (\MicrosoftSQLServerSettings' {useBcpFullLoad} -> useBcpFullLoad) (\s@MicrosoftSQLServerSettings' {} a -> s {useBcpFullLoad = a} :: MicrosoftSQLServerSettings)

-- | Fully qualified domain name of the endpoint.
microsoftSQLServerSettings_serverName :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_serverName = Lens.lens (\MicrosoftSQLServerSettings' {serverName} -> serverName) (\s@MicrosoftSQLServerSettings' {} a -> s {serverName = a} :: MicrosoftSQLServerSettings)

-- | Cleans and recreates table metadata information on the replication
-- instance when a mismatch occurs. An example is a situation where running
-- an alter DDL statement on a table might result in different information
-- about the table cached in the replication instance.
microsoftSQLServerSettings_querySingleAlwaysOnNode :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_querySingleAlwaysOnNode = Lens.lens (\MicrosoftSQLServerSettings' {querySingleAlwaysOnNode} -> querySingleAlwaysOnNode) (\s@MicrosoftSQLServerSettings' {} a -> s {querySingleAlwaysOnNode = a} :: MicrosoftSQLServerSettings)

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

-- | Endpoint connection user name.
microsoftSQLServerSettings_username :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_username = Lens.lens (\MicrosoftSQLServerSettings' {username} -> username) (\s@MicrosoftSQLServerSettings' {} a -> s {username = a} :: MicrosoftSQLServerSettings)

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

-- | Endpoint connection password.
microsoftSQLServerSettings_password :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_password = Lens.lens (\MicrosoftSQLServerSettings' {password} -> password) (\s@MicrosoftSQLServerSettings' {} a -> s {password = a} :: MicrosoftSQLServerSettings) Prelude.. Lens.mapping Core._Sensitive

-- | Database name for the endpoint.
microsoftSQLServerSettings_databaseName :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_databaseName = Lens.lens (\MicrosoftSQLServerSettings' {databaseName} -> databaseName) (\s@MicrosoftSQLServerSettings' {} a -> s {databaseName = a} :: MicrosoftSQLServerSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
-- details.
microsoftSQLServerSettings_secretsManagerSecretId :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_secretsManagerSecretId = Lens.lens (\MicrosoftSQLServerSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@MicrosoftSQLServerSettings' {} a -> s {secretsManagerSecretId = a} :: MicrosoftSQLServerSettings)

-- | When this attribute is set to @Y@, DMS only reads changes from
-- transaction log backups and doesn\'t read from the active transaction
-- log file during ongoing replication. Setting this parameter to @Y@
-- enables you to control active transaction log file growth during full
-- load and ongoing replication tasks. However, it can add some source
-- latency to ongoing replication.
microsoftSQLServerSettings_readBackupOnly :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_readBackupOnly = Lens.lens (\MicrosoftSQLServerSettings' {readBackupOnly} -> readBackupOnly) (\s@MicrosoftSQLServerSettings' {} a -> s {readBackupOnly = a} :: MicrosoftSQLServerSettings)

-- | When this attribute is set to @Y@, DMS processes third-party transaction
-- log backups if they are created in native format.
microsoftSQLServerSettings_useThirdPartyBackupDevice :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_useThirdPartyBackupDevice = Lens.lens (\MicrosoftSQLServerSettings' {useThirdPartyBackupDevice} -> useThirdPartyBackupDevice) (\s@MicrosoftSQLServerSettings' {} a -> s {useThirdPartyBackupDevice = a} :: MicrosoftSQLServerSettings)

-- | Specifies a file group for the DMS internal tables. When the replication
-- task starts, all the internal DMS control tables (awsdms_
-- apply_exception, awsdms_apply, awsdms_changes) are created for the
-- specified file group.
microsoftSQLServerSettings_controlTablesFileGroup :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_controlTablesFileGroup = Lens.lens (\MicrosoftSQLServerSettings' {controlTablesFileGroup} -> controlTablesFileGroup) (\s@MicrosoftSQLServerSettings' {} a -> s {controlTablesFileGroup = a} :: MicrosoftSQLServerSettings)

-- | Endpoint TCP port.
microsoftSQLServerSettings_port :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Int)
microsoftSQLServerSettings_port = Lens.lens (\MicrosoftSQLServerSettings' {port} -> port) (\s@MicrosoftSQLServerSettings' {} a -> s {port = a} :: MicrosoftSQLServerSettings)

instance Core.FromJSON MicrosoftSQLServerSettings where
  parseJSON =
    Core.withObject
      "MicrosoftSQLServerSettings"
      ( \x ->
          MicrosoftSQLServerSettings'
            Prelude.<$> (x Core..:? "BcpPacketSize")
            Prelude.<*> (x Core..:? "UseBcpFullLoad")
            Prelude.<*> (x Core..:? "ServerName")
            Prelude.<*> (x Core..:? "QuerySingleAlwaysOnNode")
            Prelude.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "SafeguardPolicy")
            Prelude.<*> (x Core..:? "Password")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "SecretsManagerSecretId")
            Prelude.<*> (x Core..:? "ReadBackupOnly")
            Prelude.<*> (x Core..:? "UseThirdPartyBackupDevice")
            Prelude.<*> (x Core..:? "ControlTablesFileGroup")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable MicrosoftSQLServerSettings

instance Prelude.NFData MicrosoftSQLServerSettings

instance Core.ToJSON MicrosoftSQLServerSettings where
  toJSON MicrosoftSQLServerSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BcpPacketSize" Core..=) Prelude.<$> bcpPacketSize,
            ("UseBcpFullLoad" Core..=)
              Prelude.<$> useBcpFullLoad,
            ("ServerName" Core..=) Prelude.<$> serverName,
            ("QuerySingleAlwaysOnNode" Core..=)
              Prelude.<$> querySingleAlwaysOnNode,
            ("SecretsManagerAccessRoleArn" Core..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("Username" Core..=) Prelude.<$> username,
            ("SafeguardPolicy" Core..=)
              Prelude.<$> safeguardPolicy,
            ("Password" Core..=) Prelude.<$> password,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("SecretsManagerSecretId" Core..=)
              Prelude.<$> secretsManagerSecretId,
            ("ReadBackupOnly" Core..=)
              Prelude.<$> readBackupOnly,
            ("UseThirdPartyBackupDevice" Core..=)
              Prelude.<$> useThirdPartyBackupDevice,
            ("ControlTablesFileGroup" Core..=)
              Prelude.<$> controlTablesFileGroup,
            ("Port" Core..=) Prelude.<$> port
          ]
      )
