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
-- Module      : Network.AWS.DMS.Types.MicrosoftSQLServerSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MicrosoftSQLServerSettings where

import Network.AWS.DMS.Types.SafeguardPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines a Microsoft SQL Server endpoint.
--
-- /See:/ 'newMicrosoftSQLServerSettings' smart constructor.
data MicrosoftSQLServerSettings = MicrosoftSQLServerSettings'
  { -- | Use this to attribute to transfer data for full-load operations using
    -- BCP. When the target table contains an identity column that does not
    -- exist in the source table, you must disable the use BCP for loading
    -- table option.
    useBcpFullLoad :: Prelude.Maybe Prelude.Bool,
    -- | Use this attribute to minimize the need to access the backup log and
    -- enable AWS DMS to prevent truncation using one of the following two
    -- methods.
    --
    -- /Start transactions in the database:/ This is the default method. When
    -- this method is used, AWS DMS prevents TLOG truncation by mimicking a
    -- transaction in the database. As long as such a transaction is open,
    -- changes that appear after the transaction started aren\'t truncated. If
    -- you need Microsoft Replication to be enabled in your database, then you
    -- must choose this method.
    --
    -- /Exclusively use sp_repldone within a single task/: When this method is
    -- used, AWS DMS reads the changes and then uses sp_repldone to mark the
    -- TLOG transactions as ready for truncation. Although this method doesn\'t
    -- involve any transactional activities, it can only be used when Microsoft
    -- Replication isn\'t running. Also, when using this method, only one AWS
    -- DMS task can access the database at any given time. Therefore, if you
    -- need to run parallel AWS DMS tasks against the same database, use the
    -- default method.
    safeguardPolicy :: Prelude.Maybe SafeguardPolicy,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The maximum size of the packets (in bytes) used to transfer data using
    -- BCP.
    bcpPacketSize :: Prelude.Maybe Prelude.Int,
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
    -- DMS as the trusted entity and grants the required permissions to access
    -- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
    -- value of the AWS Secrets Manager secret that allows access to the SQL
    -- Server endpoint.
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
    -- | Specifies a file group for the AWS DMS internal tables. When the
    -- replication task starts, all the internal AWS DMS control tables
    -- (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created for
    -- the specified file group.
    controlTablesFileGroup :: Prelude.Maybe Prelude.Text,
    -- | When this attribute is set to @Y@, AWS DMS only reads changes from
    -- transaction log backups and doesn\'t read from the active transaction
    -- log file during ongoing replication. Setting this parameter to @Y@
    -- enables you to control active transaction log file growth during full
    -- load and ongoing replication tasks. However, it can add some source
    -- latency to ongoing replication.
    readBackupOnly :: Prelude.Maybe Prelude.Bool,
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MicrosoftSQLServerSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useBcpFullLoad', 'microsoftSQLServerSettings_useBcpFullLoad' - Use this to attribute to transfer data for full-load operations using
-- BCP. When the target table contains an identity column that does not
-- exist in the source table, you must disable the use BCP for loading
-- table option.
--
-- 'safeguardPolicy', 'microsoftSQLServerSettings_safeguardPolicy' - Use this attribute to minimize the need to access the backup log and
-- enable AWS DMS to prevent truncation using one of the following two
-- methods.
--
-- /Start transactions in the database:/ This is the default method. When
-- this method is used, AWS DMS prevents TLOG truncation by mimicking a
-- transaction in the database. As long as such a transaction is open,
-- changes that appear after the transaction started aren\'t truncated. If
-- you need Microsoft Replication to be enabled in your database, then you
-- must choose this method.
--
-- /Exclusively use sp_repldone within a single task/: When this method is
-- used, AWS DMS reads the changes and then uses sp_repldone to mark the
-- TLOG transactions as ready for truncation. Although this method doesn\'t
-- involve any transactional activities, it can only be used when Microsoft
-- Replication isn\'t running. Also, when using this method, only one AWS
-- DMS task can access the database at any given time. Therefore, if you
-- need to run parallel AWS DMS tasks against the same database, use the
-- default method.
--
-- 'secretsManagerSecretId', 'microsoftSQLServerSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
-- details.
--
-- 'serverName', 'microsoftSQLServerSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'password', 'microsoftSQLServerSettings_password' - Endpoint connection password.
--
-- 'bcpPacketSize', 'microsoftSQLServerSettings_bcpPacketSize' - The maximum size of the packets (in bytes) used to transfer data using
-- BCP.
--
-- 'port', 'microsoftSQLServerSettings_port' - Endpoint TCP port.
--
-- 'username', 'microsoftSQLServerSettings_username' - Endpoint connection user name.
--
-- 'secretsManagerAccessRoleArn', 'microsoftSQLServerSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the SQL
-- Server endpoint.
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
-- 'controlTablesFileGroup', 'microsoftSQLServerSettings_controlTablesFileGroup' - Specifies a file group for the AWS DMS internal tables. When the
-- replication task starts, all the internal AWS DMS control tables
-- (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created for
-- the specified file group.
--
-- 'readBackupOnly', 'microsoftSQLServerSettings_readBackupOnly' - When this attribute is set to @Y@, AWS DMS only reads changes from
-- transaction log backups and doesn\'t read from the active transaction
-- log file during ongoing replication. Setting this parameter to @Y@
-- enables you to control active transaction log file growth during full
-- load and ongoing replication tasks. However, it can add some source
-- latency to ongoing replication.
--
-- 'databaseName', 'microsoftSQLServerSettings_databaseName' - Database name for the endpoint.
newMicrosoftSQLServerSettings ::
  MicrosoftSQLServerSettings
newMicrosoftSQLServerSettings =
  MicrosoftSQLServerSettings'
    { useBcpFullLoad =
        Prelude.Nothing,
      safeguardPolicy = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      serverName = Prelude.Nothing,
      password = Prelude.Nothing,
      bcpPacketSize = Prelude.Nothing,
      port = Prelude.Nothing,
      username = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      controlTablesFileGroup = Prelude.Nothing,
      readBackupOnly = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | Use this to attribute to transfer data for full-load operations using
-- BCP. When the target table contains an identity column that does not
-- exist in the source table, you must disable the use BCP for loading
-- table option.
microsoftSQLServerSettings_useBcpFullLoad :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_useBcpFullLoad = Lens.lens (\MicrosoftSQLServerSettings' {useBcpFullLoad} -> useBcpFullLoad) (\s@MicrosoftSQLServerSettings' {} a -> s {useBcpFullLoad = a} :: MicrosoftSQLServerSettings)

-- | Use this attribute to minimize the need to access the backup log and
-- enable AWS DMS to prevent truncation using one of the following two
-- methods.
--
-- /Start transactions in the database:/ This is the default method. When
-- this method is used, AWS DMS prevents TLOG truncation by mimicking a
-- transaction in the database. As long as such a transaction is open,
-- changes that appear after the transaction started aren\'t truncated. If
-- you need Microsoft Replication to be enabled in your database, then you
-- must choose this method.
--
-- /Exclusively use sp_repldone within a single task/: When this method is
-- used, AWS DMS reads the changes and then uses sp_repldone to mark the
-- TLOG transactions as ready for truncation. Although this method doesn\'t
-- involve any transactional activities, it can only be used when Microsoft
-- Replication isn\'t running. Also, when using this method, only one AWS
-- DMS task can access the database at any given time. Therefore, if you
-- need to run parallel AWS DMS tasks against the same database, use the
-- default method.
microsoftSQLServerSettings_safeguardPolicy :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe SafeguardPolicy)
microsoftSQLServerSettings_safeguardPolicy = Lens.lens (\MicrosoftSQLServerSettings' {safeguardPolicy} -> safeguardPolicy) (\s@MicrosoftSQLServerSettings' {} a -> s {safeguardPolicy = a} :: MicrosoftSQLServerSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SQL Server endpoint connection
-- details.
microsoftSQLServerSettings_secretsManagerSecretId :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_secretsManagerSecretId = Lens.lens (\MicrosoftSQLServerSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@MicrosoftSQLServerSettings' {} a -> s {secretsManagerSecretId = a} :: MicrosoftSQLServerSettings)

-- | Fully qualified domain name of the endpoint.
microsoftSQLServerSettings_serverName :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_serverName = Lens.lens (\MicrosoftSQLServerSettings' {serverName} -> serverName) (\s@MicrosoftSQLServerSettings' {} a -> s {serverName = a} :: MicrosoftSQLServerSettings)

-- | Endpoint connection password.
microsoftSQLServerSettings_password :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_password = Lens.lens (\MicrosoftSQLServerSettings' {password} -> password) (\s@MicrosoftSQLServerSettings' {} a -> s {password = a} :: MicrosoftSQLServerSettings) Prelude.. Lens.mapping Prelude._Sensitive

-- | The maximum size of the packets (in bytes) used to transfer data using
-- BCP.
microsoftSQLServerSettings_bcpPacketSize :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Int)
microsoftSQLServerSettings_bcpPacketSize = Lens.lens (\MicrosoftSQLServerSettings' {bcpPacketSize} -> bcpPacketSize) (\s@MicrosoftSQLServerSettings' {} a -> s {bcpPacketSize = a} :: MicrosoftSQLServerSettings)

-- | Endpoint TCP port.
microsoftSQLServerSettings_port :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Int)
microsoftSQLServerSettings_port = Lens.lens (\MicrosoftSQLServerSettings' {port} -> port) (\s@MicrosoftSQLServerSettings' {} a -> s {port = a} :: MicrosoftSQLServerSettings)

-- | Endpoint connection user name.
microsoftSQLServerSettings_username :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_username = Lens.lens (\MicrosoftSQLServerSettings' {username} -> username) (\s@MicrosoftSQLServerSettings' {} a -> s {username = a} :: MicrosoftSQLServerSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the SQL
-- Server endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
microsoftSQLServerSettings_secretsManagerAccessRoleArn :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_secretsManagerAccessRoleArn = Lens.lens (\MicrosoftSQLServerSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@MicrosoftSQLServerSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: MicrosoftSQLServerSettings)

-- | Specifies a file group for the AWS DMS internal tables. When the
-- replication task starts, all the internal AWS DMS control tables
-- (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created for
-- the specified file group.
microsoftSQLServerSettings_controlTablesFileGroup :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_controlTablesFileGroup = Lens.lens (\MicrosoftSQLServerSettings' {controlTablesFileGroup} -> controlTablesFileGroup) (\s@MicrosoftSQLServerSettings' {} a -> s {controlTablesFileGroup = a} :: MicrosoftSQLServerSettings)

-- | When this attribute is set to @Y@, AWS DMS only reads changes from
-- transaction log backups and doesn\'t read from the active transaction
-- log file during ongoing replication. Setting this parameter to @Y@
-- enables you to control active transaction log file growth during full
-- load and ongoing replication tasks. However, it can add some source
-- latency to ongoing replication.
microsoftSQLServerSettings_readBackupOnly :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Bool)
microsoftSQLServerSettings_readBackupOnly = Lens.lens (\MicrosoftSQLServerSettings' {readBackupOnly} -> readBackupOnly) (\s@MicrosoftSQLServerSettings' {} a -> s {readBackupOnly = a} :: MicrosoftSQLServerSettings)

-- | Database name for the endpoint.
microsoftSQLServerSettings_databaseName :: Lens.Lens' MicrosoftSQLServerSettings (Prelude.Maybe Prelude.Text)
microsoftSQLServerSettings_databaseName = Lens.lens (\MicrosoftSQLServerSettings' {databaseName} -> databaseName) (\s@MicrosoftSQLServerSettings' {} a -> s {databaseName = a} :: MicrosoftSQLServerSettings)

instance Prelude.FromJSON MicrosoftSQLServerSettings where
  parseJSON =
    Prelude.withObject
      "MicrosoftSQLServerSettings"
      ( \x ->
          MicrosoftSQLServerSettings'
            Prelude.<$> (x Prelude..:? "UseBcpFullLoad")
            Prelude.<*> (x Prelude..:? "SafeguardPolicy")
            Prelude.<*> (x Prelude..:? "SecretsManagerSecretId")
            Prelude.<*> (x Prelude..:? "ServerName")
            Prelude.<*> (x Prelude..:? "Password")
            Prelude.<*> (x Prelude..:? "BcpPacketSize")
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "Username")
            Prelude.<*> (x Prelude..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Prelude..:? "ControlTablesFileGroup")
            Prelude.<*> (x Prelude..:? "ReadBackupOnly")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable MicrosoftSQLServerSettings

instance Prelude.NFData MicrosoftSQLServerSettings

instance Prelude.ToJSON MicrosoftSQLServerSettings where
  toJSON MicrosoftSQLServerSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("UseBcpFullLoad" Prelude..=)
              Prelude.<$> useBcpFullLoad,
            ("SafeguardPolicy" Prelude..=)
              Prelude.<$> safeguardPolicy,
            ("SecretsManagerSecretId" Prelude..=)
              Prelude.<$> secretsManagerSecretId,
            ("ServerName" Prelude..=) Prelude.<$> serverName,
            ("Password" Prelude..=) Prelude.<$> password,
            ("BcpPacketSize" Prelude..=)
              Prelude.<$> bcpPacketSize,
            ("Port" Prelude..=) Prelude.<$> port,
            ("Username" Prelude..=) Prelude.<$> username,
            ("SecretsManagerAccessRoleArn" Prelude..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("ControlTablesFileGroup" Prelude..=)
              Prelude.<$> controlTablesFileGroup,
            ("ReadBackupOnly" Prelude..=)
              Prelude.<$> readBackupOnly,
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )
