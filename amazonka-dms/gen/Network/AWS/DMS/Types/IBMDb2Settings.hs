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
-- Module      : Network.AWS.DMS.Types.IBMDb2Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.IBMDb2Settings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines an IBM Db2 LUW endpoint.
--
-- /See:/ 'newIBMDb2Settings' smart constructor.
data IBMDb2Settings = IBMDb2Settings'
  { -- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence
    -- number (LSN) where you want the replication to start.
    currentLsn :: Prelude.Maybe Prelude.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
    -- DMS as the trusted entity and grants the required permissions to access
    -- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
    -- value of the AWS Secrets Manager secret that allows access to the Db2
    -- LUW endpoint.
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
    -- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is
    -- true.
    setDataCaptureChanges :: Prelude.Maybe Prelude.Bool,
    -- | Maximum number of bytes per read, as a NUMBER value. The default is 64
    -- KB.
    maxKBytesPerRead :: Prelude.Maybe Prelude.Int,
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IBMDb2Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentLsn', 'iBMDb2Settings_currentLsn' - For ongoing replication (CDC), use CurrentLSN to specify a log sequence
-- number (LSN) where you want the replication to start.
--
-- 'secretsManagerSecretId', 'iBMDb2Settings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
-- details.
--
-- 'serverName', 'iBMDb2Settings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'password', 'iBMDb2Settings_password' - Endpoint connection password.
--
-- 'port', 'iBMDb2Settings_port' - Endpoint TCP port.
--
-- 'username', 'iBMDb2Settings_username' - Endpoint connection user name.
--
-- 'secretsManagerAccessRoleArn', 'iBMDb2Settings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the Db2
-- LUW endpoint.
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
-- 'setDataCaptureChanges', 'iBMDb2Settings_setDataCaptureChanges' - Enables ongoing replication (CDC) as a BOOLEAN value. The default is
-- true.
--
-- 'maxKBytesPerRead', 'iBMDb2Settings_maxKBytesPerRead' - Maximum number of bytes per read, as a NUMBER value. The default is 64
-- KB.
--
-- 'databaseName', 'iBMDb2Settings_databaseName' - Database name for the endpoint.
newIBMDb2Settings ::
  IBMDb2Settings
newIBMDb2Settings =
  IBMDb2Settings'
    { currentLsn = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      serverName = Prelude.Nothing,
      password = Prelude.Nothing,
      port = Prelude.Nothing,
      username = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      setDataCaptureChanges = Prelude.Nothing,
      maxKBytesPerRead = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence
-- number (LSN) where you want the replication to start.
iBMDb2Settings_currentLsn :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_currentLsn = Lens.lens (\IBMDb2Settings' {currentLsn} -> currentLsn) (\s@IBMDb2Settings' {} a -> s {currentLsn = a} :: IBMDb2Settings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
-- details.
iBMDb2Settings_secretsManagerSecretId :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_secretsManagerSecretId = Lens.lens (\IBMDb2Settings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@IBMDb2Settings' {} a -> s {secretsManagerSecretId = a} :: IBMDb2Settings)

-- | Fully qualified domain name of the endpoint.
iBMDb2Settings_serverName :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_serverName = Lens.lens (\IBMDb2Settings' {serverName} -> serverName) (\s@IBMDb2Settings' {} a -> s {serverName = a} :: IBMDb2Settings)

-- | Endpoint connection password.
iBMDb2Settings_password :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_password = Lens.lens (\IBMDb2Settings' {password} -> password) (\s@IBMDb2Settings' {} a -> s {password = a} :: IBMDb2Settings) Prelude.. Lens.mapping Prelude._Sensitive

-- | Endpoint TCP port.
iBMDb2Settings_port :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Int)
iBMDb2Settings_port = Lens.lens (\IBMDb2Settings' {port} -> port) (\s@IBMDb2Settings' {} a -> s {port = a} :: IBMDb2Settings)

-- | Endpoint connection user name.
iBMDb2Settings_username :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_username = Lens.lens (\IBMDb2Settings' {username} -> username) (\s@IBMDb2Settings' {} a -> s {username = a} :: IBMDb2Settings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the Db2
-- LUW endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
iBMDb2Settings_secretsManagerAccessRoleArn :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_secretsManagerAccessRoleArn = Lens.lens (\IBMDb2Settings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@IBMDb2Settings' {} a -> s {secretsManagerAccessRoleArn = a} :: IBMDb2Settings)

-- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is
-- true.
iBMDb2Settings_setDataCaptureChanges :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Bool)
iBMDb2Settings_setDataCaptureChanges = Lens.lens (\IBMDb2Settings' {setDataCaptureChanges} -> setDataCaptureChanges) (\s@IBMDb2Settings' {} a -> s {setDataCaptureChanges = a} :: IBMDb2Settings)

-- | Maximum number of bytes per read, as a NUMBER value. The default is 64
-- KB.
iBMDb2Settings_maxKBytesPerRead :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Int)
iBMDb2Settings_maxKBytesPerRead = Lens.lens (\IBMDb2Settings' {maxKBytesPerRead} -> maxKBytesPerRead) (\s@IBMDb2Settings' {} a -> s {maxKBytesPerRead = a} :: IBMDb2Settings)

-- | Database name for the endpoint.
iBMDb2Settings_databaseName :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_databaseName = Lens.lens (\IBMDb2Settings' {databaseName} -> databaseName) (\s@IBMDb2Settings' {} a -> s {databaseName = a} :: IBMDb2Settings)

instance Prelude.FromJSON IBMDb2Settings where
  parseJSON =
    Prelude.withObject
      "IBMDb2Settings"
      ( \x ->
          IBMDb2Settings'
            Prelude.<$> (x Prelude..:? "CurrentLsn")
            Prelude.<*> (x Prelude..:? "SecretsManagerSecretId")
            Prelude.<*> (x Prelude..:? "ServerName")
            Prelude.<*> (x Prelude..:? "Password")
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "Username")
            Prelude.<*> (x Prelude..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Prelude..:? "SetDataCaptureChanges")
            Prelude.<*> (x Prelude..:? "MaxKBytesPerRead")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable IBMDb2Settings

instance Prelude.NFData IBMDb2Settings

instance Prelude.ToJSON IBMDb2Settings where
  toJSON IBMDb2Settings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CurrentLsn" Prelude..=) Prelude.<$> currentLsn,
            ("SecretsManagerSecretId" Prelude..=)
              Prelude.<$> secretsManagerSecretId,
            ("ServerName" Prelude..=) Prelude.<$> serverName,
            ("Password" Prelude..=) Prelude.<$> password,
            ("Port" Prelude..=) Prelude.<$> port,
            ("Username" Prelude..=) Prelude.<$> username,
            ("SecretsManagerAccessRoleArn" Prelude..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("SetDataCaptureChanges" Prelude..=)
              Prelude.<$> setDataCaptureChanges,
            ("MaxKBytesPerRead" Prelude..=)
              Prelude.<$> maxKBytesPerRead,
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )
