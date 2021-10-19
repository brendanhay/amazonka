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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines an IBM Db2 LUW endpoint.
--
-- /See:/ 'newIBMDb2Settings' smart constructor.
data IBMDb2Settings = IBMDb2Settings'
  { -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence
    -- number (LSN) where you want the replication to start.
    currentLsn :: Prelude.Maybe Prelude.Text,
    -- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is
    -- true.
    setDataCaptureChanges :: Prelude.Maybe Prelude.Bool,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the Db2 LUW endpoint.
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
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of bytes per read, as a NUMBER value. The default is 64
    -- KB.
    maxKBytesPerRead :: Prelude.Maybe Prelude.Int,
    -- | Endpoint TCP port. The default value is 50000.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IBMDb2Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverName', 'iBMDb2Settings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'currentLsn', 'iBMDb2Settings_currentLsn' - For ongoing replication (CDC), use CurrentLSN to specify a log sequence
-- number (LSN) where you want the replication to start.
--
-- 'setDataCaptureChanges', 'iBMDb2Settings_setDataCaptureChanges' - Enables ongoing replication (CDC) as a BOOLEAN value. The default is
-- true.
--
-- 'secretsManagerAccessRoleArn', 'iBMDb2Settings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the Db2 LUW endpoint.
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
-- 'username', 'iBMDb2Settings_username' - Endpoint connection user name.
--
-- 'password', 'iBMDb2Settings_password' - Endpoint connection password.
--
-- 'databaseName', 'iBMDb2Settings_databaseName' - Database name for the endpoint.
--
-- 'secretsManagerSecretId', 'iBMDb2Settings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
-- details.
--
-- 'maxKBytesPerRead', 'iBMDb2Settings_maxKBytesPerRead' - Maximum number of bytes per read, as a NUMBER value. The default is 64
-- KB.
--
-- 'port', 'iBMDb2Settings_port' - Endpoint TCP port. The default value is 50000.
newIBMDb2Settings ::
  IBMDb2Settings
newIBMDb2Settings =
  IBMDb2Settings'
    { serverName = Prelude.Nothing,
      currentLsn = Prelude.Nothing,
      setDataCaptureChanges = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      username = Prelude.Nothing,
      password = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      maxKBytesPerRead = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | Fully qualified domain name of the endpoint.
iBMDb2Settings_serverName :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_serverName = Lens.lens (\IBMDb2Settings' {serverName} -> serverName) (\s@IBMDb2Settings' {} a -> s {serverName = a} :: IBMDb2Settings)

-- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence
-- number (LSN) where you want the replication to start.
iBMDb2Settings_currentLsn :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_currentLsn = Lens.lens (\IBMDb2Settings' {currentLsn} -> currentLsn) (\s@IBMDb2Settings' {} a -> s {currentLsn = a} :: IBMDb2Settings)

-- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is
-- true.
iBMDb2Settings_setDataCaptureChanges :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Bool)
iBMDb2Settings_setDataCaptureChanges = Lens.lens (\IBMDb2Settings' {setDataCaptureChanges} -> setDataCaptureChanges) (\s@IBMDb2Settings' {} a -> s {setDataCaptureChanges = a} :: IBMDb2Settings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the Db2 LUW endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
iBMDb2Settings_secretsManagerAccessRoleArn :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_secretsManagerAccessRoleArn = Lens.lens (\IBMDb2Settings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@IBMDb2Settings' {} a -> s {secretsManagerAccessRoleArn = a} :: IBMDb2Settings)

-- | Endpoint connection user name.
iBMDb2Settings_username :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_username = Lens.lens (\IBMDb2Settings' {username} -> username) (\s@IBMDb2Settings' {} a -> s {username = a} :: IBMDb2Settings)

-- | Endpoint connection password.
iBMDb2Settings_password :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_password = Lens.lens (\IBMDb2Settings' {password} -> password) (\s@IBMDb2Settings' {} a -> s {password = a} :: IBMDb2Settings) Prelude.. Lens.mapping Core._Sensitive

-- | Database name for the endpoint.
iBMDb2Settings_databaseName :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_databaseName = Lens.lens (\IBMDb2Settings' {databaseName} -> databaseName) (\s@IBMDb2Settings' {} a -> s {databaseName = a} :: IBMDb2Settings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
-- details.
iBMDb2Settings_secretsManagerSecretId :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_secretsManagerSecretId = Lens.lens (\IBMDb2Settings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@IBMDb2Settings' {} a -> s {secretsManagerSecretId = a} :: IBMDb2Settings)

-- | Maximum number of bytes per read, as a NUMBER value. The default is 64
-- KB.
iBMDb2Settings_maxKBytesPerRead :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Int)
iBMDb2Settings_maxKBytesPerRead = Lens.lens (\IBMDb2Settings' {maxKBytesPerRead} -> maxKBytesPerRead) (\s@IBMDb2Settings' {} a -> s {maxKBytesPerRead = a} :: IBMDb2Settings)

-- | Endpoint TCP port. The default value is 50000.
iBMDb2Settings_port :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Int)
iBMDb2Settings_port = Lens.lens (\IBMDb2Settings' {port} -> port) (\s@IBMDb2Settings' {} a -> s {port = a} :: IBMDb2Settings)

instance Core.FromJSON IBMDb2Settings where
  parseJSON =
    Core.withObject
      "IBMDb2Settings"
      ( \x ->
          IBMDb2Settings'
            Prelude.<$> (x Core..:? "ServerName")
            Prelude.<*> (x Core..:? "CurrentLsn")
            Prelude.<*> (x Core..:? "SetDataCaptureChanges")
            Prelude.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "Password")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "SecretsManagerSecretId")
            Prelude.<*> (x Core..:? "MaxKBytesPerRead")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable IBMDb2Settings

instance Prelude.NFData IBMDb2Settings

instance Core.ToJSON IBMDb2Settings where
  toJSON IBMDb2Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServerName" Core..=) Prelude.<$> serverName,
            ("CurrentLsn" Core..=) Prelude.<$> currentLsn,
            ("SetDataCaptureChanges" Core..=)
              Prelude.<$> setDataCaptureChanges,
            ("SecretsManagerAccessRoleArn" Core..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("Username" Core..=) Prelude.<$> username,
            ("Password" Core..=) Prelude.<$> password,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("SecretsManagerSecretId" Core..=)
              Prelude.<$> secretsManagerSecretId,
            ("MaxKBytesPerRead" Core..=)
              Prelude.<$> maxKBytesPerRead,
            ("Port" Core..=) Prelude.<$> port
          ]
      )
