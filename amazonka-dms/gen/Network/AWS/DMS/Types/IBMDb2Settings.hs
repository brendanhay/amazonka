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

-- | Provides information that defines an IBM Db2 LUW endpoint.
--
-- /See:/ 'newIBMDb2Settings' smart constructor.
data IBMDb2Settings = IBMDb2Settings'
  { -- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence
    -- number (LSN) where you want the replication to start.
    currentLsn :: Core.Maybe Core.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
    -- details.
    secretsManagerSecretId :: Core.Maybe Core.Text,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Core.Maybe Core.Text,
    -- | Endpoint connection password.
    password :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Endpoint TCP port.
    port :: Core.Maybe Core.Int,
    -- | Endpoint connection user name.
    username :: Core.Maybe Core.Text,
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
    secretsManagerAccessRoleArn :: Core.Maybe Core.Text,
    -- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is
    -- true.
    setDataCaptureChanges :: Core.Maybe Core.Bool,
    -- | Maximum number of bytes per read, as a NUMBER value. The default is 64
    -- KB.
    maxKBytesPerRead :: Core.Maybe Core.Int,
    -- | Database name for the endpoint.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
    { currentLsn = Core.Nothing,
      secretsManagerSecretId = Core.Nothing,
      serverName = Core.Nothing,
      password = Core.Nothing,
      port = Core.Nothing,
      username = Core.Nothing,
      secretsManagerAccessRoleArn = Core.Nothing,
      setDataCaptureChanges = Core.Nothing,
      maxKBytesPerRead = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence
-- number (LSN) where you want the replication to start.
iBMDb2Settings_currentLsn :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
iBMDb2Settings_currentLsn = Lens.lens (\IBMDb2Settings' {currentLsn} -> currentLsn) (\s@IBMDb2Settings' {} a -> s {currentLsn = a} :: IBMDb2Settings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
-- details.
iBMDb2Settings_secretsManagerSecretId :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
iBMDb2Settings_secretsManagerSecretId = Lens.lens (\IBMDb2Settings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@IBMDb2Settings' {} a -> s {secretsManagerSecretId = a} :: IBMDb2Settings)

-- | Fully qualified domain name of the endpoint.
iBMDb2Settings_serverName :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
iBMDb2Settings_serverName = Lens.lens (\IBMDb2Settings' {serverName} -> serverName) (\s@IBMDb2Settings' {} a -> s {serverName = a} :: IBMDb2Settings)

-- | Endpoint connection password.
iBMDb2Settings_password :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
iBMDb2Settings_password = Lens.lens (\IBMDb2Settings' {password} -> password) (\s@IBMDb2Settings' {} a -> s {password = a} :: IBMDb2Settings) Core.. Lens.mapping Core._Sensitive

-- | Endpoint TCP port.
iBMDb2Settings_port :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Int)
iBMDb2Settings_port = Lens.lens (\IBMDb2Settings' {port} -> port) (\s@IBMDb2Settings' {} a -> s {port = a} :: IBMDb2Settings)

-- | Endpoint connection user name.
iBMDb2Settings_username :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
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
iBMDb2Settings_secretsManagerAccessRoleArn :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
iBMDb2Settings_secretsManagerAccessRoleArn = Lens.lens (\IBMDb2Settings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@IBMDb2Settings' {} a -> s {secretsManagerAccessRoleArn = a} :: IBMDb2Settings)

-- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is
-- true.
iBMDb2Settings_setDataCaptureChanges :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Bool)
iBMDb2Settings_setDataCaptureChanges = Lens.lens (\IBMDb2Settings' {setDataCaptureChanges} -> setDataCaptureChanges) (\s@IBMDb2Settings' {} a -> s {setDataCaptureChanges = a} :: IBMDb2Settings)

-- | Maximum number of bytes per read, as a NUMBER value. The default is 64
-- KB.
iBMDb2Settings_maxKBytesPerRead :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Int)
iBMDb2Settings_maxKBytesPerRead = Lens.lens (\IBMDb2Settings' {maxKBytesPerRead} -> maxKBytesPerRead) (\s@IBMDb2Settings' {} a -> s {maxKBytesPerRead = a} :: IBMDb2Settings)

-- | Database name for the endpoint.
iBMDb2Settings_databaseName :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
iBMDb2Settings_databaseName = Lens.lens (\IBMDb2Settings' {databaseName} -> databaseName) (\s@IBMDb2Settings' {} a -> s {databaseName = a} :: IBMDb2Settings)

instance Core.FromJSON IBMDb2Settings where
  parseJSON =
    Core.withObject
      "IBMDb2Settings"
      ( \x ->
          IBMDb2Settings'
            Core.<$> (x Core..:? "CurrentLsn")
            Core.<*> (x Core..:? "SecretsManagerSecretId")
            Core.<*> (x Core..:? "ServerName")
            Core.<*> (x Core..:? "Password")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Core.<*> (x Core..:? "SetDataCaptureChanges")
            Core.<*> (x Core..:? "MaxKBytesPerRead")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable IBMDb2Settings

instance Core.NFData IBMDb2Settings

instance Core.ToJSON IBMDb2Settings where
  toJSON IBMDb2Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CurrentLsn" Core..=) Core.<$> currentLsn,
            ("SecretsManagerSecretId" Core..=)
              Core.<$> secretsManagerSecretId,
            ("ServerName" Core..=) Core.<$> serverName,
            ("Password" Core..=) Core.<$> password,
            ("Port" Core..=) Core.<$> port,
            ("Username" Core..=) Core.<$> username,
            ("SecretsManagerAccessRoleArn" Core..=)
              Core.<$> secretsManagerAccessRoleArn,
            ("SetDataCaptureChanges" Core..=)
              Core.<$> setDataCaptureChanges,
            ("MaxKBytesPerRead" Core..=)
              Core.<$> maxKBytesPerRead,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )
