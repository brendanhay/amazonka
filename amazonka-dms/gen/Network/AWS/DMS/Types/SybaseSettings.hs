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
-- Module      : Network.AWS.DMS.Types.SybaseSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SybaseSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information that defines a SAP ASE endpoint.
--
-- /See:/ 'newSybaseSettings' smart constructor.
data SybaseSettings = SybaseSettings'
  { -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the SAP SAE endpoint connection
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
    -- value of the AWS Secrets Manager secret that allows access to the SAP
    -- ASE endpoint.
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
    -- | Database name for the endpoint.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'SybaseSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretsManagerSecretId', 'sybaseSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SAP SAE endpoint connection
-- details.
--
-- 'serverName', 'sybaseSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'password', 'sybaseSettings_password' - Endpoint connection password.
--
-- 'port', 'sybaseSettings_port' - Endpoint TCP port.
--
-- 'username', 'sybaseSettings_username' - Endpoint connection user name.
--
-- 'secretsManagerAccessRoleArn', 'sybaseSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the SAP
-- ASE endpoint.
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
-- 'databaseName', 'sybaseSettings_databaseName' - Database name for the endpoint.
newSybaseSettings ::
  SybaseSettings
newSybaseSettings =
  SybaseSettings'
    { secretsManagerSecretId =
        Core.Nothing,
      serverName = Core.Nothing,
      password = Core.Nothing,
      port = Core.Nothing,
      username = Core.Nothing,
      secretsManagerAccessRoleArn = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SAP SAE endpoint connection
-- details.
sybaseSettings_secretsManagerSecretId :: Lens.Lens' SybaseSettings (Core.Maybe Core.Text)
sybaseSettings_secretsManagerSecretId = Lens.lens (\SybaseSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@SybaseSettings' {} a -> s {secretsManagerSecretId = a} :: SybaseSettings)

-- | Fully qualified domain name of the endpoint.
sybaseSettings_serverName :: Lens.Lens' SybaseSettings (Core.Maybe Core.Text)
sybaseSettings_serverName = Lens.lens (\SybaseSettings' {serverName} -> serverName) (\s@SybaseSettings' {} a -> s {serverName = a} :: SybaseSettings)

-- | Endpoint connection password.
sybaseSettings_password :: Lens.Lens' SybaseSettings (Core.Maybe Core.Text)
sybaseSettings_password = Lens.lens (\SybaseSettings' {password} -> password) (\s@SybaseSettings' {} a -> s {password = a} :: SybaseSettings) Core.. Lens.mapping Core._Sensitive

-- | Endpoint TCP port.
sybaseSettings_port :: Lens.Lens' SybaseSettings (Core.Maybe Core.Int)
sybaseSettings_port = Lens.lens (\SybaseSettings' {port} -> port) (\s@SybaseSettings' {} a -> s {port = a} :: SybaseSettings)

-- | Endpoint connection user name.
sybaseSettings_username :: Lens.Lens' SybaseSettings (Core.Maybe Core.Text)
sybaseSettings_username = Lens.lens (\SybaseSettings' {username} -> username) (\s@SybaseSettings' {} a -> s {username = a} :: SybaseSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the SAP
-- ASE endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
sybaseSettings_secretsManagerAccessRoleArn :: Lens.Lens' SybaseSettings (Core.Maybe Core.Text)
sybaseSettings_secretsManagerAccessRoleArn = Lens.lens (\SybaseSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@SybaseSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: SybaseSettings)

-- | Database name for the endpoint.
sybaseSettings_databaseName :: Lens.Lens' SybaseSettings (Core.Maybe Core.Text)
sybaseSettings_databaseName = Lens.lens (\SybaseSettings' {databaseName} -> databaseName) (\s@SybaseSettings' {} a -> s {databaseName = a} :: SybaseSettings)

instance Core.FromJSON SybaseSettings where
  parseJSON =
    Core.withObject
      "SybaseSettings"
      ( \x ->
          SybaseSettings'
            Core.<$> (x Core..:? "SecretsManagerSecretId")
            Core.<*> (x Core..:? "ServerName")
            Core.<*> (x Core..:? "Password")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable SybaseSettings

instance Core.NFData SybaseSettings

instance Core.ToJSON SybaseSettings where
  toJSON SybaseSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecretsManagerSecretId" Core..=)
              Core.<$> secretsManagerSecretId,
            ("ServerName" Core..=) Core.<$> serverName,
            ("Password" Core..=) Core.<$> password,
            ("Port" Core..=) Core.<$> port,
            ("Username" Core..=) Core.<$> username,
            ("SecretsManagerAccessRoleArn" Core..=)
              Core.<$> secretsManagerAccessRoleArn,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )
