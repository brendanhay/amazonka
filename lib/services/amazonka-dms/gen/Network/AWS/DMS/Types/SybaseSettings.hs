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
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines a SAP ASE endpoint.
--
-- /See:/ 'newSybaseSettings' smart constructor.
data SybaseSettings = SybaseSettings'
  { -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the SAP ASE endpoint.
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
    -- @SecretsManagerSecret@ that contains the SAP SAE endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SybaseSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverName', 'sybaseSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'secretsManagerAccessRoleArn', 'sybaseSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the SAP ASE endpoint.
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
-- 'username', 'sybaseSettings_username' - Endpoint connection user name.
--
-- 'password', 'sybaseSettings_password' - Endpoint connection password.
--
-- 'databaseName', 'sybaseSettings_databaseName' - Database name for the endpoint.
--
-- 'secretsManagerSecretId', 'sybaseSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SAP SAE endpoint connection
-- details.
--
-- 'port', 'sybaseSettings_port' - Endpoint TCP port.
newSybaseSettings ::
  SybaseSettings
newSybaseSettings =
  SybaseSettings'
    { serverName = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      username = Prelude.Nothing,
      password = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | Fully qualified domain name of the endpoint.
sybaseSettings_serverName :: Lens.Lens' SybaseSettings (Prelude.Maybe Prelude.Text)
sybaseSettings_serverName = Lens.lens (\SybaseSettings' {serverName} -> serverName) (\s@SybaseSettings' {} a -> s {serverName = a} :: SybaseSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the SAP ASE endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
sybaseSettings_secretsManagerAccessRoleArn :: Lens.Lens' SybaseSettings (Prelude.Maybe Prelude.Text)
sybaseSettings_secretsManagerAccessRoleArn = Lens.lens (\SybaseSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@SybaseSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: SybaseSettings)

-- | Endpoint connection user name.
sybaseSettings_username :: Lens.Lens' SybaseSettings (Prelude.Maybe Prelude.Text)
sybaseSettings_username = Lens.lens (\SybaseSettings' {username} -> username) (\s@SybaseSettings' {} a -> s {username = a} :: SybaseSettings)

-- | Endpoint connection password.
sybaseSettings_password :: Lens.Lens' SybaseSettings (Prelude.Maybe Prelude.Text)
sybaseSettings_password = Lens.lens (\SybaseSettings' {password} -> password) (\s@SybaseSettings' {} a -> s {password = a} :: SybaseSettings) Prelude.. Lens.mapping Core._Sensitive

-- | Database name for the endpoint.
sybaseSettings_databaseName :: Lens.Lens' SybaseSettings (Prelude.Maybe Prelude.Text)
sybaseSettings_databaseName = Lens.lens (\SybaseSettings' {databaseName} -> databaseName) (\s@SybaseSettings' {} a -> s {databaseName = a} :: SybaseSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the SAP SAE endpoint connection
-- details.
sybaseSettings_secretsManagerSecretId :: Lens.Lens' SybaseSettings (Prelude.Maybe Prelude.Text)
sybaseSettings_secretsManagerSecretId = Lens.lens (\SybaseSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@SybaseSettings' {} a -> s {secretsManagerSecretId = a} :: SybaseSettings)

-- | Endpoint TCP port.
sybaseSettings_port :: Lens.Lens' SybaseSettings (Prelude.Maybe Prelude.Int)
sybaseSettings_port = Lens.lens (\SybaseSettings' {port} -> port) (\s@SybaseSettings' {} a -> s {port = a} :: SybaseSettings)

instance Core.FromJSON SybaseSettings where
  parseJSON =
    Core.withObject
      "SybaseSettings"
      ( \x ->
          SybaseSettings'
            Prelude.<$> (x Core..:? "ServerName")
            Prelude.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "Password")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "SecretsManagerSecretId")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable SybaseSettings

instance Prelude.NFData SybaseSettings

instance Core.ToJSON SybaseSettings where
  toJSON SybaseSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServerName" Core..=) Prelude.<$> serverName,
            ("SecretsManagerAccessRoleArn" Core..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("Username" Core..=) Prelude.<$> username,
            ("Password" Core..=) Prelude.<$> password,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("SecretsManagerSecretId" Core..=)
              Prelude.<$> secretsManagerSecretId,
            ("Port" Core..=) Prelude.<$> port
          ]
      )
