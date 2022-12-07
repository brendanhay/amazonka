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
-- Module      : Amazonka.DMS.Types.IBMDb2Settings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.IBMDb2Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines an IBM Db2 LUW endpoint.
--
-- /See:/ 'newIBMDb2Settings' smart constructor.
data IBMDb2Settings = IBMDb2Settings'
  { -- | Endpoint TCP port. The default value is 50000.
    port :: Prelude.Maybe Prelude.Int,
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
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is
    -- true.
    setDataCaptureChanges :: Prelude.Maybe Prelude.Bool,
    -- | Maximum number of bytes per read, as a NUMBER value. The default is 64
    -- KB.
    maxKBytesPerRead :: Prelude.Maybe Prelude.Int,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence
    -- number (LSN) where you want the replication to start.
    currentLsn :: Prelude.Maybe Prelude.Text
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
-- 'port', 'iBMDb2Settings_port' - Endpoint TCP port. The default value is 50000.
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
-- 'password', 'iBMDb2Settings_password' - Endpoint connection password.
--
-- 'serverName', 'iBMDb2Settings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'databaseName', 'iBMDb2Settings_databaseName' - Database name for the endpoint.
--
-- 'username', 'iBMDb2Settings_username' - Endpoint connection user name.
--
-- 'setDataCaptureChanges', 'iBMDb2Settings_setDataCaptureChanges' - Enables ongoing replication (CDC) as a BOOLEAN value. The default is
-- true.
--
-- 'maxKBytesPerRead', 'iBMDb2Settings_maxKBytesPerRead' - Maximum number of bytes per read, as a NUMBER value. The default is 64
-- KB.
--
-- 'secretsManagerSecretId', 'iBMDb2Settings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
-- details.
--
-- 'currentLsn', 'iBMDb2Settings_currentLsn' - For ongoing replication (CDC), use CurrentLSN to specify a log sequence
-- number (LSN) where you want the replication to start.
newIBMDb2Settings ::
  IBMDb2Settings
newIBMDb2Settings =
  IBMDb2Settings'
    { port = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      password = Prelude.Nothing,
      serverName = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      username = Prelude.Nothing,
      setDataCaptureChanges = Prelude.Nothing,
      maxKBytesPerRead = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      currentLsn = Prelude.Nothing
    }

-- | Endpoint TCP port. The default value is 50000.
iBMDb2Settings_port :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Int)
iBMDb2Settings_port = Lens.lens (\IBMDb2Settings' {port} -> port) (\s@IBMDb2Settings' {} a -> s {port = a} :: IBMDb2Settings)

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

-- | Endpoint connection password.
iBMDb2Settings_password :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_password = Lens.lens (\IBMDb2Settings' {password} -> password) (\s@IBMDb2Settings' {} a -> s {password = a} :: IBMDb2Settings) Prelude.. Lens.mapping Data._Sensitive

-- | Fully qualified domain name of the endpoint.
iBMDb2Settings_serverName :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_serverName = Lens.lens (\IBMDb2Settings' {serverName} -> serverName) (\s@IBMDb2Settings' {} a -> s {serverName = a} :: IBMDb2Settings)

-- | Database name for the endpoint.
iBMDb2Settings_databaseName :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_databaseName = Lens.lens (\IBMDb2Settings' {databaseName} -> databaseName) (\s@IBMDb2Settings' {} a -> s {databaseName = a} :: IBMDb2Settings)

-- | Endpoint connection user name.
iBMDb2Settings_username :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_username = Lens.lens (\IBMDb2Settings' {username} -> username) (\s@IBMDb2Settings' {} a -> s {username = a} :: IBMDb2Settings)

-- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is
-- true.
iBMDb2Settings_setDataCaptureChanges :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Bool)
iBMDb2Settings_setDataCaptureChanges = Lens.lens (\IBMDb2Settings' {setDataCaptureChanges} -> setDataCaptureChanges) (\s@IBMDb2Settings' {} a -> s {setDataCaptureChanges = a} :: IBMDb2Settings)

-- | Maximum number of bytes per read, as a NUMBER value. The default is 64
-- KB.
iBMDb2Settings_maxKBytesPerRead :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Int)
iBMDb2Settings_maxKBytesPerRead = Lens.lens (\IBMDb2Settings' {maxKBytesPerRead} -> maxKBytesPerRead) (\s@IBMDb2Settings' {} a -> s {maxKBytesPerRead = a} :: IBMDb2Settings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Db2 LUW endpoint connection
-- details.
iBMDb2Settings_secretsManagerSecretId :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_secretsManagerSecretId = Lens.lens (\IBMDb2Settings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@IBMDb2Settings' {} a -> s {secretsManagerSecretId = a} :: IBMDb2Settings)

-- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence
-- number (LSN) where you want the replication to start.
iBMDb2Settings_currentLsn :: Lens.Lens' IBMDb2Settings (Prelude.Maybe Prelude.Text)
iBMDb2Settings_currentLsn = Lens.lens (\IBMDb2Settings' {currentLsn} -> currentLsn) (\s@IBMDb2Settings' {} a -> s {currentLsn = a} :: IBMDb2Settings)

instance Data.FromJSON IBMDb2Settings where
  parseJSON =
    Data.withObject
      "IBMDb2Settings"
      ( \x ->
          IBMDb2Settings'
            Prelude.<$> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Data..:? "Password")
            Prelude.<*> (x Data..:? "ServerName")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Username")
            Prelude.<*> (x Data..:? "SetDataCaptureChanges")
            Prelude.<*> (x Data..:? "MaxKBytesPerRead")
            Prelude.<*> (x Data..:? "SecretsManagerSecretId")
            Prelude.<*> (x Data..:? "CurrentLsn")
      )

instance Prelude.Hashable IBMDb2Settings where
  hashWithSalt _salt IBMDb2Settings' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` secretsManagerAccessRoleArn
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` setDataCaptureChanges
      `Prelude.hashWithSalt` maxKBytesPerRead
      `Prelude.hashWithSalt` secretsManagerSecretId
      `Prelude.hashWithSalt` currentLsn

instance Prelude.NFData IBMDb2Settings where
  rnf IBMDb2Settings' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf secretsManagerAccessRoleArn
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf setDataCaptureChanges
      `Prelude.seq` Prelude.rnf maxKBytesPerRead
      `Prelude.seq` Prelude.rnf secretsManagerSecretId
      `Prelude.seq` Prelude.rnf currentLsn

instance Data.ToJSON IBMDb2Settings where
  toJSON IBMDb2Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Port" Data..=) Prelude.<$> port,
            ("SecretsManagerAccessRoleArn" Data..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("Password" Data..=) Prelude.<$> password,
            ("ServerName" Data..=) Prelude.<$> serverName,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("Username" Data..=) Prelude.<$> username,
            ("SetDataCaptureChanges" Data..=)
              Prelude.<$> setDataCaptureChanges,
            ("MaxKBytesPerRead" Data..=)
              Prelude.<$> maxKBytesPerRead,
            ("SecretsManagerSecretId" Data..=)
              Prelude.<$> secretsManagerSecretId,
            ("CurrentLsn" Data..=) Prelude.<$> currentLsn
          ]
      )
