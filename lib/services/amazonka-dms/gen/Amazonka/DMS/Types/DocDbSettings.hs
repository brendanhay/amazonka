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
-- Module      : Amazonka.DMS.Types.DocDbSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DocDbSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.NestingLevelValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines a DocumentDB endpoint.
--
-- /See:/ 'newDocDbSettings' smart constructor.
data DocDbSettings = DocDbSettings'
  { -- | The database name on the DocumentDB source endpoint.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the number of documents to preview to determine the document
    -- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
    --
    -- Must be a positive value greater than @0@. Default value is @1000@.
    docsToInvestigate :: Prelude.Maybe Prelude.Int,
    -- | Specifies the document ID. Use this setting when @NestingLevel@ is set
    -- to @\"none\"@.
    --
    -- Default value is @\"false\"@.
    extractDocId :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key identifier that is used to encrypt the content on the
    -- replication instance. If you don\'t specify a value for the @KmsKeyId@
    -- parameter, then DMS uses your default encryption key. KMS creates the
    -- default encryption key for your Amazon Web Services account. Your Amazon
    -- Web Services account has a different default encryption key for each
    -- Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies either document or table mode.
    --
    -- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
    -- Specify @\"one\"@ to use table mode.
    nestingLevel :: Prelude.Maybe NestingLevelValue,
    -- | The password for the user account you use to access the DocumentDB
    -- source endpoint.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The port value for the DocumentDB source endpoint.
    port :: Prelude.Maybe Prelude.Int,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the DocumentDB endpoint.
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
    -- @SecretsManagerSecret@ that contains the DocumentDB endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | The name of the server on the DocumentDB source endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The user name you use to access the DocumentDB source endpoint.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocDbSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'docDbSettings_databaseName' - The database name on the DocumentDB source endpoint.
--
-- 'docsToInvestigate', 'docDbSettings_docsToInvestigate' - Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
--
-- 'extractDocId', 'docDbSettings_extractDocId' - Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
--
-- 'kmsKeyId', 'docDbSettings_kmsKeyId' - The KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then DMS uses your default encryption key. KMS creates the
-- default encryption key for your Amazon Web Services account. Your Amazon
-- Web Services account has a different default encryption key for each
-- Amazon Web Services Region.
--
-- 'nestingLevel', 'docDbSettings_nestingLevel' - Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
--
-- 'password', 'docDbSettings_password' - The password for the user account you use to access the DocumentDB
-- source endpoint.
--
-- 'port', 'docDbSettings_port' - The port value for the DocumentDB source endpoint.
--
-- 'secretsManagerAccessRoleArn', 'docDbSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the DocumentDB endpoint.
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
-- 'secretsManagerSecretId', 'docDbSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the DocumentDB endpoint connection
-- details.
--
-- 'serverName', 'docDbSettings_serverName' - The name of the server on the DocumentDB source endpoint.
--
-- 'username', 'docDbSettings_username' - The user name you use to access the DocumentDB source endpoint.
newDocDbSettings ::
  DocDbSettings
newDocDbSettings =
  DocDbSettings'
    { databaseName = Prelude.Nothing,
      docsToInvestigate = Prelude.Nothing,
      extractDocId = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      nestingLevel = Prelude.Nothing,
      password = Prelude.Nothing,
      port = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      serverName = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The database name on the DocumentDB source endpoint.
docDbSettings_databaseName :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_databaseName = Lens.lens (\DocDbSettings' {databaseName} -> databaseName) (\s@DocDbSettings' {} a -> s {databaseName = a} :: DocDbSettings)

-- | Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
docDbSettings_docsToInvestigate :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Int)
docDbSettings_docsToInvestigate = Lens.lens (\DocDbSettings' {docsToInvestigate} -> docsToInvestigate) (\s@DocDbSettings' {} a -> s {docsToInvestigate = a} :: DocDbSettings)

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
docDbSettings_extractDocId :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Bool)
docDbSettings_extractDocId = Lens.lens (\DocDbSettings' {extractDocId} -> extractDocId) (\s@DocDbSettings' {} a -> s {extractDocId = a} :: DocDbSettings)

-- | The KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then DMS uses your default encryption key. KMS creates the
-- default encryption key for your Amazon Web Services account. Your Amazon
-- Web Services account has a different default encryption key for each
-- Amazon Web Services Region.
docDbSettings_kmsKeyId :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_kmsKeyId = Lens.lens (\DocDbSettings' {kmsKeyId} -> kmsKeyId) (\s@DocDbSettings' {} a -> s {kmsKeyId = a} :: DocDbSettings)

-- | Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
docDbSettings_nestingLevel :: Lens.Lens' DocDbSettings (Prelude.Maybe NestingLevelValue)
docDbSettings_nestingLevel = Lens.lens (\DocDbSettings' {nestingLevel} -> nestingLevel) (\s@DocDbSettings' {} a -> s {nestingLevel = a} :: DocDbSettings)

-- | The password for the user account you use to access the DocumentDB
-- source endpoint.
docDbSettings_password :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_password = Lens.lens (\DocDbSettings' {password} -> password) (\s@DocDbSettings' {} a -> s {password = a} :: DocDbSettings) Prelude.. Lens.mapping Data._Sensitive

-- | The port value for the DocumentDB source endpoint.
docDbSettings_port :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Int)
docDbSettings_port = Lens.lens (\DocDbSettings' {port} -> port) (\s@DocDbSettings' {} a -> s {port = a} :: DocDbSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the DocumentDB endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
docDbSettings_secretsManagerAccessRoleArn :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_secretsManagerAccessRoleArn = Lens.lens (\DocDbSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@DocDbSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: DocDbSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the DocumentDB endpoint connection
-- details.
docDbSettings_secretsManagerSecretId :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_secretsManagerSecretId = Lens.lens (\DocDbSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@DocDbSettings' {} a -> s {secretsManagerSecretId = a} :: DocDbSettings)

-- | The name of the server on the DocumentDB source endpoint.
docDbSettings_serverName :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_serverName = Lens.lens (\DocDbSettings' {serverName} -> serverName) (\s@DocDbSettings' {} a -> s {serverName = a} :: DocDbSettings)

-- | The user name you use to access the DocumentDB source endpoint.
docDbSettings_username :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_username = Lens.lens (\DocDbSettings' {username} -> username) (\s@DocDbSettings' {} a -> s {username = a} :: DocDbSettings)

instance Data.FromJSON DocDbSettings where
  parseJSON =
    Data.withObject
      "DocDbSettings"
      ( \x ->
          DocDbSettings'
            Prelude.<$> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "DocsToInvestigate")
            Prelude.<*> (x Data..:? "ExtractDocId")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "NestingLevel")
            Prelude.<*> (x Data..:? "Password")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Data..:? "SecretsManagerSecretId")
            Prelude.<*> (x Data..:? "ServerName")
            Prelude.<*> (x Data..:? "Username")
      )

instance Prelude.Hashable DocDbSettings where
  hashWithSalt _salt DocDbSettings' {..} =
    _salt `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` docsToInvestigate
      `Prelude.hashWithSalt` extractDocId
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` nestingLevel
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` secretsManagerAccessRoleArn
      `Prelude.hashWithSalt` secretsManagerSecretId
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` username

instance Prelude.NFData DocDbSettings where
  rnf DocDbSettings' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf docsToInvestigate
      `Prelude.seq` Prelude.rnf extractDocId
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf nestingLevel
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf secretsManagerAccessRoleArn
      `Prelude.seq` Prelude.rnf secretsManagerSecretId
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf username

instance Data.ToJSON DocDbSettings where
  toJSON DocDbSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("DocsToInvestigate" Data..=)
              Prelude.<$> docsToInvestigate,
            ("ExtractDocId" Data..=) Prelude.<$> extractDocId,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("NestingLevel" Data..=) Prelude.<$> nestingLevel,
            ("Password" Data..=) Prelude.<$> password,
            ("Port" Data..=) Prelude.<$> port,
            ("SecretsManagerAccessRoleArn" Data..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("SecretsManagerSecretId" Data..=)
              Prelude.<$> secretsManagerSecretId,
            ("ServerName" Data..=) Prelude.<$> serverName,
            ("Username" Data..=) Prelude.<$> username
          ]
      )
