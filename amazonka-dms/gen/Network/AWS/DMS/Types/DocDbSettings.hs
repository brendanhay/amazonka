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
-- Module      : Network.AWS.DMS.Types.DocDbSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DocDbSettings where

import Network.AWS.DMS.Types.NestingLevelValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines a DocumentDB endpoint.
--
-- /See:/ 'newDocDbSettings' smart constructor.
data DocDbSettings = DocDbSettings'
  { -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the DocumentDB endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | The name of the server on the DocumentDB source endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The AWS KMS key identifier that is used to encrypt the content on the
    -- replication instance. If you don\'t specify a value for the @KmsKeyId@
    -- parameter, then AWS DMS uses your default encryption key. AWS KMS
    -- creates the default encryption key for your AWS account. Your AWS
    -- account has a different default encryption key for each AWS Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The password for the user account you use to access the DocumentDB
    -- source endpoint.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The port value for the DocumentDB source endpoint.
    port :: Prelude.Maybe Prelude.Int,
    -- | The user name you use to access the DocumentDB source endpoint.
    username :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
    -- DMS as the trusted entity and grants the required permissions to access
    -- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
    -- value of the AWS Secrets Manager secret that allows access to the
    -- DocumentDB endpoint.
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
    -- | Specifies the document ID. Use this setting when @NestingLevel@ is set
    -- to @\"none\"@.
    --
    -- Default value is @\"false\"@.
    extractDocId :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the number of documents to preview to determine the document
    -- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
    --
    -- Must be a positive value greater than @0@. Default value is @1000@.
    docsToInvestigate :: Prelude.Maybe Prelude.Int,
    -- | Specifies either document or table mode.
    --
    -- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
    -- Specify @\"one\"@ to use table mode.
    nestingLevel :: Prelude.Maybe NestingLevelValue,
    -- | The database name on the DocumentDB source endpoint.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DocDbSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretsManagerSecretId', 'docDbSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the DocumentDB endpoint connection
-- details.
--
-- 'serverName', 'docDbSettings_serverName' - The name of the server on the DocumentDB source endpoint.
--
-- 'kmsKeyId', 'docDbSettings_kmsKeyId' - The AWS KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then AWS DMS uses your default encryption key. AWS KMS
-- creates the default encryption key for your AWS account. Your AWS
-- account has a different default encryption key for each AWS Region.
--
-- 'password', 'docDbSettings_password' - The password for the user account you use to access the DocumentDB
-- source endpoint.
--
-- 'port', 'docDbSettings_port' - The port value for the DocumentDB source endpoint.
--
-- 'username', 'docDbSettings_username' - The user name you use to access the DocumentDB source endpoint.
--
-- 'secretsManagerAccessRoleArn', 'docDbSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the
-- DocumentDB endpoint.
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
-- 'extractDocId', 'docDbSettings_extractDocId' - Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
--
-- 'docsToInvestigate', 'docDbSettings_docsToInvestigate' - Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
--
-- 'nestingLevel', 'docDbSettings_nestingLevel' - Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
--
-- 'databaseName', 'docDbSettings_databaseName' - The database name on the DocumentDB source endpoint.
newDocDbSettings ::
  DocDbSettings
newDocDbSettings =
  DocDbSettings'
    { secretsManagerSecretId =
        Prelude.Nothing,
      serverName = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      password = Prelude.Nothing,
      port = Prelude.Nothing,
      username = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      extractDocId = Prelude.Nothing,
      docsToInvestigate = Prelude.Nothing,
      nestingLevel = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the DocumentDB endpoint connection
-- details.
docDbSettings_secretsManagerSecretId :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_secretsManagerSecretId = Lens.lens (\DocDbSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@DocDbSettings' {} a -> s {secretsManagerSecretId = a} :: DocDbSettings)

-- | The name of the server on the DocumentDB source endpoint.
docDbSettings_serverName :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_serverName = Lens.lens (\DocDbSettings' {serverName} -> serverName) (\s@DocDbSettings' {} a -> s {serverName = a} :: DocDbSettings)

-- | The AWS KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then AWS DMS uses your default encryption key. AWS KMS
-- creates the default encryption key for your AWS account. Your AWS
-- account has a different default encryption key for each AWS Region.
docDbSettings_kmsKeyId :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_kmsKeyId = Lens.lens (\DocDbSettings' {kmsKeyId} -> kmsKeyId) (\s@DocDbSettings' {} a -> s {kmsKeyId = a} :: DocDbSettings)

-- | The password for the user account you use to access the DocumentDB
-- source endpoint.
docDbSettings_password :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_password = Lens.lens (\DocDbSettings' {password} -> password) (\s@DocDbSettings' {} a -> s {password = a} :: DocDbSettings) Prelude.. Lens.mapping Prelude._Sensitive

-- | The port value for the DocumentDB source endpoint.
docDbSettings_port :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Int)
docDbSettings_port = Lens.lens (\DocDbSettings' {port} -> port) (\s@DocDbSettings' {} a -> s {port = a} :: DocDbSettings)

-- | The user name you use to access the DocumentDB source endpoint.
docDbSettings_username :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_username = Lens.lens (\DocDbSettings' {username} -> username) (\s@DocDbSettings' {} a -> s {username = a} :: DocDbSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the
-- DocumentDB endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
docDbSettings_secretsManagerAccessRoleArn :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_secretsManagerAccessRoleArn = Lens.lens (\DocDbSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@DocDbSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: DocDbSettings)

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
docDbSettings_extractDocId :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Bool)
docDbSettings_extractDocId = Lens.lens (\DocDbSettings' {extractDocId} -> extractDocId) (\s@DocDbSettings' {} a -> s {extractDocId = a} :: DocDbSettings)

-- | Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
docDbSettings_docsToInvestigate :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Int)
docDbSettings_docsToInvestigate = Lens.lens (\DocDbSettings' {docsToInvestigate} -> docsToInvestigate) (\s@DocDbSettings' {} a -> s {docsToInvestigate = a} :: DocDbSettings)

-- | Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
docDbSettings_nestingLevel :: Lens.Lens' DocDbSettings (Prelude.Maybe NestingLevelValue)
docDbSettings_nestingLevel = Lens.lens (\DocDbSettings' {nestingLevel} -> nestingLevel) (\s@DocDbSettings' {} a -> s {nestingLevel = a} :: DocDbSettings)

-- | The database name on the DocumentDB source endpoint.
docDbSettings_databaseName :: Lens.Lens' DocDbSettings (Prelude.Maybe Prelude.Text)
docDbSettings_databaseName = Lens.lens (\DocDbSettings' {databaseName} -> databaseName) (\s@DocDbSettings' {} a -> s {databaseName = a} :: DocDbSettings)

instance Prelude.FromJSON DocDbSettings where
  parseJSON =
    Prelude.withObject
      "DocDbSettings"
      ( \x ->
          DocDbSettings'
            Prelude.<$> (x Prelude..:? "SecretsManagerSecretId")
            Prelude.<*> (x Prelude..:? "ServerName")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "Password")
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "Username")
            Prelude.<*> (x Prelude..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Prelude..:? "ExtractDocId")
            Prelude.<*> (x Prelude..:? "DocsToInvestigate")
            Prelude.<*> (x Prelude..:? "NestingLevel")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable DocDbSettings

instance Prelude.NFData DocDbSettings

instance Prelude.ToJSON DocDbSettings where
  toJSON DocDbSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecretsManagerSecretId" Prelude..=)
              Prelude.<$> secretsManagerSecretId,
            ("ServerName" Prelude..=) Prelude.<$> serverName,
            ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("Password" Prelude..=) Prelude.<$> password,
            ("Port" Prelude..=) Prelude.<$> port,
            ("Username" Prelude..=) Prelude.<$> username,
            ("SecretsManagerAccessRoleArn" Prelude..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("ExtractDocId" Prelude..=) Prelude.<$> extractDocId,
            ("DocsToInvestigate" Prelude..=)
              Prelude.<$> docsToInvestigate,
            ("NestingLevel" Prelude..=) Prelude.<$> nestingLevel,
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )
