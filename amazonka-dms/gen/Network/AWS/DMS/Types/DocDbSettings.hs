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

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.NestingLevelValue
import qualified Network.AWS.Lens as Lens

-- | Provides information that defines a DocumentDB endpoint.
--
-- /See:/ 'newDocDbSettings' smart constructor.
data DocDbSettings = DocDbSettings'
  { -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the DocumentDB endpoint connection
    -- details.
    secretsManagerSecretId :: Core.Maybe Core.Text,
    -- | The name of the server on the DocumentDB source endpoint.
    serverName :: Core.Maybe Core.Text,
    -- | The AWS KMS key identifier that is used to encrypt the content on the
    -- replication instance. If you don\'t specify a value for the @KmsKeyId@
    -- parameter, then AWS DMS uses your default encryption key. AWS KMS
    -- creates the default encryption key for your AWS account. Your AWS
    -- account has a different default encryption key for each AWS Region.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The password for the user account you use to access the DocumentDB
    -- source endpoint.
    password :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The port value for the DocumentDB source endpoint.
    port :: Core.Maybe Core.Int,
    -- | The user name you use to access the DocumentDB source endpoint.
    username :: Core.Maybe Core.Text,
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
    secretsManagerAccessRoleArn :: Core.Maybe Core.Text,
    -- | Specifies the document ID. Use this setting when @NestingLevel@ is set
    -- to @\"none\"@.
    --
    -- Default value is @\"false\"@.
    extractDocId :: Core.Maybe Core.Bool,
    -- | Indicates the number of documents to preview to determine the document
    -- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
    --
    -- Must be a positive value greater than @0@. Default value is @1000@.
    docsToInvestigate :: Core.Maybe Core.Int,
    -- | Specifies either document or table mode.
    --
    -- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
    -- Specify @\"one\"@ to use table mode.
    nestingLevel :: Core.Maybe NestingLevelValue,
    -- | The database name on the DocumentDB source endpoint.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
        Core.Nothing,
      serverName = Core.Nothing,
      kmsKeyId = Core.Nothing,
      password = Core.Nothing,
      port = Core.Nothing,
      username = Core.Nothing,
      secretsManagerAccessRoleArn = Core.Nothing,
      extractDocId = Core.Nothing,
      docsToInvestigate = Core.Nothing,
      nestingLevel = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the DocumentDB endpoint connection
-- details.
docDbSettings_secretsManagerSecretId :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
docDbSettings_secretsManagerSecretId = Lens.lens (\DocDbSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@DocDbSettings' {} a -> s {secretsManagerSecretId = a} :: DocDbSettings)

-- | The name of the server on the DocumentDB source endpoint.
docDbSettings_serverName :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
docDbSettings_serverName = Lens.lens (\DocDbSettings' {serverName} -> serverName) (\s@DocDbSettings' {} a -> s {serverName = a} :: DocDbSettings)

-- | The AWS KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then AWS DMS uses your default encryption key. AWS KMS
-- creates the default encryption key for your AWS account. Your AWS
-- account has a different default encryption key for each AWS Region.
docDbSettings_kmsKeyId :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
docDbSettings_kmsKeyId = Lens.lens (\DocDbSettings' {kmsKeyId} -> kmsKeyId) (\s@DocDbSettings' {} a -> s {kmsKeyId = a} :: DocDbSettings)

-- | The password for the user account you use to access the DocumentDB
-- source endpoint.
docDbSettings_password :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
docDbSettings_password = Lens.lens (\DocDbSettings' {password} -> password) (\s@DocDbSettings' {} a -> s {password = a} :: DocDbSettings) Core.. Lens.mapping Core._Sensitive

-- | The port value for the DocumentDB source endpoint.
docDbSettings_port :: Lens.Lens' DocDbSettings (Core.Maybe Core.Int)
docDbSettings_port = Lens.lens (\DocDbSettings' {port} -> port) (\s@DocDbSettings' {} a -> s {port = a} :: DocDbSettings)

-- | The user name you use to access the DocumentDB source endpoint.
docDbSettings_username :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
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
docDbSettings_secretsManagerAccessRoleArn :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
docDbSettings_secretsManagerAccessRoleArn = Lens.lens (\DocDbSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@DocDbSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: DocDbSettings)

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
docDbSettings_extractDocId :: Lens.Lens' DocDbSettings (Core.Maybe Core.Bool)
docDbSettings_extractDocId = Lens.lens (\DocDbSettings' {extractDocId} -> extractDocId) (\s@DocDbSettings' {} a -> s {extractDocId = a} :: DocDbSettings)

-- | Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
docDbSettings_docsToInvestigate :: Lens.Lens' DocDbSettings (Core.Maybe Core.Int)
docDbSettings_docsToInvestigate = Lens.lens (\DocDbSettings' {docsToInvestigate} -> docsToInvestigate) (\s@DocDbSettings' {} a -> s {docsToInvestigate = a} :: DocDbSettings)

-- | Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
docDbSettings_nestingLevel :: Lens.Lens' DocDbSettings (Core.Maybe NestingLevelValue)
docDbSettings_nestingLevel = Lens.lens (\DocDbSettings' {nestingLevel} -> nestingLevel) (\s@DocDbSettings' {} a -> s {nestingLevel = a} :: DocDbSettings)

-- | The database name on the DocumentDB source endpoint.
docDbSettings_databaseName :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
docDbSettings_databaseName = Lens.lens (\DocDbSettings' {databaseName} -> databaseName) (\s@DocDbSettings' {} a -> s {databaseName = a} :: DocDbSettings)

instance Core.FromJSON DocDbSettings where
  parseJSON =
    Core.withObject
      "DocDbSettings"
      ( \x ->
          DocDbSettings'
            Core.<$> (x Core..:? "SecretsManagerSecretId")
            Core.<*> (x Core..:? "ServerName")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "Password")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Core.<*> (x Core..:? "ExtractDocId")
            Core.<*> (x Core..:? "DocsToInvestigate")
            Core.<*> (x Core..:? "NestingLevel")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable DocDbSettings

instance Core.NFData DocDbSettings

instance Core.ToJSON DocDbSettings where
  toJSON DocDbSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecretsManagerSecretId" Core..=)
              Core.<$> secretsManagerSecretId,
            ("ServerName" Core..=) Core.<$> serverName,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("Password" Core..=) Core.<$> password,
            ("Port" Core..=) Core.<$> port,
            ("Username" Core..=) Core.<$> username,
            ("SecretsManagerAccessRoleArn" Core..=)
              Core.<$> secretsManagerAccessRoleArn,
            ("ExtractDocId" Core..=) Core.<$> extractDocId,
            ("DocsToInvestigate" Core..=)
              Core.<$> docsToInvestigate,
            ("NestingLevel" Core..=) Core.<$> nestingLevel,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )
