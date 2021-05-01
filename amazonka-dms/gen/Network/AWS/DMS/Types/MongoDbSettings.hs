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
-- Module      : Network.AWS.DMS.Types.MongoDbSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MongoDbSettings where

import Network.AWS.DMS.Types.AuthMechanismValue
import Network.AWS.DMS.Types.AuthTypeValue
import Network.AWS.DMS.Types.NestingLevelValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines a MongoDB endpoint.
--
-- /See:/ 'newMongoDbSettings' smart constructor.
data MongoDbSettings = MongoDbSettings'
  { -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the MongoDB endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | The MongoDB database name. This setting isn\'t used when @AuthType@ is
    -- set to @\"no\"@.
    --
    -- The default is @\"admin\"@.
    authSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the server on the MongoDB source endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The AWS KMS key identifier that is used to encrypt the content on the
    -- replication instance. If you don\'t specify a value for the @KmsKeyId@
    -- parameter, then AWS DMS uses your default encryption key. AWS KMS
    -- creates the default encryption key for your AWS account. Your AWS
    -- account has a different default encryption key for each AWS Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The password for the user account you use to access the MongoDB source
    -- endpoint.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The port value for the MongoDB source endpoint.
    port :: Prelude.Maybe Prelude.Int,
    -- | The user name you use to access the MongoDB source endpoint.
    username :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
    -- DMS as the trusted entity and grants the required permissions to access
    -- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
    -- value of the AWS Secrets Manager secret that allows access to the
    -- MongoDB endpoint.
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
    -- | The authentication mechanism you use to access the MongoDB source
    -- endpoint.
    --
    -- For the default value, in MongoDB version 2.x, @\"default\"@ is
    -- @\"mongodb_cr\"@. For MongoDB version 3.x or later, @\"default\"@ is
    -- @\"scram_sha_1\"@. This setting isn\'t used when @AuthType@ is set to
    -- @\"no\"@.
    authMechanism :: Prelude.Maybe AuthMechanismValue,
    -- | Specifies the document ID. Use this setting when @NestingLevel@ is set
    -- to @\"none\"@.
    --
    -- Default value is @\"false\"@.
    extractDocId :: Prelude.Maybe Prelude.Text,
    -- | The authentication type you use to access the MongoDB source endpoint.
    --
    -- When when set to @\"no\"@, user name and password parameters are not
    -- used and can be empty.
    authType :: Prelude.Maybe AuthTypeValue,
    -- | Indicates the number of documents to preview to determine the document
    -- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
    --
    -- Must be a positive value greater than @0@. Default value is @1000@.
    docsToInvestigate :: Prelude.Maybe Prelude.Text,
    -- | Specifies either document or table mode.
    --
    -- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
    -- Specify @\"one\"@ to use table mode.
    nestingLevel :: Prelude.Maybe NestingLevelValue,
    -- | The database name on the MongoDB source endpoint.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MongoDbSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretsManagerSecretId', 'mongoDbSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MongoDB endpoint connection
-- details.
--
-- 'authSource', 'mongoDbSettings_authSource' - The MongoDB database name. This setting isn\'t used when @AuthType@ is
-- set to @\"no\"@.
--
-- The default is @\"admin\"@.
--
-- 'serverName', 'mongoDbSettings_serverName' - The name of the server on the MongoDB source endpoint.
--
-- 'kmsKeyId', 'mongoDbSettings_kmsKeyId' - The AWS KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then AWS DMS uses your default encryption key. AWS KMS
-- creates the default encryption key for your AWS account. Your AWS
-- account has a different default encryption key for each AWS Region.
--
-- 'password', 'mongoDbSettings_password' - The password for the user account you use to access the MongoDB source
-- endpoint.
--
-- 'port', 'mongoDbSettings_port' - The port value for the MongoDB source endpoint.
--
-- 'username', 'mongoDbSettings_username' - The user name you use to access the MongoDB source endpoint.
--
-- 'secretsManagerAccessRoleArn', 'mongoDbSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the
-- MongoDB endpoint.
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
-- 'authMechanism', 'mongoDbSettings_authMechanism' - The authentication mechanism you use to access the MongoDB source
-- endpoint.
--
-- For the default value, in MongoDB version 2.x, @\"default\"@ is
-- @\"mongodb_cr\"@. For MongoDB version 3.x or later, @\"default\"@ is
-- @\"scram_sha_1\"@. This setting isn\'t used when @AuthType@ is set to
-- @\"no\"@.
--
-- 'extractDocId', 'mongoDbSettings_extractDocId' - Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
--
-- 'authType', 'mongoDbSettings_authType' - The authentication type you use to access the MongoDB source endpoint.
--
-- When when set to @\"no\"@, user name and password parameters are not
-- used and can be empty.
--
-- 'docsToInvestigate', 'mongoDbSettings_docsToInvestigate' - Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
--
-- 'nestingLevel', 'mongoDbSettings_nestingLevel' - Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
--
-- 'databaseName', 'mongoDbSettings_databaseName' - The database name on the MongoDB source endpoint.
newMongoDbSettings ::
  MongoDbSettings
newMongoDbSettings =
  MongoDbSettings'
    { secretsManagerSecretId =
        Prelude.Nothing,
      authSource = Prelude.Nothing,
      serverName = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      password = Prelude.Nothing,
      port = Prelude.Nothing,
      username = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      authMechanism = Prelude.Nothing,
      extractDocId = Prelude.Nothing,
      authType = Prelude.Nothing,
      docsToInvestigate = Prelude.Nothing,
      nestingLevel = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MongoDB endpoint connection
-- details.
mongoDbSettings_secretsManagerSecretId :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_secretsManagerSecretId = Lens.lens (\MongoDbSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@MongoDbSettings' {} a -> s {secretsManagerSecretId = a} :: MongoDbSettings)

-- | The MongoDB database name. This setting isn\'t used when @AuthType@ is
-- set to @\"no\"@.
--
-- The default is @\"admin\"@.
mongoDbSettings_authSource :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_authSource = Lens.lens (\MongoDbSettings' {authSource} -> authSource) (\s@MongoDbSettings' {} a -> s {authSource = a} :: MongoDbSettings)

-- | The name of the server on the MongoDB source endpoint.
mongoDbSettings_serverName :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_serverName = Lens.lens (\MongoDbSettings' {serverName} -> serverName) (\s@MongoDbSettings' {} a -> s {serverName = a} :: MongoDbSettings)

-- | The AWS KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then AWS DMS uses your default encryption key. AWS KMS
-- creates the default encryption key for your AWS account. Your AWS
-- account has a different default encryption key for each AWS Region.
mongoDbSettings_kmsKeyId :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_kmsKeyId = Lens.lens (\MongoDbSettings' {kmsKeyId} -> kmsKeyId) (\s@MongoDbSettings' {} a -> s {kmsKeyId = a} :: MongoDbSettings)

-- | The password for the user account you use to access the MongoDB source
-- endpoint.
mongoDbSettings_password :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_password = Lens.lens (\MongoDbSettings' {password} -> password) (\s@MongoDbSettings' {} a -> s {password = a} :: MongoDbSettings) Prelude.. Lens.mapping Prelude._Sensitive

-- | The port value for the MongoDB source endpoint.
mongoDbSettings_port :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Int)
mongoDbSettings_port = Lens.lens (\MongoDbSettings' {port} -> port) (\s@MongoDbSettings' {} a -> s {port = a} :: MongoDbSettings)

-- | The user name you use to access the MongoDB source endpoint.
mongoDbSettings_username :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_username = Lens.lens (\MongoDbSettings' {username} -> username) (\s@MongoDbSettings' {} a -> s {username = a} :: MongoDbSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the
-- MongoDB endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
mongoDbSettings_secretsManagerAccessRoleArn :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_secretsManagerAccessRoleArn = Lens.lens (\MongoDbSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@MongoDbSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: MongoDbSettings)

-- | The authentication mechanism you use to access the MongoDB source
-- endpoint.
--
-- For the default value, in MongoDB version 2.x, @\"default\"@ is
-- @\"mongodb_cr\"@. For MongoDB version 3.x or later, @\"default\"@ is
-- @\"scram_sha_1\"@. This setting isn\'t used when @AuthType@ is set to
-- @\"no\"@.
mongoDbSettings_authMechanism :: Lens.Lens' MongoDbSettings (Prelude.Maybe AuthMechanismValue)
mongoDbSettings_authMechanism = Lens.lens (\MongoDbSettings' {authMechanism} -> authMechanism) (\s@MongoDbSettings' {} a -> s {authMechanism = a} :: MongoDbSettings)

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
mongoDbSettings_extractDocId :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_extractDocId = Lens.lens (\MongoDbSettings' {extractDocId} -> extractDocId) (\s@MongoDbSettings' {} a -> s {extractDocId = a} :: MongoDbSettings)

-- | The authentication type you use to access the MongoDB source endpoint.
--
-- When when set to @\"no\"@, user name and password parameters are not
-- used and can be empty.
mongoDbSettings_authType :: Lens.Lens' MongoDbSettings (Prelude.Maybe AuthTypeValue)
mongoDbSettings_authType = Lens.lens (\MongoDbSettings' {authType} -> authType) (\s@MongoDbSettings' {} a -> s {authType = a} :: MongoDbSettings)

-- | Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
mongoDbSettings_docsToInvestigate :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_docsToInvestigate = Lens.lens (\MongoDbSettings' {docsToInvestigate} -> docsToInvestigate) (\s@MongoDbSettings' {} a -> s {docsToInvestigate = a} :: MongoDbSettings)

-- | Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
mongoDbSettings_nestingLevel :: Lens.Lens' MongoDbSettings (Prelude.Maybe NestingLevelValue)
mongoDbSettings_nestingLevel = Lens.lens (\MongoDbSettings' {nestingLevel} -> nestingLevel) (\s@MongoDbSettings' {} a -> s {nestingLevel = a} :: MongoDbSettings)

-- | The database name on the MongoDB source endpoint.
mongoDbSettings_databaseName :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_databaseName = Lens.lens (\MongoDbSettings' {databaseName} -> databaseName) (\s@MongoDbSettings' {} a -> s {databaseName = a} :: MongoDbSettings)

instance Prelude.FromJSON MongoDbSettings where
  parseJSON =
    Prelude.withObject
      "MongoDbSettings"
      ( \x ->
          MongoDbSettings'
            Prelude.<$> (x Prelude..:? "SecretsManagerSecretId")
            Prelude.<*> (x Prelude..:? "AuthSource")
            Prelude.<*> (x Prelude..:? "ServerName")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "Password")
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "Username")
            Prelude.<*> (x Prelude..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Prelude..:? "AuthMechanism")
            Prelude.<*> (x Prelude..:? "ExtractDocId")
            Prelude.<*> (x Prelude..:? "AuthType")
            Prelude.<*> (x Prelude..:? "DocsToInvestigate")
            Prelude.<*> (x Prelude..:? "NestingLevel")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable MongoDbSettings

instance Prelude.NFData MongoDbSettings

instance Prelude.ToJSON MongoDbSettings where
  toJSON MongoDbSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecretsManagerSecretId" Prelude..=)
              Prelude.<$> secretsManagerSecretId,
            ("AuthSource" Prelude..=) Prelude.<$> authSource,
            ("ServerName" Prelude..=) Prelude.<$> serverName,
            ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("Password" Prelude..=) Prelude.<$> password,
            ("Port" Prelude..=) Prelude.<$> port,
            ("Username" Prelude..=) Prelude.<$> username,
            ("SecretsManagerAccessRoleArn" Prelude..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("AuthMechanism" Prelude..=)
              Prelude.<$> authMechanism,
            ("ExtractDocId" Prelude..=) Prelude.<$> extractDocId,
            ("AuthType" Prelude..=) Prelude.<$> authType,
            ("DocsToInvestigate" Prelude..=)
              Prelude.<$> docsToInvestigate,
            ("NestingLevel" Prelude..=) Prelude.<$> nestingLevel,
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )
