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
-- Module      : Amazonka.DMS.Types.MongoDbSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.MongoDbSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.AuthMechanismValue
import Amazonka.DMS.Types.AuthTypeValue
import Amazonka.DMS.Types.NestingLevelValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines a MongoDB endpoint.
--
-- /See:/ 'newMongoDbSettings' smart constructor.
data MongoDbSettings = MongoDbSettings'
  { -- | The authentication mechanism you use to access the MongoDB source
    -- endpoint.
    --
    -- For the default value, in MongoDB version 2.x, @\"default\"@ is
    -- @\"mongodb_cr\"@. For MongoDB version 3.x or later, @\"default\"@ is
    -- @\"scram_sha_1\"@. This setting isn\'t used when @AuthType@ is set to
    -- @\"no\"@.
    authMechanism :: Prelude.Maybe AuthMechanismValue,
    -- | The MongoDB database name. This setting isn\'t used when @AuthType@ is
    -- set to @\"no\"@.
    --
    -- The default is @\"admin\"@.
    authSource :: Prelude.Maybe Prelude.Text,
    -- | The authentication type you use to access the MongoDB source endpoint.
    --
    -- When when set to @\"no\"@, user name and password parameters are not
    -- used and can be empty.
    authType :: Prelude.Maybe AuthTypeValue,
    -- | The database name on the MongoDB source endpoint.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the number of documents to preview to determine the document
    -- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
    --
    -- Must be a positive value greater than @0@. Default value is @1000@.
    docsToInvestigate :: Prelude.Maybe Prelude.Text,
    -- | Specifies the document ID. Use this setting when @NestingLevel@ is set
    -- to @\"none\"@.
    --
    -- Default value is @\"false\"@.
    extractDocId :: Prelude.Maybe Prelude.Text,
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
    -- | The password for the user account you use to access the MongoDB source
    -- endpoint.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The port value for the MongoDB source endpoint.
    port :: Prelude.Maybe Prelude.Int,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the MongoDB endpoint.
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
    -- @SecretsManagerSecret@ that contains the MongoDB endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | The name of the server on the MongoDB source endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The user name you use to access the MongoDB source endpoint.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MongoDbSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authMechanism', 'mongoDbSettings_authMechanism' - The authentication mechanism you use to access the MongoDB source
-- endpoint.
--
-- For the default value, in MongoDB version 2.x, @\"default\"@ is
-- @\"mongodb_cr\"@. For MongoDB version 3.x or later, @\"default\"@ is
-- @\"scram_sha_1\"@. This setting isn\'t used when @AuthType@ is set to
-- @\"no\"@.
--
-- 'authSource', 'mongoDbSettings_authSource' - The MongoDB database name. This setting isn\'t used when @AuthType@ is
-- set to @\"no\"@.
--
-- The default is @\"admin\"@.
--
-- 'authType', 'mongoDbSettings_authType' - The authentication type you use to access the MongoDB source endpoint.
--
-- When when set to @\"no\"@, user name and password parameters are not
-- used and can be empty.
--
-- 'databaseName', 'mongoDbSettings_databaseName' - The database name on the MongoDB source endpoint.
--
-- 'docsToInvestigate', 'mongoDbSettings_docsToInvestigate' - Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
--
-- 'extractDocId', 'mongoDbSettings_extractDocId' - Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
--
-- 'kmsKeyId', 'mongoDbSettings_kmsKeyId' - The KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then DMS uses your default encryption key. KMS creates the
-- default encryption key for your Amazon Web Services account. Your Amazon
-- Web Services account has a different default encryption key for each
-- Amazon Web Services Region.
--
-- 'nestingLevel', 'mongoDbSettings_nestingLevel' - Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
--
-- 'password', 'mongoDbSettings_password' - The password for the user account you use to access the MongoDB source
-- endpoint.
--
-- 'port', 'mongoDbSettings_port' - The port value for the MongoDB source endpoint.
--
-- 'secretsManagerAccessRoleArn', 'mongoDbSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the MongoDB endpoint.
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
-- 'secretsManagerSecretId', 'mongoDbSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MongoDB endpoint connection
-- details.
--
-- 'serverName', 'mongoDbSettings_serverName' - The name of the server on the MongoDB source endpoint.
--
-- 'username', 'mongoDbSettings_username' - The user name you use to access the MongoDB source endpoint.
newMongoDbSettings ::
  MongoDbSettings
newMongoDbSettings =
  MongoDbSettings'
    { authMechanism = Prelude.Nothing,
      authSource = Prelude.Nothing,
      authType = Prelude.Nothing,
      databaseName = Prelude.Nothing,
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

-- | The authentication mechanism you use to access the MongoDB source
-- endpoint.
--
-- For the default value, in MongoDB version 2.x, @\"default\"@ is
-- @\"mongodb_cr\"@. For MongoDB version 3.x or later, @\"default\"@ is
-- @\"scram_sha_1\"@. This setting isn\'t used when @AuthType@ is set to
-- @\"no\"@.
mongoDbSettings_authMechanism :: Lens.Lens' MongoDbSettings (Prelude.Maybe AuthMechanismValue)
mongoDbSettings_authMechanism = Lens.lens (\MongoDbSettings' {authMechanism} -> authMechanism) (\s@MongoDbSettings' {} a -> s {authMechanism = a} :: MongoDbSettings)

-- | The MongoDB database name. This setting isn\'t used when @AuthType@ is
-- set to @\"no\"@.
--
-- The default is @\"admin\"@.
mongoDbSettings_authSource :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_authSource = Lens.lens (\MongoDbSettings' {authSource} -> authSource) (\s@MongoDbSettings' {} a -> s {authSource = a} :: MongoDbSettings)

-- | The authentication type you use to access the MongoDB source endpoint.
--
-- When when set to @\"no\"@, user name and password parameters are not
-- used and can be empty.
mongoDbSettings_authType :: Lens.Lens' MongoDbSettings (Prelude.Maybe AuthTypeValue)
mongoDbSettings_authType = Lens.lens (\MongoDbSettings' {authType} -> authType) (\s@MongoDbSettings' {} a -> s {authType = a} :: MongoDbSettings)

-- | The database name on the MongoDB source endpoint.
mongoDbSettings_databaseName :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_databaseName = Lens.lens (\MongoDbSettings' {databaseName} -> databaseName) (\s@MongoDbSettings' {} a -> s {databaseName = a} :: MongoDbSettings)

-- | Indicates the number of documents to preview to determine the document
-- organization. Use this setting when @NestingLevel@ is set to @\"one\"@.
--
-- Must be a positive value greater than @0@. Default value is @1000@.
mongoDbSettings_docsToInvestigate :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_docsToInvestigate = Lens.lens (\MongoDbSettings' {docsToInvestigate} -> docsToInvestigate) (\s@MongoDbSettings' {} a -> s {docsToInvestigate = a} :: MongoDbSettings)

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set
-- to @\"none\"@.
--
-- Default value is @\"false\"@.
mongoDbSettings_extractDocId :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_extractDocId = Lens.lens (\MongoDbSettings' {extractDocId} -> extractDocId) (\s@MongoDbSettings' {} a -> s {extractDocId = a} :: MongoDbSettings)

-- | The KMS key identifier that is used to encrypt the content on the
-- replication instance. If you don\'t specify a value for the @KmsKeyId@
-- parameter, then DMS uses your default encryption key. KMS creates the
-- default encryption key for your Amazon Web Services account. Your Amazon
-- Web Services account has a different default encryption key for each
-- Amazon Web Services Region.
mongoDbSettings_kmsKeyId :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_kmsKeyId = Lens.lens (\MongoDbSettings' {kmsKeyId} -> kmsKeyId) (\s@MongoDbSettings' {} a -> s {kmsKeyId = a} :: MongoDbSettings)

-- | Specifies either document or table mode.
--
-- Default value is @\"none\"@. Specify @\"none\"@ to use document mode.
-- Specify @\"one\"@ to use table mode.
mongoDbSettings_nestingLevel :: Lens.Lens' MongoDbSettings (Prelude.Maybe NestingLevelValue)
mongoDbSettings_nestingLevel = Lens.lens (\MongoDbSettings' {nestingLevel} -> nestingLevel) (\s@MongoDbSettings' {} a -> s {nestingLevel = a} :: MongoDbSettings)

-- | The password for the user account you use to access the MongoDB source
-- endpoint.
mongoDbSettings_password :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_password = Lens.lens (\MongoDbSettings' {password} -> password) (\s@MongoDbSettings' {} a -> s {password = a} :: MongoDbSettings) Prelude.. Lens.mapping Data._Sensitive

-- | The port value for the MongoDB source endpoint.
mongoDbSettings_port :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Int)
mongoDbSettings_port = Lens.lens (\MongoDbSettings' {port} -> port) (\s@MongoDbSettings' {} a -> s {port = a} :: MongoDbSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the MongoDB endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
mongoDbSettings_secretsManagerAccessRoleArn :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_secretsManagerAccessRoleArn = Lens.lens (\MongoDbSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@MongoDbSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: MongoDbSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the MongoDB endpoint connection
-- details.
mongoDbSettings_secretsManagerSecretId :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_secretsManagerSecretId = Lens.lens (\MongoDbSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@MongoDbSettings' {} a -> s {secretsManagerSecretId = a} :: MongoDbSettings)

-- | The name of the server on the MongoDB source endpoint.
mongoDbSettings_serverName :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_serverName = Lens.lens (\MongoDbSettings' {serverName} -> serverName) (\s@MongoDbSettings' {} a -> s {serverName = a} :: MongoDbSettings)

-- | The user name you use to access the MongoDB source endpoint.
mongoDbSettings_username :: Lens.Lens' MongoDbSettings (Prelude.Maybe Prelude.Text)
mongoDbSettings_username = Lens.lens (\MongoDbSettings' {username} -> username) (\s@MongoDbSettings' {} a -> s {username = a} :: MongoDbSettings)

instance Data.FromJSON MongoDbSettings where
  parseJSON =
    Data.withObject
      "MongoDbSettings"
      ( \x ->
          MongoDbSettings'
            Prelude.<$> (x Data..:? "AuthMechanism")
            Prelude.<*> (x Data..:? "AuthSource")
            Prelude.<*> (x Data..:? "AuthType")
            Prelude.<*> (x Data..:? "DatabaseName")
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

instance Prelude.Hashable MongoDbSettings where
  hashWithSalt _salt MongoDbSettings' {..} =
    _salt
      `Prelude.hashWithSalt` authMechanism
      `Prelude.hashWithSalt` authSource
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` databaseName
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

instance Prelude.NFData MongoDbSettings where
  rnf MongoDbSettings' {..} =
    Prelude.rnf authMechanism
      `Prelude.seq` Prelude.rnf authSource
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf databaseName
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

instance Data.ToJSON MongoDbSettings where
  toJSON MongoDbSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthMechanism" Data..=) Prelude.<$> authMechanism,
            ("AuthSource" Data..=) Prelude.<$> authSource,
            ("AuthType" Data..=) Prelude.<$> authType,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
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
