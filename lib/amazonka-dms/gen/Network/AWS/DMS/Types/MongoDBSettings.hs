{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MongoDBSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MongoDBSettings
  ( MongoDBSettings (..),

    -- * Smart constructor
    mkMongoDBSettings,

    -- * Lenses
    mdsServerName,
    mdsAuthMechanism,
    mdsUsername,
    mdsKMSKeyId,
    mdsPassword,
    mdsNestingLevel,
    mdsDatabaseName,
    mdsDocsToInvestigate,
    mdsAuthSource,
    mdsExtractDocId,
    mdsAuthType,
    mdsPort,
  )
where

import Network.AWS.DMS.Types.AuthMechanismValue
import Network.AWS.DMS.Types.AuthTypeValue
import Network.AWS.DMS.Types.NestingLevelValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines a MongoDB endpoint.
--
-- /See:/ 'mkMongoDBSettings' smart constructor.
data MongoDBSettings = MongoDBSettings'
  { -- | The name of the server on the MongoDB source endpoint.
    serverName :: Lude.Maybe Lude.Text,
    -- | The authentication mechanism you use to access the MongoDB source endpoint.
    --
    -- For the default value, in MongoDB version 2.x, @"default"@ is @"mongodb_cr"@ . For MongoDB version 3.x or later, @"default"@ is @"scram_sha_1"@ . This setting isn't used when @AuthType@ is set to @"no"@ .
    authMechanism :: Lude.Maybe AuthMechanismValue,
    -- | The user name you use to access the MongoDB source endpoint.
    username :: Lude.Maybe Lude.Text,
    -- | The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The password for the user account you use to access the MongoDB source endpoint.
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Specifies either document or table mode.
    --
    -- Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
    nestingLevel :: Lude.Maybe NestingLevelValue,
    -- | The database name on the MongoDB source endpoint.
    databaseName :: Lude.Maybe Lude.Text,
    -- | Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .
    --
    -- Must be a positive value greater than @0@ . Default value is @1000@ .
    docsToInvestigate :: Lude.Maybe Lude.Text,
    -- | The MongoDB database name. This setting isn't used when @AuthType@ is set to @"no"@ .
    --
    -- The default is @"admin"@ .
    authSource :: Lude.Maybe Lude.Text,
    -- | Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .
    --
    -- Default value is @"false"@ .
    extractDocId :: Lude.Maybe Lude.Text,
    -- | The authentication type you use to access the MongoDB source endpoint.
    --
    -- When when set to @"no"@ , user name and password parameters are not used and can be empty.
    authType :: Lude.Maybe AuthTypeValue,
    -- | The port value for the MongoDB source endpoint.
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MongoDBSettings' with the minimum fields required to make a request.
--
-- * 'serverName' - The name of the server on the MongoDB source endpoint.
-- * 'authMechanism' - The authentication mechanism you use to access the MongoDB source endpoint.
--
-- For the default value, in MongoDB version 2.x, @"default"@ is @"mongodb_cr"@ . For MongoDB version 3.x or later, @"default"@ is @"scram_sha_1"@ . This setting isn't used when @AuthType@ is set to @"no"@ .
-- * 'username' - The user name you use to access the MongoDB source endpoint.
-- * 'kmsKeyId' - The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'password' - The password for the user account you use to access the MongoDB source endpoint.
-- * 'nestingLevel' - Specifies either document or table mode.
--
-- Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
-- * 'databaseName' - The database name on the MongoDB source endpoint.
-- * 'docsToInvestigate' - Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .
--
-- Must be a positive value greater than @0@ . Default value is @1000@ .
-- * 'authSource' - The MongoDB database name. This setting isn't used when @AuthType@ is set to @"no"@ .
--
-- The default is @"admin"@ .
-- * 'extractDocId' - Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .
--
-- Default value is @"false"@ .
-- * 'authType' - The authentication type you use to access the MongoDB source endpoint.
--
-- When when set to @"no"@ , user name and password parameters are not used and can be empty.
-- * 'port' - The port value for the MongoDB source endpoint.
mkMongoDBSettings ::
  MongoDBSettings
mkMongoDBSettings =
  MongoDBSettings'
    { serverName = Lude.Nothing,
      authMechanism = Lude.Nothing,
      username = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      password = Lude.Nothing,
      nestingLevel = Lude.Nothing,
      databaseName = Lude.Nothing,
      docsToInvestigate = Lude.Nothing,
      authSource = Lude.Nothing,
      extractDocId = Lude.Nothing,
      authType = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The name of the server on the MongoDB source endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsServerName :: Lens.Lens' MongoDBSettings (Lude.Maybe Lude.Text)
mdsServerName = Lens.lens (serverName :: MongoDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: MongoDBSettings)
{-# DEPRECATED mdsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The authentication mechanism you use to access the MongoDB source endpoint.
--
-- For the default value, in MongoDB version 2.x, @"default"@ is @"mongodb_cr"@ . For MongoDB version 3.x or later, @"default"@ is @"scram_sha_1"@ . This setting isn't used when @AuthType@ is set to @"no"@ .
--
-- /Note:/ Consider using 'authMechanism' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsAuthMechanism :: Lens.Lens' MongoDBSettings (Lude.Maybe AuthMechanismValue)
mdsAuthMechanism = Lens.lens (authMechanism :: MongoDBSettings -> Lude.Maybe AuthMechanismValue) (\s a -> s {authMechanism = a} :: MongoDBSettings)
{-# DEPRECATED mdsAuthMechanism "Use generic-lens or generic-optics with 'authMechanism' instead." #-}

-- | The user name you use to access the MongoDB source endpoint.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsUsername :: Lens.Lens' MongoDBSettings (Lude.Maybe Lude.Text)
mdsUsername = Lens.lens (username :: MongoDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: MongoDBSettings)
{-# DEPRECATED mdsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsKMSKeyId :: Lens.Lens' MongoDBSettings (Lude.Maybe Lude.Text)
mdsKMSKeyId = Lens.lens (kmsKeyId :: MongoDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: MongoDBSettings)
{-# DEPRECATED mdsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The password for the user account you use to access the MongoDB source endpoint.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsPassword :: Lens.Lens' MongoDBSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
mdsPassword = Lens.lens (password :: MongoDBSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: MongoDBSettings)
{-# DEPRECATED mdsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Specifies either document or table mode.
--
-- Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
--
-- /Note:/ Consider using 'nestingLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsNestingLevel :: Lens.Lens' MongoDBSettings (Lude.Maybe NestingLevelValue)
mdsNestingLevel = Lens.lens (nestingLevel :: MongoDBSettings -> Lude.Maybe NestingLevelValue) (\s a -> s {nestingLevel = a} :: MongoDBSettings)
{-# DEPRECATED mdsNestingLevel "Use generic-lens or generic-optics with 'nestingLevel' instead." #-}

-- | The database name on the MongoDB source endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsDatabaseName :: Lens.Lens' MongoDBSettings (Lude.Maybe Lude.Text)
mdsDatabaseName = Lens.lens (databaseName :: MongoDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: MongoDBSettings)
{-# DEPRECATED mdsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .
--
-- Must be a positive value greater than @0@ . Default value is @1000@ .
--
-- /Note:/ Consider using 'docsToInvestigate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsDocsToInvestigate :: Lens.Lens' MongoDBSettings (Lude.Maybe Lude.Text)
mdsDocsToInvestigate = Lens.lens (docsToInvestigate :: MongoDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {docsToInvestigate = a} :: MongoDBSettings)
{-# DEPRECATED mdsDocsToInvestigate "Use generic-lens or generic-optics with 'docsToInvestigate' instead." #-}

-- | The MongoDB database name. This setting isn't used when @AuthType@ is set to @"no"@ .
--
-- The default is @"admin"@ .
--
-- /Note:/ Consider using 'authSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsAuthSource :: Lens.Lens' MongoDBSettings (Lude.Maybe Lude.Text)
mdsAuthSource = Lens.lens (authSource :: MongoDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {authSource = a} :: MongoDBSettings)
{-# DEPRECATED mdsAuthSource "Use generic-lens or generic-optics with 'authSource' instead." #-}

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .
--
-- Default value is @"false"@ .
--
-- /Note:/ Consider using 'extractDocId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsExtractDocId :: Lens.Lens' MongoDBSettings (Lude.Maybe Lude.Text)
mdsExtractDocId = Lens.lens (extractDocId :: MongoDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {extractDocId = a} :: MongoDBSettings)
{-# DEPRECATED mdsExtractDocId "Use generic-lens or generic-optics with 'extractDocId' instead." #-}

-- | The authentication type you use to access the MongoDB source endpoint.
--
-- When when set to @"no"@ , user name and password parameters are not used and can be empty.
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsAuthType :: Lens.Lens' MongoDBSettings (Lude.Maybe AuthTypeValue)
mdsAuthType = Lens.lens (authType :: MongoDBSettings -> Lude.Maybe AuthTypeValue) (\s a -> s {authType = a} :: MongoDBSettings)
{-# DEPRECATED mdsAuthType "Use generic-lens or generic-optics with 'authType' instead." #-}

-- | The port value for the MongoDB source endpoint.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsPort :: Lens.Lens' MongoDBSettings (Lude.Maybe Lude.Int)
mdsPort = Lens.lens (port :: MongoDBSettings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: MongoDBSettings)
{-# DEPRECATED mdsPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON MongoDBSettings where
  parseJSON =
    Lude.withObject
      "MongoDBSettings"
      ( \x ->
          MongoDBSettings'
            Lude.<$> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "AuthMechanism")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "NestingLevel")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "DocsToInvestigate")
            Lude.<*> (x Lude..:? "AuthSource")
            Lude.<*> (x Lude..:? "ExtractDocId")
            Lude.<*> (x Lude..:? "AuthType")
            Lude.<*> (x Lude..:? "Port")
      )

instance Lude.ToJSON MongoDBSettings where
  toJSON MongoDBSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServerName" Lude..=) Lude.<$> serverName,
            ("AuthMechanism" Lude..=) Lude.<$> authMechanism,
            ("Username" Lude..=) Lude.<$> username,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("Password" Lude..=) Lude.<$> password,
            ("NestingLevel" Lude..=) Lude.<$> nestingLevel,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("DocsToInvestigate" Lude..=) Lude.<$> docsToInvestigate,
            ("AuthSource" Lude..=) Lude.<$> authSource,
            ("ExtractDocId" Lude..=) Lude.<$> extractDocId,
            ("AuthType" Lude..=) Lude.<$> authType,
            ("Port" Lude..=) Lude.<$> port
          ]
      )
