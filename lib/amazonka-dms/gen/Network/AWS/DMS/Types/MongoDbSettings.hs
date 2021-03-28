{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MongoDbSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.MongoDbSettings
  ( MongoDbSettings (..)
  -- * Smart constructor
  , mkMongoDbSettings
  -- * Lenses
  , mdsAuthMechanism
  , mdsAuthSource
  , mdsAuthType
  , mdsDatabaseName
  , mdsDocsToInvestigate
  , mdsExtractDocId
  , mdsKmsKeyId
  , mdsNestingLevel
  , mdsPassword
  , mdsPort
  , mdsServerName
  , mdsUsername
  ) where

import qualified Network.AWS.DMS.Types.AuthMechanismValue as Types
import qualified Network.AWS.DMS.Types.AuthTypeValue as Types
import qualified Network.AWS.DMS.Types.NestingLevelValue as Types
import qualified Network.AWS.DMS.Types.Password as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines a MongoDB endpoint.
--
-- /See:/ 'mkMongoDbSettings' smart constructor.
data MongoDbSettings = MongoDbSettings'
  { authMechanism :: Core.Maybe Types.AuthMechanismValue
    -- ^ The authentication mechanism you use to access the MongoDB source endpoint.
--
-- For the default value, in MongoDB version 2.x, @"default"@ is @"mongodb_cr"@ . For MongoDB version 3.x or later, @"default"@ is @"scram_sha_1"@ . This setting isn't used when @AuthType@ is set to @"no"@ .
  , authSource :: Core.Maybe Core.Text
    -- ^ The MongoDB database name. This setting isn't used when @AuthType@ is set to @"no"@ . 
--
-- The default is @"admin"@ .
  , authType :: Core.Maybe Types.AuthTypeValue
    -- ^ The authentication type you use to access the MongoDB source endpoint.
--
-- When when set to @"no"@ , user name and password parameters are not used and can be empty. 
  , databaseName :: Core.Maybe Core.Text
    -- ^ The database name on the MongoDB source endpoint. 
  , docsToInvestigate :: Core.Maybe Core.Text
    -- ^ Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ . 
--
-- Must be a positive value greater than @0@ . Default value is @1000@ .
  , extractDocId :: Core.Maybe Core.Text
    -- ^ Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ . 
--
-- Default value is @"false"@ . 
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
  , nestingLevel :: Core.Maybe Types.NestingLevelValue
    -- ^ Specifies either document or table mode. 
--
-- Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
  , password :: Core.Maybe Types.Password
    -- ^ The password for the user account you use to access the MongoDB source endpoint. 
  , port :: Core.Maybe Core.Int
    -- ^ The port value for the MongoDB source endpoint. 
  , serverName :: Core.Maybe Core.Text
    -- ^ The name of the server on the MongoDB source endpoint. 
  , username :: Core.Maybe Core.Text
    -- ^ The user name you use to access the MongoDB source endpoint. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MongoDbSettings' value with any optional fields omitted.
mkMongoDbSettings
    :: MongoDbSettings
mkMongoDbSettings
  = MongoDbSettings'{authMechanism = Core.Nothing,
                     authSource = Core.Nothing, authType = Core.Nothing,
                     databaseName = Core.Nothing, docsToInvestigate = Core.Nothing,
                     extractDocId = Core.Nothing, kmsKeyId = Core.Nothing,
                     nestingLevel = Core.Nothing, password = Core.Nothing,
                     port = Core.Nothing, serverName = Core.Nothing,
                     username = Core.Nothing}

-- | The authentication mechanism you use to access the MongoDB source endpoint.
--
-- For the default value, in MongoDB version 2.x, @"default"@ is @"mongodb_cr"@ . For MongoDB version 3.x or later, @"default"@ is @"scram_sha_1"@ . This setting isn't used when @AuthType@ is set to @"no"@ .
--
-- /Note:/ Consider using 'authMechanism' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsAuthMechanism :: Lens.Lens' MongoDbSettings (Core.Maybe Types.AuthMechanismValue)
mdsAuthMechanism = Lens.field @"authMechanism"
{-# INLINEABLE mdsAuthMechanism #-}
{-# DEPRECATED authMechanism "Use generic-lens or generic-optics with 'authMechanism' instead"  #-}

-- | The MongoDB database name. This setting isn't used when @AuthType@ is set to @"no"@ . 
--
-- The default is @"admin"@ .
--
-- /Note:/ Consider using 'authSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsAuthSource :: Lens.Lens' MongoDbSettings (Core.Maybe Core.Text)
mdsAuthSource = Lens.field @"authSource"
{-# INLINEABLE mdsAuthSource #-}
{-# DEPRECATED authSource "Use generic-lens or generic-optics with 'authSource' instead"  #-}

-- | The authentication type you use to access the MongoDB source endpoint.
--
-- When when set to @"no"@ , user name and password parameters are not used and can be empty. 
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsAuthType :: Lens.Lens' MongoDbSettings (Core.Maybe Types.AuthTypeValue)
mdsAuthType = Lens.field @"authType"
{-# INLINEABLE mdsAuthType #-}
{-# DEPRECATED authType "Use generic-lens or generic-optics with 'authType' instead"  #-}

-- | The database name on the MongoDB source endpoint. 
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsDatabaseName :: Lens.Lens' MongoDbSettings (Core.Maybe Core.Text)
mdsDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE mdsDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ . 
--
-- Must be a positive value greater than @0@ . Default value is @1000@ .
--
-- /Note:/ Consider using 'docsToInvestigate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsDocsToInvestigate :: Lens.Lens' MongoDbSettings (Core.Maybe Core.Text)
mdsDocsToInvestigate = Lens.field @"docsToInvestigate"
{-# INLINEABLE mdsDocsToInvestigate #-}
{-# DEPRECATED docsToInvestigate "Use generic-lens or generic-optics with 'docsToInvestigate' instead"  #-}

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ . 
--
-- Default value is @"false"@ . 
--
-- /Note:/ Consider using 'extractDocId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsExtractDocId :: Lens.Lens' MongoDbSettings (Core.Maybe Core.Text)
mdsExtractDocId = Lens.field @"extractDocId"
{-# INLINEABLE mdsExtractDocId #-}
{-# DEPRECATED extractDocId "Use generic-lens or generic-optics with 'extractDocId' instead"  #-}

-- | The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsKmsKeyId :: Lens.Lens' MongoDbSettings (Core.Maybe Core.Text)
mdsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE mdsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | Specifies either document or table mode. 
--
-- Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
--
-- /Note:/ Consider using 'nestingLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsNestingLevel :: Lens.Lens' MongoDbSettings (Core.Maybe Types.NestingLevelValue)
mdsNestingLevel = Lens.field @"nestingLevel"
{-# INLINEABLE mdsNestingLevel #-}
{-# DEPRECATED nestingLevel "Use generic-lens or generic-optics with 'nestingLevel' instead"  #-}

-- | The password for the user account you use to access the MongoDB source endpoint. 
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsPassword :: Lens.Lens' MongoDbSettings (Core.Maybe Types.Password)
mdsPassword = Lens.field @"password"
{-# INLINEABLE mdsPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The port value for the MongoDB source endpoint. 
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsPort :: Lens.Lens' MongoDbSettings (Core.Maybe Core.Int)
mdsPort = Lens.field @"port"
{-# INLINEABLE mdsPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The name of the server on the MongoDB source endpoint. 
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsServerName :: Lens.Lens' MongoDbSettings (Core.Maybe Core.Text)
mdsServerName = Lens.field @"serverName"
{-# INLINEABLE mdsServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The user name you use to access the MongoDB source endpoint. 
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsUsername :: Lens.Lens' MongoDbSettings (Core.Maybe Core.Text)
mdsUsername = Lens.field @"username"
{-# INLINEABLE mdsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON MongoDbSettings where
        toJSON MongoDbSettings{..}
          = Core.object
              (Core.catMaybes
                 [("AuthMechanism" Core..=) Core.<$> authMechanism,
                  ("AuthSource" Core..=) Core.<$> authSource,
                  ("AuthType" Core..=) Core.<$> authType,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("DocsToInvestigate" Core..=) Core.<$> docsToInvestigate,
                  ("ExtractDocId" Core..=) Core.<$> extractDocId,
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
                  ("NestingLevel" Core..=) Core.<$> nestingLevel,
                  ("Password" Core..=) Core.<$> password,
                  ("Port" Core..=) Core.<$> port,
                  ("ServerName" Core..=) Core.<$> serverName,
                  ("Username" Core..=) Core.<$> username])

instance Core.FromJSON MongoDbSettings where
        parseJSON
          = Core.withObject "MongoDbSettings" Core.$
              \ x ->
                MongoDbSettings' Core.<$>
                  (x Core..:? "AuthMechanism") Core.<*> x Core..:? "AuthSource"
                    Core.<*> x Core..:? "AuthType"
                    Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "DocsToInvestigate"
                    Core.<*> x Core..:? "ExtractDocId"
                    Core.<*> x Core..:? "KmsKeyId"
                    Core.<*> x Core..:? "NestingLevel"
                    Core.<*> x Core..:? "Password"
                    Core.<*> x Core..:? "Port"
                    Core.<*> x Core..:? "ServerName"
                    Core.<*> x Core..:? "Username"
