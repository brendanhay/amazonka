{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DocDbSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.DocDbSettings
  ( DocDbSettings (..)
  -- * Smart constructor
  , mkDocDbSettings
  -- * Lenses
  , ddsDatabaseName
  , ddsDocsToInvestigate
  , ddsExtractDocId
  , ddsKmsKeyId
  , ddsNestingLevel
  , ddsPassword
  , ddsPort
  , ddsServerName
  , ddsUsername
  ) where

import qualified Network.AWS.DMS.Types.NestingLevelValue as Types
import qualified Network.AWS.DMS.Types.Password as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines a DocumentDB endpoint.
--
-- /See:/ 'mkDocDbSettings' smart constructor.
data DocDbSettings = DocDbSettings'
  { databaseName :: Core.Maybe Core.Text
    -- ^ The database name on the DocumentDB source endpoint. 
  , docsToInvestigate :: Core.Maybe Core.Int
    -- ^ Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ . 
--
-- Must be a positive value greater than @0@ . Default value is @1000@ .
  , extractDocId :: Core.Maybe Core.Bool
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
    -- ^ The password for the user account you use to access the DocumentDB source endpoint. 
  , port :: Core.Maybe Core.Int
    -- ^ The port value for the DocumentDB source endpoint. 
  , serverName :: Core.Maybe Core.Text
    -- ^ The name of the server on the DocumentDB source endpoint. 
  , username :: Core.Maybe Core.Text
    -- ^ The user name you use to access the DocumentDB source endpoint. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocDbSettings' value with any optional fields omitted.
mkDocDbSettings
    :: DocDbSettings
mkDocDbSettings
  = DocDbSettings'{databaseName = Core.Nothing,
                   docsToInvestigate = Core.Nothing, extractDocId = Core.Nothing,
                   kmsKeyId = Core.Nothing, nestingLevel = Core.Nothing,
                   password = Core.Nothing, port = Core.Nothing,
                   serverName = Core.Nothing, username = Core.Nothing}

-- | The database name on the DocumentDB source endpoint. 
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDatabaseName :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
ddsDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE ddsDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ . 
--
-- Must be a positive value greater than @0@ . Default value is @1000@ .
--
-- /Note:/ Consider using 'docsToInvestigate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDocsToInvestigate :: Lens.Lens' DocDbSettings (Core.Maybe Core.Int)
ddsDocsToInvestigate = Lens.field @"docsToInvestigate"
{-# INLINEABLE ddsDocsToInvestigate #-}
{-# DEPRECATED docsToInvestigate "Use generic-lens or generic-optics with 'docsToInvestigate' instead"  #-}

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ . 
--
-- Default value is @"false"@ . 
--
-- /Note:/ Consider using 'extractDocId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsExtractDocId :: Lens.Lens' DocDbSettings (Core.Maybe Core.Bool)
ddsExtractDocId = Lens.field @"extractDocId"
{-# INLINEABLE ddsExtractDocId #-}
{-# DEPRECATED extractDocId "Use generic-lens or generic-optics with 'extractDocId' instead"  #-}

-- | The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsKmsKeyId :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
ddsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE ddsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | Specifies either document or table mode. 
--
-- Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
--
-- /Note:/ Consider using 'nestingLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsNestingLevel :: Lens.Lens' DocDbSettings (Core.Maybe Types.NestingLevelValue)
ddsNestingLevel = Lens.field @"nestingLevel"
{-# INLINEABLE ddsNestingLevel #-}
{-# DEPRECATED nestingLevel "Use generic-lens or generic-optics with 'nestingLevel' instead"  #-}

-- | The password for the user account you use to access the DocumentDB source endpoint. 
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsPassword :: Lens.Lens' DocDbSettings (Core.Maybe Types.Password)
ddsPassword = Lens.field @"password"
{-# INLINEABLE ddsPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The port value for the DocumentDB source endpoint. 
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsPort :: Lens.Lens' DocDbSettings (Core.Maybe Core.Int)
ddsPort = Lens.field @"port"
{-# INLINEABLE ddsPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The name of the server on the DocumentDB source endpoint. 
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsServerName :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
ddsServerName = Lens.field @"serverName"
{-# INLINEABLE ddsServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The user name you use to access the DocumentDB source endpoint. 
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsUsername :: Lens.Lens' DocDbSettings (Core.Maybe Core.Text)
ddsUsername = Lens.field @"username"
{-# INLINEABLE ddsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON DocDbSettings where
        toJSON DocDbSettings{..}
          = Core.object
              (Core.catMaybes
                 [("DatabaseName" Core..=) Core.<$> databaseName,
                  ("DocsToInvestigate" Core..=) Core.<$> docsToInvestigate,
                  ("ExtractDocId" Core..=) Core.<$> extractDocId,
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
                  ("NestingLevel" Core..=) Core.<$> nestingLevel,
                  ("Password" Core..=) Core.<$> password,
                  ("Port" Core..=) Core.<$> port,
                  ("ServerName" Core..=) Core.<$> serverName,
                  ("Username" Core..=) Core.<$> username])

instance Core.FromJSON DocDbSettings where
        parseJSON
          = Core.withObject "DocDbSettings" Core.$
              \ x ->
                DocDbSettings' Core.<$>
                  (x Core..:? "DatabaseName") Core.<*> x Core..:? "DocsToInvestigate"
                    Core.<*> x Core..:? "ExtractDocId"
                    Core.<*> x Core..:? "KmsKeyId"
                    Core.<*> x Core..:? "NestingLevel"
                    Core.<*> x Core..:? "Password"
                    Core.<*> x Core..:? "Port"
                    Core.<*> x Core..:? "ServerName"
                    Core.<*> x Core..:? "Username"
