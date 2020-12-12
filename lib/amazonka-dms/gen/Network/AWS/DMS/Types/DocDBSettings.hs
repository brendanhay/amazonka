{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DocDBSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DocDBSettings
  ( DocDBSettings (..),

    -- * Smart constructor
    mkDocDBSettings,

    -- * Lenses
    ddsServerName,
    ddsUsername,
    ddsKMSKeyId,
    ddsPassword,
    ddsNestingLevel,
    ddsDatabaseName,
    ddsDocsToInvestigate,
    ddsExtractDocId,
    ddsPort,
  )
where

import Network.AWS.DMS.Types.NestingLevelValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines a DocumentDB endpoint.
--
-- /See:/ 'mkDocDBSettings' smart constructor.
data DocDBSettings = DocDBSettings'
  { serverName ::
      Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    nestingLevel :: Lude.Maybe NestingLevelValue,
    databaseName :: Lude.Maybe Lude.Text,
    docsToInvestigate :: Lude.Maybe Lude.Int,
    extractDocId :: Lude.Maybe Lude.Bool,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocDBSettings' with the minimum fields required to make a request.
--
-- * 'databaseName' - The database name on the DocumentDB source endpoint.
-- * 'docsToInvestigate' - Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .
--
-- Must be a positive value greater than @0@ . Default value is @1000@ .
-- * 'extractDocId' - Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .
--
-- Default value is @"false"@ .
-- * 'kmsKeyId' - The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'nestingLevel' - Specifies either document or table mode.
--
-- Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
-- * 'password' - The password for the user account you use to access the DocumentDB source endpoint.
-- * 'port' - The port value for the DocumentDB source endpoint.
-- * 'serverName' - The name of the server on the DocumentDB source endpoint.
-- * 'username' - The user name you use to access the DocumentDB source endpoint.
mkDocDBSettings ::
  DocDBSettings
mkDocDBSettings =
  DocDBSettings'
    { serverName = Lude.Nothing,
      username = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      password = Lude.Nothing,
      nestingLevel = Lude.Nothing,
      databaseName = Lude.Nothing,
      docsToInvestigate = Lude.Nothing,
      extractDocId = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The name of the server on the DocumentDB source endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsServerName :: Lens.Lens' DocDBSettings (Lude.Maybe Lude.Text)
ddsServerName = Lens.lens (serverName :: DocDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: DocDBSettings)
{-# DEPRECATED ddsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The user name you use to access the DocumentDB source endpoint.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsUsername :: Lens.Lens' DocDBSettings (Lude.Maybe Lude.Text)
ddsUsername = Lens.lens (username :: DocDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: DocDBSettings)
{-# DEPRECATED ddsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsKMSKeyId :: Lens.Lens' DocDBSettings (Lude.Maybe Lude.Text)
ddsKMSKeyId = Lens.lens (kmsKeyId :: DocDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DocDBSettings)
{-# DEPRECATED ddsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The password for the user account you use to access the DocumentDB source endpoint.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsPassword :: Lens.Lens' DocDBSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
ddsPassword = Lens.lens (password :: DocDBSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: DocDBSettings)
{-# DEPRECATED ddsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Specifies either document or table mode.
--
-- Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
--
-- /Note:/ Consider using 'nestingLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsNestingLevel :: Lens.Lens' DocDBSettings (Lude.Maybe NestingLevelValue)
ddsNestingLevel = Lens.lens (nestingLevel :: DocDBSettings -> Lude.Maybe NestingLevelValue) (\s a -> s {nestingLevel = a} :: DocDBSettings)
{-# DEPRECATED ddsNestingLevel "Use generic-lens or generic-optics with 'nestingLevel' instead." #-}

-- | The database name on the DocumentDB source endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDatabaseName :: Lens.Lens' DocDBSettings (Lude.Maybe Lude.Text)
ddsDatabaseName = Lens.lens (databaseName :: DocDBSettings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: DocDBSettings)
{-# DEPRECATED ddsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .
--
-- Must be a positive value greater than @0@ . Default value is @1000@ .
--
-- /Note:/ Consider using 'docsToInvestigate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDocsToInvestigate :: Lens.Lens' DocDBSettings (Lude.Maybe Lude.Int)
ddsDocsToInvestigate = Lens.lens (docsToInvestigate :: DocDBSettings -> Lude.Maybe Lude.Int) (\s a -> s {docsToInvestigate = a} :: DocDBSettings)
{-# DEPRECATED ddsDocsToInvestigate "Use generic-lens or generic-optics with 'docsToInvestigate' instead." #-}

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .
--
-- Default value is @"false"@ .
--
-- /Note:/ Consider using 'extractDocId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsExtractDocId :: Lens.Lens' DocDBSettings (Lude.Maybe Lude.Bool)
ddsExtractDocId = Lens.lens (extractDocId :: DocDBSettings -> Lude.Maybe Lude.Bool) (\s a -> s {extractDocId = a} :: DocDBSettings)
{-# DEPRECATED ddsExtractDocId "Use generic-lens or generic-optics with 'extractDocId' instead." #-}

-- | The port value for the DocumentDB source endpoint.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsPort :: Lens.Lens' DocDBSettings (Lude.Maybe Lude.Int)
ddsPort = Lens.lens (port :: DocDBSettings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: DocDBSettings)
{-# DEPRECATED ddsPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON DocDBSettings where
  parseJSON =
    Lude.withObject
      "DocDBSettings"
      ( \x ->
          DocDBSettings'
            Lude.<$> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "NestingLevel")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "DocsToInvestigate")
            Lude.<*> (x Lude..:? "ExtractDocId")
            Lude.<*> (x Lude..:? "Port")
      )

instance Lude.ToJSON DocDBSettings where
  toJSON DocDBSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServerName" Lude..=) Lude.<$> serverName,
            ("Username" Lude..=) Lude.<$> username,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("Password" Lude..=) Lude.<$> password,
            ("NestingLevel" Lude..=) Lude.<$> nestingLevel,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("DocsToInvestigate" Lude..=) Lude.<$> docsToInvestigate,
            ("ExtractDocId" Lude..=) Lude.<$> extractDocId,
            ("Port" Lude..=) Lude.<$> port
          ]
      )
