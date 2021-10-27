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
-- Module      : Network.AWS.Kendra.Types.SalesforceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SalesforceConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.SalesforceChatterFeedConfiguration
import Network.AWS.Kendra.Types.SalesforceKnowledgeArticleConfiguration
import Network.AWS.Kendra.Types.SalesforceStandardObjectAttachmentConfiguration
import Network.AWS.Kendra.Types.SalesforceStandardObjectConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for connecting to a Salesforce data
-- source.
--
-- /See:/ 'newSalesforceConfiguration' smart constructor.
data SalesforceConfiguration = SalesforceConfiguration'
  { -- | Specifies configuration information for the knowledge article types that
    -- Amazon Kendra indexes. Amazon Kendra indexes standard knowledge articles
    -- and the standard fields of knowledge articles, or the custom fields of
    -- custom knowledge articles, but not both.
    knowledgeArticleConfiguration :: Prelude.Maybe SalesforceKnowledgeArticleConfiguration,
    -- | Indicates whether Amazon Kendra should index attachments to Salesforce
    -- objects.
    crawlAttachments :: Prelude.Maybe Prelude.Bool,
    -- | Provides configuration information for processing attachments to
    -- Salesforce standard objects.
    standardObjectAttachmentConfiguration :: Prelude.Maybe SalesforceStandardObjectAttachmentConfiguration,
    -- | Specifies configuration information for Salesforce chatter feeds.
    chatterFeedConfiguration :: Prelude.Maybe SalesforceChatterFeedConfiguration,
    -- | A list of regular expression patterns. Documents that match the patterns
    -- are excluded from the index. Documents that don\'t match the patterns
    -- are included in the index. If a document matches both an exclusion
    -- pattern and an inclusion pattern, the document is not included in the
    -- index.
    --
    -- The regex is applied to the name of the attached file.
    excludeAttachmentFilePatterns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the Salesforce standard objects that Amazon Kendra indexes.
    standardObjectConfigurations :: Prelude.Maybe (Prelude.NonEmpty SalesforceStandardObjectConfiguration),
    -- | A list of regular expression patterns. Documents that match the patterns
    -- are included in the index. Documents that don\'t match the patterns are
    -- excluded from the index. If a document matches both an inclusion pattern
    -- and an exclusion pattern, the document is not included in the index.
    --
    -- The regex is applied to the name of the attached file.
    includeAttachmentFilePatterns :: Prelude.Maybe [Prelude.Text],
    -- | The instance URL for the Salesforce site that you want to index.
    serverUrl :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
    -- the key\/value pairs required to connect to your Salesforce instance.
    -- The secret must contain a JSON structure with the following keys:
    --
    -- -   authenticationUrl - The OAUTH endpoint that Amazon Kendra connects
    --     to get an OAUTH token.
    --
    -- -   consumerKey - The application public key generated when you created
    --     your Salesforce application.
    --
    -- -   consumerSecret - The application private key generated when you
    --     created your Salesforce application.
    --
    -- -   password - The password associated with the user logging in to the
    --     Salesforce instance.
    --
    -- -   securityToken - The token associated with the user account logging
    --     in to the Salesforce instance.
    --
    -- -   username - The user name of the user logging in to the Salesforce
    --     instance.
    secretArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeArticleConfiguration', 'salesforceConfiguration_knowledgeArticleConfiguration' - Specifies configuration information for the knowledge article types that
-- Amazon Kendra indexes. Amazon Kendra indexes standard knowledge articles
-- and the standard fields of knowledge articles, or the custom fields of
-- custom knowledge articles, but not both.
--
-- 'crawlAttachments', 'salesforceConfiguration_crawlAttachments' - Indicates whether Amazon Kendra should index attachments to Salesforce
-- objects.
--
-- 'standardObjectAttachmentConfiguration', 'salesforceConfiguration_standardObjectAttachmentConfiguration' - Provides configuration information for processing attachments to
-- Salesforce standard objects.
--
-- 'chatterFeedConfiguration', 'salesforceConfiguration_chatterFeedConfiguration' - Specifies configuration information for Salesforce chatter feeds.
--
-- 'excludeAttachmentFilePatterns', 'salesforceConfiguration_excludeAttachmentFilePatterns' - A list of regular expression patterns. Documents that match the patterns
-- are excluded from the index. Documents that don\'t match the patterns
-- are included in the index. If a document matches both an exclusion
-- pattern and an inclusion pattern, the document is not included in the
-- index.
--
-- The regex is applied to the name of the attached file.
--
-- 'standardObjectConfigurations', 'salesforceConfiguration_standardObjectConfigurations' - Specifies the Salesforce standard objects that Amazon Kendra indexes.
--
-- 'includeAttachmentFilePatterns', 'salesforceConfiguration_includeAttachmentFilePatterns' - A list of regular expression patterns. Documents that match the patterns
-- are included in the index. Documents that don\'t match the patterns are
-- excluded from the index. If a document matches both an inclusion pattern
-- and an exclusion pattern, the document is not included in the index.
--
-- The regex is applied to the name of the attached file.
--
-- 'serverUrl', 'salesforceConfiguration_serverUrl' - The instance URL for the Salesforce site that you want to index.
--
-- 'secretArn', 'salesforceConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
-- the key\/value pairs required to connect to your Salesforce instance.
-- The secret must contain a JSON structure with the following keys:
--
-- -   authenticationUrl - The OAUTH endpoint that Amazon Kendra connects
--     to get an OAUTH token.
--
-- -   consumerKey - The application public key generated when you created
--     your Salesforce application.
--
-- -   consumerSecret - The application private key generated when you
--     created your Salesforce application.
--
-- -   password - The password associated with the user logging in to the
--     Salesforce instance.
--
-- -   securityToken - The token associated with the user account logging
--     in to the Salesforce instance.
--
-- -   username - The user name of the user logging in to the Salesforce
--     instance.
newSalesforceConfiguration ::
  -- | 'serverUrl'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  SalesforceConfiguration
newSalesforceConfiguration pServerUrl_ pSecretArn_ =
  SalesforceConfiguration'
    { knowledgeArticleConfiguration =
        Prelude.Nothing,
      crawlAttachments = Prelude.Nothing,
      standardObjectAttachmentConfiguration =
        Prelude.Nothing,
      chatterFeedConfiguration = Prelude.Nothing,
      excludeAttachmentFilePatterns = Prelude.Nothing,
      standardObjectConfigurations = Prelude.Nothing,
      includeAttachmentFilePatterns = Prelude.Nothing,
      serverUrl = pServerUrl_,
      secretArn = pSecretArn_
    }

-- | Specifies configuration information for the knowledge article types that
-- Amazon Kendra indexes. Amazon Kendra indexes standard knowledge articles
-- and the standard fields of knowledge articles, or the custom fields of
-- custom knowledge articles, but not both.
salesforceConfiguration_knowledgeArticleConfiguration :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe SalesforceKnowledgeArticleConfiguration)
salesforceConfiguration_knowledgeArticleConfiguration = Lens.lens (\SalesforceConfiguration' {knowledgeArticleConfiguration} -> knowledgeArticleConfiguration) (\s@SalesforceConfiguration' {} a -> s {knowledgeArticleConfiguration = a} :: SalesforceConfiguration)

-- | Indicates whether Amazon Kendra should index attachments to Salesforce
-- objects.
salesforceConfiguration_crawlAttachments :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe Prelude.Bool)
salesforceConfiguration_crawlAttachments = Lens.lens (\SalesforceConfiguration' {crawlAttachments} -> crawlAttachments) (\s@SalesforceConfiguration' {} a -> s {crawlAttachments = a} :: SalesforceConfiguration)

-- | Provides configuration information for processing attachments to
-- Salesforce standard objects.
salesforceConfiguration_standardObjectAttachmentConfiguration :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe SalesforceStandardObjectAttachmentConfiguration)
salesforceConfiguration_standardObjectAttachmentConfiguration = Lens.lens (\SalesforceConfiguration' {standardObjectAttachmentConfiguration} -> standardObjectAttachmentConfiguration) (\s@SalesforceConfiguration' {} a -> s {standardObjectAttachmentConfiguration = a} :: SalesforceConfiguration)

-- | Specifies configuration information for Salesforce chatter feeds.
salesforceConfiguration_chatterFeedConfiguration :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe SalesforceChatterFeedConfiguration)
salesforceConfiguration_chatterFeedConfiguration = Lens.lens (\SalesforceConfiguration' {chatterFeedConfiguration} -> chatterFeedConfiguration) (\s@SalesforceConfiguration' {} a -> s {chatterFeedConfiguration = a} :: SalesforceConfiguration)

-- | A list of regular expression patterns. Documents that match the patterns
-- are excluded from the index. Documents that don\'t match the patterns
-- are included in the index. If a document matches both an exclusion
-- pattern and an inclusion pattern, the document is not included in the
-- index.
--
-- The regex is applied to the name of the attached file.
salesforceConfiguration_excludeAttachmentFilePatterns :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe [Prelude.Text])
salesforceConfiguration_excludeAttachmentFilePatterns = Lens.lens (\SalesforceConfiguration' {excludeAttachmentFilePatterns} -> excludeAttachmentFilePatterns) (\s@SalesforceConfiguration' {} a -> s {excludeAttachmentFilePatterns = a} :: SalesforceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the Salesforce standard objects that Amazon Kendra indexes.
salesforceConfiguration_standardObjectConfigurations :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe (Prelude.NonEmpty SalesforceStandardObjectConfiguration))
salesforceConfiguration_standardObjectConfigurations = Lens.lens (\SalesforceConfiguration' {standardObjectConfigurations} -> standardObjectConfigurations) (\s@SalesforceConfiguration' {} a -> s {standardObjectConfigurations = a} :: SalesforceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns. Documents that match the patterns
-- are included in the index. Documents that don\'t match the patterns are
-- excluded from the index. If a document matches both an inclusion pattern
-- and an exclusion pattern, the document is not included in the index.
--
-- The regex is applied to the name of the attached file.
salesforceConfiguration_includeAttachmentFilePatterns :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe [Prelude.Text])
salesforceConfiguration_includeAttachmentFilePatterns = Lens.lens (\SalesforceConfiguration' {includeAttachmentFilePatterns} -> includeAttachmentFilePatterns) (\s@SalesforceConfiguration' {} a -> s {includeAttachmentFilePatterns = a} :: SalesforceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The instance URL for the Salesforce site that you want to index.
salesforceConfiguration_serverUrl :: Lens.Lens' SalesforceConfiguration Prelude.Text
salesforceConfiguration_serverUrl = Lens.lens (\SalesforceConfiguration' {serverUrl} -> serverUrl) (\s@SalesforceConfiguration' {} a -> s {serverUrl = a} :: SalesforceConfiguration)

-- | The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
-- the key\/value pairs required to connect to your Salesforce instance.
-- The secret must contain a JSON structure with the following keys:
--
-- -   authenticationUrl - The OAUTH endpoint that Amazon Kendra connects
--     to get an OAUTH token.
--
-- -   consumerKey - The application public key generated when you created
--     your Salesforce application.
--
-- -   consumerSecret - The application private key generated when you
--     created your Salesforce application.
--
-- -   password - The password associated with the user logging in to the
--     Salesforce instance.
--
-- -   securityToken - The token associated with the user account logging
--     in to the Salesforce instance.
--
-- -   username - The user name of the user logging in to the Salesforce
--     instance.
salesforceConfiguration_secretArn :: Lens.Lens' SalesforceConfiguration Prelude.Text
salesforceConfiguration_secretArn = Lens.lens (\SalesforceConfiguration' {secretArn} -> secretArn) (\s@SalesforceConfiguration' {} a -> s {secretArn = a} :: SalesforceConfiguration)

instance Core.FromJSON SalesforceConfiguration where
  parseJSON =
    Core.withObject
      "SalesforceConfiguration"
      ( \x ->
          SalesforceConfiguration'
            Prelude.<$> (x Core..:? "KnowledgeArticleConfiguration")
            Prelude.<*> (x Core..:? "CrawlAttachments")
            Prelude.<*> (x Core..:? "StandardObjectAttachmentConfiguration")
            Prelude.<*> (x Core..:? "ChatterFeedConfiguration")
            Prelude.<*> ( x Core..:? "ExcludeAttachmentFilePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "StandardObjectConfigurations")
            Prelude.<*> ( x Core..:? "IncludeAttachmentFilePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "ServerUrl")
            Prelude.<*> (x Core..: "SecretArn")
      )

instance Prelude.Hashable SalesforceConfiguration

instance Prelude.NFData SalesforceConfiguration

instance Core.ToJSON SalesforceConfiguration where
  toJSON SalesforceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KnowledgeArticleConfiguration" Core..=)
              Prelude.<$> knowledgeArticleConfiguration,
            ("CrawlAttachments" Core..=)
              Prelude.<$> crawlAttachments,
            ("StandardObjectAttachmentConfiguration" Core..=)
              Prelude.<$> standardObjectAttachmentConfiguration,
            ("ChatterFeedConfiguration" Core..=)
              Prelude.<$> chatterFeedConfiguration,
            ("ExcludeAttachmentFilePatterns" Core..=)
              Prelude.<$> excludeAttachmentFilePatterns,
            ("StandardObjectConfigurations" Core..=)
              Prelude.<$> standardObjectConfigurations,
            ("IncludeAttachmentFilePatterns" Core..=)
              Prelude.<$> includeAttachmentFilePatterns,
            Prelude.Just ("ServerUrl" Core..= serverUrl),
            Prelude.Just ("SecretArn" Core..= secretArn)
          ]
      )
