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
-- Module      : Amazonka.Kendra.Types.SalesforceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SalesforceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.SalesforceChatterFeedConfiguration
import Amazonka.Kendra.Types.SalesforceKnowledgeArticleConfiguration
import Amazonka.Kendra.Types.SalesforceStandardObjectAttachmentConfiguration
import Amazonka.Kendra.Types.SalesforceStandardObjectConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Salesforce as your
-- data source.
--
-- /See:/ 'newSalesforceConfiguration' smart constructor.
data SalesforceConfiguration = SalesforceConfiguration'
  { -- | A list of regular expression patterns to include certain documents in
    -- your Salesforce. Documents that match the patterns are included in the
    -- index. Documents that don\'t match the patterns are excluded from the
    -- index. If a document matches both an inclusion and exclusion pattern,
    -- the exclusion pattern takes precedence and the document isn\'t included
    -- in the index.
    --
    -- The pattern is applied to the name of the attached file.
    includeAttachmentFilePatterns :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether Amazon Kendra should index attachments to Salesforce
    -- objects.
    crawlAttachments :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns to exclude certain documents in
    -- your Salesforce. Documents that match the patterns are excluded from the
    -- index. Documents that don\'t match the patterns are included in the
    -- index. If a document matches both an inclusion and exclusion pattern,
    -- the exclusion pattern takes precedence and the document isn\'t included
    -- in the index.
    --
    -- The pattern is applied to the name of the attached file.
    excludeAttachmentFilePatterns :: Prelude.Maybe [Prelude.Text],
    -- | Configuration information for processing attachments to Salesforce
    -- standard objects.
    standardObjectAttachmentConfiguration :: Prelude.Maybe SalesforceStandardObjectAttachmentConfiguration,
    -- | Configuration information for Salesforce chatter feeds.
    chatterFeedConfiguration :: Prelude.Maybe SalesforceChatterFeedConfiguration,
    -- | Configuration information for the knowledge article types that Amazon
    -- Kendra indexes. Amazon Kendra indexes standard knowledge articles and
    -- the standard fields of knowledge articles, or the custom fields of
    -- custom knowledge articles, but not both.
    knowledgeArticleConfiguration :: Prelude.Maybe SalesforceKnowledgeArticleConfiguration,
    -- | Configuration of the Salesforce standard objects that Amazon Kendra
    -- indexes.
    standardObjectConfigurations :: Prelude.Maybe (Prelude.NonEmpty SalesforceStandardObjectConfiguration),
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
-- 'includeAttachmentFilePatterns', 'salesforceConfiguration_includeAttachmentFilePatterns' - A list of regular expression patterns to include certain documents in
-- your Salesforce. Documents that match the patterns are included in the
-- index. Documents that don\'t match the patterns are excluded from the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The pattern is applied to the name of the attached file.
--
-- 'crawlAttachments', 'salesforceConfiguration_crawlAttachments' - Indicates whether Amazon Kendra should index attachments to Salesforce
-- objects.
--
-- 'excludeAttachmentFilePatterns', 'salesforceConfiguration_excludeAttachmentFilePatterns' - A list of regular expression patterns to exclude certain documents in
-- your Salesforce. Documents that match the patterns are excluded from the
-- index. Documents that don\'t match the patterns are included in the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The pattern is applied to the name of the attached file.
--
-- 'standardObjectAttachmentConfiguration', 'salesforceConfiguration_standardObjectAttachmentConfiguration' - Configuration information for processing attachments to Salesforce
-- standard objects.
--
-- 'chatterFeedConfiguration', 'salesforceConfiguration_chatterFeedConfiguration' - Configuration information for Salesforce chatter feeds.
--
-- 'knowledgeArticleConfiguration', 'salesforceConfiguration_knowledgeArticleConfiguration' - Configuration information for the knowledge article types that Amazon
-- Kendra indexes. Amazon Kendra indexes standard knowledge articles and
-- the standard fields of knowledge articles, or the custom fields of
-- custom knowledge articles, but not both.
--
-- 'standardObjectConfigurations', 'salesforceConfiguration_standardObjectConfigurations' - Configuration of the Salesforce standard objects that Amazon Kendra
-- indexes.
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
    { includeAttachmentFilePatterns =
        Prelude.Nothing,
      crawlAttachments = Prelude.Nothing,
      excludeAttachmentFilePatterns = Prelude.Nothing,
      standardObjectAttachmentConfiguration =
        Prelude.Nothing,
      chatterFeedConfiguration = Prelude.Nothing,
      knowledgeArticleConfiguration = Prelude.Nothing,
      standardObjectConfigurations = Prelude.Nothing,
      serverUrl = pServerUrl_,
      secretArn = pSecretArn_
    }

-- | A list of regular expression patterns to include certain documents in
-- your Salesforce. Documents that match the patterns are included in the
-- index. Documents that don\'t match the patterns are excluded from the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The pattern is applied to the name of the attached file.
salesforceConfiguration_includeAttachmentFilePatterns :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe [Prelude.Text])
salesforceConfiguration_includeAttachmentFilePatterns = Lens.lens (\SalesforceConfiguration' {includeAttachmentFilePatterns} -> includeAttachmentFilePatterns) (\s@SalesforceConfiguration' {} a -> s {includeAttachmentFilePatterns = a} :: SalesforceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether Amazon Kendra should index attachments to Salesforce
-- objects.
salesforceConfiguration_crawlAttachments :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe Prelude.Bool)
salesforceConfiguration_crawlAttachments = Lens.lens (\SalesforceConfiguration' {crawlAttachments} -> crawlAttachments) (\s@SalesforceConfiguration' {} a -> s {crawlAttachments = a} :: SalesforceConfiguration)

-- | A list of regular expression patterns to exclude certain documents in
-- your Salesforce. Documents that match the patterns are excluded from the
-- index. Documents that don\'t match the patterns are included in the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The pattern is applied to the name of the attached file.
salesforceConfiguration_excludeAttachmentFilePatterns :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe [Prelude.Text])
salesforceConfiguration_excludeAttachmentFilePatterns = Lens.lens (\SalesforceConfiguration' {excludeAttachmentFilePatterns} -> excludeAttachmentFilePatterns) (\s@SalesforceConfiguration' {} a -> s {excludeAttachmentFilePatterns = a} :: SalesforceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information for processing attachments to Salesforce
-- standard objects.
salesforceConfiguration_standardObjectAttachmentConfiguration :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe SalesforceStandardObjectAttachmentConfiguration)
salesforceConfiguration_standardObjectAttachmentConfiguration = Lens.lens (\SalesforceConfiguration' {standardObjectAttachmentConfiguration} -> standardObjectAttachmentConfiguration) (\s@SalesforceConfiguration' {} a -> s {standardObjectAttachmentConfiguration = a} :: SalesforceConfiguration)

-- | Configuration information for Salesforce chatter feeds.
salesforceConfiguration_chatterFeedConfiguration :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe SalesforceChatterFeedConfiguration)
salesforceConfiguration_chatterFeedConfiguration = Lens.lens (\SalesforceConfiguration' {chatterFeedConfiguration} -> chatterFeedConfiguration) (\s@SalesforceConfiguration' {} a -> s {chatterFeedConfiguration = a} :: SalesforceConfiguration)

-- | Configuration information for the knowledge article types that Amazon
-- Kendra indexes. Amazon Kendra indexes standard knowledge articles and
-- the standard fields of knowledge articles, or the custom fields of
-- custom knowledge articles, but not both.
salesforceConfiguration_knowledgeArticleConfiguration :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe SalesforceKnowledgeArticleConfiguration)
salesforceConfiguration_knowledgeArticleConfiguration = Lens.lens (\SalesforceConfiguration' {knowledgeArticleConfiguration} -> knowledgeArticleConfiguration) (\s@SalesforceConfiguration' {} a -> s {knowledgeArticleConfiguration = a} :: SalesforceConfiguration)

-- | Configuration of the Salesforce standard objects that Amazon Kendra
-- indexes.
salesforceConfiguration_standardObjectConfigurations :: Lens.Lens' SalesforceConfiguration (Prelude.Maybe (Prelude.NonEmpty SalesforceStandardObjectConfiguration))
salesforceConfiguration_standardObjectConfigurations = Lens.lens (\SalesforceConfiguration' {standardObjectConfigurations} -> standardObjectConfigurations) (\s@SalesforceConfiguration' {} a -> s {standardObjectConfigurations = a} :: SalesforceConfiguration) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> ( x Core..:? "IncludeAttachmentFilePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CrawlAttachments")
            Prelude.<*> ( x Core..:? "ExcludeAttachmentFilePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "StandardObjectAttachmentConfiguration")
            Prelude.<*> (x Core..:? "ChatterFeedConfiguration")
            Prelude.<*> (x Core..:? "KnowledgeArticleConfiguration")
            Prelude.<*> (x Core..:? "StandardObjectConfigurations")
            Prelude.<*> (x Core..: "ServerUrl")
            Prelude.<*> (x Core..: "SecretArn")
      )

instance Prelude.Hashable SalesforceConfiguration where
  hashWithSalt _salt SalesforceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` includeAttachmentFilePatterns
      `Prelude.hashWithSalt` crawlAttachments
      `Prelude.hashWithSalt` excludeAttachmentFilePatterns
      `Prelude.hashWithSalt` standardObjectAttachmentConfiguration
      `Prelude.hashWithSalt` chatterFeedConfiguration
      `Prelude.hashWithSalt` knowledgeArticleConfiguration
      `Prelude.hashWithSalt` standardObjectConfigurations
      `Prelude.hashWithSalt` serverUrl
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData SalesforceConfiguration where
  rnf SalesforceConfiguration' {..} =
    Prelude.rnf includeAttachmentFilePatterns
      `Prelude.seq` Prelude.rnf crawlAttachments
      `Prelude.seq` Prelude.rnf excludeAttachmentFilePatterns
      `Prelude.seq` Prelude.rnf standardObjectAttachmentConfiguration
      `Prelude.seq` Prelude.rnf chatterFeedConfiguration
      `Prelude.seq` Prelude.rnf knowledgeArticleConfiguration
      `Prelude.seq` Prelude.rnf standardObjectConfigurations
      `Prelude.seq` Prelude.rnf serverUrl
      `Prelude.seq` Prelude.rnf secretArn

instance Core.ToJSON SalesforceConfiguration where
  toJSON SalesforceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IncludeAttachmentFilePatterns" Core..=)
              Prelude.<$> includeAttachmentFilePatterns,
            ("CrawlAttachments" Core..=)
              Prelude.<$> crawlAttachments,
            ("ExcludeAttachmentFilePatterns" Core..=)
              Prelude.<$> excludeAttachmentFilePatterns,
            ("StandardObjectAttachmentConfiguration" Core..=)
              Prelude.<$> standardObjectAttachmentConfiguration,
            ("ChatterFeedConfiguration" Core..=)
              Prelude.<$> chatterFeedConfiguration,
            ("KnowledgeArticleConfiguration" Core..=)
              Prelude.<$> knowledgeArticleConfiguration,
            ("StandardObjectConfigurations" Core..=)
              Prelude.<$> standardObjectConfigurations,
            Prelude.Just ("ServerUrl" Core..= serverUrl),
            Prelude.Just ("SecretArn" Core..= secretArn)
          ]
      )
