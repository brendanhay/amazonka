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
-- Module      : Amazonka.Kendra.Types.AlfrescoConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AlfrescoConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.AlfrescoEntity
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import Amazonka.Kendra.Types.S3Path
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Alfresco as your
-- data source.
--
-- Alfresco data source connector is currently in preview mode. Basic
-- authentication is currently supported. If you would like to use Alfresco
-- connector in production, contact
-- <http://aws.amazon.com/contact-us/ Support>.
--
-- /See:/ 'newAlfrescoConfiguration' smart constructor.
data AlfrescoConfiguration = AlfrescoConfiguration'
  { -- | Configuration information for an Amazon Virtual Private Cloud to connect
    -- to your Alfresco. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of Alfresco wikis to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to
    -- Alfresco fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Alfresco data source field names must exist in your Alfresco custom
    -- metadata.
    wikiFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of Alfresco document libraries to Amazon Kendra index field
    -- names. To create custom fields, use the @UpdateIndex@ API before you map
    -- to Alfresco fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Alfresco data source field names must exist in your Alfresco custom
    -- metadata.
    documentLibraryFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | @TRUE@ to index shared files.
    crawlSystemFolders :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns to include certain files in your
    -- Alfresco data source. Files that match the patterns are included in the
    -- index. Files that don\'t match the patterns are excluded from the index.
    -- If a file matches both an inclusion pattern and an exclusion pattern,
    -- the exclusion pattern takes precedence and the file isn\'t included in
    -- the index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Specify whether to index document libraries, wikis, or blogs. You can
    -- specify one or more of these options.
    entityFilter :: Prelude.Maybe (Prelude.NonEmpty AlfrescoEntity),
    -- | @TRUE@ to index comments of blogs and other content.
    crawlComments :: Prelude.Maybe Prelude.Bool,
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of Alfresco blogs to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to
    -- Alfresco fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Alfresco data source field names must exist in your Alfresco custom
    -- metadata.
    blogFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to exclude certain files in your
    -- Alfresco data source. Files that match the patterns are excluded from
    -- the index. Files that don\'t match the patterns are included in the
    -- index. If a file matches both an inclusion pattern and an exclusion
    -- pattern, the exclusion pattern takes precedence and the file isn\'t
    -- included in the index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The URL of the Alfresco site. For example, /https:\/\/hostname:8080/.
    siteUrl :: Prelude.Text,
    -- | The identifier of the Alfresco site. For example, /my-site/.
    siteId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
    -- contains the key-value pairs required to connect to your Alfresco data
    -- source. The secret must contain a JSON structure with the following
    -- keys:
    --
    -- -   username—The user name of the Alfresco account.
    --
    -- -   password—The password of the Alfresco account.
    secretArn :: Prelude.Text,
    -- | The path to the SSL certificate stored in an Amazon S3 bucket. You use
    -- this to connect to Alfresco if you require a secure SSL connection.
    --
    -- You can simply generate a self-signed X509 certificate on any computer
    -- using OpenSSL. For an example of using OpenSSL to create an X509
    -- certificate, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
    sslCertificateS3Path :: S3Path
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlfrescoConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfiguration', 'alfrescoConfiguration_vpcConfiguration' - Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Alfresco. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
--
-- 'wikiFieldMappings', 'alfrescoConfiguration_wikiFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Alfresco wikis to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to
-- Alfresco fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Alfresco data source field names must exist in your Alfresco custom
-- metadata.
--
-- 'documentLibraryFieldMappings', 'alfrescoConfiguration_documentLibraryFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Alfresco document libraries to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to Alfresco fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Alfresco data source field names must exist in your Alfresco custom
-- metadata.
--
-- 'crawlSystemFolders', 'alfrescoConfiguration_crawlSystemFolders' - @TRUE@ to index shared files.
--
-- 'inclusionPatterns', 'alfrescoConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain files in your
-- Alfresco data source. Files that match the patterns are included in the
-- index. Files that don\'t match the patterns are excluded from the index.
-- If a file matches both an inclusion pattern and an exclusion pattern,
-- the exclusion pattern takes precedence and the file isn\'t included in
-- the index.
--
-- 'entityFilter', 'alfrescoConfiguration_entityFilter' - Specify whether to index document libraries, wikis, or blogs. You can
-- specify one or more of these options.
--
-- 'crawlComments', 'alfrescoConfiguration_crawlComments' - @TRUE@ to index comments of blogs and other content.
--
-- 'blogFieldMappings', 'alfrescoConfiguration_blogFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Alfresco blogs to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to
-- Alfresco fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Alfresco data source field names must exist in your Alfresco custom
-- metadata.
--
-- 'exclusionPatterns', 'alfrescoConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain files in your
-- Alfresco data source. Files that match the patterns are excluded from
-- the index. Files that don\'t match the patterns are included in the
-- index. If a file matches both an inclusion pattern and an exclusion
-- pattern, the exclusion pattern takes precedence and the file isn\'t
-- included in the index.
--
-- 'siteUrl', 'alfrescoConfiguration_siteUrl' - The URL of the Alfresco site. For example, /https:\/\/hostname:8080/.
--
-- 'siteId', 'alfrescoConfiguration_siteId' - The identifier of the Alfresco site. For example, /my-site/.
--
-- 'secretArn', 'alfrescoConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your Alfresco data
-- source. The secret must contain a JSON structure with the following
-- keys:
--
-- -   username—The user name of the Alfresco account.
--
-- -   password—The password of the Alfresco account.
--
-- 'sslCertificateS3Path', 'alfrescoConfiguration_sslCertificateS3Path' - The path to the SSL certificate stored in an Amazon S3 bucket. You use
-- this to connect to Alfresco if you require a secure SSL connection.
--
-- You can simply generate a self-signed X509 certificate on any computer
-- using OpenSSL. For an example of using OpenSSL to create an X509
-- certificate, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
newAlfrescoConfiguration ::
  -- | 'siteUrl'
  Prelude.Text ->
  -- | 'siteId'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'sslCertificateS3Path'
  S3Path ->
  AlfrescoConfiguration
newAlfrescoConfiguration
  pSiteUrl_
  pSiteId_
  pSecretArn_
  pSslCertificateS3Path_ =
    AlfrescoConfiguration'
      { vpcConfiguration =
          Prelude.Nothing,
        wikiFieldMappings = Prelude.Nothing,
        documentLibraryFieldMappings = Prelude.Nothing,
        crawlSystemFolders = Prelude.Nothing,
        inclusionPatterns = Prelude.Nothing,
        entityFilter = Prelude.Nothing,
        crawlComments = Prelude.Nothing,
        blogFieldMappings = Prelude.Nothing,
        exclusionPatterns = Prelude.Nothing,
        siteUrl = pSiteUrl_,
        siteId = pSiteId_,
        secretArn = pSecretArn_,
        sslCertificateS3Path = pSslCertificateS3Path_
      }

-- | Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Alfresco. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
alfrescoConfiguration_vpcConfiguration :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
alfrescoConfiguration_vpcConfiguration = Lens.lens (\AlfrescoConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@AlfrescoConfiguration' {} a -> s {vpcConfiguration = a} :: AlfrescoConfiguration)

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Alfresco wikis to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to
-- Alfresco fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Alfresco data source field names must exist in your Alfresco custom
-- metadata.
alfrescoConfiguration_wikiFieldMappings :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
alfrescoConfiguration_wikiFieldMappings = Lens.lens (\AlfrescoConfiguration' {wikiFieldMappings} -> wikiFieldMappings) (\s@AlfrescoConfiguration' {} a -> s {wikiFieldMappings = a} :: AlfrescoConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Alfresco document libraries to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to Alfresco fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Alfresco data source field names must exist in your Alfresco custom
-- metadata.
alfrescoConfiguration_documentLibraryFieldMappings :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
alfrescoConfiguration_documentLibraryFieldMappings = Lens.lens (\AlfrescoConfiguration' {documentLibraryFieldMappings} -> documentLibraryFieldMappings) (\s@AlfrescoConfiguration' {} a -> s {documentLibraryFieldMappings = a} :: AlfrescoConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to index shared files.
alfrescoConfiguration_crawlSystemFolders :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe Prelude.Bool)
alfrescoConfiguration_crawlSystemFolders = Lens.lens (\AlfrescoConfiguration' {crawlSystemFolders} -> crawlSystemFolders) (\s@AlfrescoConfiguration' {} a -> s {crawlSystemFolders = a} :: AlfrescoConfiguration)

-- | A list of regular expression patterns to include certain files in your
-- Alfresco data source. Files that match the patterns are included in the
-- index. Files that don\'t match the patterns are excluded from the index.
-- If a file matches both an inclusion pattern and an exclusion pattern,
-- the exclusion pattern takes precedence and the file isn\'t included in
-- the index.
alfrescoConfiguration_inclusionPatterns :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe [Prelude.Text])
alfrescoConfiguration_inclusionPatterns = Lens.lens (\AlfrescoConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@AlfrescoConfiguration' {} a -> s {inclusionPatterns = a} :: AlfrescoConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specify whether to index document libraries, wikis, or blogs. You can
-- specify one or more of these options.
alfrescoConfiguration_entityFilter :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe (Prelude.NonEmpty AlfrescoEntity))
alfrescoConfiguration_entityFilter = Lens.lens (\AlfrescoConfiguration' {entityFilter} -> entityFilter) (\s@AlfrescoConfiguration' {} a -> s {entityFilter = a} :: AlfrescoConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to index comments of blogs and other content.
alfrescoConfiguration_crawlComments :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe Prelude.Bool)
alfrescoConfiguration_crawlComments = Lens.lens (\AlfrescoConfiguration' {crawlComments} -> crawlComments) (\s@AlfrescoConfiguration' {} a -> s {crawlComments = a} :: AlfrescoConfiguration)

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Alfresco blogs to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to
-- Alfresco fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Alfresco data source field names must exist in your Alfresco custom
-- metadata.
alfrescoConfiguration_blogFieldMappings :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
alfrescoConfiguration_blogFieldMappings = Lens.lens (\AlfrescoConfiguration' {blogFieldMappings} -> blogFieldMappings) (\s@AlfrescoConfiguration' {} a -> s {blogFieldMappings = a} :: AlfrescoConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to exclude certain files in your
-- Alfresco data source. Files that match the patterns are excluded from
-- the index. Files that don\'t match the patterns are included in the
-- index. If a file matches both an inclusion pattern and an exclusion
-- pattern, the exclusion pattern takes precedence and the file isn\'t
-- included in the index.
alfrescoConfiguration_exclusionPatterns :: Lens.Lens' AlfrescoConfiguration (Prelude.Maybe [Prelude.Text])
alfrescoConfiguration_exclusionPatterns = Lens.lens (\AlfrescoConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@AlfrescoConfiguration' {} a -> s {exclusionPatterns = a} :: AlfrescoConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the Alfresco site. For example, /https:\/\/hostname:8080/.
alfrescoConfiguration_siteUrl :: Lens.Lens' AlfrescoConfiguration Prelude.Text
alfrescoConfiguration_siteUrl = Lens.lens (\AlfrescoConfiguration' {siteUrl} -> siteUrl) (\s@AlfrescoConfiguration' {} a -> s {siteUrl = a} :: AlfrescoConfiguration)

-- | The identifier of the Alfresco site. For example, /my-site/.
alfrescoConfiguration_siteId :: Lens.Lens' AlfrescoConfiguration Prelude.Text
alfrescoConfiguration_siteId = Lens.lens (\AlfrescoConfiguration' {siteId} -> siteId) (\s@AlfrescoConfiguration' {} a -> s {siteId = a} :: AlfrescoConfiguration)

-- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your Alfresco data
-- source. The secret must contain a JSON structure with the following
-- keys:
--
-- -   username—The user name of the Alfresco account.
--
-- -   password—The password of the Alfresco account.
alfrescoConfiguration_secretArn :: Lens.Lens' AlfrescoConfiguration Prelude.Text
alfrescoConfiguration_secretArn = Lens.lens (\AlfrescoConfiguration' {secretArn} -> secretArn) (\s@AlfrescoConfiguration' {} a -> s {secretArn = a} :: AlfrescoConfiguration)

-- | The path to the SSL certificate stored in an Amazon S3 bucket. You use
-- this to connect to Alfresco if you require a secure SSL connection.
--
-- You can simply generate a self-signed X509 certificate on any computer
-- using OpenSSL. For an example of using OpenSSL to create an X509
-- certificate, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
alfrescoConfiguration_sslCertificateS3Path :: Lens.Lens' AlfrescoConfiguration S3Path
alfrescoConfiguration_sslCertificateS3Path = Lens.lens (\AlfrescoConfiguration' {sslCertificateS3Path} -> sslCertificateS3Path) (\s@AlfrescoConfiguration' {} a -> s {sslCertificateS3Path = a} :: AlfrescoConfiguration)

instance Data.FromJSON AlfrescoConfiguration where
  parseJSON =
    Data.withObject
      "AlfrescoConfiguration"
      ( \x ->
          AlfrescoConfiguration'
            Prelude.<$> (x Data..:? "VpcConfiguration")
            Prelude.<*> (x Data..:? "WikiFieldMappings")
            Prelude.<*> (x Data..:? "DocumentLibraryFieldMappings")
            Prelude.<*> (x Data..:? "CrawlSystemFolders")
            Prelude.<*> ( x Data..:? "InclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EntityFilter")
            Prelude.<*> (x Data..:? "CrawlComments")
            Prelude.<*> (x Data..:? "BlogFieldMappings")
            Prelude.<*> ( x Data..:? "ExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "SiteUrl")
            Prelude.<*> (x Data..: "SiteId")
            Prelude.<*> (x Data..: "SecretArn")
            Prelude.<*> (x Data..: "SslCertificateS3Path")
      )

instance Prelude.Hashable AlfrescoConfiguration where
  hashWithSalt _salt AlfrescoConfiguration' {..} =
    _salt `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` wikiFieldMappings
      `Prelude.hashWithSalt` documentLibraryFieldMappings
      `Prelude.hashWithSalt` crawlSystemFolders
      `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` entityFilter
      `Prelude.hashWithSalt` crawlComments
      `Prelude.hashWithSalt` blogFieldMappings
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` siteUrl
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` sslCertificateS3Path

instance Prelude.NFData AlfrescoConfiguration where
  rnf AlfrescoConfiguration' {..} =
    Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf wikiFieldMappings
      `Prelude.seq` Prelude.rnf documentLibraryFieldMappings
      `Prelude.seq` Prelude.rnf crawlSystemFolders
      `Prelude.seq` Prelude.rnf inclusionPatterns
      `Prelude.seq` Prelude.rnf entityFilter
      `Prelude.seq` Prelude.rnf crawlComments
      `Prelude.seq` Prelude.rnf blogFieldMappings
      `Prelude.seq` Prelude.rnf exclusionPatterns
      `Prelude.seq` Prelude.rnf siteUrl
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf sslCertificateS3Path

instance Data.ToJSON AlfrescoConfiguration where
  toJSON AlfrescoConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            ("WikiFieldMappings" Data..=)
              Prelude.<$> wikiFieldMappings,
            ("DocumentLibraryFieldMappings" Data..=)
              Prelude.<$> documentLibraryFieldMappings,
            ("CrawlSystemFolders" Data..=)
              Prelude.<$> crawlSystemFolders,
            ("InclusionPatterns" Data..=)
              Prelude.<$> inclusionPatterns,
            ("EntityFilter" Data..=) Prelude.<$> entityFilter,
            ("CrawlComments" Data..=) Prelude.<$> crawlComments,
            ("BlogFieldMappings" Data..=)
              Prelude.<$> blogFieldMappings,
            ("ExclusionPatterns" Data..=)
              Prelude.<$> exclusionPatterns,
            Prelude.Just ("SiteUrl" Data..= siteUrl),
            Prelude.Just ("SiteId" Data..= siteId),
            Prelude.Just ("SecretArn" Data..= secretArn),
            Prelude.Just
              ( "SslCertificateS3Path"
                  Data..= sslCertificateS3Path
              )
          ]
      )
