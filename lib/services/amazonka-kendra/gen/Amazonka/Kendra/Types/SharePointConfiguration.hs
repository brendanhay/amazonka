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
-- Module      : Amazonka.Kendra.Types.SharePointConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SharePointConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import Amazonka.Kendra.Types.ProxyConfiguration
import Amazonka.Kendra.Types.S3Path
import Amazonka.Kendra.Types.SharePointOnlineAuthenticationType
import Amazonka.Kendra.Types.SharePointVersion
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Microsoft
-- SharePoint as your data source.
--
-- /See:/ 'newSharePointConfiguration' smart constructor.
data SharePointConfiguration = SharePointConfiguration'
  { -- | Whether you want to connect to SharePoint using basic authentication of
    -- user name and password, or OAuth authentication of user name, password,
    -- client ID, and client secret. You can use OAuth authentication for
    -- SharePoint Online.
    authenticationType :: Prelude.Maybe SharePointOnlineAuthenticationType,
    -- | @TRUE@ to index document attachments.
    crawlAttachments :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to disable local groups information.
    disableLocalGroups :: Prelude.Maybe Prelude.Bool,
    -- | The Microsoft SharePoint attribute field that contains the title of the
    -- document.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
    -- | A list of regular expression patterns to exclude certain documents in
    -- your SharePoint. Documents that match the patterns are excluded from the
    -- index. Documents that don\'t match the patterns are included in the
    -- index. If a document matches both an inclusion and exclusion pattern,
    -- the exclusion pattern takes precedence and the document isn\'t included
    -- in the index.
    --
    -- The regex applies to the display URL of the SharePoint document.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map SharePoint
    -- data source attributes or field names to Amazon Kendra index field
    -- names. To create custom fields, use the @UpdateIndex@ API before you map
    -- to SharePoint fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The SharePoint data source field names must exist in your SharePoint
    -- custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to include certain documents in
    -- your SharePoint. Documents that match the patterns are included in the
    -- index. Documents that don\'t match the patterns are excluded from the
    -- index. If a document matches both an inclusion and exclusion pattern,
    -- the exclusion pattern takes precedence and the document isn\'t included
    -- in the index.
    --
    -- The regex applies to the display URL of the SharePoint document.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Configuration information to connect to your Microsoft SharePoint site
    -- URLs via instance via a web proxy. You can use this option for
    -- SharePoint Server.
    --
    -- You must provide the website host name and port number. For example, the
    -- host name of /https:\/\/a.example.com\/page1.html/ is \"a.example.com\"
    -- and the port is 443, the standard port for HTTPS.
    --
    -- Web proxy credentials are optional and you can use them to connect to a
    -- web proxy server that requires basic authentication of user name and
    -- password. To store web proxy credentials, you use a secret in Secrets
    -- Manager.
    --
    -- It is recommended that you follow best security practices when
    -- configuring your web proxy. This includes setting up throttling, setting
    -- up logging and monitoring, and applying security patches on a regular
    -- basis. If you use your web proxy with multiple data sources, sync jobs
    -- that occur at the same time could strain the load on your proxy. It is
    -- recommended you prepare your proxy beforehand for any security and load
    -- requirements.
    proxyConfiguration :: Prelude.Maybe ProxyConfiguration,
    -- | The path to the SSL certificate stored in an Amazon S3 bucket. You use
    -- this to connect to SharePoint Server if you require a secure SSL
    -- connection.
    --
    -- You can simply generate a self-signed X509 certificate on any computer
    -- using OpenSSL. For an example of using OpenSSL to create an X509
    -- certificate, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
    sslCertificateS3Path :: Prelude.Maybe S3Path,
    -- | @TRUE@ to use the SharePoint change log to determine which documents
    -- require updating in the index. Depending on the change log\'s size, it
    -- may take longer for Amazon Kendra to use the change log than to scan all
    -- of your documents in SharePoint.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | Configuration information for an Amazon Virtual Private Cloud to connect
    -- to your Microsoft SharePoint. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | The version of Microsoft SharePoint that you use.
    sharePointVersion :: SharePointVersion,
    -- | The Microsoft SharePoint site URLs for the documents you want to index.
    urls :: Prelude.NonEmpty Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
    -- contains the user name and password required to connect to the
    -- SharePoint instance. If you use SharePoint Server, you also need to
    -- provide the sever domain name as part of the credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a Microsoft SharePoint Data Source>.
    --
    -- You can also provide OAuth authentication credentials of user name,
    -- password, client ID, and client secret. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a SharePoint data source>.
    secretArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SharePointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationType', 'sharePointConfiguration_authenticationType' - Whether you want to connect to SharePoint using basic authentication of
-- user name and password, or OAuth authentication of user name, password,
-- client ID, and client secret. You can use OAuth authentication for
-- SharePoint Online.
--
-- 'crawlAttachments', 'sharePointConfiguration_crawlAttachments' - @TRUE@ to index document attachments.
--
-- 'disableLocalGroups', 'sharePointConfiguration_disableLocalGroups' - @TRUE@ to disable local groups information.
--
-- 'documentTitleFieldName', 'sharePointConfiguration_documentTitleFieldName' - The Microsoft SharePoint attribute field that contains the title of the
-- document.
--
-- 'exclusionPatterns', 'sharePointConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain documents in
-- your SharePoint. Documents that match the patterns are excluded from the
-- index. Documents that don\'t match the patterns are included in the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The regex applies to the display URL of the SharePoint document.
--
-- 'fieldMappings', 'sharePointConfiguration_fieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map SharePoint
-- data source attributes or field names to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to SharePoint fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The SharePoint data source field names must exist in your SharePoint
-- custom metadata.
--
-- 'inclusionPatterns', 'sharePointConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain documents in
-- your SharePoint. Documents that match the patterns are included in the
-- index. Documents that don\'t match the patterns are excluded from the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The regex applies to the display URL of the SharePoint document.
--
-- 'proxyConfiguration', 'sharePointConfiguration_proxyConfiguration' - Configuration information to connect to your Microsoft SharePoint site
-- URLs via instance via a web proxy. You can use this option for
-- SharePoint Server.
--
-- You must provide the website host name and port number. For example, the
-- host name of /https:\/\/a.example.com\/page1.html/ is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS.
--
-- Web proxy credentials are optional and you can use them to connect to a
-- web proxy server that requires basic authentication of user name and
-- password. To store web proxy credentials, you use a secret in Secrets
-- Manager.
--
-- It is recommended that you follow best security practices when
-- configuring your web proxy. This includes setting up throttling, setting
-- up logging and monitoring, and applying security patches on a regular
-- basis. If you use your web proxy with multiple data sources, sync jobs
-- that occur at the same time could strain the load on your proxy. It is
-- recommended you prepare your proxy beforehand for any security and load
-- requirements.
--
-- 'sslCertificateS3Path', 'sharePointConfiguration_sslCertificateS3Path' - The path to the SSL certificate stored in an Amazon S3 bucket. You use
-- this to connect to SharePoint Server if you require a secure SSL
-- connection.
--
-- You can simply generate a self-signed X509 certificate on any computer
-- using OpenSSL. For an example of using OpenSSL to create an X509
-- certificate, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
--
-- 'useChangeLog', 'sharePointConfiguration_useChangeLog' - @TRUE@ to use the SharePoint change log to determine which documents
-- require updating in the index. Depending on the change log\'s size, it
-- may take longer for Amazon Kendra to use the change log than to scan all
-- of your documents in SharePoint.
--
-- 'vpcConfiguration', 'sharePointConfiguration_vpcConfiguration' - Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Microsoft SharePoint. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
--
-- 'sharePointVersion', 'sharePointConfiguration_sharePointVersion' - The version of Microsoft SharePoint that you use.
--
-- 'urls', 'sharePointConfiguration_urls' - The Microsoft SharePoint site URLs for the documents you want to index.
--
-- 'secretArn', 'sharePointConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the user name and password required to connect to the
-- SharePoint instance. If you use SharePoint Server, you also need to
-- provide the sever domain name as part of the credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a Microsoft SharePoint Data Source>.
--
-- You can also provide OAuth authentication credentials of user name,
-- password, client ID, and client secret. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a SharePoint data source>.
newSharePointConfiguration ::
  -- | 'sharePointVersion'
  SharePointVersion ->
  -- | 'urls'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  SharePointConfiguration
newSharePointConfiguration
  pSharePointVersion_
  pUrls_
  pSecretArn_ =
    SharePointConfiguration'
      { authenticationType =
          Prelude.Nothing,
        crawlAttachments = Prelude.Nothing,
        disableLocalGroups = Prelude.Nothing,
        documentTitleFieldName = Prelude.Nothing,
        exclusionPatterns = Prelude.Nothing,
        fieldMappings = Prelude.Nothing,
        inclusionPatterns = Prelude.Nothing,
        proxyConfiguration = Prelude.Nothing,
        sslCertificateS3Path = Prelude.Nothing,
        useChangeLog = Prelude.Nothing,
        vpcConfiguration = Prelude.Nothing,
        sharePointVersion = pSharePointVersion_,
        urls = Lens.coerced Lens.# pUrls_,
        secretArn = pSecretArn_
      }

-- | Whether you want to connect to SharePoint using basic authentication of
-- user name and password, or OAuth authentication of user name, password,
-- client ID, and client secret. You can use OAuth authentication for
-- SharePoint Online.
sharePointConfiguration_authenticationType :: Lens.Lens' SharePointConfiguration (Prelude.Maybe SharePointOnlineAuthenticationType)
sharePointConfiguration_authenticationType = Lens.lens (\SharePointConfiguration' {authenticationType} -> authenticationType) (\s@SharePointConfiguration' {} a -> s {authenticationType = a} :: SharePointConfiguration)

-- | @TRUE@ to index document attachments.
sharePointConfiguration_crawlAttachments :: Lens.Lens' SharePointConfiguration (Prelude.Maybe Prelude.Bool)
sharePointConfiguration_crawlAttachments = Lens.lens (\SharePointConfiguration' {crawlAttachments} -> crawlAttachments) (\s@SharePointConfiguration' {} a -> s {crawlAttachments = a} :: SharePointConfiguration)

-- | @TRUE@ to disable local groups information.
sharePointConfiguration_disableLocalGroups :: Lens.Lens' SharePointConfiguration (Prelude.Maybe Prelude.Bool)
sharePointConfiguration_disableLocalGroups = Lens.lens (\SharePointConfiguration' {disableLocalGroups} -> disableLocalGroups) (\s@SharePointConfiguration' {} a -> s {disableLocalGroups = a} :: SharePointConfiguration)

-- | The Microsoft SharePoint attribute field that contains the title of the
-- document.
sharePointConfiguration_documentTitleFieldName :: Lens.Lens' SharePointConfiguration (Prelude.Maybe Prelude.Text)
sharePointConfiguration_documentTitleFieldName = Lens.lens (\SharePointConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SharePointConfiguration' {} a -> s {documentTitleFieldName = a} :: SharePointConfiguration)

-- | A list of regular expression patterns to exclude certain documents in
-- your SharePoint. Documents that match the patterns are excluded from the
-- index. Documents that don\'t match the patterns are included in the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The regex applies to the display URL of the SharePoint document.
sharePointConfiguration_exclusionPatterns :: Lens.Lens' SharePointConfiguration (Prelude.Maybe [Prelude.Text])
sharePointConfiguration_exclusionPatterns = Lens.lens (\SharePointConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@SharePointConfiguration' {} a -> s {exclusionPatterns = a} :: SharePointConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map SharePoint
-- data source attributes or field names to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to SharePoint fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The SharePoint data source field names must exist in your SharePoint
-- custom metadata.
sharePointConfiguration_fieldMappings :: Lens.Lens' SharePointConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
sharePointConfiguration_fieldMappings = Lens.lens (\SharePointConfiguration' {fieldMappings} -> fieldMappings) (\s@SharePointConfiguration' {} a -> s {fieldMappings = a} :: SharePointConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain documents in
-- your SharePoint. Documents that match the patterns are included in the
-- index. Documents that don\'t match the patterns are excluded from the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The regex applies to the display URL of the SharePoint document.
sharePointConfiguration_inclusionPatterns :: Lens.Lens' SharePointConfiguration (Prelude.Maybe [Prelude.Text])
sharePointConfiguration_inclusionPatterns = Lens.lens (\SharePointConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@SharePointConfiguration' {} a -> s {inclusionPatterns = a} :: SharePointConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information to connect to your Microsoft SharePoint site
-- URLs via instance via a web proxy. You can use this option for
-- SharePoint Server.
--
-- You must provide the website host name and port number. For example, the
-- host name of /https:\/\/a.example.com\/page1.html/ is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS.
--
-- Web proxy credentials are optional and you can use them to connect to a
-- web proxy server that requires basic authentication of user name and
-- password. To store web proxy credentials, you use a secret in Secrets
-- Manager.
--
-- It is recommended that you follow best security practices when
-- configuring your web proxy. This includes setting up throttling, setting
-- up logging and monitoring, and applying security patches on a regular
-- basis. If you use your web proxy with multiple data sources, sync jobs
-- that occur at the same time could strain the load on your proxy. It is
-- recommended you prepare your proxy beforehand for any security and load
-- requirements.
sharePointConfiguration_proxyConfiguration :: Lens.Lens' SharePointConfiguration (Prelude.Maybe ProxyConfiguration)
sharePointConfiguration_proxyConfiguration = Lens.lens (\SharePointConfiguration' {proxyConfiguration} -> proxyConfiguration) (\s@SharePointConfiguration' {} a -> s {proxyConfiguration = a} :: SharePointConfiguration)

-- | The path to the SSL certificate stored in an Amazon S3 bucket. You use
-- this to connect to SharePoint Server if you require a secure SSL
-- connection.
--
-- You can simply generate a self-signed X509 certificate on any computer
-- using OpenSSL. For an example of using OpenSSL to create an X509
-- certificate, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/configuring-https-ssl.html Create and sign an X509 certificate>.
sharePointConfiguration_sslCertificateS3Path :: Lens.Lens' SharePointConfiguration (Prelude.Maybe S3Path)
sharePointConfiguration_sslCertificateS3Path = Lens.lens (\SharePointConfiguration' {sslCertificateS3Path} -> sslCertificateS3Path) (\s@SharePointConfiguration' {} a -> s {sslCertificateS3Path = a} :: SharePointConfiguration)

-- | @TRUE@ to use the SharePoint change log to determine which documents
-- require updating in the index. Depending on the change log\'s size, it
-- may take longer for Amazon Kendra to use the change log than to scan all
-- of your documents in SharePoint.
sharePointConfiguration_useChangeLog :: Lens.Lens' SharePointConfiguration (Prelude.Maybe Prelude.Bool)
sharePointConfiguration_useChangeLog = Lens.lens (\SharePointConfiguration' {useChangeLog} -> useChangeLog) (\s@SharePointConfiguration' {} a -> s {useChangeLog = a} :: SharePointConfiguration)

-- | Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Microsoft SharePoint. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
sharePointConfiguration_vpcConfiguration :: Lens.Lens' SharePointConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
sharePointConfiguration_vpcConfiguration = Lens.lens (\SharePointConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@SharePointConfiguration' {} a -> s {vpcConfiguration = a} :: SharePointConfiguration)

-- | The version of Microsoft SharePoint that you use.
sharePointConfiguration_sharePointVersion :: Lens.Lens' SharePointConfiguration SharePointVersion
sharePointConfiguration_sharePointVersion = Lens.lens (\SharePointConfiguration' {sharePointVersion} -> sharePointVersion) (\s@SharePointConfiguration' {} a -> s {sharePointVersion = a} :: SharePointConfiguration)

-- | The Microsoft SharePoint site URLs for the documents you want to index.
sharePointConfiguration_urls :: Lens.Lens' SharePointConfiguration (Prelude.NonEmpty Prelude.Text)
sharePointConfiguration_urls = Lens.lens (\SharePointConfiguration' {urls} -> urls) (\s@SharePointConfiguration' {} a -> s {urls = a} :: SharePointConfiguration) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the user name and password required to connect to the
-- SharePoint instance. If you use SharePoint Server, you also need to
-- provide the sever domain name as part of the credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a Microsoft SharePoint Data Source>.
--
-- You can also provide OAuth authentication credentials of user name,
-- password, client ID, and client secret. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a SharePoint data source>.
sharePointConfiguration_secretArn :: Lens.Lens' SharePointConfiguration Prelude.Text
sharePointConfiguration_secretArn = Lens.lens (\SharePointConfiguration' {secretArn} -> secretArn) (\s@SharePointConfiguration' {} a -> s {secretArn = a} :: SharePointConfiguration)

instance Data.FromJSON SharePointConfiguration where
  parseJSON =
    Data.withObject
      "SharePointConfiguration"
      ( \x ->
          SharePointConfiguration'
            Prelude.<$> (x Data..:? "AuthenticationType")
            Prelude.<*> (x Data..:? "CrawlAttachments")
            Prelude.<*> (x Data..:? "DisableLocalGroups")
            Prelude.<*> (x Data..:? "DocumentTitleFieldName")
            Prelude.<*> ( x Data..:? "ExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FieldMappings")
            Prelude.<*> ( x Data..:? "InclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProxyConfiguration")
            Prelude.<*> (x Data..:? "SslCertificateS3Path")
            Prelude.<*> (x Data..:? "UseChangeLog")
            Prelude.<*> (x Data..:? "VpcConfiguration")
            Prelude.<*> (x Data..: "SharePointVersion")
            Prelude.<*> (x Data..: "Urls")
            Prelude.<*> (x Data..: "SecretArn")
      )

instance Prelude.Hashable SharePointConfiguration where
  hashWithSalt _salt SharePointConfiguration' {..} =
    _salt `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` crawlAttachments
      `Prelude.hashWithSalt` disableLocalGroups
      `Prelude.hashWithSalt` documentTitleFieldName
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` fieldMappings
      `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` proxyConfiguration
      `Prelude.hashWithSalt` sslCertificateS3Path
      `Prelude.hashWithSalt` useChangeLog
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` sharePointVersion
      `Prelude.hashWithSalt` urls
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData SharePointConfiguration where
  rnf SharePointConfiguration' {..} =
    Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf crawlAttachments
      `Prelude.seq` Prelude.rnf disableLocalGroups
      `Prelude.seq` Prelude.rnf documentTitleFieldName
      `Prelude.seq` Prelude.rnf exclusionPatterns
      `Prelude.seq` Prelude.rnf fieldMappings
      `Prelude.seq` Prelude.rnf inclusionPatterns
      `Prelude.seq` Prelude.rnf proxyConfiguration
      `Prelude.seq` Prelude.rnf sslCertificateS3Path
      `Prelude.seq` Prelude.rnf useChangeLog
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf sharePointVersion
      `Prelude.seq` Prelude.rnf urls
      `Prelude.seq` Prelude.rnf secretArn

instance Data.ToJSON SharePointConfiguration where
  toJSON SharePointConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthenticationType" Data..=)
              Prelude.<$> authenticationType,
            ("CrawlAttachments" Data..=)
              Prelude.<$> crawlAttachments,
            ("DisableLocalGroups" Data..=)
              Prelude.<$> disableLocalGroups,
            ("DocumentTitleFieldName" Data..=)
              Prelude.<$> documentTitleFieldName,
            ("ExclusionPatterns" Data..=)
              Prelude.<$> exclusionPatterns,
            ("FieldMappings" Data..=) Prelude.<$> fieldMappings,
            ("InclusionPatterns" Data..=)
              Prelude.<$> inclusionPatterns,
            ("ProxyConfiguration" Data..=)
              Prelude.<$> proxyConfiguration,
            ("SslCertificateS3Path" Data..=)
              Prelude.<$> sslCertificateS3Path,
            ("UseChangeLog" Data..=) Prelude.<$> useChangeLog,
            ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            Prelude.Just
              ("SharePointVersion" Data..= sharePointVersion),
            Prelude.Just ("Urls" Data..= urls),
            Prelude.Just ("SecretArn" Data..= secretArn)
          ]
      )
