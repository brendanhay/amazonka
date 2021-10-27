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
-- Module      : Network.AWS.Kendra.Types.SharePointConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SharePointConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import Network.AWS.Kendra.Types.DataSourceVpcConfiguration
import Network.AWS.Kendra.Types.S3Path
import Network.AWS.Kendra.Types.SharePointVersion
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for connecting to a Microsoft
-- SharePoint data source.
--
-- /See:/ 'newSharePointConfiguration' smart constructor.
data SharePointConfiguration = SharePointConfiguration'
  { -- | A list of @DataSourceToIndexFieldMapping@ objects that map Microsoft
    -- SharePoint attributes to custom fields in the Amazon Kendra index. You
    -- must first create the index fields using the @UpdateIndex@ operation
    -- before you map SharePoint attributes. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping Data Source Fields>.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | Set to @TRUE@ to use the Microsoft SharePoint change log to determine
    -- the documents that need to be updated in the index. Depending on the
    -- size of the SharePoint change log, it may take longer for Amazon Kendra
    -- to use the change log than it takes it to determine the changed
    -- documents using the Amazon Kendra document crawler.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to include attachments to documents stored in your Microsoft
    -- SharePoint site in the index; otherwise, @FALSE@.
    crawlAttachments :: Prelude.Maybe Prelude.Bool,
    sslCertificateS3Path :: Prelude.Maybe S3Path,
    -- | A list of regular expression patterns. Documents that match the patterns
    -- are excluded from the index. Documents that don\'t match the patterns
    -- are included in the index. If a document matches both an exclusion
    -- pattern and an inclusion pattern, the document is not included in the
    -- index.
    --
    -- The regex is applied to the display URL of the SharePoint document.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The Microsoft SharePoint attribute field that contains the title of the
    -- document.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that specifies whether local groups are disabled
    -- (@True@) or enabled (@False@).
    disableLocalGroups :: Prelude.Maybe Prelude.Bool,
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | A list of regular expression patterns. Documents that match the patterns
    -- are included in the index. Documents that don\'t match the patterns are
    -- excluded from the index. If a document matches both an inclusion pattern
    -- and an exclusion pattern, the document is not included in the index.
    --
    -- The regex is applied to the display URL of the SharePoint document.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The version of Microsoft SharePoint that you are using as a data source.
    sharePointVersion :: SharePointVersion,
    -- | The URLs of the Microsoft SharePoint site that contains the documents
    -- that should be indexed.
    urls :: Prelude.NonEmpty Prelude.Text,
    -- | The Amazon Resource Name (ARN) of credentials stored in Secrets Manager.
    -- The credentials should be a user\/password pair. If you use SharePoint
    -- Server, you also need to provide the sever domain name as part of the
    -- credentials. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a Microsoft SharePoint Data Source>.
    -- For more information about Secrets Manager, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html What Is Secrets Manager>
    -- in the /Secrets Manager/ user guide.
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
-- 'fieldMappings', 'sharePointConfiguration_fieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map Microsoft
-- SharePoint attributes to custom fields in the Amazon Kendra index. You
-- must first create the index fields using the @UpdateIndex@ operation
-- before you map SharePoint attributes. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping Data Source Fields>.
--
-- 'useChangeLog', 'sharePointConfiguration_useChangeLog' - Set to @TRUE@ to use the Microsoft SharePoint change log to determine
-- the documents that need to be updated in the index. Depending on the
-- size of the SharePoint change log, it may take longer for Amazon Kendra
-- to use the change log than it takes it to determine the changed
-- documents using the Amazon Kendra document crawler.
--
-- 'crawlAttachments', 'sharePointConfiguration_crawlAttachments' - @TRUE@ to include attachments to documents stored in your Microsoft
-- SharePoint site in the index; otherwise, @FALSE@.
--
-- 'sslCertificateS3Path', 'sharePointConfiguration_sslCertificateS3Path' - Undocumented member.
--
-- 'exclusionPatterns', 'sharePointConfiguration_exclusionPatterns' - A list of regular expression patterns. Documents that match the patterns
-- are excluded from the index. Documents that don\'t match the patterns
-- are included in the index. If a document matches both an exclusion
-- pattern and an inclusion pattern, the document is not included in the
-- index.
--
-- The regex is applied to the display URL of the SharePoint document.
--
-- 'documentTitleFieldName', 'sharePointConfiguration_documentTitleFieldName' - The Microsoft SharePoint attribute field that contains the title of the
-- document.
--
-- 'disableLocalGroups', 'sharePointConfiguration_disableLocalGroups' - A Boolean value that specifies whether local groups are disabled
-- (@True@) or enabled (@False@).
--
-- 'vpcConfiguration', 'sharePointConfiguration_vpcConfiguration' - Undocumented member.
--
-- 'inclusionPatterns', 'sharePointConfiguration_inclusionPatterns' - A list of regular expression patterns. Documents that match the patterns
-- are included in the index. Documents that don\'t match the patterns are
-- excluded from the index. If a document matches both an inclusion pattern
-- and an exclusion pattern, the document is not included in the index.
--
-- The regex is applied to the display URL of the SharePoint document.
--
-- 'sharePointVersion', 'sharePointConfiguration_sharePointVersion' - The version of Microsoft SharePoint that you are using as a data source.
--
-- 'urls', 'sharePointConfiguration_urls' - The URLs of the Microsoft SharePoint site that contains the documents
-- that should be indexed.
--
-- 'secretArn', 'sharePointConfiguration_secretArn' - The Amazon Resource Name (ARN) of credentials stored in Secrets Manager.
-- The credentials should be a user\/password pair. If you use SharePoint
-- Server, you also need to provide the sever domain name as part of the
-- credentials. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a Microsoft SharePoint Data Source>.
-- For more information about Secrets Manager, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html What Is Secrets Manager>
-- in the /Secrets Manager/ user guide.
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
      { fieldMappings =
          Prelude.Nothing,
        useChangeLog = Prelude.Nothing,
        crawlAttachments = Prelude.Nothing,
        sslCertificateS3Path = Prelude.Nothing,
        exclusionPatterns = Prelude.Nothing,
        documentTitleFieldName = Prelude.Nothing,
        disableLocalGroups = Prelude.Nothing,
        vpcConfiguration = Prelude.Nothing,
        inclusionPatterns = Prelude.Nothing,
        sharePointVersion = pSharePointVersion_,
        urls = Lens.coerced Lens.# pUrls_,
        secretArn = pSecretArn_
      }

-- | A list of @DataSourceToIndexFieldMapping@ objects that map Microsoft
-- SharePoint attributes to custom fields in the Amazon Kendra index. You
-- must first create the index fields using the @UpdateIndex@ operation
-- before you map SharePoint attributes. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping Data Source Fields>.
sharePointConfiguration_fieldMappings :: Lens.Lens' SharePointConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
sharePointConfiguration_fieldMappings = Lens.lens (\SharePointConfiguration' {fieldMappings} -> fieldMappings) (\s@SharePointConfiguration' {} a -> s {fieldMappings = a} :: SharePointConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Set to @TRUE@ to use the Microsoft SharePoint change log to determine
-- the documents that need to be updated in the index. Depending on the
-- size of the SharePoint change log, it may take longer for Amazon Kendra
-- to use the change log than it takes it to determine the changed
-- documents using the Amazon Kendra document crawler.
sharePointConfiguration_useChangeLog :: Lens.Lens' SharePointConfiguration (Prelude.Maybe Prelude.Bool)
sharePointConfiguration_useChangeLog = Lens.lens (\SharePointConfiguration' {useChangeLog} -> useChangeLog) (\s@SharePointConfiguration' {} a -> s {useChangeLog = a} :: SharePointConfiguration)

-- | @TRUE@ to include attachments to documents stored in your Microsoft
-- SharePoint site in the index; otherwise, @FALSE@.
sharePointConfiguration_crawlAttachments :: Lens.Lens' SharePointConfiguration (Prelude.Maybe Prelude.Bool)
sharePointConfiguration_crawlAttachments = Lens.lens (\SharePointConfiguration' {crawlAttachments} -> crawlAttachments) (\s@SharePointConfiguration' {} a -> s {crawlAttachments = a} :: SharePointConfiguration)

-- | Undocumented member.
sharePointConfiguration_sslCertificateS3Path :: Lens.Lens' SharePointConfiguration (Prelude.Maybe S3Path)
sharePointConfiguration_sslCertificateS3Path = Lens.lens (\SharePointConfiguration' {sslCertificateS3Path} -> sslCertificateS3Path) (\s@SharePointConfiguration' {} a -> s {sslCertificateS3Path = a} :: SharePointConfiguration)

-- | A list of regular expression patterns. Documents that match the patterns
-- are excluded from the index. Documents that don\'t match the patterns
-- are included in the index. If a document matches both an exclusion
-- pattern and an inclusion pattern, the document is not included in the
-- index.
--
-- The regex is applied to the display URL of the SharePoint document.
sharePointConfiguration_exclusionPatterns :: Lens.Lens' SharePointConfiguration (Prelude.Maybe [Prelude.Text])
sharePointConfiguration_exclusionPatterns = Lens.lens (\SharePointConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@SharePointConfiguration' {} a -> s {exclusionPatterns = a} :: SharePointConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Microsoft SharePoint attribute field that contains the title of the
-- document.
sharePointConfiguration_documentTitleFieldName :: Lens.Lens' SharePointConfiguration (Prelude.Maybe Prelude.Text)
sharePointConfiguration_documentTitleFieldName = Lens.lens (\SharePointConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SharePointConfiguration' {} a -> s {documentTitleFieldName = a} :: SharePointConfiguration)

-- | A Boolean value that specifies whether local groups are disabled
-- (@True@) or enabled (@False@).
sharePointConfiguration_disableLocalGroups :: Lens.Lens' SharePointConfiguration (Prelude.Maybe Prelude.Bool)
sharePointConfiguration_disableLocalGroups = Lens.lens (\SharePointConfiguration' {disableLocalGroups} -> disableLocalGroups) (\s@SharePointConfiguration' {} a -> s {disableLocalGroups = a} :: SharePointConfiguration)

-- | Undocumented member.
sharePointConfiguration_vpcConfiguration :: Lens.Lens' SharePointConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
sharePointConfiguration_vpcConfiguration = Lens.lens (\SharePointConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@SharePointConfiguration' {} a -> s {vpcConfiguration = a} :: SharePointConfiguration)

-- | A list of regular expression patterns. Documents that match the patterns
-- are included in the index. Documents that don\'t match the patterns are
-- excluded from the index. If a document matches both an inclusion pattern
-- and an exclusion pattern, the document is not included in the index.
--
-- The regex is applied to the display URL of the SharePoint document.
sharePointConfiguration_inclusionPatterns :: Lens.Lens' SharePointConfiguration (Prelude.Maybe [Prelude.Text])
sharePointConfiguration_inclusionPatterns = Lens.lens (\SharePointConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@SharePointConfiguration' {} a -> s {inclusionPatterns = a} :: SharePointConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The version of Microsoft SharePoint that you are using as a data source.
sharePointConfiguration_sharePointVersion :: Lens.Lens' SharePointConfiguration SharePointVersion
sharePointConfiguration_sharePointVersion = Lens.lens (\SharePointConfiguration' {sharePointVersion} -> sharePointVersion) (\s@SharePointConfiguration' {} a -> s {sharePointVersion = a} :: SharePointConfiguration)

-- | The URLs of the Microsoft SharePoint site that contains the documents
-- that should be indexed.
sharePointConfiguration_urls :: Lens.Lens' SharePointConfiguration (Prelude.NonEmpty Prelude.Text)
sharePointConfiguration_urls = Lens.lens (\SharePointConfiguration' {urls} -> urls) (\s@SharePointConfiguration' {} a -> s {urls = a} :: SharePointConfiguration) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of credentials stored in Secrets Manager.
-- The credentials should be a user\/password pair. If you use SharePoint
-- Server, you also need to provide the sever domain name as part of the
-- credentials. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-sharepoint.html Using a Microsoft SharePoint Data Source>.
-- For more information about Secrets Manager, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html What Is Secrets Manager>
-- in the /Secrets Manager/ user guide.
sharePointConfiguration_secretArn :: Lens.Lens' SharePointConfiguration Prelude.Text
sharePointConfiguration_secretArn = Lens.lens (\SharePointConfiguration' {secretArn} -> secretArn) (\s@SharePointConfiguration' {} a -> s {secretArn = a} :: SharePointConfiguration)

instance Core.FromJSON SharePointConfiguration where
  parseJSON =
    Core.withObject
      "SharePointConfiguration"
      ( \x ->
          SharePointConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
            Prelude.<*> (x Core..:? "UseChangeLog")
            Prelude.<*> (x Core..:? "CrawlAttachments")
            Prelude.<*> (x Core..:? "SslCertificateS3Path")
            Prelude.<*> ( x Core..:? "ExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DocumentTitleFieldName")
            Prelude.<*> (x Core..:? "DisableLocalGroups")
            Prelude.<*> (x Core..:? "VpcConfiguration")
            Prelude.<*> ( x Core..:? "InclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "SharePointVersion")
            Prelude.<*> (x Core..: "Urls")
            Prelude.<*> (x Core..: "SecretArn")
      )

instance Prelude.Hashable SharePointConfiguration

instance Prelude.NFData SharePointConfiguration

instance Core.ToJSON SharePointConfiguration where
  toJSON SharePointConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
            ("UseChangeLog" Core..=) Prelude.<$> useChangeLog,
            ("CrawlAttachments" Core..=)
              Prelude.<$> crawlAttachments,
            ("SslCertificateS3Path" Core..=)
              Prelude.<$> sslCertificateS3Path,
            ("ExclusionPatterns" Core..=)
              Prelude.<$> exclusionPatterns,
            ("DocumentTitleFieldName" Core..=)
              Prelude.<$> documentTitleFieldName,
            ("DisableLocalGroups" Core..=)
              Prelude.<$> disableLocalGroups,
            ("VpcConfiguration" Core..=)
              Prelude.<$> vpcConfiguration,
            ("InclusionPatterns" Core..=)
              Prelude.<$> inclusionPatterns,
            Prelude.Just
              ("SharePointVersion" Core..= sharePointVersion),
            Prelude.Just ("Urls" Core..= urls),
            Prelude.Just ("SecretArn" Core..= secretArn)
          ]
      )
