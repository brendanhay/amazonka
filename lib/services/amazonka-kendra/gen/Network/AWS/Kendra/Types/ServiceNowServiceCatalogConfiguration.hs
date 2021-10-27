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
-- Module      : Network.AWS.Kendra.Types.ServiceNowServiceCatalogConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ServiceNowServiceCatalogConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for crawling service catalog items in
-- the ServiceNow site
--
-- /See:/ 'newServiceNowServiceCatalogConfiguration' smart constructor.
data ServiceNowServiceCatalogConfiguration = ServiceNowServiceCatalogConfiguration'
  { -- | Mapping between ServiceNow fields and Amazon Kendra index fields. You
    -- must create the index field before you map the field.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | Indicates whether Amazon Kendra should crawl attachments to the service
    -- catalog items.
    crawlAttachments :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns. Documents that match the patterns
    -- are excluded from the index. Documents that don\'t match the patterns
    -- are included in the index. If a document matches both an exclusion
    -- pattern and an inclusion pattern, the document is not included in the
    -- index.
    --
    -- The regex is applied to the file name of the attachment.
    excludeAttachmentFilePatterns :: Prelude.Maybe [Prelude.Text],
    -- | The name of the ServiceNow field that is mapped to the index document
    -- title field.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
    -- | A list of regular expression patterns. Documents that match the patterns
    -- are included in the index. Documents that don\'t match the patterns are
    -- excluded from the index. If a document matches both an exclusion pattern
    -- and an inclusion pattern, the document is not included in the index.
    --
    -- The regex is applied to the file name of the attachment.
    includeAttachmentFilePatterns :: Prelude.Maybe [Prelude.Text],
    -- | The name of the ServiceNow field that is mapped to the index document
    -- contents field in the Amazon Kendra index.
    documentDataFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNowServiceCatalogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldMappings', 'serviceNowServiceCatalogConfiguration_fieldMappings' - Mapping between ServiceNow fields and Amazon Kendra index fields. You
-- must create the index field before you map the field.
--
-- 'crawlAttachments', 'serviceNowServiceCatalogConfiguration_crawlAttachments' - Indicates whether Amazon Kendra should crawl attachments to the service
-- catalog items.
--
-- 'excludeAttachmentFilePatterns', 'serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns' - A list of regular expression patterns. Documents that match the patterns
-- are excluded from the index. Documents that don\'t match the patterns
-- are included in the index. If a document matches both an exclusion
-- pattern and an inclusion pattern, the document is not included in the
-- index.
--
-- The regex is applied to the file name of the attachment.
--
-- 'documentTitleFieldName', 'serviceNowServiceCatalogConfiguration_documentTitleFieldName' - The name of the ServiceNow field that is mapped to the index document
-- title field.
--
-- 'includeAttachmentFilePatterns', 'serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns' - A list of regular expression patterns. Documents that match the patterns
-- are included in the index. Documents that don\'t match the patterns are
-- excluded from the index. If a document matches both an exclusion pattern
-- and an inclusion pattern, the document is not included in the index.
--
-- The regex is applied to the file name of the attachment.
--
-- 'documentDataFieldName', 'serviceNowServiceCatalogConfiguration_documentDataFieldName' - The name of the ServiceNow field that is mapped to the index document
-- contents field in the Amazon Kendra index.
newServiceNowServiceCatalogConfiguration ::
  -- | 'documentDataFieldName'
  Prelude.Text ->
  ServiceNowServiceCatalogConfiguration
newServiceNowServiceCatalogConfiguration
  pDocumentDataFieldName_ =
    ServiceNowServiceCatalogConfiguration'
      { fieldMappings =
          Prelude.Nothing,
        crawlAttachments = Prelude.Nothing,
        excludeAttachmentFilePatterns =
          Prelude.Nothing,
        documentTitleFieldName =
          Prelude.Nothing,
        includeAttachmentFilePatterns =
          Prelude.Nothing,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

-- | Mapping between ServiceNow fields and Amazon Kendra index fields. You
-- must create the index field before you map the field.
serviceNowServiceCatalogConfiguration_fieldMappings :: Lens.Lens' ServiceNowServiceCatalogConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
serviceNowServiceCatalogConfiguration_fieldMappings = Lens.lens (\ServiceNowServiceCatalogConfiguration' {fieldMappings} -> fieldMappings) (\s@ServiceNowServiceCatalogConfiguration' {} a -> s {fieldMappings = a} :: ServiceNowServiceCatalogConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether Amazon Kendra should crawl attachments to the service
-- catalog items.
serviceNowServiceCatalogConfiguration_crawlAttachments :: Lens.Lens' ServiceNowServiceCatalogConfiguration (Prelude.Maybe Prelude.Bool)
serviceNowServiceCatalogConfiguration_crawlAttachments = Lens.lens (\ServiceNowServiceCatalogConfiguration' {crawlAttachments} -> crawlAttachments) (\s@ServiceNowServiceCatalogConfiguration' {} a -> s {crawlAttachments = a} :: ServiceNowServiceCatalogConfiguration)

-- | A list of regular expression patterns. Documents that match the patterns
-- are excluded from the index. Documents that don\'t match the patterns
-- are included in the index. If a document matches both an exclusion
-- pattern and an inclusion pattern, the document is not included in the
-- index.
--
-- The regex is applied to the file name of the attachment.
serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns :: Lens.Lens' ServiceNowServiceCatalogConfiguration (Prelude.Maybe [Prelude.Text])
serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns = Lens.lens (\ServiceNowServiceCatalogConfiguration' {excludeAttachmentFilePatterns} -> excludeAttachmentFilePatterns) (\s@ServiceNowServiceCatalogConfiguration' {} a -> s {excludeAttachmentFilePatterns = a} :: ServiceNowServiceCatalogConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the ServiceNow field that is mapped to the index document
-- title field.
serviceNowServiceCatalogConfiguration_documentTitleFieldName :: Lens.Lens' ServiceNowServiceCatalogConfiguration (Prelude.Maybe Prelude.Text)
serviceNowServiceCatalogConfiguration_documentTitleFieldName = Lens.lens (\ServiceNowServiceCatalogConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@ServiceNowServiceCatalogConfiguration' {} a -> s {documentTitleFieldName = a} :: ServiceNowServiceCatalogConfiguration)

-- | A list of regular expression patterns. Documents that match the patterns
-- are included in the index. Documents that don\'t match the patterns are
-- excluded from the index. If a document matches both an exclusion pattern
-- and an inclusion pattern, the document is not included in the index.
--
-- The regex is applied to the file name of the attachment.
serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns :: Lens.Lens' ServiceNowServiceCatalogConfiguration (Prelude.Maybe [Prelude.Text])
serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns = Lens.lens (\ServiceNowServiceCatalogConfiguration' {includeAttachmentFilePatterns} -> includeAttachmentFilePatterns) (\s@ServiceNowServiceCatalogConfiguration' {} a -> s {includeAttachmentFilePatterns = a} :: ServiceNowServiceCatalogConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the ServiceNow field that is mapped to the index document
-- contents field in the Amazon Kendra index.
serviceNowServiceCatalogConfiguration_documentDataFieldName :: Lens.Lens' ServiceNowServiceCatalogConfiguration Prelude.Text
serviceNowServiceCatalogConfiguration_documentDataFieldName = Lens.lens (\ServiceNowServiceCatalogConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@ServiceNowServiceCatalogConfiguration' {} a -> s {documentDataFieldName = a} :: ServiceNowServiceCatalogConfiguration)

instance
  Core.FromJSON
    ServiceNowServiceCatalogConfiguration
  where
  parseJSON =
    Core.withObject
      "ServiceNowServiceCatalogConfiguration"
      ( \x ->
          ServiceNowServiceCatalogConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
            Prelude.<*> (x Core..:? "CrawlAttachments")
            Prelude.<*> ( x Core..:? "ExcludeAttachmentFilePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DocumentTitleFieldName")
            Prelude.<*> ( x Core..:? "IncludeAttachmentFilePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    ServiceNowServiceCatalogConfiguration

instance
  Prelude.NFData
    ServiceNowServiceCatalogConfiguration

instance
  Core.ToJSON
    ServiceNowServiceCatalogConfiguration
  where
  toJSON ServiceNowServiceCatalogConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
            ("CrawlAttachments" Core..=)
              Prelude.<$> crawlAttachments,
            ("ExcludeAttachmentFilePatterns" Core..=)
              Prelude.<$> excludeAttachmentFilePatterns,
            ("DocumentTitleFieldName" Core..=)
              Prelude.<$> documentTitleFieldName,
            ("IncludeAttachmentFilePatterns" Core..=)
              Prelude.<$> includeAttachmentFilePatterns,
            Prelude.Just
              ( "DocumentDataFieldName"
                  Core..= documentDataFieldName
              )
          ]
      )
