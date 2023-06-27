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
-- Module      : Amazonka.Kendra.Types.ServiceNowKnowledgeArticleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ServiceNowKnowledgeArticleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for crawling knowledge articles
-- in the ServiceNow site.
--
-- /See:/ 'newServiceNowKnowledgeArticleConfiguration' smart constructor.
data ServiceNowKnowledgeArticleConfiguration = ServiceNowKnowledgeArticleConfiguration'
  { -- | @TRUE@ to index attachments to knowledge articles.
    crawlAttachments :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ServiceNow field that is mapped to the index document
    -- title field.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
    -- | A list of regular expression patterns applied to exclude certain
    -- knowledge article attachments. Attachments that match the patterns are
    -- excluded from the index. Items that don\'t match the patterns are
    -- included in the index. If an item matches both an inclusion and
    -- exclusion pattern, the exclusion pattern takes precedence and the item
    -- isn\'t included in the index.
    excludeAttachmentFilePatterns :: Prelude.Maybe [Prelude.Text],
    -- | Maps attributes or field names of knoweldge articles to Amazon Kendra
    -- index field names. To create custom fields, use the @UpdateIndex@ API
    -- before you map to ServiceNow fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The ServiceNow data source field names must exist in your ServiceNow
    -- custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A query that selects the knowledge articles to index. The query can
    -- return articles from multiple knowledge bases, and the knowledge bases
    -- can be public or private.
    --
    -- The query string must be one generated by the ServiceNow console. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/servicenow-query.html Specifying documents to index with a query>.
    filterQuery :: Prelude.Maybe Prelude.Text,
    -- | A list of regular expression patterns applied to include knowledge
    -- article attachments. Attachments that match the patterns are included in
    -- the index. Items that don\'t match the patterns are excluded from the
    -- index. If an item matches both an inclusion and exclusion pattern, the
    -- exclusion pattern takes precedence and the item isn\'t included in the
    -- index.
    includeAttachmentFilePatterns :: Prelude.Maybe [Prelude.Text],
    -- | The name of the ServiceNow field that is mapped to the index document
    -- contents field in the Amazon Kendra index.
    documentDataFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNowKnowledgeArticleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlAttachments', 'serviceNowKnowledgeArticleConfiguration_crawlAttachments' - @TRUE@ to index attachments to knowledge articles.
--
-- 'documentTitleFieldName', 'serviceNowKnowledgeArticleConfiguration_documentTitleFieldName' - The name of the ServiceNow field that is mapped to the index document
-- title field.
--
-- 'excludeAttachmentFilePatterns', 'serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns' - A list of regular expression patterns applied to exclude certain
-- knowledge article attachments. Attachments that match the patterns are
-- excluded from the index. Items that don\'t match the patterns are
-- included in the index. If an item matches both an inclusion and
-- exclusion pattern, the exclusion pattern takes precedence and the item
-- isn\'t included in the index.
--
-- 'fieldMappings', 'serviceNowKnowledgeArticleConfiguration_fieldMappings' - Maps attributes or field names of knoweldge articles to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to ServiceNow fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The ServiceNow data source field names must exist in your ServiceNow
-- custom metadata.
--
-- 'filterQuery', 'serviceNowKnowledgeArticleConfiguration_filterQuery' - A query that selects the knowledge articles to index. The query can
-- return articles from multiple knowledge bases, and the knowledge bases
-- can be public or private.
--
-- The query string must be one generated by the ServiceNow console. For
-- more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/servicenow-query.html Specifying documents to index with a query>.
--
-- 'includeAttachmentFilePatterns', 'serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns' - A list of regular expression patterns applied to include knowledge
-- article attachments. Attachments that match the patterns are included in
-- the index. Items that don\'t match the patterns are excluded from the
-- index. If an item matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the item isn\'t included in the
-- index.
--
-- 'documentDataFieldName', 'serviceNowKnowledgeArticleConfiguration_documentDataFieldName' - The name of the ServiceNow field that is mapped to the index document
-- contents field in the Amazon Kendra index.
newServiceNowKnowledgeArticleConfiguration ::
  -- | 'documentDataFieldName'
  Prelude.Text ->
  ServiceNowKnowledgeArticleConfiguration
newServiceNowKnowledgeArticleConfiguration
  pDocumentDataFieldName_ =
    ServiceNowKnowledgeArticleConfiguration'
      { crawlAttachments =
          Prelude.Nothing,
        documentTitleFieldName =
          Prelude.Nothing,
        excludeAttachmentFilePatterns =
          Prelude.Nothing,
        fieldMappings = Prelude.Nothing,
        filterQuery = Prelude.Nothing,
        includeAttachmentFilePatterns =
          Prelude.Nothing,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

-- | @TRUE@ to index attachments to knowledge articles.
serviceNowKnowledgeArticleConfiguration_crawlAttachments :: Lens.Lens' ServiceNowKnowledgeArticleConfiguration (Prelude.Maybe Prelude.Bool)
serviceNowKnowledgeArticleConfiguration_crawlAttachments = Lens.lens (\ServiceNowKnowledgeArticleConfiguration' {crawlAttachments} -> crawlAttachments) (\s@ServiceNowKnowledgeArticleConfiguration' {} a -> s {crawlAttachments = a} :: ServiceNowKnowledgeArticleConfiguration)

-- | The name of the ServiceNow field that is mapped to the index document
-- title field.
serviceNowKnowledgeArticleConfiguration_documentTitleFieldName :: Lens.Lens' ServiceNowKnowledgeArticleConfiguration (Prelude.Maybe Prelude.Text)
serviceNowKnowledgeArticleConfiguration_documentTitleFieldName = Lens.lens (\ServiceNowKnowledgeArticleConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@ServiceNowKnowledgeArticleConfiguration' {} a -> s {documentTitleFieldName = a} :: ServiceNowKnowledgeArticleConfiguration)

-- | A list of regular expression patterns applied to exclude certain
-- knowledge article attachments. Attachments that match the patterns are
-- excluded from the index. Items that don\'t match the patterns are
-- included in the index. If an item matches both an inclusion and
-- exclusion pattern, the exclusion pattern takes precedence and the item
-- isn\'t included in the index.
serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns :: Lens.Lens' ServiceNowKnowledgeArticleConfiguration (Prelude.Maybe [Prelude.Text])
serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns = Lens.lens (\ServiceNowKnowledgeArticleConfiguration' {excludeAttachmentFilePatterns} -> excludeAttachmentFilePatterns) (\s@ServiceNowKnowledgeArticleConfiguration' {} a -> s {excludeAttachmentFilePatterns = a} :: ServiceNowKnowledgeArticleConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Maps attributes or field names of knoweldge articles to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to ServiceNow fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The ServiceNow data source field names must exist in your ServiceNow
-- custom metadata.
serviceNowKnowledgeArticleConfiguration_fieldMappings :: Lens.Lens' ServiceNowKnowledgeArticleConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
serviceNowKnowledgeArticleConfiguration_fieldMappings = Lens.lens (\ServiceNowKnowledgeArticleConfiguration' {fieldMappings} -> fieldMappings) (\s@ServiceNowKnowledgeArticleConfiguration' {} a -> s {fieldMappings = a} :: ServiceNowKnowledgeArticleConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A query that selects the knowledge articles to index. The query can
-- return articles from multiple knowledge bases, and the knowledge bases
-- can be public or private.
--
-- The query string must be one generated by the ServiceNow console. For
-- more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/servicenow-query.html Specifying documents to index with a query>.
serviceNowKnowledgeArticleConfiguration_filterQuery :: Lens.Lens' ServiceNowKnowledgeArticleConfiguration (Prelude.Maybe Prelude.Text)
serviceNowKnowledgeArticleConfiguration_filterQuery = Lens.lens (\ServiceNowKnowledgeArticleConfiguration' {filterQuery} -> filterQuery) (\s@ServiceNowKnowledgeArticleConfiguration' {} a -> s {filterQuery = a} :: ServiceNowKnowledgeArticleConfiguration)

-- | A list of regular expression patterns applied to include knowledge
-- article attachments. Attachments that match the patterns are included in
-- the index. Items that don\'t match the patterns are excluded from the
-- index. If an item matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the item isn\'t included in the
-- index.
serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns :: Lens.Lens' ServiceNowKnowledgeArticleConfiguration (Prelude.Maybe [Prelude.Text])
serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns = Lens.lens (\ServiceNowKnowledgeArticleConfiguration' {includeAttachmentFilePatterns} -> includeAttachmentFilePatterns) (\s@ServiceNowKnowledgeArticleConfiguration' {} a -> s {includeAttachmentFilePatterns = a} :: ServiceNowKnowledgeArticleConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the ServiceNow field that is mapped to the index document
-- contents field in the Amazon Kendra index.
serviceNowKnowledgeArticleConfiguration_documentDataFieldName :: Lens.Lens' ServiceNowKnowledgeArticleConfiguration Prelude.Text
serviceNowKnowledgeArticleConfiguration_documentDataFieldName = Lens.lens (\ServiceNowKnowledgeArticleConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@ServiceNowKnowledgeArticleConfiguration' {} a -> s {documentDataFieldName = a} :: ServiceNowKnowledgeArticleConfiguration)

instance
  Data.FromJSON
    ServiceNowKnowledgeArticleConfiguration
  where
  parseJSON =
    Data.withObject
      "ServiceNowKnowledgeArticleConfiguration"
      ( \x ->
          ServiceNowKnowledgeArticleConfiguration'
            Prelude.<$> (x Data..:? "CrawlAttachments")
            Prelude.<*> (x Data..:? "DocumentTitleFieldName")
            Prelude.<*> ( x
                            Data..:? "ExcludeAttachmentFilePatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FieldMappings")
            Prelude.<*> (x Data..:? "FilterQuery")
            Prelude.<*> ( x
                            Data..:? "IncludeAttachmentFilePatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    ServiceNowKnowledgeArticleConfiguration
  where
  hashWithSalt
    _salt
    ServiceNowKnowledgeArticleConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` crawlAttachments
        `Prelude.hashWithSalt` documentTitleFieldName
        `Prelude.hashWithSalt` excludeAttachmentFilePatterns
        `Prelude.hashWithSalt` fieldMappings
        `Prelude.hashWithSalt` filterQuery
        `Prelude.hashWithSalt` includeAttachmentFilePatterns
        `Prelude.hashWithSalt` documentDataFieldName

instance
  Prelude.NFData
    ServiceNowKnowledgeArticleConfiguration
  where
  rnf ServiceNowKnowledgeArticleConfiguration' {..} =
    Prelude.rnf crawlAttachments
      `Prelude.seq` Prelude.rnf documentTitleFieldName
      `Prelude.seq` Prelude.rnf excludeAttachmentFilePatterns
      `Prelude.seq` Prelude.rnf fieldMappings
      `Prelude.seq` Prelude.rnf filterQuery
      `Prelude.seq` Prelude.rnf includeAttachmentFilePatterns
      `Prelude.seq` Prelude.rnf documentDataFieldName

instance
  Data.ToJSON
    ServiceNowKnowledgeArticleConfiguration
  where
  toJSON ServiceNowKnowledgeArticleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrawlAttachments" Data..=)
              Prelude.<$> crawlAttachments,
            ("DocumentTitleFieldName" Data..=)
              Prelude.<$> documentTitleFieldName,
            ("ExcludeAttachmentFilePatterns" Data..=)
              Prelude.<$> excludeAttachmentFilePatterns,
            ("FieldMappings" Data..=) Prelude.<$> fieldMappings,
            ("FilterQuery" Data..=) Prelude.<$> filterQuery,
            ("IncludeAttachmentFilePatterns" Data..=)
              Prelude.<$> includeAttachmentFilePatterns,
            Prelude.Just
              ( "DocumentDataFieldName"
                  Data..= documentDataFieldName
              )
          ]
      )
