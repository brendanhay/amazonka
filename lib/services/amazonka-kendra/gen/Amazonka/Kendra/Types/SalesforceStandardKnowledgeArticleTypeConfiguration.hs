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
-- Module      : Amazonka.Kendra.Types.SalesforceStandardKnowledgeArticleTypeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SalesforceStandardKnowledgeArticleTypeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for standard Salesforce knowledge
-- articles.
--
-- /See:/ 'newSalesforceStandardKnowledgeArticleTypeConfiguration' smart constructor.
data SalesforceStandardKnowledgeArticleTypeConfiguration = SalesforceStandardKnowledgeArticleTypeConfiguration'
  { -- | Maps attributes or field names of the knowledge article to Amazon Kendra
    -- index field names. To create custom fields, use the @UpdateIndex@ API
    -- before you map to Salesforce fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Salesforce data source field names must exist in your Salesforce
    -- custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The name of the field that contains the document title.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
    -- | The name of the field that contains the document data to index.
    documentDataFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceStandardKnowledgeArticleTypeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldMappings', 'salesforceStandardKnowledgeArticleTypeConfiguration_fieldMappings' - Maps attributes or field names of the knowledge article to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Salesforce fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Salesforce data source field names must exist in your Salesforce
-- custom metadata.
--
-- 'documentTitleFieldName', 'salesforceStandardKnowledgeArticleTypeConfiguration_documentTitleFieldName' - The name of the field that contains the document title.
--
-- 'documentDataFieldName', 'salesforceStandardKnowledgeArticleTypeConfiguration_documentDataFieldName' - The name of the field that contains the document data to index.
newSalesforceStandardKnowledgeArticleTypeConfiguration ::
  -- | 'documentDataFieldName'
  Prelude.Text ->
  SalesforceStandardKnowledgeArticleTypeConfiguration
newSalesforceStandardKnowledgeArticleTypeConfiguration
  pDocumentDataFieldName_ =
    SalesforceStandardKnowledgeArticleTypeConfiguration'
      { fieldMappings =
          Prelude.Nothing,
        documentTitleFieldName =
          Prelude.Nothing,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

-- | Maps attributes or field names of the knowledge article to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Salesforce fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Salesforce data source field names must exist in your Salesforce
-- custom metadata.
salesforceStandardKnowledgeArticleTypeConfiguration_fieldMappings :: Lens.Lens' SalesforceStandardKnowledgeArticleTypeConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
salesforceStandardKnowledgeArticleTypeConfiguration_fieldMappings = Lens.lens (\SalesforceStandardKnowledgeArticleTypeConfiguration' {fieldMappings} -> fieldMappings) (\s@SalesforceStandardKnowledgeArticleTypeConfiguration' {} a -> s {fieldMappings = a} :: SalesforceStandardKnowledgeArticleTypeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the field that contains the document title.
salesforceStandardKnowledgeArticleTypeConfiguration_documentTitleFieldName :: Lens.Lens' SalesforceStandardKnowledgeArticleTypeConfiguration (Prelude.Maybe Prelude.Text)
salesforceStandardKnowledgeArticleTypeConfiguration_documentTitleFieldName = Lens.lens (\SalesforceStandardKnowledgeArticleTypeConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SalesforceStandardKnowledgeArticleTypeConfiguration' {} a -> s {documentTitleFieldName = a} :: SalesforceStandardKnowledgeArticleTypeConfiguration)

-- | The name of the field that contains the document data to index.
salesforceStandardKnowledgeArticleTypeConfiguration_documentDataFieldName :: Lens.Lens' SalesforceStandardKnowledgeArticleTypeConfiguration Prelude.Text
salesforceStandardKnowledgeArticleTypeConfiguration_documentDataFieldName = Lens.lens (\SalesforceStandardKnowledgeArticleTypeConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@SalesforceStandardKnowledgeArticleTypeConfiguration' {} a -> s {documentDataFieldName = a} :: SalesforceStandardKnowledgeArticleTypeConfiguration)

instance
  Core.FromJSON
    SalesforceStandardKnowledgeArticleTypeConfiguration
  where
  parseJSON =
    Core.withObject
      "SalesforceStandardKnowledgeArticleTypeConfiguration"
      ( \x ->
          SalesforceStandardKnowledgeArticleTypeConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
              Prelude.<*> (x Core..:? "DocumentTitleFieldName")
              Prelude.<*> (x Core..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    SalesforceStandardKnowledgeArticleTypeConfiguration
  where
  hashWithSalt
    _salt
    SalesforceStandardKnowledgeArticleTypeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` fieldMappings
        `Prelude.hashWithSalt` documentTitleFieldName
        `Prelude.hashWithSalt` documentDataFieldName

instance
  Prelude.NFData
    SalesforceStandardKnowledgeArticleTypeConfiguration
  where
  rnf
    SalesforceStandardKnowledgeArticleTypeConfiguration' {..} =
      Prelude.rnf fieldMappings
        `Prelude.seq` Prelude.rnf documentTitleFieldName
        `Prelude.seq` Prelude.rnf documentDataFieldName

instance
  Core.ToJSON
    SalesforceStandardKnowledgeArticleTypeConfiguration
  where
  toJSON
    SalesforceStandardKnowledgeArticleTypeConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
              ("DocumentTitleFieldName" Core..=)
                Prelude.<$> documentTitleFieldName,
              Prelude.Just
                ( "DocumentDataFieldName"
                    Core..= documentDataFieldName
                )
            ]
        )
