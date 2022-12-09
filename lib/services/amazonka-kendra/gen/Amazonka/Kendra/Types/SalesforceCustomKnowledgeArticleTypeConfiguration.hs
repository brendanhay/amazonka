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
-- Module      : Amazonka.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for indexing Salesforce custom
-- articles.
--
-- /See:/ 'newSalesforceCustomKnowledgeArticleTypeConfiguration' smart constructor.
data SalesforceCustomKnowledgeArticleTypeConfiguration = SalesforceCustomKnowledgeArticleTypeConfiguration'
  { -- | The name of the field in the custom knowledge article that contains the
    -- document title.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
    -- | Maps attributes or field names of the custom knowledge article to Amazon
    -- Kendra index field names. To create custom fields, use the @UpdateIndex@
    -- API before you map to Salesforce fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Salesforce data source field names must exist in your Salesforce
    -- custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The name of the configuration.
    name :: Prelude.Text,
    -- | The name of the field in the custom knowledge article that contains the
    -- document data to index.
    documentDataFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceCustomKnowledgeArticleTypeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentTitleFieldName', 'salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName' - The name of the field in the custom knowledge article that contains the
-- document title.
--
-- 'fieldMappings', 'salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings' - Maps attributes or field names of the custom knowledge article to Amazon
-- Kendra index field names. To create custom fields, use the @UpdateIndex@
-- API before you map to Salesforce fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Salesforce data source field names must exist in your Salesforce
-- custom metadata.
--
-- 'name', 'salesforceCustomKnowledgeArticleTypeConfiguration_name' - The name of the configuration.
--
-- 'documentDataFieldName', 'salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName' - The name of the field in the custom knowledge article that contains the
-- document data to index.
newSalesforceCustomKnowledgeArticleTypeConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'documentDataFieldName'
  Prelude.Text ->
  SalesforceCustomKnowledgeArticleTypeConfiguration
newSalesforceCustomKnowledgeArticleTypeConfiguration
  pName_
  pDocumentDataFieldName_ =
    SalesforceCustomKnowledgeArticleTypeConfiguration'
      { documentTitleFieldName =
          Prelude.Nothing,
        fieldMappings =
          Prelude.Nothing,
        name = pName_,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

-- | The name of the field in the custom knowledge article that contains the
-- document title.
salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName :: Lens.Lens' SalesforceCustomKnowledgeArticleTypeConfiguration (Prelude.Maybe Prelude.Text)
salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName = Lens.lens (\SalesforceCustomKnowledgeArticleTypeConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SalesforceCustomKnowledgeArticleTypeConfiguration' {} a -> s {documentTitleFieldName = a} :: SalesforceCustomKnowledgeArticleTypeConfiguration)

-- | Maps attributes or field names of the custom knowledge article to Amazon
-- Kendra index field names. To create custom fields, use the @UpdateIndex@
-- API before you map to Salesforce fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Salesforce data source field names must exist in your Salesforce
-- custom metadata.
salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings :: Lens.Lens' SalesforceCustomKnowledgeArticleTypeConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings = Lens.lens (\SalesforceCustomKnowledgeArticleTypeConfiguration' {fieldMappings} -> fieldMappings) (\s@SalesforceCustomKnowledgeArticleTypeConfiguration' {} a -> s {fieldMappings = a} :: SalesforceCustomKnowledgeArticleTypeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the configuration.
salesforceCustomKnowledgeArticleTypeConfiguration_name :: Lens.Lens' SalesforceCustomKnowledgeArticleTypeConfiguration Prelude.Text
salesforceCustomKnowledgeArticleTypeConfiguration_name = Lens.lens (\SalesforceCustomKnowledgeArticleTypeConfiguration' {name} -> name) (\s@SalesforceCustomKnowledgeArticleTypeConfiguration' {} a -> s {name = a} :: SalesforceCustomKnowledgeArticleTypeConfiguration)

-- | The name of the field in the custom knowledge article that contains the
-- document data to index.
salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName :: Lens.Lens' SalesforceCustomKnowledgeArticleTypeConfiguration Prelude.Text
salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName = Lens.lens (\SalesforceCustomKnowledgeArticleTypeConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@SalesforceCustomKnowledgeArticleTypeConfiguration' {} a -> s {documentDataFieldName = a} :: SalesforceCustomKnowledgeArticleTypeConfiguration)

instance
  Data.FromJSON
    SalesforceCustomKnowledgeArticleTypeConfiguration
  where
  parseJSON =
    Data.withObject
      "SalesforceCustomKnowledgeArticleTypeConfiguration"
      ( \x ->
          SalesforceCustomKnowledgeArticleTypeConfiguration'
            Prelude.<$> (x Data..:? "DocumentTitleFieldName")
              Prelude.<*> (x Data..:? "FieldMappings")
              Prelude.<*> (x Data..: "Name")
              Prelude.<*> (x Data..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    SalesforceCustomKnowledgeArticleTypeConfiguration
  where
  hashWithSalt
    _salt
    SalesforceCustomKnowledgeArticleTypeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` documentTitleFieldName
        `Prelude.hashWithSalt` fieldMappings
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` documentDataFieldName

instance
  Prelude.NFData
    SalesforceCustomKnowledgeArticleTypeConfiguration
  where
  rnf
    SalesforceCustomKnowledgeArticleTypeConfiguration' {..} =
      Prelude.rnf documentTitleFieldName
        `Prelude.seq` Prelude.rnf fieldMappings
        `Prelude.seq` Prelude.rnf name
        `Prelude.seq` Prelude.rnf documentDataFieldName

instance
  Data.ToJSON
    SalesforceCustomKnowledgeArticleTypeConfiguration
  where
  toJSON
    SalesforceCustomKnowledgeArticleTypeConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DocumentTitleFieldName" Data..=)
                Prelude.<$> documentTitleFieldName,
              ("FieldMappings" Data..=) Prelude.<$> fieldMappings,
              Prelude.Just ("Name" Data..= name),
              Prelude.Just
                ( "DocumentDataFieldName"
                    Data..= documentDataFieldName
                )
            ]
        )
