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
-- Module      : Network.AWS.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for indexing Salesforce custom
-- articles.
--
-- /See:/ 'newSalesforceCustomKnowledgeArticleTypeConfiguration' smart constructor.
data SalesforceCustomKnowledgeArticleTypeConfiguration = SalesforceCustomKnowledgeArticleTypeConfiguration'
  { -- | One or more objects that map fields in the custom knowledge article to
    -- fields in the Amazon Kendra index.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The name of the field in the custom knowledge article that contains the
    -- document title.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
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
-- 'fieldMappings', 'salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings' - One or more objects that map fields in the custom knowledge article to
-- fields in the Amazon Kendra index.
--
-- 'documentTitleFieldName', 'salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName' - The name of the field in the custom knowledge article that contains the
-- document title.
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
      { fieldMappings =
          Prelude.Nothing,
        documentTitleFieldName =
          Prelude.Nothing,
        name = pName_,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

-- | One or more objects that map fields in the custom knowledge article to
-- fields in the Amazon Kendra index.
salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings :: Lens.Lens' SalesforceCustomKnowledgeArticleTypeConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings = Lens.lens (\SalesforceCustomKnowledgeArticleTypeConfiguration' {fieldMappings} -> fieldMappings) (\s@SalesforceCustomKnowledgeArticleTypeConfiguration' {} a -> s {fieldMappings = a} :: SalesforceCustomKnowledgeArticleTypeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the field in the custom knowledge article that contains the
-- document title.
salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName :: Lens.Lens' SalesforceCustomKnowledgeArticleTypeConfiguration (Prelude.Maybe Prelude.Text)
salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName = Lens.lens (\SalesforceCustomKnowledgeArticleTypeConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SalesforceCustomKnowledgeArticleTypeConfiguration' {} a -> s {documentTitleFieldName = a} :: SalesforceCustomKnowledgeArticleTypeConfiguration)

-- | The name of the configuration.
salesforceCustomKnowledgeArticleTypeConfiguration_name :: Lens.Lens' SalesforceCustomKnowledgeArticleTypeConfiguration Prelude.Text
salesforceCustomKnowledgeArticleTypeConfiguration_name = Lens.lens (\SalesforceCustomKnowledgeArticleTypeConfiguration' {name} -> name) (\s@SalesforceCustomKnowledgeArticleTypeConfiguration' {} a -> s {name = a} :: SalesforceCustomKnowledgeArticleTypeConfiguration)

-- | The name of the field in the custom knowledge article that contains the
-- document data to index.
salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName :: Lens.Lens' SalesforceCustomKnowledgeArticleTypeConfiguration Prelude.Text
salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName = Lens.lens (\SalesforceCustomKnowledgeArticleTypeConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@SalesforceCustomKnowledgeArticleTypeConfiguration' {} a -> s {documentDataFieldName = a} :: SalesforceCustomKnowledgeArticleTypeConfiguration)

instance
  Core.FromJSON
    SalesforceCustomKnowledgeArticleTypeConfiguration
  where
  parseJSON =
    Core.withObject
      "SalesforceCustomKnowledgeArticleTypeConfiguration"
      ( \x ->
          SalesforceCustomKnowledgeArticleTypeConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
              Prelude.<*> (x Core..:? "DocumentTitleFieldName")
              Prelude.<*> (x Core..: "Name")
              Prelude.<*> (x Core..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    SalesforceCustomKnowledgeArticleTypeConfiguration

instance
  Prelude.NFData
    SalesforceCustomKnowledgeArticleTypeConfiguration

instance
  Core.ToJSON
    SalesforceCustomKnowledgeArticleTypeConfiguration
  where
  toJSON
    SalesforceCustomKnowledgeArticleTypeConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
              ("DocumentTitleFieldName" Core..=)
                Prelude.<$> documentTitleFieldName,
              Prelude.Just ("Name" Core..= name),
              Prelude.Just
                ( "DocumentDataFieldName"
                    Core..= documentDataFieldName
                )
            ]
        )
