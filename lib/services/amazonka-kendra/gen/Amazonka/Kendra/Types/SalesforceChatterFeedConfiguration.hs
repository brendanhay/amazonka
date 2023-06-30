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
-- Module      : Amazonka.Kendra.Types.SalesforceChatterFeedConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SalesforceChatterFeedConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.SalesforceChatterFeedIncludeFilterType
import qualified Amazonka.Prelude as Prelude

-- | The configuration information for syncing a Salesforce chatter feed. The
-- contents of the object comes from the Salesforce FeedItem table.
--
-- /See:/ 'newSalesforceChatterFeedConfiguration' smart constructor.
data SalesforceChatterFeedConfiguration = SalesforceChatterFeedConfiguration'
  { -- | The name of the column in the Salesforce FeedItem table that contains
    -- the title of the document. This is typically the @Title@ column.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
    -- | Maps fields from a Salesforce chatter feed into Amazon Kendra index
    -- fields.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | Filters the documents in the feed based on status of the user. When you
    -- specify @ACTIVE_USERS@ only documents from users who have an active
    -- account are indexed. When you specify @STANDARD_USER@ only documents for
    -- Salesforce standard users are documented. You can specify both.
    includeFilterTypes :: Prelude.Maybe (Prelude.NonEmpty SalesforceChatterFeedIncludeFilterType),
    -- | The name of the column in the Salesforce FeedItem table that contains
    -- the content to index. Typically this is the @Body@ column.
    documentDataFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceChatterFeedConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentTitleFieldName', 'salesforceChatterFeedConfiguration_documentTitleFieldName' - The name of the column in the Salesforce FeedItem table that contains
-- the title of the document. This is typically the @Title@ column.
--
-- 'fieldMappings', 'salesforceChatterFeedConfiguration_fieldMappings' - Maps fields from a Salesforce chatter feed into Amazon Kendra index
-- fields.
--
-- 'includeFilterTypes', 'salesforceChatterFeedConfiguration_includeFilterTypes' - Filters the documents in the feed based on status of the user. When you
-- specify @ACTIVE_USERS@ only documents from users who have an active
-- account are indexed. When you specify @STANDARD_USER@ only documents for
-- Salesforce standard users are documented. You can specify both.
--
-- 'documentDataFieldName', 'salesforceChatterFeedConfiguration_documentDataFieldName' - The name of the column in the Salesforce FeedItem table that contains
-- the content to index. Typically this is the @Body@ column.
newSalesforceChatterFeedConfiguration ::
  -- | 'documentDataFieldName'
  Prelude.Text ->
  SalesforceChatterFeedConfiguration
newSalesforceChatterFeedConfiguration
  pDocumentDataFieldName_ =
    SalesforceChatterFeedConfiguration'
      { documentTitleFieldName =
          Prelude.Nothing,
        fieldMappings = Prelude.Nothing,
        includeFilterTypes = Prelude.Nothing,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

-- | The name of the column in the Salesforce FeedItem table that contains
-- the title of the document. This is typically the @Title@ column.
salesforceChatterFeedConfiguration_documentTitleFieldName :: Lens.Lens' SalesforceChatterFeedConfiguration (Prelude.Maybe Prelude.Text)
salesforceChatterFeedConfiguration_documentTitleFieldName = Lens.lens (\SalesforceChatterFeedConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SalesforceChatterFeedConfiguration' {} a -> s {documentTitleFieldName = a} :: SalesforceChatterFeedConfiguration)

-- | Maps fields from a Salesforce chatter feed into Amazon Kendra index
-- fields.
salesforceChatterFeedConfiguration_fieldMappings :: Lens.Lens' SalesforceChatterFeedConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
salesforceChatterFeedConfiguration_fieldMappings = Lens.lens (\SalesforceChatterFeedConfiguration' {fieldMappings} -> fieldMappings) (\s@SalesforceChatterFeedConfiguration' {} a -> s {fieldMappings = a} :: SalesforceChatterFeedConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Filters the documents in the feed based on status of the user. When you
-- specify @ACTIVE_USERS@ only documents from users who have an active
-- account are indexed. When you specify @STANDARD_USER@ only documents for
-- Salesforce standard users are documented. You can specify both.
salesforceChatterFeedConfiguration_includeFilterTypes :: Lens.Lens' SalesforceChatterFeedConfiguration (Prelude.Maybe (Prelude.NonEmpty SalesforceChatterFeedIncludeFilterType))
salesforceChatterFeedConfiguration_includeFilterTypes = Lens.lens (\SalesforceChatterFeedConfiguration' {includeFilterTypes} -> includeFilterTypes) (\s@SalesforceChatterFeedConfiguration' {} a -> s {includeFilterTypes = a} :: SalesforceChatterFeedConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the column in the Salesforce FeedItem table that contains
-- the content to index. Typically this is the @Body@ column.
salesforceChatterFeedConfiguration_documentDataFieldName :: Lens.Lens' SalesforceChatterFeedConfiguration Prelude.Text
salesforceChatterFeedConfiguration_documentDataFieldName = Lens.lens (\SalesforceChatterFeedConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@SalesforceChatterFeedConfiguration' {} a -> s {documentDataFieldName = a} :: SalesforceChatterFeedConfiguration)

instance
  Data.FromJSON
    SalesforceChatterFeedConfiguration
  where
  parseJSON =
    Data.withObject
      "SalesforceChatterFeedConfiguration"
      ( \x ->
          SalesforceChatterFeedConfiguration'
            Prelude.<$> (x Data..:? "DocumentTitleFieldName")
            Prelude.<*> (x Data..:? "FieldMappings")
            Prelude.<*> (x Data..:? "IncludeFilterTypes")
            Prelude.<*> (x Data..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    SalesforceChatterFeedConfiguration
  where
  hashWithSalt
    _salt
    SalesforceChatterFeedConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` documentTitleFieldName
        `Prelude.hashWithSalt` fieldMappings
        `Prelude.hashWithSalt` includeFilterTypes
        `Prelude.hashWithSalt` documentDataFieldName

instance
  Prelude.NFData
    SalesforceChatterFeedConfiguration
  where
  rnf SalesforceChatterFeedConfiguration' {..} =
    Prelude.rnf documentTitleFieldName
      `Prelude.seq` Prelude.rnf fieldMappings
      `Prelude.seq` Prelude.rnf includeFilterTypes
      `Prelude.seq` Prelude.rnf documentDataFieldName

instance
  Data.ToJSON
    SalesforceChatterFeedConfiguration
  where
  toJSON SalesforceChatterFeedConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentTitleFieldName" Data..=)
              Prelude.<$> documentTitleFieldName,
            ("FieldMappings" Data..=) Prelude.<$> fieldMappings,
            ("IncludeFilterTypes" Data..=)
              Prelude.<$> includeFilterTypes,
            Prelude.Just
              ( "DocumentDataFieldName"
                  Data..= documentDataFieldName
              )
          ]
      )
