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
-- Module      : Network.AWS.Kendra.Types.SalesforceChatterFeedConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SalesforceChatterFeedConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import Network.AWS.Kendra.Types.SalesforceChatterFeedIncludeFilterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines configuration for syncing a Salesforce chatter feed. The
-- contents of the object comes from the Salesforce FeedItem table.
--
-- /See:/ 'newSalesforceChatterFeedConfiguration' smart constructor.
data SalesforceChatterFeedConfiguration = SalesforceChatterFeedConfiguration'
  { -- | Maps fields from a Salesforce chatter feed into Amazon Kendra index
    -- fields.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | Filters the documents in the feed based on status of the user. When you
    -- specify @ACTIVE_USERS@ only documents from users who have an active
    -- account are indexed. When you specify @STANDARD_USER@ only documents for
    -- Salesforce standard users are documented. You can specify both.
    includeFilterTypes :: Prelude.Maybe (Prelude.NonEmpty SalesforceChatterFeedIncludeFilterType),
    -- | The name of the column in the Salesforce FeedItem table that contains
    -- the title of the document. This is typically the @Title@ column.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
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
-- 'fieldMappings', 'salesforceChatterFeedConfiguration_fieldMappings' - Maps fields from a Salesforce chatter feed into Amazon Kendra index
-- fields.
--
-- 'includeFilterTypes', 'salesforceChatterFeedConfiguration_includeFilterTypes' - Filters the documents in the feed based on status of the user. When you
-- specify @ACTIVE_USERS@ only documents from users who have an active
-- account are indexed. When you specify @STANDARD_USER@ only documents for
-- Salesforce standard users are documented. You can specify both.
--
-- 'documentTitleFieldName', 'salesforceChatterFeedConfiguration_documentTitleFieldName' - The name of the column in the Salesforce FeedItem table that contains
-- the title of the document. This is typically the @Title@ column.
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
      { fieldMappings =
          Prelude.Nothing,
        includeFilterTypes = Prelude.Nothing,
        documentTitleFieldName =
          Prelude.Nothing,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

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
-- the title of the document. This is typically the @Title@ column.
salesforceChatterFeedConfiguration_documentTitleFieldName :: Lens.Lens' SalesforceChatterFeedConfiguration (Prelude.Maybe Prelude.Text)
salesforceChatterFeedConfiguration_documentTitleFieldName = Lens.lens (\SalesforceChatterFeedConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SalesforceChatterFeedConfiguration' {} a -> s {documentTitleFieldName = a} :: SalesforceChatterFeedConfiguration)

-- | The name of the column in the Salesforce FeedItem table that contains
-- the content to index. Typically this is the @Body@ column.
salesforceChatterFeedConfiguration_documentDataFieldName :: Lens.Lens' SalesforceChatterFeedConfiguration Prelude.Text
salesforceChatterFeedConfiguration_documentDataFieldName = Lens.lens (\SalesforceChatterFeedConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@SalesforceChatterFeedConfiguration' {} a -> s {documentDataFieldName = a} :: SalesforceChatterFeedConfiguration)

instance
  Core.FromJSON
    SalesforceChatterFeedConfiguration
  where
  parseJSON =
    Core.withObject
      "SalesforceChatterFeedConfiguration"
      ( \x ->
          SalesforceChatterFeedConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
            Prelude.<*> (x Core..:? "IncludeFilterTypes")
            Prelude.<*> (x Core..:? "DocumentTitleFieldName")
            Prelude.<*> (x Core..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    SalesforceChatterFeedConfiguration

instance
  Prelude.NFData
    SalesforceChatterFeedConfiguration

instance
  Core.ToJSON
    SalesforceChatterFeedConfiguration
  where
  toJSON SalesforceChatterFeedConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
            ("IncludeFilterTypes" Core..=)
              Prelude.<$> includeFilterTypes,
            ("DocumentTitleFieldName" Core..=)
              Prelude.<$> documentTitleFieldName,
            Prelude.Just
              ( "DocumentDataFieldName"
                  Core..= documentDataFieldName
              )
          ]
      )
