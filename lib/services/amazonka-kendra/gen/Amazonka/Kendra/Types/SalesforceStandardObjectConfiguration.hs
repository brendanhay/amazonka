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
-- Module      : Amazonka.Kendra.Types.SalesforceStandardObjectConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SalesforceStandardObjectConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.SalesforceStandardObjectName
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for indexing a single standard
-- object.
--
-- /See:/ 'newSalesforceStandardObjectConfiguration' smart constructor.
data SalesforceStandardObjectConfiguration = SalesforceStandardObjectConfiguration'
  { -- | The name of the field in the standard object table that contains the
    -- document title.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
    -- | Maps attributes or field names of the standard object to Amazon Kendra
    -- index field names. To create custom fields, use the @UpdateIndex@ API
    -- before you map to Salesforce fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Salesforce data source field names must exist in your Salesforce
    -- custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The name of the standard object.
    name :: SalesforceStandardObjectName,
    -- | The name of the field in the standard object table that contains the
    -- document contents.
    documentDataFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceStandardObjectConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentTitleFieldName', 'salesforceStandardObjectConfiguration_documentTitleFieldName' - The name of the field in the standard object table that contains the
-- document title.
--
-- 'fieldMappings', 'salesforceStandardObjectConfiguration_fieldMappings' - Maps attributes or field names of the standard object to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Salesforce fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Salesforce data source field names must exist in your Salesforce
-- custom metadata.
--
-- 'name', 'salesforceStandardObjectConfiguration_name' - The name of the standard object.
--
-- 'documentDataFieldName', 'salesforceStandardObjectConfiguration_documentDataFieldName' - The name of the field in the standard object table that contains the
-- document contents.
newSalesforceStandardObjectConfiguration ::
  -- | 'name'
  SalesforceStandardObjectName ->
  -- | 'documentDataFieldName'
  Prelude.Text ->
  SalesforceStandardObjectConfiguration
newSalesforceStandardObjectConfiguration
  pName_
  pDocumentDataFieldName_ =
    SalesforceStandardObjectConfiguration'
      { documentTitleFieldName =
          Prelude.Nothing,
        fieldMappings = Prelude.Nothing,
        name = pName_,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

-- | The name of the field in the standard object table that contains the
-- document title.
salesforceStandardObjectConfiguration_documentTitleFieldName :: Lens.Lens' SalesforceStandardObjectConfiguration (Prelude.Maybe Prelude.Text)
salesforceStandardObjectConfiguration_documentTitleFieldName = Lens.lens (\SalesforceStandardObjectConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SalesforceStandardObjectConfiguration' {} a -> s {documentTitleFieldName = a} :: SalesforceStandardObjectConfiguration)

-- | Maps attributes or field names of the standard object to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Salesforce fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Salesforce data source field names must exist in your Salesforce
-- custom metadata.
salesforceStandardObjectConfiguration_fieldMappings :: Lens.Lens' SalesforceStandardObjectConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
salesforceStandardObjectConfiguration_fieldMappings = Lens.lens (\SalesforceStandardObjectConfiguration' {fieldMappings} -> fieldMappings) (\s@SalesforceStandardObjectConfiguration' {} a -> s {fieldMappings = a} :: SalesforceStandardObjectConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the standard object.
salesforceStandardObjectConfiguration_name :: Lens.Lens' SalesforceStandardObjectConfiguration SalesforceStandardObjectName
salesforceStandardObjectConfiguration_name = Lens.lens (\SalesforceStandardObjectConfiguration' {name} -> name) (\s@SalesforceStandardObjectConfiguration' {} a -> s {name = a} :: SalesforceStandardObjectConfiguration)

-- | The name of the field in the standard object table that contains the
-- document contents.
salesforceStandardObjectConfiguration_documentDataFieldName :: Lens.Lens' SalesforceStandardObjectConfiguration Prelude.Text
salesforceStandardObjectConfiguration_documentDataFieldName = Lens.lens (\SalesforceStandardObjectConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@SalesforceStandardObjectConfiguration' {} a -> s {documentDataFieldName = a} :: SalesforceStandardObjectConfiguration)

instance
  Data.FromJSON
    SalesforceStandardObjectConfiguration
  where
  parseJSON =
    Data.withObject
      "SalesforceStandardObjectConfiguration"
      ( \x ->
          SalesforceStandardObjectConfiguration'
            Prelude.<$> (x Data..:? "DocumentTitleFieldName")
            Prelude.<*> (x Data..:? "FieldMappings")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    SalesforceStandardObjectConfiguration
  where
  hashWithSalt
    _salt
    SalesforceStandardObjectConfiguration' {..} =
      _salt `Prelude.hashWithSalt` documentTitleFieldName
        `Prelude.hashWithSalt` fieldMappings
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` documentDataFieldName

instance
  Prelude.NFData
    SalesforceStandardObjectConfiguration
  where
  rnf SalesforceStandardObjectConfiguration' {..} =
    Prelude.rnf documentTitleFieldName
      `Prelude.seq` Prelude.rnf fieldMappings
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf documentDataFieldName

instance
  Data.ToJSON
    SalesforceStandardObjectConfiguration
  where
  toJSON SalesforceStandardObjectConfiguration' {..} =
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
