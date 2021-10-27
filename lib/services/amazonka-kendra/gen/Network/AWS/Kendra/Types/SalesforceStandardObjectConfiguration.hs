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
-- Module      : Network.AWS.Kendra.Types.SalesforceStandardObjectConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SalesforceStandardObjectConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import Network.AWS.Kendra.Types.SalesforceStandardObjectName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies configuration information for indexing a single standard
-- object.
--
-- /See:/ 'newSalesforceStandardObjectConfiguration' smart constructor.
data SalesforceStandardObjectConfiguration = SalesforceStandardObjectConfiguration'
  { -- | One or more objects that map fields in the standard object to Amazon
    -- Kendra index fields. The index field must exist before you can map a
    -- Salesforce field to it.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The name of the field in the standard object table that contains the
    -- document title.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text,
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
-- 'fieldMappings', 'salesforceStandardObjectConfiguration_fieldMappings' - One or more objects that map fields in the standard object to Amazon
-- Kendra index fields. The index field must exist before you can map a
-- Salesforce field to it.
--
-- 'documentTitleFieldName', 'salesforceStandardObjectConfiguration_documentTitleFieldName' - The name of the field in the standard object table that contains the
-- document title.
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
      { fieldMappings =
          Prelude.Nothing,
        documentTitleFieldName =
          Prelude.Nothing,
        name = pName_,
        documentDataFieldName =
          pDocumentDataFieldName_
      }

-- | One or more objects that map fields in the standard object to Amazon
-- Kendra index fields. The index field must exist before you can map a
-- Salesforce field to it.
salesforceStandardObjectConfiguration_fieldMappings :: Lens.Lens' SalesforceStandardObjectConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
salesforceStandardObjectConfiguration_fieldMappings = Lens.lens (\SalesforceStandardObjectConfiguration' {fieldMappings} -> fieldMappings) (\s@SalesforceStandardObjectConfiguration' {} a -> s {fieldMappings = a} :: SalesforceStandardObjectConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the field in the standard object table that contains the
-- document title.
salesforceStandardObjectConfiguration_documentTitleFieldName :: Lens.Lens' SalesforceStandardObjectConfiguration (Prelude.Maybe Prelude.Text)
salesforceStandardObjectConfiguration_documentTitleFieldName = Lens.lens (\SalesforceStandardObjectConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SalesforceStandardObjectConfiguration' {} a -> s {documentTitleFieldName = a} :: SalesforceStandardObjectConfiguration)

-- | The name of the standard object.
salesforceStandardObjectConfiguration_name :: Lens.Lens' SalesforceStandardObjectConfiguration SalesforceStandardObjectName
salesforceStandardObjectConfiguration_name = Lens.lens (\SalesforceStandardObjectConfiguration' {name} -> name) (\s@SalesforceStandardObjectConfiguration' {} a -> s {name = a} :: SalesforceStandardObjectConfiguration)

-- | The name of the field in the standard object table that contains the
-- document contents.
salesforceStandardObjectConfiguration_documentDataFieldName :: Lens.Lens' SalesforceStandardObjectConfiguration Prelude.Text
salesforceStandardObjectConfiguration_documentDataFieldName = Lens.lens (\SalesforceStandardObjectConfiguration' {documentDataFieldName} -> documentDataFieldName) (\s@SalesforceStandardObjectConfiguration' {} a -> s {documentDataFieldName = a} :: SalesforceStandardObjectConfiguration)

instance
  Core.FromJSON
    SalesforceStandardObjectConfiguration
  where
  parseJSON =
    Core.withObject
      "SalesforceStandardObjectConfiguration"
      ( \x ->
          SalesforceStandardObjectConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
            Prelude.<*> (x Core..:? "DocumentTitleFieldName")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "DocumentDataFieldName")
      )

instance
  Prelude.Hashable
    SalesforceStandardObjectConfiguration

instance
  Prelude.NFData
    SalesforceStandardObjectConfiguration

instance
  Core.ToJSON
    SalesforceStandardObjectConfiguration
  where
  toJSON SalesforceStandardObjectConfiguration' {..} =
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
