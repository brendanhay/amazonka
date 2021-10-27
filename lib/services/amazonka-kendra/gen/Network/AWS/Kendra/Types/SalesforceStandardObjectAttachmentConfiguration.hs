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
-- Module      : Network.AWS.Kendra.Types.SalesforceStandardObjectAttachmentConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SalesforceStandardObjectAttachmentConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for processing attachments to
-- Salesforce standard objects.
--
-- /See:/ 'newSalesforceStandardObjectAttachmentConfiguration' smart constructor.
data SalesforceStandardObjectAttachmentConfiguration = SalesforceStandardObjectAttachmentConfiguration'
  { -- | One or more objects that map fields in attachments to Amazon Kendra
    -- index fields.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The name of the field used for the document title.
    documentTitleFieldName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceStandardObjectAttachmentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldMappings', 'salesforceStandardObjectAttachmentConfiguration_fieldMappings' - One or more objects that map fields in attachments to Amazon Kendra
-- index fields.
--
-- 'documentTitleFieldName', 'salesforceStandardObjectAttachmentConfiguration_documentTitleFieldName' - The name of the field used for the document title.
newSalesforceStandardObjectAttachmentConfiguration ::
  SalesforceStandardObjectAttachmentConfiguration
newSalesforceStandardObjectAttachmentConfiguration =
  SalesforceStandardObjectAttachmentConfiguration'
    { fieldMappings =
        Prelude.Nothing,
      documentTitleFieldName =
        Prelude.Nothing
    }

-- | One or more objects that map fields in attachments to Amazon Kendra
-- index fields.
salesforceStandardObjectAttachmentConfiguration_fieldMappings :: Lens.Lens' SalesforceStandardObjectAttachmentConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
salesforceStandardObjectAttachmentConfiguration_fieldMappings = Lens.lens (\SalesforceStandardObjectAttachmentConfiguration' {fieldMappings} -> fieldMappings) (\s@SalesforceStandardObjectAttachmentConfiguration' {} a -> s {fieldMappings = a} :: SalesforceStandardObjectAttachmentConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the field used for the document title.
salesforceStandardObjectAttachmentConfiguration_documentTitleFieldName :: Lens.Lens' SalesforceStandardObjectAttachmentConfiguration (Prelude.Maybe Prelude.Text)
salesforceStandardObjectAttachmentConfiguration_documentTitleFieldName = Lens.lens (\SalesforceStandardObjectAttachmentConfiguration' {documentTitleFieldName} -> documentTitleFieldName) (\s@SalesforceStandardObjectAttachmentConfiguration' {} a -> s {documentTitleFieldName = a} :: SalesforceStandardObjectAttachmentConfiguration)

instance
  Core.FromJSON
    SalesforceStandardObjectAttachmentConfiguration
  where
  parseJSON =
    Core.withObject
      "SalesforceStandardObjectAttachmentConfiguration"
      ( \x ->
          SalesforceStandardObjectAttachmentConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
              Prelude.<*> (x Core..:? "DocumentTitleFieldName")
      )

instance
  Prelude.Hashable
    SalesforceStandardObjectAttachmentConfiguration

instance
  Prelude.NFData
    SalesforceStandardObjectAttachmentConfiguration

instance
  Core.ToJSON
    SalesforceStandardObjectAttachmentConfiguration
  where
  toJSON
    SalesforceStandardObjectAttachmentConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
              ("DocumentTitleFieldName" Core..=)
                Prelude.<$> documentTitleFieldName
            ]
        )
